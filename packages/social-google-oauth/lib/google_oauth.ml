(** Shared Google OAuth 2.0 infrastructure

    Provides PKCE, token exchange, refresh, revocation, and user info
    retrieval for all Google-based providers (YouTube, Google Business Profile).
*)

module Pkce = struct
  let generate_challenge code_verifier =
    let digest = Digestif.SHA256.digest_string code_verifier in
    let raw = Digestif.SHA256.to_raw_string digest in
    Base64.encode_string ~pad:false raw
    |> String.map (function '+' -> '-' | '/' -> '_' | c -> c)

  let challenge_method = "S256"
end

module Endpoints = struct
  let authorization = "https://accounts.google.com/o/oauth2/v2/auth"
  let token = "https://oauth2.googleapis.com/token"
  let revocation = "https://oauth2.googleapis.com/revoke"
  let userinfo = "https://www.googleapis.com/oauth2/v2/userinfo"
end

let get_authorization_url ~client_id ~redirect_uri ~state ~code_verifier ~scopes =
  let code_challenge = Pkce.generate_challenge code_verifier in
  let scope_str = String.concat " " scopes in
  let params = [
    ("client_id", client_id);
    ("redirect_uri", redirect_uri);
    ("response_type", "code");
    ("scope", scope_str);
    ("state", state);
    ("access_type", "offline");
    ("prompt", "consent");
    ("code_challenge", code_challenge);
    ("code_challenge_method", Pkce.challenge_method);
  ] in
  let query = List.map (fun (k, v) ->
    Printf.sprintf "%s=%s" k (Uri.pct_encode v)
  ) params |> String.concat "&" in
  Printf.sprintf "%s?%s" Endpoints.authorization query

(** Parse expires_in from token response into RFC 3339 timestamp *)
let expires_at_of_expires_in expires_in =
  let now = Ptime_clock.now () in
  match Ptime.add_span now (Ptime.Span.of_int_s expires_in) with
  | Some exp -> Some (Ptime.to_rfc3339 exp)
  | None -> None

module Make (Http : Social_core.HTTP_CLIENT) = struct
  let exchange_code ~client_id ~client_secret ~redirect_uri ~code ~code_verifier on_success on_error =
    let body = Printf.sprintf
      "grant_type=authorization_code&code=%s&redirect_uri=%s&client_id=%s&client_secret=%s&code_verifier=%s"
      (Uri.pct_encode code)
      (Uri.pct_encode redirect_uri)
      (Uri.pct_encode client_id)
      (Uri.pct_encode client_secret)
      (Uri.pct_encode code_verifier)
    in
    let headers = [
      ("Content-Type", "application/x-www-form-urlencoded");
    ] in

    Http.post ~headers ~body Endpoints.token
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let open Yojson.Basic.Util in
            let json = Yojson.Basic.from_string response.body in
            let access_token = json |> member "access_token" |> to_string in
            let refresh_token =
              try Some (json |> member "refresh_token" |> to_string)
              with _ -> None in
            let expires_in =
              try json |> member "expires_in" |> to_int
              with _ -> 3600
            in
            let token_type_str =
              try json |> member "token_type" |> to_string
              with _ -> "Bearer" in
            let creds : Social_core.credentials = {
              access_token;
              refresh_token;
              expires_at = expires_at_of_expires_in expires_in;
              auth_type = Social_core.auth_type_of_string token_type_str;
            } in
            on_success creds
          with e ->
            on_error (Printf.sprintf "Failed to parse token response: %s" (Printexc.to_string e))
        else
          on_error (Printf.sprintf "Token exchange failed (%d): %s" response.status response.body))
      on_error

  (** If Google does not return a new refresh token, the original is preserved. *)
  let refresh_token ~client_id ~client_secret ~refresh_token on_success on_error =
    let body = Printf.sprintf
      "grant_type=refresh_token&refresh_token=%s&client_id=%s&client_secret=%s"
      (Uri.pct_encode refresh_token)
      (Uri.pct_encode client_id)
      (Uri.pct_encode client_secret)
    in
    let headers = [
      ("Content-Type", "application/x-www-form-urlencoded");
    ] in

    Http.post ~headers ~body Endpoints.token
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let open Yojson.Basic.Util in
            let json = Yojson.Basic.from_string response.body in
            let access_token = json |> member "access_token" |> to_string in
            let new_refresh =
              try json |> member "refresh_token" |> to_string
              with _ -> refresh_token
            in
            let expires_in = json |> member "expires_in" |> to_int in
            let token_type_str =
              try json |> member "token_type" |> to_string
              with _ -> "Bearer" in
            let creds : Social_core.credentials = {
              access_token;
              refresh_token = Some new_refresh;
              expires_at = expires_at_of_expires_in expires_in;
              auth_type = Social_core.auth_type_of_string token_type_str;
            } in
            on_success creds
          with e ->
            on_error (Printf.sprintf "Failed to parse token response: %s" (Printexc.to_string e))
        else
          on_error (Printf.sprintf "Token refresh failed (%d): %s" response.status response.body))
      on_error

  let revoke_token ~platform ~token on_result =
    let url = Printf.sprintf "%s?token=%s"
      Endpoints.revocation (Uri.pct_encode token) in

    Http.post ~headers:[] ~body:"" url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          on_result (Ok ())
        else
          on_result (Error (Error_types.Api_error {
            status_code = response.status;
            message = Printf.sprintf "Token revocation failed: %s" response.body;
            platform;
            raw_response = Some response.body;
            request_id = None;
          })))
      (fun err -> on_result (Error (Error_types.Internal_error err)))

  let get_user_info ~platform ~access_token on_result =
    let headers = [
      ("Authorization", "Bearer " ^ access_token);
    ] in

    Http.get ~headers Endpoints.userinfo
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let json = Yojson.Basic.from_string response.body in
            on_result (Ok json)
          with e ->
            on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse user info: %s" (Printexc.to_string e))))
        else
          on_result (Error (Error_types.Api_error {
            status_code = response.status;
            message = Printf.sprintf "Get user info failed: %s" response.body;
            platform;
            raw_response = Some response.body;
            request_id = None;
          })))
      (fun err -> on_result (Error (Error_types.Internal_error err)))
end
