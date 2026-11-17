(** Shared Google OAuth 2.0 infrastructure

    Provides PKCE, token exchange, refresh, revocation, and user info
    retrieval for all Google-based providers (YouTube, Google Business Profile).
*)

module Pkce = struct
  let generate_verifier () = Social_oauth_pkce.generate_verifier ()
  let generate_challenge = Social_oauth_pkce.challenge_s256
  let challenge_method = Social_oauth_pkce.challenge_method
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
  Social_token_expiry.expires_at_of_expires_in expires_in

(** {1 Typed reads} *)

(** The Google account behind a token, from the OAuth2 v2 userinfo
    endpoint ([GET https://www.googleapis.com/oauth2/v2/userinfo], schema
    [Userinfo] in
    {{:https://www.googleapis.com/discovery/v1/apis/oauth2/v2/rest}}).
    [email] needs the [userinfo.email] scope; the name and picture fields
    need [userinfo.profile]. *)
type userinfo = {
  id : string;  (** Obfuscated user id; the OpenID Connect [sub]. *)
  email : string option;
  verified_email : bool option;
  name : string option;
  given_name : string option;
  family_name : string option;
  picture : string option;
  locale : string option;
  hd : string option;  (** Hosted (Workspace) domain. *)
}

let json_text fields key =
  match List.assoc_opt key fields with
  | Some (`String value) when value <> "" -> Some value
  | Some (`String _ | `Null | `Bool _ | `Int _ | `Float _ | `List _ | `Assoc _) | None -> None

(** Parses a userinfo body. Accepts [sub] when [id] is absent (the OpenID
    Connect userinfo shape). [Error] without either. *)
let parse_userinfo body : (userinfo, string) result =
  match Yojson.Basic.from_string body with
  | exception Yojson.Json_error message -> Error ("userinfo response is not JSON: " ^ message)
  | `Assoc fields -> (
      let id =
        match json_text fields "id" with
        | Some _ as id -> id
        | None -> json_text fields "sub"
      in
      let verified =
        match List.assoc_opt "verified_email" fields, List.assoc_opt "email_verified" fields with
        | Some (`Bool verified), _ | None, Some (`Bool verified) -> Some verified
        | Some (`Null | `Int _ | `Float _ | `String _ | `List _ | `Assoc _), _
        | None, (Some (`Null | `Int _ | `Float _ | `String _ | `List _ | `Assoc _) | None) -> None
      in
      match id with
      | None -> Error "userinfo response has no id"
      | Some id ->
          Ok
            { id;
              email = json_text fields "email";
              verified_email = verified;
              name = json_text fields "name";
              given_name = json_text fields "given_name";
              family_name = json_text fields "family_name";
              picture = json_text fields "picture";
              locale = json_text fields "locale";
              hd = json_text fields "hd" })
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ -> Error "userinfo response is not an object"

(** The SDK error for a failed Google API call, from the standard error
    envelope [{"error": {"code", "message", "status", "errors": [{"reason"}]}}]
    ({{:https://cloud.google.com/apis/design/errors}}):
    - 401: [Auth_error Token_invalid];
    - 403 with reason [quotaExceeded], [rateLimitExceeded],
      [userRateLimitExceeded] or [dailyLimitExceeded], and any 429:
      [Rate_limited];
    - other 403: [Auth_error (Insufficient_permissions {required =
      required_scopes; ...})];
    - 404: [Resource_not_found];
    - otherwise [Api_error] with the body kept. *)
let api_error ~platform ~required_scopes ~status_code body =
  let error_fields =
    match Yojson.Basic.from_string body with
    | `Assoc fields -> (
        match List.assoc_opt "error" fields with
        | Some (`Assoc error) -> error
        | Some (`Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _) | None -> [])
    | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ -> []
    | exception Yojson.Json_error _ -> []
  in
  let message = Option.value (json_text error_fields "message") ~default:body in
  let reasons =
    match List.assoc_opt "errors" error_fields with
    | Some (`List items) ->
        List.filter_map
          (function
            | `Assoc item -> json_text item "reason"
            | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ -> None)
          items
    | Some (`Null | `Bool _ | `Int _ | `Float _ | `String _ | `Assoc _) | None -> []
  in
  let rate_limit_reason =
    List.exists
      (fun reason ->
        List.mem reason [ "quotaExceeded"; "rateLimitExceeded"; "userRateLimitExceeded"; "dailyLimitExceeded" ])
      reasons
  in
  let rate_limited = Error_types.make_rate_limited ~remaining:0 () in
  match status_code with
  | 401 -> Error_types.Auth_error Error_types.Token_invalid
  | 429 -> rate_limited
  | 403 when rate_limit_reason -> rate_limited
  | 403 ->
      Error_types.Auth_error
        (Error_types.Insufficient_permissions { required = required_scopes; platform_message = Some message })
  | 404 -> Error_types.Resource_not_found message
  | _ -> Error_types.make_api_error ~platform ~status_code ~message ~raw_response:body ()

module Make (Http : Social_core.HTTP_CLIENT) = struct
  let exchange_code ~client_id ~client_secret ~redirect_uri ~code ~code_verifier on_success on_error =
    let body = Social_core.form_urlencode_kvs [
      ("grant_type", "authorization_code");
      ("code", code);
      ("redirect_uri", redirect_uri);
      ("client_id", client_id);
      ("client_secret", client_secret);
      ("code_verifier", code_verifier);
    ] in
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
            let scope =
              try Some (json |> member "scope" |> to_string)
              with _ -> None in
            let creds : Social_core.credentials = {
              access_token;
              refresh_token;
              expires_at = expires_at_of_expires_in expires_in;
              auth_type = Social_core.auth_type_of_string token_type_str;
              scope;
            } in
            on_success creds
          with e ->
            on_error (Printf.sprintf "Failed to parse token response: %s" (Printexc.to_string e))
        else
          on_error (Printf.sprintf "Token exchange failed (%d): %s" response.status response.body))
      on_error

  (** If Google does not return a new refresh token, the original is preserved.
      Likewise, if the refresh response omits [scope], the caller's
      [prior_scope] is preserved so the stored grant set never silently
      narrows on a routine refresh. *)
  let refresh_token ?prior_scope ~client_id ~client_secret ~refresh_token on_success on_error =
    let body = Social_core.form_urlencode_kvs [
      ("grant_type", "refresh_token");
      ("refresh_token", refresh_token);
      ("client_id", client_id);
      ("client_secret", client_secret);
    ] in
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
            let scope =
              match
                try Some (json |> member "scope" |> to_string) with _ -> None
              with
              | Some _ as s -> s
              | None -> prior_scope
            in
            let creds : Social_core.credentials = {
              access_token;
              refresh_token = Some new_refresh;
              expires_at = expires_at_of_expires_in expires_in;
              auth_type = Social_core.auth_type_of_string token_type_str;
              scope;
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

  (** Typed {!get_user_info}: [GET] {!Endpoints.userinfo} as {!userinfo},
      failures through {!api_error}. [platform] tags [Api_error]s. *)
  let get_userinfo ~platform ~access_token on_result =
    Http.get ~headers:[ ("Authorization", "Bearer " ^ access_token) ] Endpoints.userinfo
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          match parse_userinfo response.body with
          | Ok userinfo -> on_result (Ok userinfo)
          | Error message -> on_result (Error (Error_types.Internal_error message))
        else
          on_result
            (Error
               (api_error ~platform
                  ~required_scopes:[ "https://www.googleapis.com/auth/userinfo.profile" ]
                  ~status_code:response.status response.body)))
      (fun err -> on_result (Error (Error_types.Network_error (Error_types.Connection_failed err))))
end
