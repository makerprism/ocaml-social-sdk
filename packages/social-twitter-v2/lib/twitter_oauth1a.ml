(** Twitter OAuth 1.0a signing and three-legged authorization flow.

    Pure signing functions have no HTTP dependency. The [Make] functor provides
    the three-legged flow (request_token, authorize URL, access_token exchange)
    using an HTTP_CLIENT. *)

(* ---------- RFC 5849 percent-encoding ---------- *)

let percent_encode s =
  let buf = Buffer.create (String.length s * 3) in
  String.iter (fun c ->
    match c with
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '.' | '_' | '~' ->
        Buffer.add_char buf c
    | _ ->
        Printf.bprintf buf "%%%02X" (Char.code c)
  ) s;
  Buffer.contents buf

(* ---------- Nonce / timestamp ---------- *)

let () = Random.self_init ()

let generate_nonce () =
  (* Combine timestamp, pid, and random values into a SHA-256 hash for uniqueness *)
  let seed = Printf.sprintf "%f-%d-%d-%d-%d"
    (Unix.gettimeofday ()) (Unix.getpid ())
    (Random.bits ()) (Random.bits ()) (Random.bits ()) in
  let hash = Digestif.SHA256.digest_string seed in
  Digestif.SHA256.to_hex hash

let generate_timestamp () =
  string_of_int (int_of_float (Unix.time ()))

(* ---------- Signature base string ---------- *)

let parse_query_params url =
  let uri = Uri.of_string url in
  match Uri.query uri with
  | [] -> []
  | pairs ->
      List.concat_map (fun (k, vs) ->
        match vs with
        | [] -> [(k, "")]
        | _ -> List.map (fun v -> (k, v)) vs
      ) pairs

let base_url_without_query url =
  let uri = Uri.of_string url in
  let uri_no_query = Uri.with_query uri [] in
  (* Also remove fragment *)
  let uri_clean = Uri.with_fragment uri_no_query None in
  Uri.to_string uri_clean

let signature_base_string ~http_method ~url ~params =
  let sorted = List.sort (fun (k1, v1) (k2, v2) ->
    let cmp = String.compare k1 k2 in
    if cmp <> 0 then cmp else String.compare v1 v2
  ) params in
  let param_string = String.concat "&"
    (List.map (fun (k, v) -> percent_encode k ^ "=" ^ percent_encode v) sorted) in
  let base_url = base_url_without_query url in
  String.uppercase_ascii http_method
  ^ "&" ^ percent_encode base_url
  ^ "&" ^ percent_encode param_string

(* ---------- HMAC-SHA1 signing ---------- *)

let sign ~consumer_secret ~token_secret ~base_string =
  let signing_key = percent_encode consumer_secret ^ "&" ^ percent_encode token_secret in
  let mac = Digestif.SHA1.hmac_string ~key:signing_key base_string in
  Base64.encode_exn (Digestif.SHA1.to_raw_string mac)

(* ---------- Authorization header ---------- *)

let authorization_header
    ~consumer_key ~consumer_secret
    ~token ~token_secret
    ~http_method ~url
    ?(extra_params=[])
    () =
  let nonce = generate_nonce () in
  let timestamp = generate_timestamp () in
  let oauth_params = [
    ("oauth_consumer_key", consumer_key);
    ("oauth_nonce", nonce);
    ("oauth_signature_method", "HMAC-SHA1");
    ("oauth_timestamp", timestamp);
    ("oauth_version", "1.0");
  ] @ (if token <> "" then [("oauth_token", token)] else []) in
  let query_params = parse_query_params url in
  let all_params = oauth_params @ query_params @ extra_params in
  let base_string = signature_base_string ~http_method ~url ~params:all_params in
  let signature = sign ~consumer_secret ~token_secret ~base_string in
  let signed_params = oauth_params @ [("oauth_signature", signature)] in
  let header_value = String.concat ", "
    (List.map (fun (k, v) ->
      Printf.sprintf "%s=\"%s\"" k (percent_encode v)
    ) signed_params) in
  "OAuth " ^ header_value

(* ---------- Response types ---------- *)

type request_token_result = {
  oauth_token : string;
  oauth_token_secret : string;
  oauth_callback_confirmed : bool;
}

type access_token_result = {
  oauth_token : string;
  oauth_token_secret : string;
  user_id : string;
  screen_name : string;
}

(* ---------- Form-encoded response parsing ---------- *)

let parse_form_encoded body =
  String.split_on_char '&' body
  |> List.filter_map (fun pair ->
    match String.split_on_char '=' pair with
    | [k; v] -> Some (Uri.pct_decode k, Uri.pct_decode v)
    | [k] -> Some (Uri.pct_decode k, "")
    | _ -> None)

let find_field fields key =
  match List.assoc_opt key fields with
  | Some v -> v
  | None -> failwith (Printf.sprintf "OAuth 1.0a response missing field: %s" key)

(* ---------- Three-legged auth flow functor ---------- *)

module Make (Http : Social_core.HTTP_CLIENT) = struct

  let get_request_token ~consumer_key ~consumer_secret ~callback_url on_success on_error =
    let url = "https://api.x.com/oauth/request_token" in
    let extra_params = [("oauth_callback", callback_url)] in
    let auth_header = authorization_header
      ~consumer_key ~consumer_secret
      ~token:"" ~token_secret:""
      ~http_method:"POST" ~url
      ~extra_params
      () in
    let body = "oauth_callback=" ^ percent_encode callback_url in
    Http.post
      ~headers:[
        ("Authorization", auth_header);
        ("Content-Type", "application/x-www-form-urlencoded");
      ]
      ~body
      url
      (fun response ->
        if response.Social_core.status >= 200 && response.status < 300 then begin
          try
            let fields = parse_form_encoded response.body in
            let result = {
              oauth_token = find_field fields "oauth_token";
              oauth_token_secret = find_field fields "oauth_token_secret";
              oauth_callback_confirmed =
                (try find_field fields "oauth_callback_confirmed" = "true"
                 with Failure _ -> false);
            } in
            on_success result
          with e -> on_error (Printf.sprintf "Failed to parse request_token response: %s" (Printexc.to_string e))
        end else
          on_error (Printf.sprintf "request_token failed (%d): %s" response.status response.body))
      on_error

  let get_authorize_url ~oauth_token ?(force_login=true) () =
    let base = "https://api.x.com/oauth/authorize" in
    let params = [("oauth_token", oauth_token)]
      @ (if force_login then [("force_login", "true")] else []) in
    let query = String.concat "&"
      (List.map (fun (k, v) -> Uri.pct_encode k ^ "=" ^ Uri.pct_encode v) params) in
    base ^ "?" ^ query

  let exchange_access_token ~consumer_key ~consumer_secret
      ~oauth_token ~oauth_token_secret ~oauth_verifier
      on_success on_error =
    let url = "https://api.x.com/oauth/access_token" in
    let extra_params = [("oauth_verifier", oauth_verifier)] in
    let auth_header = authorization_header
      ~consumer_key ~consumer_secret
      ~token:oauth_token ~token_secret:oauth_token_secret
      ~http_method:"POST" ~url
      ~extra_params
      () in
    let body = "oauth_verifier=" ^ percent_encode oauth_verifier in
    Http.post
      ~headers:[
        ("Authorization", auth_header);
        ("Content-Type", "application/x-www-form-urlencoded");
      ]
      ~body
      url
      (fun response ->
        if response.Social_core.status >= 200 && response.status < 300 then begin
          try
            let fields = parse_form_encoded response.body in
            let result = {
              oauth_token = find_field fields "oauth_token";
              oauth_token_secret = find_field fields "oauth_token_secret";
              user_id = find_field fields "user_id";
              screen_name = find_field fields "screen_name";
            } in
            on_success result
          with e -> on_error (Printf.sprintf "Failed to parse access_token response: %s" (Printexc.to_string e))
        end else
          on_error (Printf.sprintf "access_token exchange failed (%d): %s" response.status response.body))
      on_error
end
