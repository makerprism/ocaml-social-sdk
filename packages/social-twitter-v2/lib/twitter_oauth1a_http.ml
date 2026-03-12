(** HTTP client wrapper that signs outgoing requests with OAuth 1.0a.

    When credentials are provided (access_token non-empty), replaces any
    existing Authorization header with an OAuth 1.0a signature. When
    access_token is empty, passes through to the inner HTTP client unchanged
    (supporting Bearer-token fallback for legacy OAuth 2.0 accounts). *)

module type OAUTH1A_CREDS = sig
  val consumer_key : unit -> string
  val consumer_secret : unit -> string
  val access_token : unit -> string
  val access_token_secret : unit -> string
end

module Make (Inner : Social_core.HTTP_CLIENT) (Creds : OAUTH1A_CREDS) : Social_core.HTTP_CLIENT = struct

  let is_signing_enabled () =
    Creds.access_token () <> ""

  let strip_auth_header headers =
    List.filter (fun (k, _) ->
      String.lowercase_ascii k <> "authorization"
    ) headers

  let sign_request ~http_method ~url ?(extra_params=[]) headers =
    let clean = strip_auth_header headers in
    let auth = Twitter_oauth1a.authorization_header
      ~consumer_key:(Creds.consumer_key ())
      ~consumer_secret:(Creds.consumer_secret ())
      ~token:(Creds.access_token ())
      ~token_secret:(Creds.access_token_secret ())
      ~http_method ~url
      ~extra_params
      () in
    ("Authorization", auth) :: clean

  let get ?(headers=[]) url on_success on_error =
    if is_signing_enabled () then
      let signed = sign_request ~http_method:"GET" ~url headers in
      Inner.get ~headers:signed url on_success on_error
    else
      Inner.get ~headers url on_success on_error

  let post ?(headers=[]) ?(body="") url on_success on_error =
    if is_signing_enabled () then
      let signed = sign_request ~http_method:"POST" ~url headers in
      Inner.post ~headers:signed ~body url on_success on_error
    else
      Inner.post ~headers ~body url on_success on_error

  let put ?(headers=[]) ?(body="") url on_success on_error =
    if is_signing_enabled () then
      let signed = sign_request ~http_method:"PUT" ~url headers in
      Inner.put ~headers:signed ~body url on_success on_error
    else
      Inner.put ~headers ~body url on_success on_error

  let delete ?(headers=[]) url on_success on_error =
    if is_signing_enabled () then
      let signed = sign_request ~http_method:"DELETE" ~url headers in
      Inner.delete ~headers:signed url on_success on_error
    else
      Inner.delete ~headers url on_success on_error

  let post_multipart ?(headers=[]) ~parts url on_success on_error =
    if is_signing_enabled () then
      let signed = sign_request ~http_method:"POST" ~url headers in
      Inner.post_multipart ~headers:signed ~parts url on_success on_error
    else
      Inner.post_multipart ~headers ~parts url on_success on_error
end
