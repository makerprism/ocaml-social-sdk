let challenge_method = "S256"

let base64url_no_pad raw =
  Base64.encode_exn ~pad:false ~alphabet:Base64.uri_safe_alphabet raw

let generate_verifier ?(bytes = 32) () =
  if bytes < 32 || bytes > 96 then
    invalid_arg "Social_oauth_pkce.generate_verifier: bytes must be between 32 and 96"
  else base64url_no_pad (Social_secure_random.bytes bytes)

let challenge_s256 verifier =
  Digestif.SHA256.digest_string verifier
  |> Digestif.SHA256.to_raw_string |> base64url_no_pad
