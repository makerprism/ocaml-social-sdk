(* RFC 7636 Appendix B: the reference verifier and its S256 challenge. *)
let test_rfc7636_known_answer () =
  let verifier = "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk" in
  assert (
    Social_oauth_pkce.challenge_s256 verifier
    = "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM");
  assert (Social_oauth_pkce.challenge_method = "S256");
  print_endline "ok: S256 challenge matches RFC 7636 appendix B"

let is_unreserved_base64url c =
  match c with
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' -> true
  | _ -> false

let test_verifier_shape () =
  let check ~bytes ~length =
    let verifier = Social_oauth_pkce.generate_verifier ~bytes () in
    assert (String.length verifier = length);
    assert (String.for_all is_unreserved_base64url verifier)
  in
  check ~bytes:32 ~length:43;
  check ~bytes:48 ~length:64;
  check ~bytes:96 ~length:128;
  assert (String.length (Social_oauth_pkce.generate_verifier ()) = 43);
  print_endline "ok: verifiers are 43-128 base64url characters"

let test_verifier_length_bounds () =
  let rejects bytes =
    match Social_oauth_pkce.generate_verifier ~bytes () with
    | _ -> false
    | exception Invalid_argument _ -> true
  in
  assert (rejects 31);
  assert (rejects 97);
  print_endline "ok: verifier byte counts outside 32..96 are rejected"

(* Not a statistical test: it catches a constant or unseeded generator,
   which would repeat across calls. *)
let test_verifiers_do_not_repeat () =
  let samples = List.init 64 (fun _ -> Social_oauth_pkce.generate_verifier ()) in
  assert (List.length (List.sort_uniq String.compare samples) = 64);
  assert (String.length (Social_secure_random.bytes 0) = 0);
  assert (String.length (Social_secure_random.bytes 17) = 17);
  print_endline "ok: verifiers do not repeat"

let () =
  test_rfc7636_known_answer ();
  test_verifier_shape ();
  test_verifier_length_bounds ();
  test_verifiers_do_not_repeat ();
  print_endline "Social_oauth_pkce tests passed"
