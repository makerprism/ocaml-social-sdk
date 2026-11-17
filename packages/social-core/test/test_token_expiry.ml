let now =
  match Ptime.of_rfc3339 "2026-01-01T00:00:00Z" with
  | Ok (t, _, _) -> t
  | Error _ -> assert false

let json body = Yojson.Basic.from_string body

let test_expires_in_of_json () =
  let read body = Token_expiry.expires_in_of_json (json body) in
  assert (read {|{"expires_in": 3600}|} = Some 3600);
  assert (read {|{"expires_in": 3599.2}|} = Some 3600);
  assert (read {|{"expires_in": "5183944"}|} = Some 5183944);
  assert (read {|{"expires_in": -5}|} = Some 0);
  assert (read {|{"expires_in": 99999999999}|} = Some Token_expiry.max_expires_in);
  assert (read {|{"expires_in": 1e300}|} = Some Token_expiry.max_expires_in);
  assert (read {|{"expires_in": -1e300}|} = Some 0);
  assert (read {|{"expires_in": null}|} = None);
  assert (read {|{"expires_in": "soon"}|} = None);
  assert (read {|{"access_token": "x"}|} = None);
  assert (read {|[1]|} = None);
  print_endline "ok: expires_in reads int, float and string, clamped"

let test_expires_at () =
  assert (
    Token_expiry.expires_at_of_expires_in ~now 3600
    = Some "2026-01-01T01:00:00-00:00");
  assert (
    Token_expiry.expires_at_of_expires_in ~now (-10)
    = Some "2026-01-01T00:00:00-00:00");
  print_endline "ok: expires_at is now + expires_in in RFC 3339"

let test_seconds_until () =
  assert (Token_expiry.seconds_until ~now "2026-01-01T00:10:00Z" = Some 600);
  assert (Token_expiry.seconds_until ~now "2025-12-31T23:59:00Z" = Some (-60));
  (* Any offset is accepted on read. *)
  assert (Token_expiry.seconds_until ~now "2026-01-01T01:00:00+01:00" = Some 0);
  assert (Token_expiry.seconds_until ~now "not a date" = None);
  print_endline "ok: seconds_until handles past, future and offsets"

let test_is_expired () =
  let later = Some "2026-01-01T00:30:00Z" in
  assert (not (Token_expiry.is_expired ~now None));
  assert (not (Token_expiry.is_expired ~now later));
  assert (Token_expiry.is_expired ~buffer_seconds:1800 ~now later);
  assert (not (Token_expiry.is_expired ~buffer_seconds:1799 ~now later));
  assert (Token_expiry.is_expired ~now (Some "2025-12-31T00:00:00Z"));
  assert (Token_expiry.is_expired ~now (Some "garbage"));
  (* Exact instants: 0.25 s left is not expired with no buffer. *)
  let now_frac =
    match Ptime.of_rfc3339 "2026-01-01T00:00:00.75Z" with
    | Ok (t, _, _) -> t
    | Error _ -> assert false
  in
  assert (not (Token_expiry.is_expired ~now:now_frac (Some "2026-01-01T00:00:01Z")));
  (* Round trip with what expires_at_of_expires_in writes. *)
  let written = Token_expiry.expires_at_of_expires_in ~now 60 in
  assert (not (Token_expiry.is_expired ~now written));
  assert (Token_expiry.is_expired ~buffer_seconds:60 ~now written);
  print_endline "ok: is_expired honours the buffer; None never expires"

let () =
  test_expires_in_of_json ();
  test_expires_at ();
  test_seconds_until ();
  test_is_expired ()
