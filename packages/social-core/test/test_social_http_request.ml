(* Request-building mechanics shared by the provider clients: repeated
   query keys, percent-encoded query strings, auth and content-type
   headers. Reserved characters and repeated values are the cases the
   builders exist for. *)

open Social_http_request

(* One pair per value: the shape Uri.with_query needs for a repeated key,
   and the opposite of the comma-joined list it would build itself. *)
let test_repeated_query_param () =
  assert (
    repeated_query_param "uris" [ "at://a"; "at://b" ]
    = [ ("uris", [ "at://a" ]); ("uris", [ "at://b" ]) ]);
  assert (repeated_query_param "uris" [] = []);
  (* The Rails array convention: the caller owns the [] suffix. *)
  assert (
    repeated_query_param "types[]" [ "mention"; "follow" ]
    = [ ("types[]", [ "mention" ]); ("types[]", [ "follow" ]) ]);
  (* Whole parameter lists expand the same way. *)
  assert (
    repeated_query_pairs [ ("limit", [ "10" ]); ("uris", [ "a"; "b" ]) ]
    = [ ("limit", [ "10" ]); ("uris", [ "a" ]); ("uris", [ "b" ]) ]);
  print_endline "ok: repeated query params"

(* Percent-encoding: reserved characters must not appear raw; a space
   becomes %20, not the form-urlencoded +. *)
let test_percent_encoded_query () =
  assert (percent_encoded_query [] = "");
  assert (percent_encoded_query [ ("k", "v") ] = "k=v");
  assert (
    percent_encoded_query [ ("a", "1"); ("b", "2") ] = "a=1&b=2");
  assert (
    percent_encoded_query [ ("q", "hello world") ] = "q=hello%20world");
  (* [Uri.pct_encode]'s path-component encoding keeps [=] raw, as the
     existing Google and Bluesky query builders always did. *)
  assert (
    percent_encoded_query [ ("s", "a/b=c?d") ] = "s=a%2Fb=c%3Fd");
  print_endline "ok: percent-encoded query"

(* Bearer headers in both shapes: element pair and one-element list. *)
let test_bearer () =
  assert (bearer_pair "t" = ("Authorization", "Bearer t"));
  assert (bearer_header "t" = [ ("Authorization", "Bearer t") ]);
  assert (bearer_header_opt (Some "t") = [ ("Authorization", "Bearer t") ]);
  assert (bearer_header_opt None = []);
  print_endline "ok: bearer headers"

let () =
  test_repeated_query_param ();
  test_percent_encoded_query ();
  test_bearer ()
