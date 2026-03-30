let assert_eq ~label expected actual =
  if expected <> actual then
    failwith (Printf.sprintf "%s: expected %S, got %S" label expected actual)

let () =
  (* Unreserved characters pass through *)
  assert_eq ~label:"unreserved"
    "key=ABCxyz09-_.~"
    (Social_core.form_urlencode_kvs [("key", "ABCxyz09-_.~")]);

  (* Spaces encode as + *)
  assert_eq ~label:"space"
    "q=hello+world"
    (Social_core.form_urlencode_kvs [("q", "hello world")]);

  (* Characters that Uri.encoded_of_query leaves raw are now encoded *)
  assert_eq ~label:"slash"
    "s=a%2Fb"
    (Social_core.form_urlencode_kvs [("s", "a/b")]);
  assert_eq ~label:"equals"
    "s=a%3Db"
    (Social_core.form_urlencode_kvs [("s", "a=b")]);
  assert_eq ~label:"at"
    "s=a%40b"
    (Social_core.form_urlencode_kvs [("s", "a@b")]);
  assert_eq ~label:"colon"
    "s=a%3Ab"
    (Social_core.form_urlencode_kvs [("s", "a:b")]);
  assert_eq ~label:"question"
    "s=a%3Fb"
    (Social_core.form_urlencode_kvs [("s", "a?b")]);
  assert_eq ~label:"exclaim"
    "s=a%21b"
    (Social_core.form_urlencode_kvs [("s", "a!b")]);
  assert_eq ~label:"star"
    "s=a%2Ab"
    (Social_core.form_urlencode_kvs [("s", "a*b")]);
  assert_eq ~label:"parens"
    "s=a%28b%29"
    (Social_core.form_urlencode_kvs [("s", "a(b)")]);
  assert_eq ~label:"plus"
    "s=a%2Bb"
    (Social_core.form_urlencode_kvs [("s", "a+b")]);

  (* Multiple pairs joined with & *)
  assert_eq ~label:"pairs"
    "a=1&b=2"
    (Social_core.form_urlencode_kvs [("a", "1"); ("b", "2")]);

  (* Realistic OAuth secret with special chars *)
  assert_eq ~label:"oauth_secret"
    "client_secret=sec%2Fret%3Dwith%2Bspecial%40chars"
    (Social_core.form_urlencode_kvs [("client_secret", "sec/ret=with+special@chars")]);

  print_endline "✓ All form_urlencode tests passed"
