(* Tests for Social_text_counter: the UTF-8 counting strategies behind the
   capability table's text limits. The same fixtures back
   Social_capabilities.count_text (see test_social_capabilities.ml). *)

let repeat s n = String.concat "" (List.init n (fun _ -> s))

(* é: one code point, two UTF-8 bytes. *)
let accent = "\xc3\xa9"

(* 😀: one code point, four UTF-8 bytes. *)
let emoji = "\xf0\x9f\x98\x80"

(* 中文: two CJK code points, six UTF-8 bytes, weight 2 each on Twitter. *)
let cjk = "\xe4\xb8\xad\xe6\x96\x87"

let expect_int ~context expected actual =
  if actual <> expected then
    failwith
      (Printf.sprintf "%s: expected %d but got %d" context expected actual)

let test_utf8_codepoint_length () =
  expect_int ~context:"empty" 0 (Social_text_counter.utf8_codepoint_length "");
  expect_int ~context:"ascii" 3
    (Social_text_counter.utf8_codepoint_length "abc");
  (* 200 é are 200 code points but 400 UTF-8 bytes. *)
  expect_int ~context:"200 é" 200
    (Social_text_counter.utf8_codepoint_length (repeat accent 200));
  expect_int ~context:"one é" 1 (Social_text_counter.utf8_codepoint_length accent);
  expect_int ~context:"mixed" 3
    (Social_text_counter.utf8_codepoint_length ("a" ^ emoji ^ "b"));
  print_endline "PASS utf8_codepoint_length counts code points"

let test_codepoints_emoji_as_bytes_length () =
  expect_int ~context:"ascii" 3
    (Social_text_counter.codepoints_emoji_as_bytes_length "abc");
  (* Non-emoji text counts one per code point, é included. *)
  expect_int ~context:"200 é" 200
    (Social_text_counter.codepoints_emoji_as_bytes_length (repeat accent 200));
  (* An astral emoji counts as its 4 UTF-8 bytes (Threads' documented rule). *)
  expect_int ~context:"emoji as bytes" 7
    (Social_text_counter.codepoints_emoji_as_bytes_length ("abc" ^ emoji));
  expect_int ~context:"exactly 500" 500
    (Social_text_counter.codepoints_emoji_as_bytes_length
       (repeat "a" 496 ^ emoji));
  expect_int ~context:"over 500" 501
    (Social_text_counter.codepoints_emoji_as_bytes_length
       (repeat "a" 497 ^ emoji));
  print_endline "PASS codepoints_emoji_as_bytes_length follows Threads' rule"

let test_twitter_weighted () =
  expect_int ~context:"ascii" 12 (Social_text_counter.count "hello, world");
  (* A 100-character URL counts as t.co's 23. *)
  let url = "https://example.com/" ^ repeat "x" 80 in
  expect_int ~context:"100-char url is 100 bytes" 100 (String.length url);
  expect_int ~context:"url wrapped at 23" 31
    (Social_text_counter.count ("see " ^ url ^ " now"));
  (* CJK code points count as 2. *)
  expect_int ~context:"cjk weight 2 each" 4 (Social_text_counter.count cjk);
  (* A leading reply mention is free in reply mode. *)
  expect_int ~context:"reply mention free" 4
    (Social_text_counter.count ~is_reply:true "@someone hi!!");
  expect_int ~context:"reply mention counted when not a reply" 13
    (Social_text_counter.count "@someone hi!!");
  print_endline "PASS Twitter weighted counting"

let () =
  test_utf8_codepoint_length ();
  test_codepoints_emoji_as_bytes_length ();
  test_twitter_weighted ()
