(* Shapes from the debug_token reference: ids as strings and as integers,
   granular_scopes with and without target_ids. *)
let facebook_body =
  {|{"data": {
      "app_id": 138483919580948,
      "type": "USER",
      "application": "Social Cafe",
      "data_access_expires_at": 1767225600,
      "expires_at": 1352419328,
      "is_valid": true,
      "issued_at": 1347235328,
      "scopes": ["public_profile", "pages_show_list", "pages_manage_posts", "business_management"],
      "granular_scopes": [
        {"scope": "pages_show_list", "target_ids": ["111", 222]},
        {"scope": "pages_manage_posts", "target_ids": ["111"]},
        {"scope": "business_management"}
      ],
      "user_id": "1207059"
  }}|}

let ok = function
  | Ok value -> value
  | Error message -> failwith message

let test_parse_facebook () =
  let t = ok (Meta_debug_token.of_body facebook_body) in
  assert t.is_valid;
  assert (t.app_id = Some "138483919580948");
  assert (t.token_type = Some "USER");
  assert (t.user_id = Some "1207059");
  assert (List.length t.scopes = 4);
  assert (t.expires_at = Some (Meta_debug_token.Expires_at 1352419328));
  assert (t.issued_at = Some 1347235328);
  assert (t.error = None);
  assert (
    Meta_debug_token.target_ids ~scope:"pages_show_list" t = Some [ "111"; "222" ]);
  assert (Meta_debug_token.target_ids ~scope:"business_management" t = None);
  assert (Meta_debug_token.expires_at_rfc3339 t = Some "2012-11-09T00:02:08-00:00");
  print_endline "ok: facebook debug_token answer parses"

let test_granted_for_target () =
  let t = ok (Meta_debug_token.of_body facebook_body) in
  let granted target scope = Meta_debug_token.granted_for_target ~target_id:target ~scope t in
  assert (granted "111" "pages_manage_posts");
  assert (not (granted "222" "pages_manage_posts"));
  assert (granted "222" "PAGES_SHOW_LIST");
  (* No target_ids: applies to everything. *)
  assert (granted "999" "business_management");
  (* No granular entry, but granted. *)
  assert (granted "999" "public_profile");
  (* Not granted at all. *)
  assert (not (granted "111" "read_insights"));
  print_endline "ok: granular scopes narrow grants per target"

let test_invalid_and_never_expiring () =
  let invalid =
    ok
      (Meta_debug_token.of_body
         {|{"data": {"is_valid": false, "scopes": [], "expires_at": 0,
            "error": {"code": 190, "subcode": 460, "message": "password changed"}}}|})
  in
  assert (not invalid.is_valid);
  assert (invalid.expires_at = Some Meta_debug_token.Never_expires);
  assert (Meta_debug_token.expires_at_rfc3339 invalid = None);
  assert (
    invalid.error
    = Some { Meta_debug_token.code = Some 190; subcode = Some 460; message = Some "password changed" });
  print_endline "ok: invalid token error and expires_at = 0"

let test_rejects_unreadable () =
  let is_error body = Result.is_error (Meta_debug_token.of_body body) in
  assert (is_error "not json");
  assert (is_error {|{"error": {"code": 190}}|});
  assert (is_error {|{"data": {"scopes": ["a"]}}|});
  assert (is_error {|{"data": {"is_valid": "true"}}|});
  (* Junk scope entries are dropped, not fatal. *)
  let t = ok (Meta_debug_token.of_body {|{"data": {"is_valid": true, "scopes": ["a", 1, null, " "]}}|}) in
  assert (t.scopes = [ "a" ]);
  print_endline "ok: unreadable answers are errors"

let test_app_tokens () =
  assert (Meta_debug_token.app_access_token ~app_id:"1" ~app_secret:"s" = "1|s");
  assert (Meta_debug_token.threads_app_access_token ~app_id:"1" ~app_secret:"s" = "TH|1|s");
  print_endline "ok: app access token formats"

let () =
  test_parse_facebook ();
  test_granted_for_target ();
  test_invalid_and_never_expiring ();
  test_rejects_unreadable ();
  test_app_tokens ()
