let test_split_shapes () =
  assert (Social_oauth_scope.split "a,b" = [ "a"; "b" ]);
  assert (Social_oauth_scope.split "a b" = [ "a"; "b" ]);
  (* Mixed and repeated delimiters, as left by an application that joined
     a comma list with spaces. *)
  assert (Social_oauth_scope.split " a, b ,,c\td\n" = [ "a"; "b"; "c"; "d" ]);
  assert (Social_oauth_scope.split "a\011b\012c" = [ "a"; "b"; "c" ]);
  assert (Social_oauth_scope.granted ~scope:(Some "read\011write") "read");
  assert (Social_oauth_scope.split "" = []);
  assert (Social_oauth_scope.split " , " = []);
  (* Case is kept so the list can be stored unchanged. *)
  assert (Social_oauth_scope.split "Tweet.Read users.read" = [ "Tweet.Read"; "users.read" ]);
  (* Google scopes are URLs; ':' and '/' are not delimiters. *)
  assert (
    Social_oauth_scope.split
      "openid https://www.googleapis.com/auth/youtube.readonly"
    = [ "openid"; "https://www.googleapis.com/auth/youtube.readonly" ]);
  print_endline "ok: split reads comma, space and mixed scope strings"

let test_join () =
  assert (Social_oauth_scope.join Social_oauth_scope.Comma [ "a"; "b" ] = "a,b");
  assert (Social_oauth_scope.join Social_oauth_scope.Space [ "a"; "b" ] = "a b");
  assert (Social_oauth_scope.join Social_oauth_scope.Comma [ "a"; ""; " b " ] = "a,b");
  assert (Social_oauth_scope.join Social_oauth_scope.Space [] = "");
  print_endline "ok: join drops empty names and uses the separator"

let test_membership () =
  assert (Social_oauth_scope.mem "PAGES_SHOW_LIST" [ "pages_show_list" ]);
  assert (not (Social_oauth_scope.mem "pages_show" [ "pages_show_list" ]));
  assert (Social_oauth_scope.granted ~scope:(Some "a, B") "b");
  assert (not (Social_oauth_scope.granted ~scope:None "a"));
  assert (not (Social_oauth_scope.granted ~scope:(Some "") "a"));
  assert (Social_oauth_scope.contains_all ~required:[] []);
  assert (Social_oauth_scope.contains_all ~required:[ "a"; "B" ] [ "b"; "c"; "a" ]);
  assert (not (Social_oauth_scope.contains_all ~required:[ "a"; "d" ] [ "a" ]));
  print_endline "ok: membership is case-insensitive and exact per name"

let test_missing_and_dedup () =
  assert (
    Social_oauth_scope.missing ~requested:[ "a"; "b"; "C" ] ~granted:[ "c" ]
    = [ "a"; "b" ]);
  assert (Social_oauth_scope.missing ~requested:[] ~granted:[ "a" ] = []);
  assert (Social_oauth_scope.dedup [ "a"; "B"; "A"; "b"; "c" ] = [ "a"; "B"; "c" ]);
  print_endline "ok: missing keeps requested order; dedup keeps first spelling"

let () =
  test_split_shapes ();
  test_join ();
  test_membership ();
  test_missing_and_dedup ()
