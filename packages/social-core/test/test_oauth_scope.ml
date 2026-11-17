let test_split_shapes () =
  assert (Oauth_scope.split "a,b" = [ "a"; "b" ]);
  assert (Oauth_scope.split "a b" = [ "a"; "b" ]);
  (* Mixed and repeated delimiters, as left by an application that joined
     a comma list with spaces. *)
  assert (Oauth_scope.split " a, b ,,c\td\n" = [ "a"; "b"; "c"; "d" ]);
  assert (Oauth_scope.split "" = []);
  assert (Oauth_scope.split " , " = []);
  (* Case is kept so the list can be stored unchanged. *)
  assert (Oauth_scope.split "Tweet.Read users.read" = [ "Tweet.Read"; "users.read" ]);
  (* Google scopes are URLs; ':' and '/' are not delimiters. *)
  assert (
    Oauth_scope.split
      "openid https://www.googleapis.com/auth/youtube.readonly"
    = [ "openid"; "https://www.googleapis.com/auth/youtube.readonly" ]);
  print_endline "ok: split reads comma, space and mixed scope strings"

let test_join () =
  assert (Oauth_scope.join Oauth_scope.Comma [ "a"; "b" ] = "a,b");
  assert (Oauth_scope.join Oauth_scope.Space [ "a"; "b" ] = "a b");
  assert (Oauth_scope.join Oauth_scope.Comma [ "a"; ""; " b " ] = "a,b");
  assert (Oauth_scope.join Oauth_scope.Space [] = "");
  print_endline "ok: join drops empty names and uses the separator"

let test_membership () =
  assert (Oauth_scope.mem "PAGES_SHOW_LIST" [ "pages_show_list" ]);
  assert (not (Oauth_scope.mem "pages_show" [ "pages_show_list" ]));
  assert (Oauth_scope.granted ~scope:(Some "a, B") "b");
  assert (not (Oauth_scope.granted ~scope:None "a"));
  assert (not (Oauth_scope.granted ~scope:(Some "") "a"));
  assert (Oauth_scope.contains_all ~required:[] []);
  assert (Oauth_scope.contains_all ~required:[ "a"; "B" ] [ "b"; "c"; "a" ]);
  assert (not (Oauth_scope.contains_all ~required:[ "a"; "d" ] [ "a" ]));
  print_endline "ok: membership is case-insensitive and exact per name"

let test_missing_and_dedup () =
  assert (
    Oauth_scope.missing ~requested:[ "a"; "b"; "C" ] ~granted:[ "c" ]
    = [ "a"; "b" ]);
  assert (Oauth_scope.missing ~requested:[] ~granted:[ "a" ] = []);
  assert (Oauth_scope.dedup [ "a"; "B"; "A"; "b"; "c" ] = [ "a"; "B"; "c" ]);
  print_endline "ok: missing keeps requested order; dedup keeps first spelling"

let () =
  test_split_shapes ();
  test_join ();
  test_membership ();
  test_missing_and_dedup ()
