(* Primitives shared by the typed provider parsers. The tests pin the
   coercion differences on purpose: a parser migrating onto
   Social_json_reader must pick the variant that matches what it accepted
   before. *)

open Social_json_reader

(* member / field: a missing member and a JSON null read the same through
   member; field keeps them apart. *)
let test_member_field () =
  let json = `Assoc [ ("id", `String "1"); ("empty", `String ""); ("n", `Null) ] in
  assert (member "id" json = `String "1");
  assert (member "absent" json = `Null);
  assert (member "n" json = `Null);
  assert (member "id" (`List []) = `Null);
  assert (field "id" json = Some (`String "1"));
  assert (field "absent" json = None);
  (* The distinction a paging parser may need: absent versus explicit null. *)
  assert (field "n" json = Some `Null);
  assert (field "id" (`String "x") = None);
  print_endline "ok: member/field lookup"

(* path walks nested members and stops at the first missing step. *)
let test_path () =
  let json = `Assoc [ ("media", `Assoc [ ("image", `Assoc [ ("src", `String "u") ]) ]) ] in
  assert (path [ "media"; "image"; "src" ] json = `String "u");
  assert (path [ "media"; "video"; "src" ] json = `Null);
  assert (path [ "a" ] (`Int 3) = `Null);
  print_endline "ok: nested path"

(* string_opt keeps an empty string; nonempty_string_opt drops it. *)
let test_string_opt () =
  assert (string_opt (`String "") = Some "");
  assert (string_opt (`String "x") = Some "x");
  assert (string_opt (`Int 1) = None);
  assert (nonempty_string_opt (`String "x") = Some "x");
  assert (nonempty_string_opt (`String "") = None);
  assert (nonempty_string_opt `Null = None);
  print_endline "ok: string readers"

(* bool_opt / int_opt are strict about their JSON shape. *)
let test_strict_readers () =
  assert (bool_opt (`Bool true) = Some true);
  assert (bool_opt (`Int 1) = None);
  assert (int_opt (`Int 5) = Some 5);
  assert (int_opt (`Float 5.0) = None);
  assert (int_opt (`String "5") = None);
  print_endline "ok: strict bool/int readers"

(* int_value: integer and whole float accepted; fractional float, other
   shapes and a missing member rejected with the caller's context. *)
let test_int_value () =
  assert (int_value ~what:"v" (Some (`Int 12)) = Ok 12);
  assert (int_value ~what:"v" (Some (`Float 12.0)) = Ok 12);
  assert (
    int_value ~what:"values[].value" (Some (`Float 12.5))
    = Error "values[].value is not an integer");
  assert (int_value ~what:"v" None = Error "v is not an integer");
  assert (int_value ~what:"v" (Some (`String "12")) = Error "v is not an integer");
  print_endline "ok: int_value coercion"

(* data_list: the paging container array, empty when absent or malformed. *)
let test_data_list () =
  let page = `Assoc [ ("data", `List [ `Assoc [ ("id", `String "a") ] ]) ] in
  assert (List.length (data_list page) = 1);
  assert (List.length (data_list (`Assoc [])) = 0);
  assert (List.length (data_list (`Assoc [ ("data", `Null) ])) = 0);
  print_endline "ok: paging data list"

(* map_result: first error wins, values keep their order. *)
let test_map_result () =
  let parse = function `Int n -> Ok n | _ -> Error "not an integer" in
  assert (map_result parse [ `Int 1; `Int 2 ] = Ok [ 1; 2 ]);
  assert (map_result parse [ `Int 1; `String "x"; `Bool true ] = Error "not an integer");
  print_endline "ok: result traversal"

let () =
  test_member_field ();
  test_path ();
  test_string_opt ();
  test_strict_readers ();
  test_int_value ();
  test_data_list ();
  test_map_result ()
