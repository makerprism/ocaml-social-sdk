type cursors = {
  before : string option;
  after : string option;
}

type 'a page = {
  data : 'a list;
  paging : cursors option;
  next_url : string option;
  previous_url : string option;
}

type position =
  | First
  | After of string
  | Before of string
  | Page_url of string

let has_next page = Option.is_some page.next_url

let next_after page =
  match page.next_url, page.paging with
  | Some _, Some { after; before = _ } -> after
  | Some _, None | None, (Some _ | None) -> None

let non_empty_string = function
  | `String s when String.trim s <> "" -> Some s
  | `String _ | `Null | `Bool _ | `Int _ | `Float _ | `List _ | `Assoc _ -> None

let member key = function
  | `Assoc fields -> Option.value ~default:`Null (List.assoc_opt key fields)
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ -> `Null

let parse_paging json =
  let paging = member "paging" json in
  let cursors = member "cursors" paging in
  let cursors =
    match non_empty_string (member "before" cursors),
          non_empty_string (member "after" cursors)
    with
    | None, None -> None
    | before, after -> Some { before; after }
  in
  (cursors, non_empty_string (member "next" paging),
   non_empty_string (member "previous" paging))

let parse ~item json =
  match member "data" json with
  | `List items -> (
      match List.map item items with
      | data ->
          let paging, next_url, previous_url = parse_paging json in
          Ok { data; paging; next_url; previous_url }
      | exception exn ->
          Error (Printf.sprintf "invalid item in data: %s" (Printexc.to_string exn)))
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `Assoc _ ->
      Error "expected a JSON object with a data list"

let of_body ~item body =
  match Yojson.Basic.from_string body with
  | json -> parse ~item json
  | exception Yojson.Json_error msg -> Error msg

let cursor_query = function
  | First | Page_url _ -> []
  | After cursor -> [ ("after", [ cursor ]) ]
  | Before cursor -> [ ("before", [ cursor ]) ]

let credential_params = [ "access_token"; "appsecret_proof" ]

let last_path_segment uri =
  match List.rev (String.split_on_char '/' (Uri.path uri)) with
  | "" :: segment :: _ | segment :: _ -> segment
  | [] -> ""

let resolve_page_url ~allowed_hosts ~edge url =
  let uri = Uri.of_string (String.trim url) in
  let host = Option.map String.lowercase_ascii (Uri.host uri) in
  let allowed =
    List.map String.lowercase_ascii allowed_hosts
  in
  if Uri.scheme uri <> Some "https" then Error "paging URL must use https"
  else if Option.is_some (Uri.userinfo uri) then
    Error "paging URL must not carry user info"
  else if Option.is_some (Uri.port uri) then
    Error "paging URL must not set a port"
  else
    match host with
    | Some h when List.mem h allowed ->
        if last_path_segment uri <> edge then
          Error (Printf.sprintf "paging URL is not a /%s page" edge)
        else
          Ok
            (Uri.with_query uri
               (List.filter
                  (fun (key, _) -> not (List.mem key credential_params))
                  (Uri.query uri)))
    | Some _ | None -> Error "paging URL host is not the API host"
