(** URL Extractor - Extract URLs from text *)

(** Extract all URLs from text using regex *)
let extract_urls text =
  let url_pattern = Re.Pcre.regexp 
    "https?://[a-zA-Z0-9][-a-zA-Z0-9@:%._\\+~#=]{0,256}\\.[a-zA-Z0-9()]{1,6}\\b[-a-zA-Z0-9()@:%_\\+.~#?&/=]*"
  in
  let matches = Re.all url_pattern text in
  List.map (fun group -> Re.Group.get group 0) matches

(** Extract first URL from text *)
let extract_first_url text =
  match extract_urls text with
  | url :: _ -> Some url
  | [] -> None

(** Check if text contains any URLs *)
let contains_url text =
  match extract_first_url text with
  | Some _ -> true
  | None -> false
