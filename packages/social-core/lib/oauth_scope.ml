type separator =
  | Comma
  | Space

let is_delimiter = function
  | ',' | ' ' | '\t' | '\n' | '\011' | '\012' | '\r' -> true
  | _ -> false

let split raw =
  let length = String.length raw in
  (* Walk backwards so the accumulator ends up in input order without a
     List.rev; [stop] is one past the last character of the current token. *)
  let rec scan index stop acc =
    if index < 0 then
      if stop > 0 then String.sub raw 0 stop :: acc else acc
    else if is_delimiter raw.[index] then
      let acc =
        if stop > index + 1 then
          String.sub raw (index + 1) (stop - index - 1) :: acc
        else acc
      in
      scan (index - 1) index acc
    else scan (index - 1) stop acc
  in
  scan (length - 1) length []

let join separator scopes =
  let sep =
    match separator with
    | Comma -> ","
    | Space -> " "
  in
  scopes |> List.concat_map split |> String.concat sep

let equal a b = String.lowercase_ascii a = String.lowercase_ascii b

let mem scope scopes = List.exists (equal scope) scopes

let granted ~scope target =
  match scope with
  | None -> false
  | Some raw -> mem target (split raw)

let contains_all ~required scopes =
  List.for_all (fun scope -> mem scope scopes) required

let missing ~requested ~granted =
  List.filter (fun scope -> not (mem scope granted)) requested

let dedup scopes =
  List.fold_left
    (fun acc scope -> if mem scope acc then acc else scope :: acc)
    [] scopes
  |> List.rev
