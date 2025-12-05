(** Thread Splitter - Split content into thread posts *)

(** Split content by two consecutive blank lines to create thread posts *)
let split_by_double_newline content =
  let parts = String.split_on_char '\n' content in
  
  let rec group_posts acc current_post prev_was_empty = function
    | [] -> 
        if current_post = [] then List.rev acc
        else List.rev (String.concat "\n" (List.rev current_post) :: acc)
    | line :: rest ->
        let is_empty = String.trim line = "" in
        if is_empty && prev_was_empty then
          (* Found two consecutive blank lines - this is a separator *)
          if current_post = [] then
            (* Empty post, skip it and continue *)
            group_posts acc [] true rest
          else
            let post_text = String.concat "\n" (List.rev current_post) in
            group_posts (post_text :: acc) [] true rest
        else
          (* Add line to current post *)
          group_posts acc (line :: current_post) is_empty rest
  in
  
  let posts = group_posts [] [] false parts in
  (* Filter out empty posts (only whitespace) *)
  List.filter (fun s -> String.trim s <> "") posts

(** Split content into chunks of maximum length, respecting word boundaries *)
let split_by_length ~max_length content =
  if String.length content <= max_length then
    [content]
  else
    let rec split_aux acc current pos =
      if pos >= String.length content then
        if String.length current > 0 then
          List.rev (current :: acc)
        else
          List.rev acc
      else if String.length current >= max_length then
        (* Try to find last space to break at word boundary *)
        let break_pos = 
          try
            String.rindex_from current (String.length current - 1) ' '
          with Not_found -> String.length current
        in
        let chunk = String.sub current 0 break_pos in
        let remainder = String.sub current break_pos (String.length current - break_pos) in
        split_aux (String.trim chunk :: acc) (String.trim remainder) pos
      else
        split_aux acc (current ^ String.make 1 content.[pos]) (pos + 1)
    in
    split_aux [] "" 0
