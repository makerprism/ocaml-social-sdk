let max_expires_in = 31_536_000

let clamp seconds =
  if seconds < 0 then 0 else if seconds > max_expires_in then max_expires_in
  else seconds

let of_float value =
  match Float.classify_float value with
  | FP_nan -> None
  | FP_infinite -> Some (if value > 0.0 then max_expires_in else 0)
  | FP_zero | FP_normal | FP_subnormal ->
      if value >= float_of_int max_expires_in then Some max_expires_in
      else Some (clamp (int_of_float (Float.ceil value)))

let expires_in_of_json json =
  match json with
  | `Assoc fields -> (
      match List.assoc_opt "expires_in" fields with
      | Some (`Int seconds) -> Some (clamp seconds)
      | Some (`Float seconds) -> of_float seconds
      | Some (`String text) -> (
          match int_of_string_opt (String.trim text) with
          | Some seconds -> Some (clamp seconds)
          | None -> Option.bind (float_of_string_opt (String.trim text)) of_float)
      | Some (`Null | `Bool _ | `List _ | `Assoc _) | None -> None)
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ -> None

let now_or_clock = function
  | Some now -> now
  | None -> Ptime_clock.now ()

let expires_at_of_expires_in ?now seconds =
  let now = now_or_clock now in
  let seconds = if seconds < 0 then 0 else seconds in
  Ptime.add_span now (Ptime.Span.of_int_s seconds)
  |> Option.map (fun instant -> Ptime.to_rfc3339 instant)

let seconds_until ?now expires_at =
  match Ptime.of_rfc3339 expires_at with
  | Error _ -> None
  | Ok (instant, _, _) ->
      let span = Ptime.diff instant (now_or_clock now) in
      Some (int_of_float (Float.round (Ptime.Span.to_float_s span)))

let is_expired ?(buffer_seconds = 0) ?now expires_at =
  match expires_at with
  | None -> false
  | Some expires_at -> (
      match seconds_until ?now expires_at with
      | None -> true
      | Some remaining -> remaining <= buffer_seconds)
