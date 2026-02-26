let is_expired_with_buffer ~buffer_seconds expires_at_opt =
  match expires_at_opt with
  | None -> false
  | Some expires_at_str ->
      try
        match Ptime.of_rfc3339 expires_at_str with
        | Ok (expires_at, _, _) ->
            let now = Ptime_clock.now () in
            let buffer = Ptime.Span.of_int_s buffer_seconds in
            (match Ptime.add_span now buffer with
             | Some future -> not (Ptime.is_later expires_at ~than:future)
             | None -> false)
        | Error _ -> true
      with _ -> true

let expires_at_from_now ~expires_in_seconds =
  let now = Ptime_clock.now () in
  match Ptime.add_span now (Ptime.Span.of_int_s expires_in_seconds) with
  | Some exp -> Some (Ptime.to_rfc3339 exp)
  | None -> Some (Ptime.to_rfc3339 now)
