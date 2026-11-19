(** Shared HTTP read-retry and retry-advice mechanics.

    Providers kept private copies of three things this module now owns:
    case-insensitive header lookup, [Retry-After]-style delay parsing, and
    the retry loop around an idempotent read. The copies had drifted (some
    retried permanent 4xx statuses, some never retried transport failures),
    so transport behaviour depended on which code path ran.

    The retry loop is deliberately CPS: it must schedule a backoff through
    the caller's [Social_core.SLEEP] implementation without blocking, and a
    promise-based loop would force a runtime dependency into core. *)

type headers = (string * string) list

let find_header headers name =
  let target = String.lowercase_ascii name in
  List.find_map
    (fun (key, value) ->
      if String.equal (String.lowercase_ascii key) target then Some value else None)
    headers

let header_int headers name =
  match find_header headers name with
  | None -> None
  | Some raw -> ( try Some (int_of_string (String.trim raw)) with _ -> None)

let parse_delay_seconds raw =
  (* A numeric value is clamped to [0, +inf) here and to max_seconds in
     delay_header_seconds: a server that answers a negative delay is saying
     "no wait", not "no advice". Non-numeric values are no advice. *)
  let of_float value =
    match classify_float value with
    | FP_nan -> None
    | FP_infinite -> if value > 0.0 then Some infinity else Some 0.0
    | FP_zero | FP_normal | FP_subnormal -> Some (max 0.0 value)
  in
  match float_of_string_opt (String.trim raw) with
  | Some value -> of_float value
  | None -> None

let delay_header_seconds ~max_seconds headers name =
  match find_header headers name with
  | None -> None
  | Some raw -> (
      match parse_delay_seconds raw with
      | None -> None
      | Some seconds ->
          let rounded = Float.ceil seconds in
          if rounded >= max_seconds then Some (int_of_float max_seconds)
          else if rounded <= 0.0 then Some 0
          else Some (int_of_float rounded))

type retry_trigger =
  | Retryable_status of Social_core.response
  | Retryable_transport of string

type read_retry_policy = {
  max_attempts: int;
  is_retryable_status: int -> bool;
  retry_transport_error: bool;
  delay_seconds: retry_trigger -> int -> float;
  max_total_delay_seconds: float;
}

let read_with_retry ~policy ~sleep ~request ~on_response ~on_transport_error =
  let rec loop ~attempt ~spent =
    let finish trigger deliver =
      let delay = max 0.0 (policy.delay_seconds trigger attempt) in
      if attempt >= policy.max_attempts || spent +. delay > policy.max_total_delay_seconds
      then deliver ()
      else
        (* If the runtime cannot schedule the backoff, hand back the last
           outcome as if retries were exhausted; retrying immediately would
           turn a failed timer into a retry storm. *)
        sleep ~seconds:delay
          (fun () -> loop ~attempt:(attempt + 1) ~spent:(spent +. delay))
          (fun _ -> deliver ())
    in
    request
      (fun (response : Social_core.response) ->
        if policy.is_retryable_status response.status then
          finish (Retryable_status response) (fun () -> on_response response)
        else on_response response)
      (fun (error : string) ->
        if policy.retry_transport_error then
          finish (Retryable_transport error) (fun () -> on_transport_error error)
        else on_transport_error error)
  in
  loop ~attempt:1 ~spent:0.0
