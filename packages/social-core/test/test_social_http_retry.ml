(* Shared HTTP read-retry mechanics: case-insensitive header lookup,
   Retry-After delay parsing with fractional values and safety caps, and
   the CPS read-retry loop (attempt limits, permanent statuses, transport
   failures, unschedulable backoff, delay budgets). *)

open Social_http_retry

let response ?(status = 200) ?(headers = []) () : Social_core.response =
  { status; headers; body = "" }

(* Case-insensitivity is the point: servers emit header names in any
   casing and the copies this module replaced each normalized their own. *)
let test_find_header () =
  assert (find_header [ ("Retry-After", "7") ] "retry-after" = Some "7");
  assert (find_header [ ("retry-after", "7") ] "Retry-After" = Some "7");
  assert (find_header [ ("X-A", "1"); ("x-a", "2") ] "X-A" = Some "1");
  assert (find_header [ ("content-type", "image/png") ] "Content-Type" = Some "image/png");
  assert (find_header [] "retry-after" = None);
  assert (find_header [ ("x-b", "1") ] "x-a" = None);
  print_endline "ok: find_header"

let test_header_int () =
  assert (header_int [ ("x-rate-limit-reset", "1799999999") ] "x-rate-limit-reset" = Some 1799999999);
  assert (header_int [ ("x-ratelimit-reset", " 42 ") ] "x-ratelimit-reset" = Some 42);
  assert (header_int [ ("x-ratelimit-reset", "1.5") ] "x-ratelimit-reset" = None);
  assert (header_int [ ("x-ratelimit-reset", "soon") ] "x-ratelimit-reset" = None);
  assert (header_int [] "x-ratelimit-reset" = None);
  print_endline "ok: header_int"

(* Delay parsing: plain integers pass through, fractional values round
   up, garbage and negatives are rejected, and the cap is the caller's. *)
let test_delay_header_seconds () =
  let max_day = 86400.0 in
  assert (delay_header_seconds ~max_seconds:max_day [ ("Retry-After", "5") ] "retry-after" = Some 5);
  assert (delay_header_seconds ~max_seconds:max_day [ ("retry-after", " 12 " ) ] "Retry-After" = Some 12);
  (* Threads sends fractional seconds; rounding up never retries early. *)
  assert (delay_header_seconds ~max_seconds:max_day [ ("Retry-After", "1.2") ] "retry-after" = Some 2);
  assert (delay_header_seconds ~max_seconds:max_day [ ("Retry-After", "0.1") ] "retry-after" = Some 1);
  assert (delay_header_seconds ~max_seconds:max_day [ ("Retry-After", "0") ] "retry-after" = Some 0);
  (* The safety cap, not the header, bounds the answer. *)
  assert (delay_header_seconds ~max_seconds:10. [ ("Retry-After", "600") ] "retry-after" = Some 10);
  assert (delay_header_seconds ~max_seconds:max_day [ ("Retry-After", "999999") ] "retry-after" = Some 86400);
  (* A numeric negative clamps to "retry now" instead of vanishing; only
     non-numeric values count as no advice. *)
  assert (delay_header_seconds ~max_seconds:max_day [ ("Retry-After", "-3") ] "retry-after" = Some 0);
  assert (delay_header_seconds ~max_seconds:max_day [ ("Retry-After", "soon") ] "retry-after" = None);
  assert (delay_header_seconds ~max_seconds:max_day [ ("Retry-After", "nan") ] "retry-after" = None);
  assert (delay_header_seconds ~max_seconds:max_day [ ("Retry-After", "infinity") ] "retry-after" = Some 86400);
  assert (delay_header_seconds ~max_seconds:max_day [] "retry-after" = None);
  print_endline "ok: delay_header_seconds"

(* A driver that scripts what request/sleep observe, so assertions can
   pin the retry loop without a runtime. *)
let run_retry ~policy ~script =
  let requests = ref 0 in
  let sleeps = ref [] in
  let final = ref None in
  read_with_retry ~policy
    ~sleep:(fun ~seconds on_continue _on_error ->
      sleeps := seconds :: !sleeps;
      on_continue ())
    ~request:(fun on_response on_error ->
      incr requests;
      match List.nth_opt script (!requests - 1) with
      | Some (`Status status) -> on_response (response ~status ())
      | Some (`Headers (status, headers)) -> on_response (response ~status ~headers ())
      | Some `Transport -> on_error "connection reset"
      | None -> on_response (response ~status:200 ()))
    ~on_response:(fun r -> final := Some (Ok r.status))
    ~on_transport_error:(fun e -> final := Some (Error e));
  (!requests, List.rev !sleeps, !final)

let retry_all_policy ?(max_attempts = 4) ?(max_total_delay = 1000.) () : read_retry_policy =
  { max_attempts;
    is_retryable_status = (fun status -> status = 429 || status >= 500);
    retry_transport_error = true;
    delay_seconds = (fun _ attempt -> float_of_int (2 * attempt));
    max_total_delay_seconds = max_total_delay }

let test_retry_loop_success_after_transients () =
  (* 429 then 5xx then 200: two sleeps, three requests, success. *)
  let requests, sleeps, final =
    run_retry ~policy:(retry_all_policy ())
      ~script:[ `Status 429; `Status 503; `Status 200 ]
  in
  assert (requests = 3);
  assert (sleeps = [ 2.; 4. ]);
  assert (final = Some (Ok 200));
  print_endline "ok: retry until success"

let test_retry_loop_permanent_status_not_retried () =
  (* A 404 is a final answer on the first attempt: no sleep, no second
     request. This is the drift the four Twitter media-fetch copies had:
     two of them retried every non-2xx status. *)
  let requests, sleeps, final =
    run_retry ~policy:(retry_all_policy ())
      ~script:[ `Status 404; `Status 200 ]
  in
  assert (requests = 1);
  assert (sleeps = []);
  assert (final = Some (Ok 404));
  print_endline "ok: permanent status not retried"

let test_retry_loop_attempts_exhausted () =
  let requests, sleeps, final =
    run_retry ~policy:(retry_all_policy ()) ~script:[ `Status 500; `Status 500; `Status 500; `Status 500; `Status 500 ]
  in
  (* max_attempts 4 = first try plus three retries. *)
  assert (requests = 4);
  assert (sleeps = [ 2.; 4.; 6. ]);
  assert (final = Some (Ok 500));
  print_endline "ok: attempts exhausted"

let test_retry_loop_transport_error () =
  (* Transport failures retry like transient statuses when the policy
     allows; sites that passed the error straight through were the other
     half of the drift. *)
  let requests, sleeps, final =
    run_retry ~policy:(retry_all_policy ()) ~script:[ `Transport; `Status 200 ]
  in
  assert (requests = 2);
  assert (sleeps = [ 2. ]);
  assert (final = Some (Ok 200));
  (* With transport retries off, the error surfaces immediately. *)
  let requests, sleeps, final =
    run_retry
      ~policy:{ (retry_all_policy ()) with retry_transport_error = false }
      ~script:[ `Transport; `Status 200 ]
  in
  assert (requests = 1);
  assert (sleeps = []);
  assert (final = Some (Error "connection reset"));
  print_endline "ok: transport error handling"

let test_retry_loop_without_transport_retry () =
  let policy = { (retry_all_policy ()) with retry_transport_error = false } in
  let requests, _, final = run_retry ~policy ~script:[ `Transport ] in
  assert (requests = 1);
  assert (final = Some (Error "connection reset"));
  print_endline "ok: no transport retry"

let test_retry_loop_budget () =
  (* Delays are 2., 4., 6.: the 10-second budget fits the first two but
     not the third, so the loop stops after two retries. *)
  let requests, sleeps, final =
    run_retry ~policy:(retry_all_policy ~max_total_delay:10. ())
      ~script:[ `Status 500; `Status 500; `Status 500; `Status 500 ]
  in
  assert (requests = 3);
  assert (sleeps = [ 2.; 4. ]);
  assert (final = Some (Ok 500));
  print_endline "ok: delay budget stops retrying"

let test_retry_loop_unschedulable_backoff () =
  (* A runtime that cannot schedule the timer gets the last outcome, not
     an immediate retry: a failed timer must not become a retry storm. *)
  let requests = ref 0 in
  let final = ref None in
  read_with_retry ~policy:(retry_all_policy ())
    ~sleep:(fun ~seconds:_ _on_continue on_error -> on_error "timer unavailable")
    ~request:(fun on_response _on_error ->
      incr requests;
      on_response (response ~status:503 ()))
    ~on_response:(fun r -> final := Some (Ok r.status))
    ~on_transport_error:(fun e -> final := Some (Error e));
  assert (!requests = 1);
  assert (!final = Some (Ok 503));
  print_endline "ok: unschedulable backoff hands back last outcome"

let test_retry_loop_delay_reads_response () =
  (* A 429's Retry-After reaches the delay policy through the trigger. *)
  let observed = ref None in
  let policy : read_retry_policy =
    { max_attempts = 3;
      is_retryable_status = (fun status -> status = 429);
      retry_transport_error = false;
      delay_seconds =
        (fun trigger _ ->
          (match trigger with
          | Retryable_status response ->
              observed :=
                delay_header_seconds ~max_seconds:600. response.headers "retry-after"
          | Retryable_transport _ -> observed := None);
          0.);
      max_total_delay_seconds = 1000. }
  in
  let _, _, final =
    run_retry ~policy
      ~script:[ `Headers (429, [ ("Retry-After", "9") ]); `Status 200 ]
  in
  assert (!observed = Some 9);
  assert (final = Some (Ok 200));
  print_endline "ok: delay policy reads Retry-After"

let test_retry_loop_single_attempt_policy () =
  (* max_attempts 1 disables retries entirely, keeping the loop usable as
     a plain pass-through. *)
  let requests, sleeps, final =
    run_retry ~policy:(retry_all_policy ~max_attempts:1 ()) ~script:[ `Status 429 ]
  in
  assert (requests = 1);
  assert (sleeps = []);
  assert (final = Some (Ok 429));
  print_endline "ok: single-attempt policy"

let () =
  test_find_header ();
  test_header_int ();
  test_delay_header_seconds ();
  test_retry_loop_success_after_transients ();
  test_retry_loop_permanent_status_not_retried ();
  test_retry_loop_attempts_exhausted ();
  test_retry_loop_transport_error ();
  test_retry_loop_without_transport_retry ();
  test_retry_loop_budget ();
  test_retry_loop_unschedulable_backoff ();
  test_retry_loop_delay_reads_response ();
  test_retry_loop_single_attempt_policy ()
