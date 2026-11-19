(* Lwt_adapter.sleep must yield to the event loop rather than block it:
   a timer that fires earlier has to run while a longer sleep is pending. *)

let test_sleep_does_not_block_the_event_loop () =
  let events = ref [] in
  let record event = events := event :: !events in
  let finished, wake = Lwt.wait () in
  Social_lwt.Lwt_adapter.sleep ~seconds:0.2
    (fun () ->
      record "sdk sleep done";
      Lwt.wakeup_later wake ())
    (fun err -> failwith ("unexpected sleep error: " ^ err));
  record "sleep returned";
  let other_work =
    let open Lwt.Syntax in
    let* () = Lwt_unix.sleep 0.01 in
    record "other work ran";
    Lwt.return_unit
  in
  Lwt_main.run (Lwt.join [ finished; other_work ]);
  assert (
    List.rev !events = [ "sleep returned"; "other work ran"; "sdk sleep done" ]);
  print_endline "ok: Lwt_adapter.sleep does not block the event loop"

let test_zero_delay_continues_once () =
  let calls = ref 0 in
  let outcome, resolve = Lwt.wait () in
  Social_lwt.Lwt_adapter.sleep ~seconds:0.0
    (fun () ->
      incr calls;
      Lwt.wakeup_later resolve ())
    (fun err -> failwith ("zero-delay sleep failed: " ^ err));
  Lwt_main.run outcome;
  assert (!calls = 1);
  print_endline "ok: Lwt_adapter.sleep continues once for a zero delay"

let test_continuation_exception_reaches_error () =
  let errors = ref [] in
  let async_errors = ref [] in
  let previous_hook = !Lwt.async_exception_hook in
  Fun.protect ~finally:(fun () -> Lwt.async_exception_hook := previous_hook)
    (fun () ->
      Lwt.async_exception_hook := (fun exn -> async_errors := exn :: !async_errors);
      Social_lwt.Lwt_adapter.sleep ~seconds:0.0
        (fun () -> failwith "continuation failed")
        (fun message -> errors := message :: !errors);
      Lwt_main.run (Lwt_unix.sleep 0.01);
      assert (!async_errors = []);
      assert (List.length !errors = 1))

(* cps_error_types_to_lwt_typed must hand the typed error through untouched:
   a consumer branching on the constructor must not see the rendered string. *)

let test_cps_error_types_to_lwt_typed_preserves_the_constructor () =
  let promise =
    Social_lwt.Lwt_adapter.cps_error_types_to_lwt_typed (fun on_success on_error ->
        ignore on_success;
        on_error (Error_types.Auth_error Error_types.Token_expired))
  in
  let result = Lwt_main.run promise in
  assert (result = Error (Error_types.Auth_error Error_types.Token_expired));
  print_endline "ok: cps_error_types_to_lwt_typed preserves the typed error"

let test_cps_error_types_to_lwt_typed_preserves_the_value () =
  let promise =
    Social_lwt.Lwt_adapter.cps_error_types_to_lwt_typed (fun on_success on_error ->
        ignore on_error;
        on_success "token")
  in
  let result = Lwt_main.run promise in
  assert (result = Ok "token");
  print_endline "ok: cps_error_types_to_lwt_typed resolves success values"

let test_cps_error_types_to_lwt_typed_reports_exceptions_as_internal () =
  let promise =
    Social_lwt.Lwt_adapter.cps_error_types_to_lwt_typed (fun _on_success _on_error ->
        failwith "caller bug")
  in
  let result = Lwt_main.run promise in
  assert (match result with
          | Error (Error_types.Internal_error message) ->
              String.length message > 0
          | _ -> false);
  print_endline "ok: cps_error_types_to_lwt_typed reports exceptions as Internal_error"

let () =
  test_sleep_does_not_block_the_event_loop ();
  test_zero_delay_continues_once ();
  test_continuation_exception_reaches_error ();
  test_cps_error_types_to_lwt_typed_preserves_the_constructor ();
  test_cps_error_types_to_lwt_typed_preserves_the_value ();
  test_cps_error_types_to_lwt_typed_reports_exceptions_as_internal ();
  print_endline "social-lwt tests passed"
