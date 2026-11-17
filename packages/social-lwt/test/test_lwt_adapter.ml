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

let () =
  test_sleep_does_not_block_the_event_loop ();
  test_zero_delay_continues_once ();
  print_endline "social-lwt tests passed"
