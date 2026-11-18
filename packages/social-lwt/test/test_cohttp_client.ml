(* Cohttp_client against local sockets: a server that answers, and one that
   accepts the connection but never responds. *)

let ( let* ) = Lwt.bind

let localhost = Unix.inet_addr_loopback

(* A listening socket on an ephemeral port. *)
let listen () =
  let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt fd Unix.SO_REUSEADDR true;
  let* () = Lwt_unix.bind fd (Unix.ADDR_INET (localhost, 0)) in
  Lwt_unix.listen fd 8;
  let port =
    match Lwt_unix.getsockname fd with
    | Unix.ADDR_INET (_, port) -> port
    | Unix.ADDR_UNIX _ -> failwith "expected an inet socket"
  in
  Lwt.return (fd, Printf.sprintf "http://127.0.0.1:%d/" port)

(* Answers one request with [200 ok] after reading its headers. *)
let serve_once fd =
  let* client, _ = Lwt_unix.accept fd in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.input client in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.output client in
  let rec read_headers () =
    let* line = Lwt_io.read_line ic in
    if String.trim line = "" then Lwt.return_unit else read_headers ()
  in
  let* () = read_headers () in
  let* () =
    Lwt_io.write oc
      "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\nok"
  in
  let* () = Lwt_io.flush oc in
  Lwt_unix.close client

(* Runs one CPS request and returns its outcome. *)
let request get url =
  let outcome, resolve = Lwt.wait () in
  get url
    (fun (response : Social_core.response) ->
      Lwt.wakeup_later resolve (Ok response))
    (fun err -> Lwt.wakeup_later resolve (Error err));
  outcome

module Fast = Social_lwt.Cohttp_client.Make_with_options (struct
  let timeout_seconds = Some 0.3
end)

let test_timeout_fires_on_a_silent_server () =
  Lwt_main.run
    (let* fd, url = listen () in
     let started = Unix.gettimeofday () in
     let* outcome = request (fun url -> Fast.get url) url in
     let elapsed = Unix.gettimeofday () -. started in
     let* () = Lwt_unix.close fd in
     (match outcome with
      | Error msg ->
          assert (msg = Social_lwt.Cohttp_client.timeout_message 0.3);
          assert (msg = "Request timed out after 0.3s")
      | Ok _ -> failwith "a silent server must time out");
     assert (elapsed < 5.0);
     print_endline "ok: request deadline fires on a silent server";
     Lwt.return_unit)

let test_response_within_deadline () =
  Lwt_main.run
    (let* fd, url = listen () in
     let server = serve_once fd in
     let* outcome = request (fun url -> Fast.post ~body:"x" url) url in
     let* () = server in
     let* () = Lwt_unix.close fd in
     (match outcome with
      | Ok response ->
          assert (response.status = 200);
          assert (response.body = "ok")
      | Error msg -> failwith ("unexpected error: " ^ msg));
     print_endline "ok: response within the deadline is delivered";
     Lwt.return_unit)

let test_default_client_has_no_deadline () =
  Lwt_main.run
    (let* fd, url = listen () in
     let server = serve_once fd in
     let* outcome =
       request (fun url -> Social_lwt.Cohttp_client.Make.get url) url
     in
     let* () = server in
     let* () = Lwt_unix.close fd in
     (match outcome with
      | Ok response -> assert (response.body = "ok")
      | Error msg -> failwith ("unexpected error: " ^ msg));
     print_endline "ok: default client still works";
     Lwt.return_unit)

let test_timeout_closes_connection ~send_headers () =
  Lwt_main.run
    (let* fd, url = listen () in
     let server =
       let* client, _ = Lwt_unix.accept fd in
       Lwt.finalize
         (fun () ->
           let ic = Lwt_io.of_fd ~mode:Lwt_io.input client in
           let rec headers () =
             let* line = Lwt_io.read_line ic in
             if String.trim line = "" then Lwt.return_unit else headers ()
           in
           let* () = headers () in
           let* () =
             if send_headers then
               let oc = Lwt_io.of_fd ~mode:Lwt_io.output client in
               let* () = Lwt_io.write oc
                 "HTTP/1.1 200 OK\r\nContent-Length: 100\r\n\r\nx" in
               Lwt_io.flush oc
             else Lwt.return_unit
           in
           let* remaining = Lwt_io.read ic in
           assert (remaining = "");
           Lwt.return_unit)
         (fun () -> Lwt_unix.close client)
     in
     Lwt.finalize
       (fun () ->
         let* outcome = request (fun url -> Fast.get url) url in
         (match outcome with
          | Error msg -> assert (msg = "Request timed out after 0.3s")
          | Ok _ -> failwith "stall must time out");
         Lwt_unix.with_timeout 1.0 (fun () -> server))
       (fun () -> Lwt_unix.close fd))

let () =
  test_timeout_closes_connection ~send_headers:false ();
  test_timeout_closes_connection ~send_headers:true ();
  test_timeout_fires_on_a_silent_server ();
  test_response_within_deadline ();
  test_default_client_has_no_deadline ();
  print_endline "Cohttp_client tests passed"
