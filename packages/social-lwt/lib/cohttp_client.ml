(** Cohttp Client - HTTP_CLIENT implementation using Cohttp *)

open Lwt.Syntax
open Social_core

(** Options for {!Make_with_options}. *)
module type OPTIONS = sig
  (** Deadline for each request, in seconds: connecting, sending, the
      response headers and reading the whole body. [None] waits as long as
      the connection stays open, which is what {!Make} does. On expiry the
      request is cancelled and the error continuation receives
      {!timeout_message}; providers report that as a [Network_error]. *)
  val timeout_seconds : float option
end

(** No deadline: the options {!Make} is built with. *)
module Default_options : OPTIONS = struct
  let timeout_seconds = None
end

(** The error message a timed-out request reports. *)
let timeout_message seconds =
  Printf.sprintf "Request timed out after %gs" seconds

(** Cohttp-based HTTP client that implements the CPS interface, with a
    per-request deadline from [Options]. *)
module Make_with_options (Options : OPTIONS) = struct
  (** Convert Cohttp response to our response type *)
  let to_response resp body_str =
    let status = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
    let headers = Cohttp.Header.to_list (Cohttp.Response.headers resp) in
    { status; headers; body = body_str }

  (* The deadline covers reading the body, not only the headers: a server
     that sends headers and then stalls must not hang the caller. *)
  let fetch request =
    let* resp, body = request () in
    let* body_str = Cohttp_lwt.Body.to_string body in
    Lwt.return (resp, body_str)

  let error_message ~deadline exn =
    match deadline, exn with
    | Some seconds, Lwt_unix.Timeout -> timeout_message seconds
    | (Some _ | None), _ -> Printexc.to_string exn

  (* [on_success] runs inside the handler, as it always has: an exception
     it raises reaches [on_error]. *)
  let run request on_success on_error =
    let deadline = Options.timeout_seconds in
    Lwt.catch
      (fun () ->
        let* resp, body_str =
          match deadline with
          | None -> fetch request
          | Some seconds -> Lwt_unix.with_timeout seconds (fun () -> fetch request)
        in
        Lwt.return (on_success (to_response resp body_str)))
      (fun exn -> Lwt.return (on_error (error_message ~deadline exn)))

  let header_of_option = function
    | Some h -> Cohttp.Header.of_list h
    | None -> Cohttp.Header.init ()

  let body_of_option = function
    | Some b -> `String b
    | None -> `Empty

  let get ?headers url on_success on_error =
    let headers = header_of_option headers in
    ignore
      (run
         (fun () -> Cohttp_lwt_unix.Client.get ~headers (Uri.of_string url))
         on_success on_error)

  let post ?headers ?body url on_success on_error =
    let headers = header_of_option headers in
    let body = body_of_option body in
    Lwt.async (fun () ->
      run
        (fun () -> Cohttp_lwt_unix.Client.post ~headers ~body (Uri.of_string url))
        on_success on_error)

  let post_multipart ?headers ~parts url on_success on_error =
    (* Generate boundary *)
    let boundary = Printf.sprintf "----Boundary%d" (Random.int 1000000) in

    (* Build multipart body *)
    let build_part part =
      let content_disposition =
        match part.filename with
        | Some fname -> Printf.sprintf "Content-Disposition: form-data; name=\"%s\"; filename=\"%s\"\r\n" part.name fname
        | None -> Printf.sprintf "Content-Disposition: form-data; name=\"%s\"\r\n" part.name
      in
      let content_type =
        match part.content_type with
        | Some ct -> Printf.sprintf "Content-Type: %s\r\n" ct
        | None -> ""
      in
      Printf.sprintf "--%s\r\n%s%s\r\n%s\r\n" boundary content_disposition content_type part.content
    in

    let body_parts = List.map build_part parts in
    let body_str = String.concat "" body_parts ^ Printf.sprintf "--%s--\r\n" boundary in

    (* Set headers *)
    let headers = match headers with
      | Some h -> h
      | None -> []
    in
    let headers = ("Content-Type", Printf.sprintf "multipart/form-data; boundary=%s" boundary) :: headers in
    let headers = Cohttp.Header.of_list headers in

    Lwt.async (fun () ->
      run
        (fun () ->
          Cohttp_lwt_unix.Client.post
            ~headers
            ~body:(`String body_str)
            (Uri.of_string url))
        on_success on_error)

  let put ?headers ?body url on_success on_error =
    let headers = header_of_option headers in
    let body = body_of_option body in
    Lwt.async (fun () ->
      run
        (fun () -> Cohttp_lwt_unix.Client.put ~headers ~body (Uri.of_string url))
        on_success on_error)

  let delete ?headers url on_success on_error =
    let headers = header_of_option headers in
    Lwt.async (fun () ->
      run
        (fun () -> Cohttp_lwt_unix.Client.delete ~headers (Uri.of_string url))
        on_success on_error)
end

(** Cohttp-based HTTP client without a request deadline. *)
module Make = Make_with_options (Default_options)

(* Both clients must keep satisfying the interface providers take. *)
module _ : HTTP_CLIENT = Make
module _ (Options : OPTIONS) : HTTP_CLIENT = Make_with_options (Options)
