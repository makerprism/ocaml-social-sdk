(** Cohttp Client - HTTP_CLIENT implementation using Cohttp *)

open Lwt.Syntax
open Social_core

(** Cohttp-based HTTP client that implements the CPS interface *)
module Make = struct
  (** Convert Cohttp response to our response type *)
  let to_response resp body_str =
    let status = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
    let headers = Cohttp.Header.to_list (Cohttp.Response.headers resp) in
    { status; headers; body = body_str }

  let get ?headers url on_success on_error =
    let headers = match headers with
      | Some h -> Cohttp.Header.of_list h
      | None -> Cohttp.Header.init ()
    in
    ignore (
      Lwt.catch
        (fun () ->
          let* resp, body = Cohttp_lwt_unix.Client.get ~headers (Uri.of_string url) in
          let* body_str = Cohttp_lwt.Body.to_string body in
          let response = to_response resp body_str in
          let _ = on_success response in
          Lwt.return_unit)
        (fun exn ->
          let _ = on_error (Printexc.to_string exn) in
          Lwt.return_unit)
    )

  let post ?headers ?body url on_success on_error =
    let headers = match headers with
      | Some h -> Cohttp.Header.of_list h
      | None -> Cohttp.Header.init ()
    in
    let body = match body with
      | Some b -> `String b
      | None -> `Empty
    in
    Lwt.async (fun () ->
      Lwt.catch
        (fun () ->
          let* resp, resp_body = Cohttp_lwt_unix.Client.post ~headers ~body (Uri.of_string url) in
          let* body_str = Cohttp_lwt.Body.to_string resp_body in
          let response = to_response resp body_str in
          Lwt.return (on_success response))
        (fun exn ->
          Lwt.return (on_error (Printexc.to_string exn))))

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
      Lwt.catch
        (fun () ->
          let* resp, resp_body = Cohttp_lwt_unix.Client.post 
            ~headers 
            ~body:(`String body_str) 
            (Uri.of_string url) in
          let* body_str = Cohttp_lwt.Body.to_string resp_body in
          let response = to_response resp body_str in
          Lwt.return (on_success response))
        (fun exn ->
          Lwt.return (on_error (Printexc.to_string exn))))

  let put ?headers ?body url on_success on_error =
    let headers = match headers with
      | Some h -> Cohttp.Header.of_list h
      | None -> Cohttp.Header.init ()
    in
    let body = match body with
      | Some b -> `String b
      | None -> `Empty
    in
    Lwt.async (fun () ->
      Lwt.catch
        (fun () ->
          let* resp, resp_body = Cohttp_lwt_unix.Client.put ~headers ~body (Uri.of_string url) in
          let* body_str = Cohttp_lwt.Body.to_string resp_body in
          let response = to_response resp body_str in
          Lwt.return (on_success response))
        (fun exn ->
          Lwt.return (on_error (Printexc.to_string exn))))

  let delete ?headers url on_success on_error =
    let headers = match headers with
      | Some h -> Cohttp.Header.of_list h
      | None -> Cohttp.Header.init ()
    in
    Lwt.async (fun () ->
      Lwt.catch
        (fun () ->
          let* resp, resp_body = Cohttp_lwt_unix.Client.delete ~headers (Uri.of_string url) in
          let* body_str = Cohttp_lwt.Body.to_string resp_body in
          let response = to_response resp body_str in
          Lwt.return (on_success response))
        (fun exn ->
          Lwt.return (on_error (Printexc.to_string exn))))
end
