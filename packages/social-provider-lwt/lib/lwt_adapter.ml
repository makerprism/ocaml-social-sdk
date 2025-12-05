(** Lwt Adapter - Convert CPS-style interfaces to Lwt *)

open Lwt.Syntax

(** Convert CPS function to Lwt promise *)
let cps_to_lwt f =
  let promise, resolver = Lwt.wait () in
  let _ = f
    (fun result -> Lwt.wakeup resolver (Ok result))
    (fun error -> Lwt.wakeup resolver (Error error))
  in
  let* result = promise in
  match result with
  | Ok v -> Lwt.return v
  | Error e -> Lwt.fail_with e

(** Adapt HTTP_CLIENT to Lwt *)
module Http_to_lwt (Client : Social_provider_core.HTTP_CLIENT) = struct
  let get ?headers url =
    cps_to_lwt (fun on_success on_error ->
      Client.get ?headers url on_success on_error)

  let post ?headers ?body url =
    cps_to_lwt (fun on_success on_error ->
      Client.post ?headers ?body url on_success on_error)

  let post_multipart ?headers ~parts url =
    cps_to_lwt (fun on_success on_error ->
      Client.post_multipart ?headers ~parts url on_success on_error)

  let put ?headers ?body url =
    cps_to_lwt (fun on_success on_error ->
      Client.put ?headers ?body url on_success on_error)

  let delete ?headers url =
    cps_to_lwt (fun on_success on_error ->
      Client.delete ?headers url on_success on_error)
end

(** Adapt STORAGE to Lwt *)
module Storage_to_lwt (Storage : Social_provider_core.STORAGE) = struct
  let download_media ~media_id =
    cps_to_lwt (fun on_success on_error ->
      Storage.download_media ~media_id on_success on_error)

  let upload_public_media ~content ~filename ~content_type =
    cps_to_lwt (fun on_success on_error ->
      Storage.upload_public_media ~content ~filename ~content_type on_success on_error)
end

(** Adapt CONFIG to Lwt *)
module Config_to_lwt (Config : Social_provider_core.CONFIG) = struct
  let get_env = Config.get_env

  let get_credentials ~account_id =
    cps_to_lwt (fun on_success on_error ->
      Config.get_credentials ~account_id on_success on_error)

  let update_credentials ~account_id ~credentials =
    cps_to_lwt (fun on_success on_error ->
      Config.update_credentials ~account_id ~credentials on_success on_error)

  let encrypt plaintext =
    cps_to_lwt (fun on_success on_error ->
      Config.encrypt plaintext on_success on_error)

  let decrypt ciphertext =
    cps_to_lwt (fun on_success on_error ->
      Config.decrypt ciphertext on_success on_error)

  let update_health_status ~account_id ~status ~error_message =
    cps_to_lwt (fun on_success on_error ->
      Config.update_health_status ~account_id ~status ~error_message on_success on_error)
end
