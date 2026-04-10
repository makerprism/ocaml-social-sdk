(** Lwt Adapter - Convert CPS-style interfaces to Lwt *)

open Lwt.Syntax

(** Convert CPS function to Lwt promise *)
let cps_to_lwt f =
  let promise, resolver = Lwt.wait () in
  let resolved = ref false in
  let resolve_once value =
    if (not !resolved) && Lwt.is_sleeping promise then begin
      resolved := true;
      Lwt.wakeup_later resolver value
    end
  in
  let on_success result = resolve_once (Ok result) in
  let on_error error = resolve_once (Error error) in
  (try f on_success on_error
   with e -> on_error (Printexc.to_string e));
  let* result = promise in
  match result with
  | Ok v -> Lwt.return v
  | Error e -> Lwt.fail_with e

(** Convert a CPS function where the error callback receives [Error_types.error]
    instead of a string. The error is converted to a string for the result. *)
let cps_error_types_to_lwt f =
  let promise, resolver = Lwt.wait () in
  let resolved = ref false in
  let resolve_once value =
    if (not !resolved) && Lwt.is_sleeping promise then begin
      resolved := true;
      Lwt.wakeup_later resolver value
    end
  in
  (try f
    (fun result -> resolve_once (Ok result))
    (fun error -> resolve_once (Error (Error_types.error_to_string error)))
   with e -> resolve_once (Error (Printexc.to_string e)));
  promise

(** Convert a single-callback result-style function to Lwt.
    For SDK functions that call [on_result] with [(a, Error_types.error) result]. *)
let result_callback_to_lwt f =
  let promise, resolver = Lwt.wait () in
  let resolved = ref false in
  let resolve_once value =
    if (not !resolved) && Lwt.is_sleeping promise then begin
      resolved := true;
      Lwt.wakeup_later resolver value
    end
  in
  (try f (fun result ->
    match result with
    | Ok value -> resolve_once (Ok value)
    | Error error -> resolve_once (Error (Error_types.error_to_string error)))
   with e -> resolve_once (Error (Printexc.to_string e)));
  promise

(** Convert a single-callback outcome-style function to Lwt.
    Returns [(result * warnings)] on success, or a string error.
    Warnings should be logged even on success as they indicate partial failures
    (e.g., alt text upload failed). *)
let outcome_to_lwt f =
  let promise, resolver = Lwt.wait () in
  let resolved = ref false in
  let resolve_once value =
    if (not !resolved) && Lwt.is_sleeping promise then begin
      resolved := true;
      Lwt.wakeup_later resolver value
    end
  in
  (try f (fun outcome ->
    match outcome with
    | Error_types.Success result ->
        resolve_once (Ok (result, []))
    | Error_types.Partial_success { result; warnings } ->
        resolve_once (Ok (result, warnings))
    | Error_types.Failure error ->
        resolve_once (Error (Error_types.error_to_string error)))
   with e -> resolve_once (Error (Printexc.to_string e)));
  promise

(** Like [outcome_to_lwt] but preserves the SDK's typed [Error_types.error]
    instead of converting to string. This allows callers to classify the error
    (auth vs transient vs permanent) without keyword matching on strings. *)
let outcome_to_lwt_typed f =
  let promise, resolver = Lwt.wait () in
  let resolved = ref false in
  let resolve_once value =
    if (not !resolved) && Lwt.is_sleeping promise then begin
      resolved := true;
      Lwt.wakeup_later resolver value
    end
  in
  (try f (fun outcome ->
    match outcome with
    | Error_types.Success result ->
        resolve_once (Ok (result, []))
    | Error_types.Partial_success { result; warnings } ->
        resolve_once (Ok (result, warnings))
    | Error_types.Failure error ->
        resolve_once (Error error))
   with e -> resolve_once (Error (Error_types.Internal_error (Printexc.to_string e))));
  promise

(** Adapt HTTP_CLIENT to Lwt *)
module Http_to_lwt (Client : Social_core.HTTP_CLIENT) = struct
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
module Storage_to_lwt (Storage : Social_core.STORAGE) = struct
  let download_media ~media_id =
    cps_to_lwt (fun on_success on_error ->
      Storage.download_media ~media_id on_success on_error)

  let upload_public_media ~content ~filename ~content_type =
    cps_to_lwt (fun on_success on_error ->
      Storage.upload_public_media ~content ~filename ~content_type on_success on_error)
end

(** Adapt CONFIG to Lwt *)
module Config_to_lwt (Config : Social_core.CONFIG) = struct
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
