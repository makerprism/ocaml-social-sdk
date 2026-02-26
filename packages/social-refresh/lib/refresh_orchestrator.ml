open Social_core
open Refresh_types

type callbacks = {
  load_credentials :
    account_id:string ->
    (credentials -> unit) ->
    (string -> unit) ->
    unit;
  perform_refresh :
    credentials:credentials ->
    refresh_token:string ->
    (refreshed_token -> unit) ->
    (string -> unit) ->
    unit;
  persist_credentials :
    account_id:string ->
    credentials:credentials ->
    (unit -> unit) ->
    (string -> unit) ->
    unit;
  update_health :
    account_id:string ->
    status:string ->
    error_message:string option ->
    (unit -> unit) ->
    (string -> unit) ->
    unit;
}

let update_health_or_fail callbacks ~account_id ~status ~error_message on_success on_error =
  callbacks.update_health ~account_id ~status ~error_message
    on_success
    (fun err -> on_error (Health_update_failed err))

let ensure_valid_token
    ?(policy = default_policy)
    ~account_id
    ~callbacks
    on_success
    on_error =
  callbacks.load_credentials ~account_id
    (fun creds ->
      match Refresh_decision.decide ~policy ~expires_at:creds.expires_at with
      | Skip ->
          update_health_or_fail callbacks ~account_id ~status:"healthy" ~error_message:None
            (fun () -> on_success (Token_reused creds.access_token))
            on_error
      | Refresh_required ->
          (match creds.refresh_token with
           | None ->
               update_health_or_fail callbacks ~account_id ~status:"token_expired"
                 ~error_message:(Some "No refresh token available")
                 (fun () -> on_error Missing_refresh_token)
                 on_error
           | Some refresh_token ->
               callbacks.perform_refresh ~credentials:creds ~refresh_token
                 (fun refreshed ->
                   let merged_refresh_token =
                     match refreshed.refresh_token with
                     | Some _ -> refreshed.refresh_token
                     | None -> creds.refresh_token
                   in
                   let merged_expires_at =
                     match refreshed.expires_at with
                     | Some _ -> refreshed.expires_at
                     | None -> creds.expires_at
                   in
                   let merged_token_type =
                     match refreshed.token_type with
                     | Some t when t <> "" -> t
                     | _ -> creds.token_type
                   in
                   let updated_creds : credentials = {
                     access_token = refreshed.access_token;
                     refresh_token = merged_refresh_token;
                     expires_at = merged_expires_at;
                     token_type = merged_token_type;
                   } in
                   callbacks.persist_credentials ~account_id ~credentials:updated_creds
                     (fun () ->
                       update_health_or_fail callbacks ~account_id ~status:"healthy" ~error_message:None
                         (fun () ->
                            let refreshed_outcome : refresh_outcome =
                              Token_refreshed {
                                access_token = refreshed.access_token;
                                refresh_token = merged_refresh_token;
                                expires_at = merged_expires_at;
                                token_type = merged_token_type;
                              }
                            in
                            on_success refreshed_outcome)
                         on_error)
                     (fun err -> on_error (Credential_persist_failed err)))
                 (fun err ->
                   update_health_or_fail callbacks ~account_id ~status:"refresh_failed"
                     ~error_message:(Some err)
                     (fun () -> on_error (Refresh_failed err))
                     on_error)))
    (fun err -> on_error (Credential_load_failed err))
