(** The answer of Meta's [debug_token] endpoint, shared by the Facebook and
    Threads packages.

    [GET /debug_token?input_token=...&access_token=<app token>] reports what
    Meta itself recorded for a token: whether it is valid, which scopes the
    user granted (as opposed to the ones the app asked for) and, per scope,
    which objects the grant is limited to. Facebook Login lets a user grant
    [pages_manage_posts] for some Pages only; the requested scope string
    cannot tell that apart from a grant for every Page.

    References (checked 2026-11-17):
    - Facebook:
      {{:https://developers.facebook.com/docs/graph-api/reference/debug_token/}}
      Fields of [data]: [app_id], [application], [type], [expires_at],
      [data_access_expires_at], [issued_at] (unix time), [is_valid],
      [scopes] (string list), [granular_scopes] ([{scope; target_ids}], no
      [target_ids] when the grant applies to every object), [user_id],
      [profile_id], [error] ([{code; message; subcode}]).
    - Threads: {{:https://developers.facebook.com/docs/threads/reference/debug}}
      ([type], [application], [data_access_expires_at], [expires_at],
      [is_valid], [issued_at], [scopes], [user_id]).

    Lives in [social-core] next to {!Meta_graph_error} so the provider
    packages gain no new dependency. *)

type expiry =
  | Never_expires
      (** [expires_at = 0]. Meta's reference does not spell this value out,
          but long-lived Page tokens and system-user tokens report it, and
          the Access Token Debugger shows them as "Never". *)
  | Expires_at of int  (** Unix time in seconds. *)

type granular_scope = {
  scope : string;
  target_ids : string list option;
      (** [None]: the grant applies to every object. Meta documents the ids
          as integers; they arrive as strings in practice. Both are read,
          as strings. *)
}

type token_error = {
  code : int option;
  subcode : int option;
  message : string option;
}

type t = {
  is_valid : bool;
  app_id : string option;
  application : string option;
  token_type : string option;  (** [type]: ["USER"], ["PAGE"], ["APP"], ... *)
  user_id : string option;
  profile_id : string option;
  scopes : string list;  (** Granted scopes as Meta spells them. *)
  granular_scopes : granular_scope list;
  expires_at : expiry option;  (** [None] when absent. *)
  data_access_expires_at : int option;
  issued_at : int option;
  error : token_error option;  (** Why an invalid token is invalid. *)
}

val of_json : Yojson.Basic.t -> (t, string) result
(** Reads a [{"data": {...}}] response. [Error] when there is no [data]
    object or it has no boolean [is_valid]. Ids may be strings or
    integers; a member of the wrong type reads as absent. *)

val of_body : string -> (t, string) result
(** {!of_json} on a response body; [Error] for a body that is not JSON. *)

val expires_at_rfc3339 : t -> string option
(** The [expires_at] instant as RFC 3339 ({!Token_expiry} format); [None]
    when absent or {!Never_expires}. *)

val target_ids : scope:string -> t -> string list option
(** The objects [scope] is limited to, from [granular_scopes] ([None] when
    unrestricted or not listed). Scope names compare case-insensitively. *)

val granted_for_target : target_id:string -> scope:string -> t -> bool
(** [true] when [scope] is in [scopes] and, if [granular_scopes] limits it
    to a list of targets, [target_id] is in that list. A token whose
    [pages_manage_posts] grant names Pages A and B is not granted it for
    Page C. *)

val app_access_token : app_id:string -> app_secret:string -> string
(** The Facebook app access token ["app_id|app_secret"], accepted as
    [access_token] by Graph without a token request
    ({{:https://developers.facebook.com/docs/facebook-login/guides/access-tokens}}).
    Keep it server-side. *)

val threads_app_access_token : app_id:string -> app_secret:string -> string
(** The Threads app access token ["TH|app_id|app_secret"]
    ({{:https://developers.facebook.com/docs/threads/get-started/app-access-tokens}}). *)
