(** Legacy account-health notifications. New integrations should use provider
    [Make_credentials_only] functors and decide health from operation results.
    These statuses encode compatibility policy, not platform facts. *)

type t = Healthy | Token_expired | Token_revoked | Refresh_failed | User_unlinked

val to_string : t -> string
(** Compatibility wire spelling. *)

val of_string : string -> t option
(** Parse a compatibility notification, rejecting unknown values. *)

val of_auth_error : Error_types.auth_error -> t
(** Legacy policy. In particular, insufficient permissions are mapped to
    revocation for compatibility, although those are different facts. *)

val of_error : Error_types.error -> t option
(** Legacy mapping; transport and uninterpreted API errors produce no status. *)

val ignore_updates :
  account_id:string -> status:string -> error_message:string option ->
  (unit -> unit) -> (string -> unit) -> unit
(** Disable legacy notifications without changing the credential operation's
    result. Useful for CONFIG adapters while retaining existing signatures. *)
