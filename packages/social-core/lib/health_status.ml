(** Account health status - the closed set of values providers may report
    through {!Social_core.CONFIG.update_health_status}.

    {1 Why this module exists}

    [CONFIG.update_health_status] takes [~status:string]. Before this module
    existed, each provider spelled the literal by hand, and at least one of
    them ([social-mastodon-v1]) spelled one that no consumer recognises
    ("invalid_token"). A consumer that parses an unknown literal has to guess,
    and guessing wrong paints a healthy account as needing reconnection.

    Providers must therefore build the string with {!to_string} rather than
    writing a literal, and consumers can parse it back with {!of_string}
    without a catch-all branch.

    {1 What is not here}

    There is deliberately no "unreachable" or "suspected" value. A server we
    could not reach has told us nothing about the stored credentials, so the
    right action is to write no status at all and leave whatever the last
    successful check recorded. {!of_error} encodes that as [None]. *)

type t =
  | Healthy
  | Token_expired
  | Token_revoked
  | Refresh_failed
  | User_unlinked

let to_string = function
  | Healthy -> "healthy"
  | Token_expired -> "token_expired"
  | Token_revoked -> "token_revoked"
  | Refresh_failed -> "refresh_failed"
  | User_unlinked -> "user_unlinked"

let of_string = function
  | "healthy" -> Some Healthy
  | "token_expired" -> Some Token_expired
  | "token_revoked" -> Some Token_revoked
  | "refresh_failed" -> Some Refresh_failed
  | "user_unlinked" -> Some User_unlinked
  | _ -> None

(** The status implied by an authentication error observed while checking or
    refreshing credentials.

    [Insufficient_permissions] maps to {!Token_revoked} because the only way a
    user can restore a missing scope is to re-run the OAuth flow, which is the
    same remedy a revoked token needs. *)
let of_auth_error : Error_types.auth_error -> t = function
  | Error_types.Token_expired -> Token_expired
  | Error_types.Token_invalid | Error_types.Token_revoked -> Token_revoked
  | Error_types.Refresh_failed _ | Error_types.Missing_credentials ->
      Refresh_failed
  | Error_types.Insufficient_permissions _ -> Token_revoked

(** [of_error err] is the health status a provider should write after [err] was
    observed while checking or refreshing an account's credentials.

    [None] means [err] is not evidence about the credentials: the server was
    unreachable, rate limited, or failed for a reason unrelated to this
    account. Callers must then leave the stored status untouched. Writing a
    credential-failure status on a connectivity blip is the bug this function
    exists to prevent.

    This assumes the provider has already turned the HTTP response into an
    {!Error_types.error}, so that a 401 arrives as an [Auth_error] rather than
    as an [Api_error] carrying [status_code = 401]. Every provider in this SDK
    does that in its response parser (for example
    [Mastodon_v1.parse_api_error]); a raw [Api_error] is treated as
    non-evidence on purpose, because its status code has not been interpreted
    against that platform's conventions. *)
let of_error : Error_types.error -> t option = function
  | Error_types.Auth_error auth -> Some (of_auth_error auth)
  | Error_types.Validation_error _
  | Error_types.Rate_limited _
  | Error_types.Api_error _
  | Error_types.Network_error _
  | Error_types.Duplicate_content
  | Error_types.Content_policy_violation _
  | Error_types.Resource_not_found _
  | Error_types.Internal_error _ ->
      None
