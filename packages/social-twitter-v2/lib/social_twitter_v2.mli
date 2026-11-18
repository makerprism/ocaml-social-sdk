(** Account credential coordination.

    [Make_coordinated] accepts [COORDINATED_CONFIG.with_refresh_lock]. Hold
    the account lock until persistence completes and release it before
    delivering completion. Reload credentials inside the lock. Do not acquire
    the same lock around account operations. Token-only OAuth/session helpers
    do not acquire a lock or persist account credentials.

    Provider contract: {{:https://docs.x.com/fundamentals/authentication/oauth-2-0/authorization-code}Credential renewal}.
*)
(* Re-export Twitter_v2 module contents at top level *)
include module type of Twitter_v2

(* Re-export character counter module *)
module Char_counter = Twitter_char_counter

(* OAuth 1.0a signing and auth flow *)
module Oauth1a = Twitter_oauth1a

(* OAuth 1.0a HTTP client wrapper *)
module Oauth1a_http = Twitter_oauth1a_http

(* The authenticated user ([GET /2/users/me]), shared by both OAuth flows *)
module User = Twitter_user
