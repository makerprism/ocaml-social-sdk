(** Shared Google OAuth 2.0 infrastructure

    Provides PKCE, token exchange, refresh, revocation, and user info
    retrieval for all Google-based providers (YouTube, Google Business Profile).
*)

(** {1 PKCE} *)

module Pkce : sig
  (** A fresh 43-character code verifier from the OS CSPRNG
      ({!Oauth_pkce.generate_verifier}). *)
  val generate_verifier : unit -> string

  (** Generate a Base64-URL-encoded SHA256 code challenge from a code verifier *)
  val generate_challenge : string -> string

  (** Code challenge method identifier ("S256") *)
  val challenge_method : string
end

(** {1 Endpoints} *)

module Endpoints : sig
  val authorization : string
  val token : string
  val revocation : string
  val userinfo : string
end

(** {1 Authorization URL} *)

(** Build a Google OAuth 2.0 authorization URL with PKCE.

    Forces [access_type=offline] and [prompt=consent] to ensure a refresh
    token is returned. Caller must supply scopes explicitly. *)
val get_authorization_url :
  client_id:string ->
  redirect_uri:string ->
  state:string ->
  code_verifier:string ->
  scopes:string list ->
  string

(** {1 Typed reads} *)

(** The Google account behind a token, from the OAuth2 v2 userinfo endpoint
    ({!Endpoints.userinfo}, schema [Userinfo] in
    {{:https://www.googleapis.com/discovery/v1/apis/oauth2/v2/rest}}).
    [email] needs the [userinfo.email] scope; the name and picture fields
    need [userinfo.profile]. *)
type userinfo = {
  id : string;  (** Obfuscated user id; the OpenID Connect [sub]. *)
  email : string option;
  verified_email : bool option;
  name : string option;
  given_name : string option;
  family_name : string option;
  picture : string option;
  locale : string option;
  hd : string option;  (** Hosted (Workspace) domain. *)
}

(** Parses a userinfo body; also accepts the OpenID Connect shape ([sub],
    [email_verified]). Empty strings read as [None]. [Error] without an
    id. *)
val parse_userinfo : string -> (userinfo, string) result

(** The SDK error for a failed Google API call, from the standard error
    envelope [{"error": {"code", "message", "status", "errors": [{"reason"}]}}]
    ({{:https://cloud.google.com/apis/design/errors}}):
    - 401: [Auth_error Token_invalid];
    - 429, and 403 with reason [quotaExceeded], [rateLimitExceeded],
      [userRateLimitExceeded] or [dailyLimitExceeded]: [Rate_limited];
    - other 403: [Auth_error (Insufficient_permissions {required =
      required_scopes; platform_message})];
    - 404: [Resource_not_found message];
    - otherwise [Api_error] tagged [platform], with the body kept. *)
val api_error :
  platform:Platform_types.platform ->
  required_scopes:string list ->
  status_code:int ->
  string ->
  Error_types.error

(** {1 Token Operations}

    Functor parameterized over the HTTP client. Each operation uses CPS:
    success and error continuations. *)

module Make (_ : Social_core.HTTP_CLIENT) : sig
  (** Exchange an authorization code for credentials (PKCE flow) *)
  val exchange_code :
    client_id:string ->
    client_secret:string ->
    redirect_uri:string ->
    code:string ->
    code_verifier:string ->
    (Social_core.credentials -> unit) ->
    (string -> unit) ->
    unit

  (** Refresh an access token. If Google does not return a new refresh token,
      the original is preserved in the returned credentials. If
      [prior_scope] is supplied and the refresh response omits [scope] (which
      Google does for refresh requests in the common case), the prior scope
      is carried over so the stored grant set does not silently narrow. *)
  val refresh_token :
    ?prior_scope:string ->
    client_id:string ->
    client_secret:string ->
    refresh_token:string ->
    (Social_core.credentials -> unit) ->
    (string -> unit) ->
    unit

  (** Revoke an access or refresh token *)
  val revoke_token :
    platform:Platform_types.platform ->
    token:string ->
    ((unit, Error_types.error) result -> unit) ->
    unit

  (** Fetch user info (email, name) for the authenticated user *)
  val get_user_info :
    platform:Platform_types.platform ->
    access_token:string ->
    ((Yojson.Basic.t, Error_types.error) result -> unit) ->
    unit

  (** Typed {!get_user_info}: {!userinfo}, failures through {!api_error}
      ([Network_error] when no response arrived). *)
  val get_userinfo :
    platform:Platform_types.platform ->
    access_token:string ->
    ((userinfo, Error_types.error) result -> unit) ->
    unit
end
