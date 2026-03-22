(** Shared Google OAuth 2.0 infrastructure

    Provides PKCE, token exchange, refresh, revocation, and user info
    retrieval for all Google-based providers (YouTube, Google Business Profile).
*)

(** {1 PKCE} *)

module Pkce : sig
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

(** {1 Token Operations}

    Functor parameterized over the HTTP client. Each operation uses CPS:
    success and error continuations. *)

module Make (Http : Social_core.HTTP_CLIENT) : sig
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
      the original is preserved in the returned credentials. *)
  val refresh_token :
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
end
