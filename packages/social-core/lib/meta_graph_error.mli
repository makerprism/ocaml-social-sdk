(** Meta Graph API error envelope and its one classification.

    Facebook Pages, Instagram and Threads all answer failures with the Graph
    envelope [{"error": {"message", "type", "code", "error_subcode",
    "fbtrace_id", ...}}] and share one code table. Each of those packages
    used to carry its own copy of that table and the copies disagreed (368
    was "temporary" in one and unknown in the others, 1/2/3/17/341 were
    missing from some, none recognised a missing object). They now all
    delegate here.

    Lives in [social-core] rather than a package of its own so that the
    provider packages gain no new dependency: consumers that pin each SDK
    package individually do not have to add a pin.

    Sources (checked 2026-11):
    - Handling errors, codes and authentication subcodes:
      {{:https://developers.facebook.com/docs/graph-api/guides/error-handling/}}
    - Rate limiting, platform and Business Use Case codes:
      {{:https://developers.facebook.com/docs/graph-api/overview/rate-limiting/}}
    - Instagram error codes (9 publishing limit, 1/2207057, media codes):
      {{:https://developers.facebook.com/docs/instagram-platform/instagram-graph-api/reference/error-codes}}
    - Code 100 / subcode 33 ("Unsupported get request. Object with ID '...'
      does not exist, cannot be loaded due to missing permissions, or does
      not support this operation") is what Graph, Instagram and Threads send
      for an id that no longer resolves. *)

type t = {
  code : int option;
  subcode : int option;  (** [error_subcode] *)
  error_type : string option;  (** [type], e.g. ["OAuthException"] *)
  message : string option;
  user_title : string option;  (** [error_user_title], localised for end users *)
  user_message : string option;  (** [error_user_msg], localised for end users *)
  fbtrace_id : string option;
  is_transient : bool option;
}

val of_body : string -> t option
(** [None] unless the body is JSON with an object under ["error"]. *)

(** Why Graph rejected the access token, from the authentication subcodes
    table of the error-handling guide. *)
type token_reason =
  | Expired  (** 463 *)
  | App_not_installed  (** 458: the user has not authorised the app, or removed it *)
  | User_checkpointed  (** 459: the user must log in on facebook.com first *)
  | Password_changed  (** 460 *)
  | Unconfirmed_user  (** 464: the user must log in on facebook.com first *)
  | Invalid_access_token  (** 467 *)
  | Invalid_session  (** 492: the user behind a Page token has no role on the Page *)
  | Unspecified  (** no subcode, or one the guide does not list *)

type classification =
  | Resource_not_found  (** 100 with subcode 33 *)
  | Invalid_request  (** any other 100 *)
  | Transient
      (** 1 unknown, 2 service: "wait and retry". Only without a subcode or
          with [is_transient = true]: Instagram reuses code 1 for validation
          failures it pins with a subcode (1/2207057, thumbnail offset out of
          range). *)
  | Rate_limited
      (** 4 app, 9 Instagram publishing limit, 17 user, 32 Page, 341 app
          limit reached, 368 temporarily blocked, 613 custom limit,
          80000-80014 Business Use Case *)
  | Token_invalid of token_reason
      (** 102 session, 190 access token, and an [OAuthException] without a
          code *)
  | Permission_denied  (** 3 capability, 10 permission denied, 200-299 *)
  | Duplicate_content  (** 506 duplicate post *)
  | Unclassified
      (** any other code. Includes 24, which Instagram sends for an unknown
          media id but Threads also sends (subcode 4279009) for a container
          that is still processing, and Instagram's media codes (9004, 352,
          36000-36004, ...). *)

val classify : t -> classification
(** Classifies by [code] first. [type = "OAuthException"] alone means an
    invalid token only when the body has no code: Instagram labels plenty of
    media errors [OAuthException] (9004 image download failed, 352 video
    format, 36003 aspect ratio), and reading those as a dead token would ask
    the user to reconnect over a bad image. *)

val to_error :
  platform:Platform_types.platform ->
  status_code:int ->
  required_permissions:string list ->
  ?retry_after_seconds:int ->
  ?request_id:string ->
  ?redact_body:(string -> string) ->
  ?redact_message:(string -> string) ->
  string ->
  Error_types.error
(** [to_error ~platform ~status_code ~required_permissions body] is the SDK
    error for a failed Graph response:
    - HTTP 429, or {!Rate_limited}: [Rate_limited] with [remaining = Some 0]
      and [retry_after_seconds];
    - {!Token_invalid}: [Auth_error Token_expired] for {!Expired} and
      {!Invalid_access_token} (the guide's action for both is "handle expired
      access tokens"), [Token_revoked] for {!App_not_installed} and
      {!Password_changed}, [Token_invalid] otherwise;
    - {!Permission_denied}: [Auth_error (Insufficient_permissions
      {required = required_permissions; platform_message})];
    - {!Resource_not_found}: [Resource_not_found message];
    - {!Duplicate_content}: [Duplicate_content];
    - {!Invalid_request}, {!Transient}, {!Unclassified} and bodies that are
      not a Graph envelope: [Api_error] with the body as [raw_response] and
      [request_id] (else [fbtrace_id]), so {!Provider_error_detail} can read
      the code back. The HTTP status is kept as sent. Graph usually sends 1
      and 2 with HTTP 500, which [Error_types.classify_error] and
      [Error_types.is_retryable] treat as transient; they only look at the
      status, so a {!Transient} body under a 4xx status reads as permanent
      there. Use {!of_error} with {!classify} to see the Graph class.

    [redact_body] is applied to the body before it is stored or used as a
    message, [redact_message] to the Graph message; both default to the
    identity. *)

val of_error : Error_types.error -> t option
(** The Graph error inside an [Api_error]'s [raw_response]; [None] for other
    constructors and for bodies that are not a Graph envelope.
    [Option.map classify (of_error err)] recovers the class of an error a
    package already converted, e.g. {!Transient} under HTTP 400. *)

val transport_error : string -> Error_types.error
(** [Network_error (Connection_failed msg)]: the request never produced a
    response, which says nothing about the account. Callers redact [msg]
    first if it may contain a URL with a token. *)
