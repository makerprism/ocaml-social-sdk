(** Structured detail of a provider's error response.

    Every package in this SDK reports an HTTP failure it could not map to a
    more specific {!Error_types.error} as an [Api_error], and keeps the
    response body in [raw_response]. The provider's own error code, subcode
    and trace id live inside that body, in a different envelope per platform.
    This module reads them back out, so consumers can branch on a provider
    code without substring-matching [message].

    It is an accessor over the existing [Api_error] fields rather than a new
    record field, so it works for errors from every package, needs no package
    changes, and leaves [Error_types.api_error] (and every consumer that
    builds or matches one) untouched.

    Errors a package already mapped to a specific constructor ([Auth_error],
    [Rate_limited], [Resource_not_found], ...) carry no response body, so
    {!of_error} returns [None] for them. *)

type t = {
  http_status : int;  (** HTTP status of the failed response. *)
  code : string option;
      (** The provider's error code as text: ["190"] (Meta Graph),
          ["url_ownership_unverified"] (TikTok), ["ExpiredToken"]
          (AT Protocol), ["invalid_grant"] (OAuth 2.0), ["quotaExceeded"]
          (Google [errors[0].reason]), ["ACCESS_DENIED"] (LinkedIn),
          ["187"] (X/Twitter [errors[0].code]). *)
  subcode : int option;
      (** A secondary numeric code: Meta [error_subcode], or LinkedIn
          [serviceErrorCode] when [code] holds LinkedIn's string code. *)
  error_type : string option;
      (** Meta [type] (e.g. ["OAuthException"]), Google [status]
          (e.g. ["PERMISSION_DENIED"]), or an RFC 7807 problem [type]. *)
  message : string option;
      (** The provider's own message text, never the SDK's formatted
          [Api_error.message]. *)
  trace_id : string option;
      (** The id support asks for: the [request_id] the package recorded,
          else Meta [fbtrace_id] or TikTok [log_id] from the body. *)
  body_excerpt : string option;
      (** The start of the response body as the package stored it, at most
          {!excerpt_max_bytes} bytes and never cut inside a UTF-8 sequence.
          Packages that scrub secrets from bodies (Threads, Mastodon) store
          the scrubbed body. [None] when there was no body. *)
}

val excerpt_max_bytes : int
(** 1024. *)

val of_response :
  status_code:int -> ?request_id:string -> ?redact:(string -> string) -> string -> t
(** [of_response ~status_code ?request_id ?redact body] parses a provider error
    body. A body that is not JSON, or JSON in no recognised envelope, still
    yields the status and the excerpt. [redact] is applied to the complete
    body before the excerpt is cut, so a redactor that replaces whole secrets
    still matches one that crosses the cut; it never touches the parsed
    structured fields (codes and trace ids are not prose). Recognised
    envelopes:
    - [{"error": {"code", "error_subcode", "type", "message", "fbtrace_id"}}]
      (Meta Graph), [{"error": {"code", "message", "log_id"}}] (TikTok),
      [{"error": {"code", "status", "message", "errors": [{"reason"}]}}]
      (Google);
    - [{"error": "Name", "message"}] (AT Protocol),
      [{"error": "name", "error_description"}] (OAuth 2.0). A bare
      [{"error": "..."}] is read as a code when it has no whitespace and as a
      message otherwise (Mastodon puts prose there);
    - [{"error": 403, "message"}] and [{"json": {"errors": [[code, msg, _]]}}]
      (Reddit);
    - RFC 7807 [{"title", "detail", "type"}] and [{"errors": [{"code",
      "message"}]}] (X/Twitter);
    - top-level [{"code", "message"}] (Pinterest),
      [{"code", "serviceErrorCode", "message"}] (LinkedIn) and
      [{"error_code", "error_message"}] (TikTok). *)

val of_api_error : Error_types.api_error -> t
(** Detail of an [Api_error], parsed from its [raw_response]. *)

val of_error : Error_types.error -> t option
(** [Some] detail for [Api_error], [None] for every other constructor. *)
