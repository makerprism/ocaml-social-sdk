(** Access-token lifetime arithmetic.

    OAuth 2.0 token responses carry a relative [expires_in] (RFC 6749
    section 5.1, in seconds). Storage and refresh decisions need an absolute
    instant. Every provider package converts the one into the other, and
    every application re-implements the "is it about to expire?" test; these
    are the shared versions.

    Absolute instants are RFC 3339 strings, the format of
    {!Social_core.credentials.expires_at}. They are written as UTC with
    [Ptime.to_rfc3339]'s default [-00:00] offset, the form every provider
    package in this SDK already stores, and read with any offset. *)

val max_expires_in : int
(** One year in seconds. {!expires_in_of_json} clamps to this: a provider
    that reports a longer lifetime is in practice reporting a token that
    does not expire, and a clamp keeps the arithmetic far from
    [Ptime] overflow. *)

val expires_in_of_json : Yojson.Basic.t -> int option
(** [expires_in_of_json json] reads the ["expires_in"] member of a token
    response. It accepts an integer, a float (rounded up) or a numeric
    string, since providers are not consistent about the JSON type. A
    negative value reads as [0] and a value above {!max_expires_in} as
    {!max_expires_in}. [None] when the member is absent, [null] or not a
    number. *)

val expires_at_of_expires_in : ?now:Ptime.t -> int -> string option
(** [expires_at_of_expires_in ?now seconds] is [now + seconds] as RFC 3339, for storing as [expires_at]. [now] defaults to the system clock.
    A negative [seconds] counts as [0]. [None] only if the instant is not
    representable. *)

val seconds_until : ?now:Ptime.t -> string -> int option
(** [seconds_until ?now expires_at] is the whole seconds from [now] to the
    RFC 3339 instant [expires_at], negative once it has passed. [None] when
    [expires_at] does not parse. *)

val is_expired : ?buffer_seconds:int -> ?now:Ptime.t -> string option -> bool
(** [is_expired ?buffer_seconds ?now expires_at] is [true] when the token
    expires within [buffer_seconds] (default [0]) of [now], so a caller can
    refresh ahead of time.

    - [None] is [false]: no recorded expiry means the token does not expire
      (Mastodon, Bluesky app passwords, Facebook Page tokens).
    - An [expires_at] that does not parse as RFC 3339 is [true]: a token
      whose lifetime is unknown is treated as one to refresh, not as one
      that is valid forever. *)
