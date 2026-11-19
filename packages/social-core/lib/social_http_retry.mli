(** Shared HTTP read-retry and retry-advice mechanics: case-insensitive
    header lookup, [Retry-After]-style delay parsing, and a retry loop for
    one idempotent read.

    {1 Ownership}

    These are HTTP mechanics, not platform facts and not consumer policy:
    which statuses count as transient, how many attempts are allowed and
    how long a caller may wait belong to the caller and stay parameters of
    {!read_retry_policy}. Write operations must not use {!read_with_retry}:
    a retried non-idempotent request can double-post. *)

(** Headers as the {!Social_core.HTTP_CLIENT} calls take them. *)
type headers = (string * string) list

(** [find_header headers name] is the value of the first header whose name
    equals [name] case-insensitively, as RFC 9110 (section 5.1) specifies
    for field names. [None] when no such header exists. *)
val find_header : headers -> string -> string option

(** [header_int headers name] is the value of header [name] parsed as a
    decimal integer after trimming whitespace. [None] when the header is
    absent or not a plain integer. This is for absolute values such as
    epoch reset timestamps; a delay whose semantics are "wait this many
    seconds" should use {!delay_header_seconds} so fractional advice and
    safety caps are handled uniformly. *)
val header_int : headers -> string -> int option

(** [delay_header_seconds ~max_seconds headers name] parses a delay header
    ({{:https://www.rfc-editor.org/rfc/rfc9110#field.retry-after}RFC 9110
    Retry-After} in its delay-seconds form): a decimal number of seconds,
    with a fractional value rounded up to the next whole second (some
    servers send ["1.5"]), clamped to [0 .. max_seconds]. [max_seconds] is
    the caller's safety cap, deliberately a parameter rather than a hidden
    constant: a server that asks for a day should not make an HTTP client
    wait a day. A numeric value is always advice (a negative one clamps
    to 0, "retry now"); [None] is reserved for an absent or non-numeric
    header. The HTTP-date form of [Retry-After] is rejected ([None]); no
    consumer needed it, and accepting it would require threading a clock
    through every caller. *)
val delay_header_seconds :
  max_seconds:float -> headers -> string -> int option

(** Why a retried attempt failed; the delay policy may read the response
    headers (for example a 429's [Retry-After]) before deciding how long
    to wait. *)
type retry_trigger =
  | Retryable_status of Social_core.response  (** a transient HTTP status *)
  | Retryable_transport of string  (** a transport-level failure *)

(** How one idempotent read may be retried.

    @field max_attempts  Total tries including the first; 1 means "never
    retry".
    @field is_retryable_status  Which HTTP statuses are worth another try.
    Permanent 4xx answers (404 for a deleted resource, 401 for a bad
    token) belong here as false: retrying them only multiplies requests.
    @field retry_transport_error  Whether a transport failure (connection
    reset, timeout) is retried. Say [false] when the caller prefers to
    surface transport failures immediately.
    @field delay_seconds  Delay before the next try, given why the failed
    attempt ended and its 1-based index. Returning [0.] retries
    immediately.
    @field max_total_delay_seconds  Budget for the sum of all delays in
    this read. The next retry is skipped once the budget cannot fit, so a
    policy cannot oversleep a caller's deadline. *)
type read_retry_policy = {
  max_attempts: int;
  is_retryable_status: int -> bool;
  retry_transport_error: bool;
  delay_seconds: retry_trigger -> int -> float;
  max_total_delay_seconds: float;
}

(** [read_with_retry ~policy ~sleep ~request ~on_response
    ~on_transport_error] issues [request on_response' on_error'] up to
    [policy.max_attempts] times, sleeping through [sleep] between attempts
    as [policy.delay_seconds] directs while the delays fit
    [policy.max_total_delay_seconds].

    [request] must be an idempotent read (a GET or equivalent). Exactly
    one of [on_response] / [on_transport_error] fires, once: with the
    final response when a non-retryable status arrives, attempts run out,
    the budget cannot fit the next delay, or [sleep] reports it cannot
    schedule the backoff (the last outcome is handed back as if retries
    were exhausted, never retried immediately); with the transport error
    when the same happens on the error path. *)
val read_with_retry :
  policy:read_retry_policy ->
  sleep:(seconds:float -> (unit -> unit) -> (string -> unit) -> unit) ->
  request:((Social_core.response -> unit) -> (string -> unit) -> unit) ->
  on_response:(Social_core.response -> unit) ->
  on_transport_error:(string -> unit) ->
  unit
