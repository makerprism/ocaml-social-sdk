(** Cryptographically secure random bytes from the operating system.

    For secrets the SDK generates itself (PKCE verifiers, OAuth 1.0a nonces).
    [Stdlib.Random] is not suitable: it is a seeded PRNG whose output can be
    predicted, and its default state is the same in every process unless
    something calls [Random.self_init].

    Reads [/dev/urandom], which never blocks once the kernel pool is
    initialised and needs no extra dependency or runtime-specific setup. *)

val bytes : int -> string
(** [bytes n] is [n] bytes from the operating system's CSPRNG.

    @raise Invalid_argument if [n < 0].
    @raise Failure if [/dev/urandom] cannot be read. That means the platform
    has no usable entropy source, which no caller can recover from; the SDK
    refuses to fall back to a predictable generator. *)
