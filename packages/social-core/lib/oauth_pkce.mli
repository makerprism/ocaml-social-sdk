(** PKCE (RFC 7636) for OAuth 2.0 authorization-code flows.

    One implementation shared by every provider package, so that verifiers
    always come from {!Secure_random} and challenges are always S256. *)

val challenge_method : string
(** ["S256"], the value of [code_challenge_method]. *)

val generate_verifier : ?bytes:int -> unit -> string
(** A fresh [code_verifier]: base64url (no padding) of [bytes] random bytes
    from {!Secure_random.bytes}. The default, 32 bytes, gives the 43-character
    verifier RFC 7636 section 4.1 recommends.

    @raise Invalid_argument unless [32 <= bytes <= 96], the range that keeps
    the verifier within the 43 to 128 characters the RFC allows.
    @raise Failure if the OS entropy source cannot be read. *)

val challenge_s256 : string -> string
(** [challenge_s256 verifier] is
    [BASE64URL-ENCODE(SHA256(ASCII(verifier)))] without padding. *)
