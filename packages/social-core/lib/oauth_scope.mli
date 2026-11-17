(** OAuth scope strings.

    Providers report granted scopes in different shapes: comma-separated
    (Meta, TikTok, Pinterest), space-separated (RFC 6749 section 3.3: Google,
    X, LinkedIn, Mastodon, Reddit) or a mix after an application stored and
    re-joined them. These helpers read every shape the same way, so a scope
    check does not depend on which provider wrote the string.

    Scope names are compared case-insensitively. No provider issues two
    scopes that differ only in case, and some echo scopes back in a
    different case than they were requested in. *)

type separator =
  | Comma  (** ["a,b"]: Meta, TikTok and Pinterest authorize URLs. *)
  | Space  (** ["a b"]: the RFC 6749 form (Google, X, LinkedIn, Reddit). *)

val split : string -> string list
(** [split raw] is the scope names in [raw], in order. Commas and any ASCII
    whitespace both delimit; empty tokens are dropped. Case is preserved, so
    the result can be stored or re-joined unchanged. *)

val join : separator -> string list -> string
(** [join sep scopes] is the scope parameter value for a provider that
    expects [sep]. Empty names are dropped. *)

val equal : string -> string -> bool
(** Case-insensitive scope name equality. *)

val mem : string -> string list -> bool
(** [mem scope scopes] is [true] if [scope] is in [scopes] ({!equal}). *)

val granted : scope:string option -> string -> bool
(** [granted ~scope target] is [true] if the raw granted-scope string [scope]
    contains [target]. [None] (no scope recorded) grants nothing. *)

val contains_all : required:string list -> string list -> bool
(** [contains_all ~required scopes] is [true] if every name in [required] is
    in [scopes]. [true] when [required] is empty. *)

val missing : requested:string list -> granted:string list -> string list
(** The names of [requested] that are not in [granted], in [requested]
    order. A provider that lets the user untick permissions returns fewer
    scopes than were requested; this is the list to show or re-request. *)

val dedup : string list -> string list
(** [dedup scopes] keeps the first occurrence of each name ({!equal}), in
    order. *)
