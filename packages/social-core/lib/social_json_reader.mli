(** Shared readers for JSON responses of provider APIs.

    Every typed provider parser needs the same three moves: look up an
    object member, project it to an OCaml option, and fail with a message
    that names the field. The provider packages had private copies of
    these, and the copies disagreed on purposeful details: some dropped
    empty strings, some accepted a whole-number JSON float where others
    demanded a JSON integer. Those differences are provider facts, so this
    module keeps them as separately named primitives instead of one
    permissive reader.

    Use the variant that matches what the platform documents and what the
    existing parser accepted; do not switch a parser to a laxer reader
    while extracting it. *)

(** The JSON representation read by every reader here. *)
type json = Yojson.Basic.t

(** [member key json] is the value at [key] of a JSON object, or [`Null]
    when [json] is not an object or has no such member. A missing member
    and an explicit [null] read the same; callers that must tell them
    apart should use {!field}. *)
val member : string -> json -> json

(** [field key json] is [Some v] when [json] is an object carrying [key],
    and [None] when it is not an object or the member is absent. Unlike
    {!member}, an explicit [null] member is [Some `Null], so the caller
    can distinguish "absent" from "present but null". *)
val field : string -> json -> json option

(** [path keys json] applies {!member} along [keys]: the value at the
    nested object route, [`Null] as soon as one step is missing. *)
val path : string list -> json -> json

(** [string_opt json] is [Some s] for any JSON string, empty included,
    [None] for every other shape. Use it where the platform can return an
    empty string as a value. *)
val string_opt : json -> string option

(** [nonempty_string_opt json] is [Some s] for a nonempty JSON string,
    [None] for an empty string and every other shape. Use it where an
    empty string means "no value" (Meta list edges commonly omit optional
    string fields rather than sending [""]). *)
val nonempty_string_opt : json -> string option

(** [bool_opt json] is [Some b] for a JSON boolean, [None] otherwise. *)
val bool_opt : json -> bool option

(** [int_opt json] is [Some n] for a JSON integer, [None] for every other
    shape, JSON floats included. Use the strict reader when the platform
    sends counts as integers and a float means the response is malformed. *)
val int_opt : json -> int option

(** [int_value ~what json_opt] is [Ok n] for a JSON integer, and for a
    JSON float that is a whole number: some encoders emit [12.0] for [12].
    Any other shape, and a missing member, is [Error] with a message
    naming [what] (e.g. ["values[].value is not an integer"]), so the
    caller can report where the response was malformed. *)
val int_value : what:string -> json option -> (int, string) result

(** [data_list json] is the ["data"] array of a paging container, [[]]
    when there is none. Meta list edges wrap every page this way. *)
val data_list : json -> json list

(** [map_result f items] applies [f] to every element and keeps the first
    [Error], or collects the values. The traversal for parsers that must
    reject a malformed entry instead of dropping it silently. *)
val map_result :
  ('a -> ('b, string) result) -> 'a list -> ('b list, string) result
