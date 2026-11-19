(** Request-building mechanics shared by the provider clients: query
    parameter shapes an HTTP library does not express directly, and the
    authorization and content-type headers every API call assembles.

    These are encoding mechanics only. Provider-specific parameter names
    (Rails array suffixes, XRPC keys), OAuth1 signing and per-provider
    version headers stay in their packages. *)

(** Headers as the {!Http} clients take them. *)
type headers = (string * string) list

(** Query pairs for an API that repeats the key once per value
    ([uris=a&uris=b]): one [(key, [value])] pair per element.

    [Uri.with_query] cannot express this: giving it [(key, values)] joins
    the values with commas ([key=a,b]), which the API reads as the single
    value ["a,b"]. The Rails/Mastodon array convention
    ([types[]=a&types[]=b]) is the same expansion with a caller-supplied
    [key] ending in [[]]: pass ["types[]"] here. *)
val repeated_query_param : string -> string list -> (string * string list) list

(** [repeated_query_pairs params] expands a whole [(key, values)] list
    with {!repeated_query_param}; the result shape [Uri.with_query]
    takes. *)
val repeated_query_pairs :
  (string * string list) list -> (string * string list) list

(** [percent_encoded_query params] is ["k=v&k2=v2"] with every key and
    value percent-encoded by [Uri.pct_encode] (space becomes ["%20"]).
    The encoding the OAuth endpoints and the Bluesky XRPC readers use in
    their query strings. Not the [application/x-www-form-urlencoded]
    encoding of {!Social_core.form_urlencode_kvs} (space becomes ["+"]):
    that one is for POST bodies, and swapping one for the other changes
    what the server decodes for ["+"], so keep them separate. *)
val percent_encoded_query : (string * string) list -> string

(** [bearer_pair token] is the [Authorization: Bearer] header pair for a
    required token: the element form, for splicing into a larger header
    list without reordering it. *)
val bearer_pair : string -> string * string

(** [bearer_header token] is the same pair as a one-element header list. *)
val bearer_header : string -> headers

(** [bearer_header_opt token] is the same header for an optional token,
    empty when there is none, so a call site can pass its [?access_jwt]
    straight through. *)
val bearer_header_opt : string option -> headers

(** The [Content-Type: application/json] header of a JSON request body. *)
val json_content_type : headers
