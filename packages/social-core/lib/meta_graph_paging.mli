(** Cursor paging of Meta Graph list responses.

    The Facebook Graph API, the Instagram API and the Threads API page every
    list edge the same way:
    {v
    { "data": [ ... ],
      "paging": { "cursors": { "before": "...", "after": "..." },
                  "next": "https://graph.../{id}/{edge}?...&after=...",
                  "previous": "https://graph.../{id}/{edge}?...&before=..." } }
    v}
    Meta's rules ({{:https://developers.facebook.com/docs/graph-api/results}
    Paginated results}): [next] is absent on the last page and is the only
    reliable end-of-list signal (a page can hold fewer items than [limit],
    or none, and still have a [next]); cursors can go stale when items are
    added or removed, so do not store them for long.

    Readers in the Meta packages accept either paging model through
    {!position}:
    - cursors: pass [After c] with [c] from {!next_after} (or [Before c]),
      and the reader rebuilds the request with the same id, edge and
      options;
    - URLs: pass [Page_url next_url] and the reader requests that URL as
      Meta returned it, after {!resolve_page_url} has checked it.

    Lives in [social-core] next to {!Meta_graph_error} so that the provider
    packages share one page type without a new package dependency. *)

type cursors = {
  before : string option;  (** [paging.cursors.before]: start of this page *)
  after : string option;  (** [paging.cursors.after]: end of this page *)
}

type 'a page = {
  data : 'a list;
  paging : cursors option;  (** [None] when the response has no cursors *)
  next_url : string option;  (** [paging.next]; [None] on the last page *)
  previous_url : string option;  (** [paging.previous]; [None] on the first page *)
}

(** Which page a list read fetches. *)
type position =
  | First
  | After of string  (** The page after this [paging.cursors.after] cursor. *)
  | Before of string  (** The page before this [paging.cursors.before] cursor. *)
  | Page_url of string
      (** A [paging.next] or [paging.previous] URL from an earlier page of
          the same edge. Its query (fields, limit, filters, cursor) is used
          as is; the reader's own options are ignored. The reader replaces
          any [access_token] / [appsecret_proof] in it with its own
          credentials, and refuses a URL that {!resolve_page_url} rejects. *)

val has_next : 'a page -> bool
(** [true] when Meta returned a [paging.next] link. *)

val next_after : 'a page -> string option
(** The [after] cursor, but only when a next page exists. Meta also returns
    an [after] cursor on the last page; following it returns an empty page,
    so loops that stop on [None] should use this rather than
    [paging.after]. *)

val parse : item:(Yojson.Basic.t -> 'a) -> Yojson.Basic.t -> ('a page, string) result
(** Reads [data] (a list, required) and [paging] (optional). [item] may
    raise; the exception is reported as [Error]. *)

val of_body : item:(Yojson.Basic.t -> 'a) -> string -> ('a page, string) result
(** {!parse} on a response body. *)

val cursor_query : position -> (string * string list) list
(** [after] or [before] query parameters for a cursor position, [[]] for
    [First] and [Page_url]. *)

val resolve_page_url :
  allowed_hosts:string list -> edge:string -> string -> (Uri.t, string) result
(** Checks a [Page_url] before a reader attaches credentials to it:
    - the scheme is [https], with no user info and no explicit port;
    - the host is one of [allowed_hosts] (compared case-insensitively), so
      the caller's token is never sent to another host;
    - the last path segment is [edge] (["comments"], ["replies"], ...), so a
      URL from another edge is not parsed with the wrong item parser.
    On success the URL is returned without its [access_token] and
    [appsecret_proof] query parameters. *)
