(** Threads location search: the typed shape of [GET /location_search],
    exposed as [Social_threads_v1.Locations].

    The request lives in [Make] ([search_locations],
    [search_locations_for_account]); this module is pure.

    Platform facts, from
    {{:https://developers.facebook.com/docs/threads/reference/location-search}
    Location search reference} and
    {{:https://developers.facebook.com/docs/threads/create-posts/location-tagging}
    Location tagging} (checked 2026-11-17):
    - [q] (text), [latitude] + [longitude] (together), or all three; at least
      one is required.
    - [fields] defaults to [id] only, so a search that wants names must ask
      for them; {!fields} lists every documented field.
    - The result is a [data] array and is not paginated.
    - Needs [threads_basic] and [threads_location_tagging]. Until the app is
      approved for location tagging, searches return only "Menlo Park".
    - The guide's search example shows [id] as a number, its media examples
      as a string, so {!parse_location} accepts both and keeps a string.
    - A search result's [id] is what a post container takes as
      [location_id]. *)

(** A point to search near. *)
type coordinates = {
  latitude : float;
  longitude : float;
}

type location = {
  id : string;
  name : string option;
  address : string option;
  city : string option;
  country : string option;
  postal_code : string option;
  coordinates : coordinates option;
      (** [None] unless both [latitude] and [longitude] came back. *)
}

val fields : string
(** ["id,name,address,city,country,latitude,longitude,postal_code"], every
    field the reference documents, which [search_locations] requests. *)

val parse_location : Yojson.Basic.t -> location
(** Raises [Failure] when the item has no [id] (string or integer); every
    other field is optional. *)

val parse_response : string -> (location list, string) result
(** A [location_search] body: its [data] array. [Error] when the body is
    not JSON, has no [data] array, or an item has no [id]. *)
