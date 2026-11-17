(** Facebook Page search results used as places: the typed shape of
    [GET /pages/search], exposed as [Social_facebook_graph_v25.Places].

    The request lives in [Make] ([search_places]); this module is pure.

    Platform facts, from
    {{:https://developers.facebook.com/docs/pages-api/search-pages} Search
    for a Page} (checked 2026-11-17):
    - [GET /pages/search?q=...&fields=...]. The endpoint documents [q] and
      [fields] only; paging and result limits are not documented.
    - [location] carries [city], [country], [latitude], [longitude],
      [state], [street] and [zip]. Many Pages have no location.
    - Access: a User access token with the app secret (appsecret_proof), or
      an App access token with the Page Public Metadata Access feature.
    - The old place search ([GET /search?type=place]) is deprecated since
      v8.0 and is not used.
    - A Page id returned here is what a Facebook post or an Instagram media
      container takes as its [place] / [location_id]; Meta rejects a place
      without a location. *)

type location = {
  street : string option;
  city : string option;
  state : string option;
  country : string option;
  zip : string option;
  latitude : float option;
  longitude : float option;
}

type place = {
  id : string;
  name : string option;
  location : location option;  (** [None] when the Page has no location. *)
}

val fields : string
(** ["id,name,location"], the fields [search_places] requests. *)

val coordinates : place -> (float * float) option
(** [(latitude, longitude)] when the place's location carries both. *)

val parse_place : Yojson.Basic.t -> place
(** Raises [Failure] when the item has no string [id]; every other field is
    optional. *)

val parse_response : string -> (place list, string) result
(** A [pages/search] body: its [data] array. [Error] when the body is not
    JSON, has no [data] array, or an item has no [id]. *)
