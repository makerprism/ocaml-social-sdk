(** The authenticated X user ([GET /2/users/me]), shared by the OAuth 2.0
    ({!Twitter_v2.OAuth.Make.get_me}) and OAuth 1.0a
    ({!Twitter_oauth1a.Make.get_me}) flows.

    Reference: {{:https://docs.x.com/x-api/users/user-lookup-me}}. Host
    [api.x.com]. OAuth 2.0 user tokens need [tweet.read] and [users.read];
    OAuth 1.0a user tokens need no scope. *)

type public_metrics = {
  followers_count : int option;
  following_count : int option;
  tweet_count : int option;  (** [tweet_count], or [post_count] where X renamed it. *)
  listed_count : int option;
  like_count : int option;
  media_count : int option;
}

type t = {
  id : string;
  username : string;  (** The handle, without [@]. *)
  name : string;  (** Display name. *)
  description : string option;
  profile_image_url : string option;
  location : string option;
  url : string option;
  created_at : string option;  (** ISO 8601. *)
  protected : bool option;
  verified : bool option;
  verified_type : string option;  (** ["blue"], ["business"], ["government"], ["none"]. *)
  public_metrics : public_metrics option;
}

val me_url_base : string
(** ["https://api.x.com/2/users/me"]. *)

val default_user_fields : string list
(** [description], [profile_image_url], [location], [url], [created_at],
    [protected], [verified], [verified_type], [public_metrics]. [id],
    [name] and [username] are always returned. *)

val me_url : user_fields:string list -> string
(** {!me_url_base} with [user.fields] when the list is not empty. *)

val parse : string -> (t, string) result
(** Parses a [{"data": {...}}] body. [Error] without [id], [username] or
    [name]. *)

val error_of_response :
  status_code:int -> headers:(string * string) list -> string -> Error_types.error
(** 401 [Token_invalid]; 403 [Insufficient_permissions ["tweet.read";
    "users.read"]] with X's [detail]; 429 [Rate_limited] with the delay
    until [x-rate-limit-reset] when given; otherwise [Api_error] with the
    body. *)
