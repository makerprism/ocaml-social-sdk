(** Token-only subreddit discovery, exposed as [Social_reddit_v1.Reads]. No
    stored account or refresh is required. [Make] exposes
    [get_moderated_subreddits_with_token], [get_subreddit_flairs_with_token],
    and [get_subreddit_about_with_token]. All take [~access_token] and use
    [REDDIT_USER_AGENT] (SDK default when unset), just like account readers.

    Official Reddit contracts:
    - {{:https://www.reddit.com/dev/api/#GET_subreddits_mine_%7Bwhere%7D} GET
       /subreddits/mine/moderator}: [mysubreddits] scope; Listing with
      [data.children] containing [t5] subreddit objects. Optional [after] is a
      fullname cursor, [limit] defaults to 100 and is clamped to 1..100.
    - {{:https://www.reddit.com/dev/api/#GET_api_link_flair_v2} GET
       /r/subreddit/api/link_flair_v2}: [flair] scope; an array of flair
      templates. Availability depends on the user's ability to set flair.
    - {{:https://www.reddit.com/dev/api/#GET_r_%7Bsubreddit%7D_about} GET
       /r/subreddit/about}: [read] scope; a [t5] object's [data] holds the
      subreddit details, including [user_is_moderator].

    Absent/nullable optional fields remain [None]; raw JSON preserves extra
    fields. [id] is the bare subreddit ID, not a [t5_] fullname. *)

type subreddit = {
  id : string;
  display_name : string;
  subscribers : int option;
  over18 : bool option;
  user_is_moderator : bool option;
  link_flair_enabled : bool option;
  icon_img : string option;
  raw : Yojson.Basic.t;
}

type subreddit_page = {
  data : subreddit list;
  after : string option;
  before : string option;
}

type flair = {
  id : string;
  text : string option;
  text_editable : bool option;
  background_color : string option;
  text_color : string option;
  raw : Yojson.Basic.t;
}

val subreddit_of_json : Yojson.Basic.t -> (subreddit, string) result
(** Decode the inner [data] object; requires [id] and [display_name]. *)

val page_of_json : Yojson.Basic.t -> (subreddit_page, string) result
(** Decode a Listing, retaining its after/before cursors. *)

val about_of_json : Yojson.Basic.t -> (subreddit, string) result
(** Decode a subreddit object wrapper. *)

val flairs_of_json : Yojson.Basic.t -> (flair list, string) result
(** Decode the template array; each template requires [id]. *)
