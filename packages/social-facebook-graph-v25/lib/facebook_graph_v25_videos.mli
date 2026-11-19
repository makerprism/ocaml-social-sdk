(** Facebook Page videos: the typed creation response of the
    [/{page-id}/videos] edge, exposed as
    [Social_facebook_graph_v25.Videos].

    The request lives in [Make] ([create_video] with a Page token,
    [post_video] with an account id); this module is pure.

    Platform facts, from the
    {{:https://developers.facebook.com/docs/graph-api/reference/page/videos/}
    Page Videos reference} (checked 2026-11-19):
    - [POST /{page-id}/videos] with [description] (UTF-8 text, emoji
      supported) and [file_url] (a publicly accessible hosted video file;
      it cannot be combined with [upload_phase], the resumable chunked
      flow) creates and publishes an ordinary Page video. Publishing needs
      a Page access token of a user with the CREATE_CONTENT Page task and
      the [pages_manage_posts], [pages_read_engagement] and
      [pages_show_list] permissions.
    - The documented return struct's only field outside the resumable
      chunked flow is [id] (numeric string): the id of the created video,
      which Facebook's read edges accept (for example
      [/{video-id}/video_insights]). The other documented fields
      ([upload_session_id], [start_offset], [end_offset], [success], ...)
      belong to [upload_phase] responses and are not parsed here.
    - [id] arrives as a JSON string; a 2xx without a string [id] reported
      no created video and is an error, not a video with an unknown id.
    - Documented creation error codes include 389 (Unable to fetch video
      file from URL), 100 (Invalid parameter), 200 (Permissions error) and
      382 (file too small); they keep their codes through the package
      error mapping.
    - The edge takes no [place] parameter: an ordinary Page video cannot
      be geotagged. Reels and stories are separate flows. *)

(** A created Page video. *)
type video = {
  id : string  (** The created video's id, as the Graph API returned it. *)
}

(** Parse the body of a successful [POST /{page-id}/videos]. A body that
    is not JSON, not an object, or has no string [id] is an [Error] with
    the offending body, because Facebook reported no created video. *)
val parse : string -> (video, string) result
