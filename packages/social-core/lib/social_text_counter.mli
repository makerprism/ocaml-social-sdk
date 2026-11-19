(** Text length counting for platforms that do not count plain bytes.

    Lives in social-core because the capability table
    ({!Social_capabilities}) needs it for every platform; the Twitter
    package keeps a deprecated alias at {!Twitter_char_counter}. *)

(** Twitter's t.co wrapped URL length *)
val tco_url_length : int

(** Maximum tweet length *)
val max_tweet_length : int

(** Twitter's weighted-count rules: which code points count as 2.
    CJK ranges and emoji per
    https://developer.twitter.com/en/docs/counting-characters. *)
val is_weighted_char : int -> bool

(** Get the Twitter character weight of a single Unicode code point *)
val get_char_weight : int -> int

(** Extract URLs from text and return their (start, end, url) positions *)
val find_urls : string -> (int * int * string) list

(** Replace URLs in text with placeholders of t.co length *)
val replace_urls_with_placeholder : string -> string

(** Remove reply mentions from the beginning of text *)
val remove_reply_mentions : string -> string

(** Convert string to list of Unicode code points using Uutf. Malformed
    bytes are skipped. *)
val string_to_code_points : string -> int list

(** Fold over the Unicode code points of a UTF-8 string, skipping malformed
    bytes. *)
val fold_code_points : (int -> 'acc -> 'acc) -> 'acc -> string -> 'acc

(** Number of Unicode code points in a UTF-8 string. Every code point counts
    as one, whatever its byte length: platforms that document a "character"
    limit count [é] and [ä] as one. *)
val utf8_codepoint_length : string -> int

(** Character count per Threads' documented rule: one per code point, except
    emoji, which count as their number of UTF-8 bytes. Threads' doc:
    "Text posts are limited to 500 characters. Emojis are counted as the
    number of UTF-8 bytes"
    (https://developers.facebook.com/docs/threads/posts). Code points
    outside the BMP are always 4 UTF-8 bytes, so each contributes 4; BMP
    emoji still contribute 1 here, which under-counts their byte length and
    is accepted as the documented approximation (no grapheme segmentation in
    this library). *)
val codepoints_emoji_as_bytes_length : string -> int

(** Count characters the way Twitter does: URLs of any length count as 23
    (t.co wrapped), media attachments don't count, leading reply @mentions
    don't count, and wide code points (CJK, emoji) count as 2. See
    https://developer.twitter.com/en/docs/counting-characters.

    @param is_reply If true, @mentions at the start are not counted
    @param has_media Unused - media doesn't affect character count anymore
    @param text The text to count
    @return The weighted character count according to Twitter's rules *)
val count : ?is_reply:bool -> ?has_media:'a -> string -> int

(** Check if text is valid for Twitter (within 280 character limit)

    @param is_reply If true, @mentions at the start are not counted
    @param has_media Unused - media doesn't affect character count anymore
    @param text The text to validate
    @return true if the text fits within Twitter's character limit *)
val is_valid : ?is_reply:bool -> ?has_media:'a -> string -> bool

(** Get remaining characters available

    @param is_reply If true, @mentions at the start are not counted
    @param has_media Unused - media doesn't affect character count anymore
    @param text The text to check
    @return Number of characters remaining (negative if over limit) *)
val remaining : ?is_reply:bool -> ?has_media:'a -> string -> int
