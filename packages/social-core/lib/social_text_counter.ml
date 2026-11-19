(** Text length counting for platforms that do not count plain bytes.

    Lives in social-core because the capability table
    ({!Social_capabilities}) needs it for every platform; the Twitter
    package keeps a deprecated alias at {!Twitter_char_counter}. *)

(** Twitter's t.co wrapped URL length *)
let tco_url_length = 23

(** Maximum tweet length *)
let max_tweet_length = 280

(** [true] for the whitespace bytes the URL scan stops at: space, tab,
    newline, carriage return. *)
let is_url_whitespace c = c = ' ' || c = '\t' || c = '\n' || c = '\r'

(** Unicode ranges that count as 2 characters (weighted characters)
    Based on Twitter's actual counting rules *)
let is_weighted_char code_point =
  (* CJK characters and other double-width characters *)
  (code_point >= 0x1100 && code_point <= 0x11FF) || (* Hangul Jamo *)
  (code_point >= 0x2E80 && code_point <= 0x2EFF) || (* CJK Radicals Supplement *)
  (code_point >= 0x2F00 && code_point <= 0x2FDF) || (* Kangxi Radicals *)
  (code_point >= 0x2FF0 && code_point <= 0x2FFF) || (* Ideographic Description Characters *)
  (code_point >= 0x3000 && code_point <= 0x303F) || (* CJK Symbols and Punctuation *)
  (code_point >= 0x3040 && code_point <= 0x309F) || (* Hiragana *)
  (code_point >= 0x30A0 && code_point <= 0x30FF) || (* Katakana *)
  (code_point >= 0x3100 && code_point <= 0x312F) || (* Bopomofo *)
  (code_point >= 0x3130 && code_point <= 0x318F) || (* Hangul Compatibility Jamo *)
  (code_point >= 0x3190 && code_point <= 0x319F) || (* Kanbun *)
  (code_point >= 0x31A0 && code_point <= 0x31BF) || (* Bopomofo Extended *)
  (code_point >= 0x31C0 && code_point <= 0x31EF) || (* CJK Strokes *)
  (code_point >= 0x31F0 && code_point <= 0x31FF) || (* Katakana Phonetic Extensions *)
  (code_point >= 0x3200 && code_point <= 0x32FF) || (* Enclosed CJK Letters and Months *)
  (code_point >= 0x3300 && code_point <= 0x33FF) || (* CJK Compatibility *)
  (code_point >= 0x3400 && code_point <= 0x4DBF) || (* CJK Unified Ideographs Extension A *)
  (code_point >= 0x4DC0 && code_point <= 0x4DFF) || (* Yijing Hexagram Symbols *)
  (code_point >= 0x4E00 && code_point <= 0x9FFF) || (* CJK Unified Ideographs *)
  (code_point >= 0xA000 && code_point <= 0xA48F) || (* Yi Syllables *)
  (code_point >= 0xA490 && code_point <= 0xA4CF) || (* Yi Radicals *)
  (code_point >= 0xAC00 && code_point <= 0xD7AF) || (* Hangul Syllables *)
  (code_point >= 0xF900 && code_point <= 0xFAFF) || (* CJK Compatibility Ideographs *)
  (code_point >= 0xFE30 && code_point <= 0xFE4F) || (* CJK Compatibility Forms *)
  (code_point >= 0xFF00 && code_point <= 0xFFEF) || (* Halfwidth and Fullwidth Forms *)
  (code_point >= 0x20000 && code_point <= 0x2A6DF) || (* CJK Unified Ideographs Extension B *)
  (code_point >= 0x2A700 && code_point <= 0x2B73F) || (* CJK Unified Ideographs Extension C *)
  (code_point >= 0x2B740 && code_point <= 0x2B81F) || (* CJK Unified Ideographs Extension D *)
  (code_point >= 0x2F800 && code_point <= 0x2FA1F) || (* CJK Compatibility Ideographs Supplement *)
  (code_point >= 0x1F000) (* Most emojis count as 2 characters *)

(** Get the Twitter character weight of a single Unicode code point *)
let get_char_weight code_point =
  if is_weighted_char code_point then 2 else 1

(** Extract URLs from text and return their positions: a run of
    non-whitespace bytes starting with [http://] or [https://], first
    occurrence first. *)
let find_urls text =
  let len = String.length text in
  let rec scan i acc =
    if i >= len then List.rev acc
    else if
      (i + 7 <= len && String.sub text i 7 = "http://")
      || (i + 8 <= len && String.sub text i 8 = "https://")
    then begin
      let rec extend j = if j < len && not (is_url_whitespace text.[j]) then extend (j + 1) else j in
      let stop = extend i in
      scan stop ((i, stop, String.sub text i (stop - i)) :: acc)
    end
    else scan (i + 1) acc
  in
  scan 0 []

(** Replace URLs in text with placeholders of t.co length *)
let replace_urls_with_placeholder text =
  let urls = find_urls text in
  let rec replace_from_end text = function
    | [] -> text
    | (start, end_pos, _url) :: rest ->
        let before = String.sub text 0 start in
        let after =
          if end_pos < String.length text then
            String.sub text end_pos (String.length text - end_pos)
          else ""
        in
        let placeholder = String.make tco_url_length 'x' in
        replace_from_end (before ^ placeholder ^ after) rest
  in
  replace_from_end text (List.rev urls)

(** Remove reply mentions from the beginning of text: one or more
    [@handle] groups, each optionally followed by spaces or tabs. *)
let remove_reply_mentions text =
  let len = String.length text in
  let is_mention_char c =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
    || (c >= '0' && c <= '9') || c = '_'
  in
  let rec skip_padding i =
    if i < len && (text.[i] = ' ' || text.[i] = '\t') then skip_padding (i + 1)
    else i
  in
  let rec drop_leading i =
    if i < len && text.[i] = '@' then
      let rec handle_end j =
        if j < len && is_mention_char text.[j] then handle_end (j + 1) else j
      in
      let after_handle = handle_end (i + 1) in
      if after_handle > i + 1 then drop_leading (skip_padding after_handle) else i
    else i
  in
  let cut = drop_leading 0 in
  if cut = 0 then text else String.sub text cut (len - cut)

(** Convert string to list of Unicode code points using Uutf *)
let string_to_code_points s =
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  let rec decode acc =
    match Uutf.decode decoder with
    | `Uchar u -> decode (Uchar.to_int u :: acc)
    | `End -> List.rev acc
    | `Malformed _ -> decode acc (* Skip malformed sequences *)
    | `Await -> decode acc (* Should not happen with string input *)
  in
  decode []

(** Fold over the Unicode code points of a UTF-8 string. Malformed bytes are
    skipped, matching what a tolerant decoder sees rather than failing. *)
let fold_code_points f acc s =
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  let rec go acc =
    match Uutf.decode decoder with
    | `Uchar u -> go (f (Uchar.to_int u) acc)
    | `End -> acc
    | `Malformed _ -> go acc
    | `Await -> go acc
  in
  go acc

(** Number of Unicode code points in a UTF-8 string. Every code point counts
    as one, whatever its byte length: platforms that document a "character"
    limit count [é] and [ä] as one. Malformed bytes are skipped. *)
let utf8_codepoint_length s = fold_code_points (fun _ acc -> acc + 1) 0 s

(** Character count per Threads' documented rule: one per code point, except
    emoji, which count as their number of UTF-8 bytes. Threads' doc: "Text
    posts are limited to 500 characters. Emojis are counted as the number of
    UTF-8 bytes." Code points outside the BMP are always 4 UTF-8 bytes, so
    each contributes 4; BMP emoji still contribute 1 here, which under-counts
    their byte length and is accepted as the documented approximation (no
    grapheme segmentation in this library). *)
let codepoints_emoji_as_bytes_length s =
  fold_code_points
    (fun cp acc -> acc + if cp >= 0x10000 then 4 else 1)
    0 s

(** Count characters the way Twitter does

    @param is_reply If true, @mentions at the start are not counted
    @param has_media Unused - media doesn't affect character count anymore
    @param text The text to count
    @return The weighted character count according to Twitter's rules
*)
let count ?(is_reply=false) ?has_media:_ text =
  (* Step 1: Replace URLs with t.co placeholders *)
  let text_with_urls = replace_urls_with_placeholder text in

  (* Step 2: Remove reply mentions if this is a reply *)
  let processed_text =
    if is_reply then remove_reply_mentions text_with_urls
    else text_with_urls
  in

  (* Step 3: Count characters with weights *)
  let code_points = string_to_code_points processed_text in
  let weighted_length = List.fold_left (fun acc cp ->
    acc + get_char_weight cp
  ) 0 code_points in

  weighted_length

(** Check if text is valid for Twitter (within 280 character limit)

    @param is_reply If true, @mentions at the start are not counted
    @param has_media Unused - media doesn't affect character count anymore
    @param text The text to validate
    @return true if the text fits within Twitter's character limit
*)
let is_valid ?(is_reply=false) ?has_media text =
  let char_count = count ~is_reply ?has_media text in
  char_count <= max_tweet_length

(** Get remaining characters available

    @param is_reply If true, @mentions at the start are not counted
    @param has_media Unused - media doesn't affect character count anymore
    @param text The text to check
    @return Number of characters remaining (negative if over limit)
*)
let remaining ?(is_reply=false) ?has_media text =
  let char_count = count ~is_reply ?has_media text in
  max_tweet_length - char_count
