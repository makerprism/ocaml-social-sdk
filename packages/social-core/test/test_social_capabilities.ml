(* Tests for Social_capabilities.eligible_video_formats: the posting
   formats a video can be published in on a platform, computed from the
   format-specific constraint records. Expected values below restate the
   capability table in packages/social-core/lib/social_capabilities.ml:
   Instagram feed takes 1:1 / 4:5 / 1.91:1 up to 60s, Instagram Reels
   9:16 up to 90s, Instagram Stories 9:16 up to 60s, Facebook feed lists
   9:16 among its ratios with a 7200s cap, TikTok video (ReelFormat) runs
   to 600s, YouTube Shorts caps at 60s and 9:16, and Twitter accepts
   9:16 up to 140s in its single video mode. *)

let check_formats ~context ~platform ~width ~height ~duration expected =
  let actual =
    Social_capabilities.eligible_video_formats ~platform ~width ~height
      ~duration_seconds:duration
  in
  if actual <> expected then
    failwith
      (Printf.sprintf "%s: expected %d eligible formats but got %d" context
         (List.length expected) (List.length actual))

let test_eligible_video_formats () =
  (* A 9:16 Short is reel- and story-shaped on Instagram but is not a
     feed ratio there. *)
  check_formats ~context:"Instagram 9:16 30s" ~platform:Instagram
    ~width:1080 ~height:1920 ~duration:30.0
    [ ReelFormat; StoryFormat ];
  (* 4:5 is Instagram's feed portrait ratio. *)
  check_formats ~context:"Instagram 4:5 30s" ~platform:Instagram
    ~width:1080 ~height:1350 ~duration:30.0
    [ Social_capabilities.PostFormat ];
  (* Landscape video at Instagram's widest feed ratio is feed-only (the
     feed table has no 16:9). *)
  check_formats ~context:"Instagram 1.91:1 30s" ~platform:Instagram
    ~width:1910 ~height:1000 ~duration:30.0
    [ Social_capabilities.PostFormat ];
  (* Past the 60s story cap but within the 90s reel cap. *)
  check_formats ~context:"Instagram 9:16 61s" ~platform:Instagram
    ~width:1080 ~height:1920 ~duration:61.0
    [ ReelFormat ];
  (* Facebook's feed table lists 9:16 as well, so a vertical video is
     eligible everywhere on Facebook. *)
  check_formats ~context:"Facebook 9:16 30s" ~platform:FacebookPage
    ~width:1080 ~height:1920 ~duration:30.0
    [ Social_capabilities.PostFormat; ReelFormat;
      StoryFormat ];
  (* Landscape video exceeds no reel/story duration but is the wrong
     ratio for them. *)
  check_formats ~context:"Facebook 16:9 120s" ~platform:FacebookPage
    ~width:1920 ~height:1080 ~duration:120.0
    [ Social_capabilities.PostFormat ];
  (* TikTok's only video mode is its ReelFormat variant. *)
  check_formats ~context:"TikTok 9:16 500s" ~platform:TikTok
    ~width:1080 ~height:1920 ~duration:500.0
    [ ReelFormat ];
  (* Platforms without format variants report their single video mode. *)
  check_formats ~context:"Twitter 9:16 30s" ~platform:Twitter
    ~width:1080 ~height:1920 ~duration:30.0
    [ Social_capabilities.PostFormat ];
  (* YouTube Shorts' own cap excludes a longer vertical video from every
     format. *)
  check_formats ~context:"YouTube 9:16 30s" ~platform:YouTubeShorts
    ~width:1080 ~height:1920 ~duration:30.0
    [ Social_capabilities.PostFormat ];
  check_formats ~context:"YouTube 9:16 61s" ~platform:YouTubeShorts
    ~width:1080 ~height:1920 ~duration:61.0
    [];
  (* Unknown dimensions match no discrete ratio. *)
  check_formats ~context:"Instagram unknown dimensions" ~platform:Instagram
    ~width:0 ~height:0 ~duration:30.0
    [];
  (* A duration of 0.0 means unknown and excludes nothing. *)
  check_formats ~context:"Instagram 9:16 unknown duration" ~platform:Instagram
    ~width:1080 ~height:1920 ~duration:0.0
    [ ReelFormat; StoryFormat ];
  (* A platform with no video support has no eligible video format. *)
  check_formats ~context:"Google Business Profile" ~platform:GoogleBusinessProfile
    ~width:1080 ~height:1920 ~duration:30.0
    [];
  print_endline "PASS eligible_video_formats follows the per-format constraints"

(* ------------------------------------------------------------------ *)
(*  Constraint model: counting, alt text, aspect range, upload caps    *)
(* ------------------------------------------------------------------ *)

let repeat s n = String.concat "" (List.init n (fun _ -> s))

let accent = "\xc3\xa9" (* one code point, two UTF-8 bytes *)
let emoji = "\xf0\x9f\x98\x80" (* one code point, four UTF-8 bytes *)

let expect_bool ~context expected actual =
  if actual <> expected then
    failwith (Printf.sprintf "%s: expected %b but got %b" context expected actual)

let expect_int ~context expected actual =
  if actual <> expected then
    failwith (Printf.sprintf "%s: expected %d but got %d" context expected actual)

(* Counting: the capability table says which strategy measures each
   platform's limit, and count_text implements it. *)
let test_count_text () =
  expect_int ~context:"codepoints: 200 é" 200
    (Social_capabilities.count_text Utf8_codepoints (repeat accent 200));
  expect_int ~context:"bytes: 200 é" 400
    (Social_capabilities.count_text Utf8_bytes (repeat accent 200));
  (* YouTube counts the description in bytes. *)
  expect_int ~context:"youtube strategy is bytes" 400
    (Social_capabilities.count_text
       (Social_capabilities.get_capability YouTubeShorts).text_counting
       (repeat accent 200));
  (* Threads counts emoji as their UTF-8 byte count. *)
  expect_int ~context:"threads strategy: emoji as bytes" 500
    (Social_capabilities.count_text
       (Social_capabilities.get_capability Threads).text_counting
       (repeat accent 100 ^ repeat emoji 100));
  (* Twitter wraps URLs at 23. *)
  let url = "https://example.com/" ^ repeat "x" 81 in
  expect_int ~context:"twitter strategy: weighted" 31
    (Social_capabilities.count_text
       (Social_capabilities.get_capability Twitter).text_counting
       ("see " ^ url ^ " now"));
  print_endline "PASS count_text applies each platform's strategy"

let test_validate_text_length () =
  (* Bluesky at 300: 300 é are 300 code points (600 bytes previously
     rejected this text). *)
  (match
     Social_capabilities.validate_text_length ~platform:Bluesky
       ~text:(repeat accent 300)
   with
  | Ok () -> ()
  | Error msg -> failwith ("300 é must fit Bluesky: " ^ msg));
  (match
     Social_capabilities.validate_text_length ~platform:Bluesky
       ~text:(repeat accent 301)
   with
  | Error _ -> ()
  | Ok () -> failwith "301 é must exceed Bluesky");
  (* YouTube's 5000 description limit is in bytes: 2500 é are exactly
     5000 bytes and fit, 2501 do not. *)
  (match
     Social_capabilities.validate_text_length ~platform:YouTubeShorts
       ~text:(repeat accent 2500)
   with
  | Ok () -> ()
  | Error msg -> failwith ("2500 é are 5000 bytes and must fit: " ^ msg));
  (match
     Social_capabilities.validate_text_length ~platform:YouTubeShorts
       ~text:(repeat accent 2501)
   with
  | Error _ -> ()
  | Ok () -> failwith "2501 é are 5002 bytes and must exceed YouTube");
  print_endline "PASS validate_text_length counts per platform"

let test_alt_text_limit () =
  (* Threads documents 1000 characters; Instagram documents 1000 for image
     posts; Bluesky's lexicon documents no cap. *)
  expect_bool ~context:"threads has a cap" true
    ((Social_capabilities.get_capability Threads).alt_text_max_length = Some 1000);
  expect_bool ~context:"instagram has a cap" true
    ((Social_capabilities.get_capability Instagram).alt_text_max_length
     = Some 1000);
  expect_bool ~context:"bluesky has no documented cap" true
    ((Social_capabilities.get_capability Bluesky).alt_text_max_length = None);
  (* Counted in code points: 1000 é fit, 1001 do not. *)
  expect_bool ~context:"threads 1000 é fit" true
    (Social_capabilities.alt_text_length_violation ~platform:Threads
       ~alt_text:(repeat accent 1000)
     = None);
  (match
     Social_capabilities.alt_text_length_violation ~platform:Threads
       ~alt_text:(repeat accent 1001)
   with
  | Some (1001, 1000) -> ()
  | v -> failwith ("expected (1001, 1000), got " ^ string_of_int (match v with Some (a, _) -> a | None -> 0)));
  (* No documented cap means never a violation. *)
  expect_bool ~context:"bluesky never violates" true
    (Social_capabilities.alt_text_length_violation ~platform:Bluesky
       ~alt_text:(repeat accent 5000)
     = None);
  print_endline "PASS alt_text_length_violation follows documented caps"

let test_instagram_aspect_range () =
  (* Instagram's feed documents a 4:5..1.91:1 range, not three discrete
     ratios: 3:2 is inside and must pass (the exemplar list rejected it). *)
  expect_bool ~context:"1.5 inside range" true
    (Social_capabilities.is_aspect_ratio_compatible ~platform:Instagram
       ~aspect_ratio:1.5);
  expect_bool ~context:"0.8 boundary" true
    (Social_capabilities.is_aspect_ratio_compatible ~platform:Instagram
       ~aspect_ratio:0.8);
  expect_bool ~context:"1.91 boundary" true
    (Social_capabilities.is_aspect_ratio_compatible ~platform:Instagram
       ~aspect_ratio:1.91);
  expect_bool ~context:"0.79 outside" false
    (Social_capabilities.is_aspect_ratio_compatible ~platform:Instagram
       ~aspect_ratio:0.79);
  expect_bool ~context:"1.92 outside" false
    (Social_capabilities.is_aspect_ratio_compatible ~platform:Instagram
       ~aspect_ratio:1.92);
  (* Twitter keeps discrete exemplars with 5% tolerance: 1.5 is far from
     1:1 / 16:9. *)
  expect_bool ~context:"twitter 1.5 outside exemplars" false
    (Social_capabilities.is_aspect_ratio_compatible ~platform:Twitter
       ~aspect_ratio:1.5);
  (* Reels keep their discrete 9:16 constraint, not the feed range. *)
  expect_bool ~context:"reels keep 9:16 only" false
    (Social_capabilities.aspect_ratio_satisfied
       (Social_capabilities.get_capability_with_format Instagram
          (Some ReelFormat))
       ~aspect_ratio:1.5);
  (* A 3:2 video is now eligible for the Instagram feed format. *)
  check_formats ~context:"Instagram 3:2 30s" ~platform:Instagram
    ~width:1500 ~height:1000 ~duration:30.0
    [ Social_capabilities.PostFormat ];
  print_endline "PASS Instagram feed aspect ratio is a documented range"

let test_image_aspect_ratio_range () =
  (* The image range is its own fact, so an image-only consumer (a crop
     validator, for instance) never has to borrow the video field. *)
  let feed =
    Social_capabilities.get_capability Instagram in
  expect_bool ~context:"instagram feed documents 4:5..1.91:1"
    (feed.image_aspect_ratio_range = Some (0.8, 1.91)) true;
  expect_bool ~context:"instagram reels reset the image range"
    ((Social_capabilities.get_capability_with_format Instagram
        (Some ReelFormat))
       .image_aspect_ratio_range = None) true;
  expect_bool ~context:"instagram stories reset the image range"
    ((Social_capabilities.get_capability_with_format Instagram
        (Some StoryFormat))
       .image_aspect_ratio_range = None) true;
  expect_bool ~context:"facebook feed has no documented image range"
    ((Social_capabilities.get_capability FacebookPage)
       .image_aspect_ratio_range = None) true;
  expect_bool ~context:"threads has no documented image range"
    ((Social_capabilities.get_capability Threads)
       .image_aspect_ratio_range = None) true;
  (* Every other platform keeps [None]: an image range is only recorded
     when the platform documents one. *)
  let others =
    List.filter
      (fun c ->
         c.Social_capabilities.platform <> Instagram)
      Social_capabilities.all_capabilities
  in
  expect_bool ~context:"only instagram documents an image range"
    (List.for_all
       (fun c -> c.Social_capabilities.image_aspect_ratio_range = None)
       others) true;
  print_endline "PASS image aspect ratio range is its own capability fact"

let test_video_size_limits () =
  let mb n = n * 1024 * 1024 in
  (* The platform accepts 256GB; this library's upload path caps at 2GB,
     so validation enforces the client ceiling, not the old 256MB value. *)
  (match
     Social_capabilities.validate_video ~platform:YouTubeShorts
       ~mime_type:"video/mp4" ~file_size_bytes:(mb 300) ~duration_seconds:30.0
   with
  | Ok () -> ()
  | Error msg -> failwith ("300MB must pass the 2GB client ceiling: " ^ msg));
  (match
     Social_capabilities.validate_video ~platform:YouTubeShorts
       ~mime_type:"video/mp4" ~file_size_bytes:(mb 3000) ~duration_seconds:30.0
   with
  | Error _ -> ()
  | Ok () -> failwith "3GB must exceed the 2GB client ceiling");
  print_endline "PASS validate_video uses the effective upload ceiling"

let () =
  test_eligible_video_formats ();
  test_count_text ();
  test_validate_text_length ();
  test_alt_text_limit ();
  test_instagram_aspect_range ();
  test_image_aspect_ratio_range ();
  test_video_size_limits ()
