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
    [ Social_capabilities.ReelFormat; Social_capabilities.StoryFormat ];
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
    [ Social_capabilities.ReelFormat ];
  (* Facebook's feed table lists 9:16 as well, so a vertical video is
     eligible everywhere on Facebook. *)
  check_formats ~context:"Facebook 9:16 30s" ~platform:FacebookPage
    ~width:1080 ~height:1920 ~duration:30.0
    [ Social_capabilities.PostFormat; Social_capabilities.ReelFormat;
      Social_capabilities.StoryFormat ];
  (* Landscape video exceeds no reel/story duration but is the wrong
     ratio for them. *)
  check_formats ~context:"Facebook 16:9 120s" ~platform:FacebookPage
    ~width:1920 ~height:1080 ~duration:120.0
    [ Social_capabilities.PostFormat ];
  (* TikTok's only video mode is its ReelFormat variant. *)
  check_formats ~context:"TikTok 9:16 500s" ~platform:TikTok
    ~width:1080 ~height:1920 ~duration:500.0
    [ Social_capabilities.ReelFormat ];
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
    [ Social_capabilities.ReelFormat; Social_capabilities.StoryFormat ];
  (* A platform with no video support has no eligible video format. *)
  check_formats ~context:"Google Business Profile" ~platform:GoogleBusinessProfile
    ~width:1080 ~height:1920 ~duration:30.0
    [];
  print_endline "PASS eligible_video_formats follows the per-format constraints"

let () =
  test_eligible_video_formats ()
