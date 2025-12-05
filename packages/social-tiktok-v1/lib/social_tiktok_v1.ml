(** TikTok Content Posting API v1 client for OCaml
    
    This module provides bindings to the TikTok Content Posting API,
    supporting video upload and OAuth 2.0 authentication.
    
    Usage:
    {[
      module Config = struct
        module Http = My_http_client
        let get_env = Sys.getenv_opt
        (* ... other config functions *)
      end
      
      module TikTok = Social_tiktok_v1.Make(Config)
      
      (* Post a video *)
      TikTok.post_single ~account_id ~text:"Check out this video!" 
        ~media_urls:["https://example.com/video.mp4"]
        (fun post_id -> print_endline ("Posted: " ^ post_id))
        (fun err -> print_endline ("Error: " ^ err))
    ]}
    
    @see <https://developers.tiktok.com/doc/content-posting-api-get-started>
*)

include Tiktok_v1
