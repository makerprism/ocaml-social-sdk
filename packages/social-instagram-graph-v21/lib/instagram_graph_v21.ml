(** Instagram Graph API v21 Provider
    
    This implementation supports Instagram Business accounts via Graph API.
    
    CRITICAL REQUIREMENTS:
    - Instagram Business or Creator account ONLY
    - Must be linked to a Facebook Page
    - Two-step publishing process: create container, then publish
    - Images must be publicly accessible URLs
    
    Rate Limits:
    - 200 API calls/hour per user
    - 25 container creations/hour
    - 25 posts/day
*)

open Social_provider_core

(** {1 Rate Limiting Types} *)

(** Rate limit usage information from X-App-Usage header *)
type rate_limit_info = {
  call_count : int;
  total_cputime : int;
  total_time : int;
  percentage_used : float;
}

(** Configuration module type for Instagram provider *)
module type CONFIG = sig
  module Http : HTTP_CLIENT
  
  val get_env : string -> string option
  val get_credentials : account_id:string -> (credentials -> unit) -> (string -> unit) -> unit
  val update_credentials : account_id:string -> credentials:credentials -> (unit -> unit) -> (string -> unit) -> unit
  val encrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val decrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val update_health_status : account_id:string -> status:string -> error_message:string option -> (unit -> unit) -> (string -> unit) -> unit
  val get_ig_user_id : account_id:string -> (string -> unit) -> (string -> unit) -> unit
  val sleep : float -> (unit -> 'a) -> 'a
  
  (** Optional: Called when rate limit info is updated *)
  val on_rate_limit_update : rate_limit_info -> unit
end

(** Make functor to create Instagram provider with given configuration *)
module Make (Config : CONFIG) = struct
  let graph_api_base = "https://graph.facebook.com/v21.0"
  
  (** {1 Rate Limiting} *)
  
  (** Parse X-App-Usage header *)
  let parse_rate_limit_header headers =
    try
      let usage_header = 
        List.find_opt (fun (k, _) -> 
          String.lowercase_ascii k = "x-app-usage"
        ) headers 
      in
      match usage_header with
      | Some (_, value) ->
          let json = Yojson.Basic.from_string value in
          let open Yojson.Basic.Util in
          let call_count = json |> member "call_count" |> to_int in
          let total_cputime = json |> member "total_cputime" |> to_int in
          let total_time = json |> member "total_time" |> to_int in
          let percentage_used = float_of_int call_count in
          Some {
            call_count;
            total_cputime;
            total_time;
            percentage_used;
          }
      | None -> None
    with _ -> None
  
  (** Update rate limit tracking from response *)
  let update_rate_limits response =
    match parse_rate_limit_header response.headers with
    | Some info -> Config.on_rate_limit_update info
    | None -> ()
  
  (** {1 Security - App Secret Proof} *)
  
  (** Compute HMAC-SHA256 app secret proof *)
  let compute_app_secret_proof ~access_token =
    match Config.get_env "FACEBOOK_APP_SECRET" with
    | Some app_secret ->
        let digest = Digestif.SHA256.hmac_string ~key:app_secret access_token in
        Some (Digestif.SHA256.to_hex digest)
    | None -> None
  
  (** Parse Instagram API error and return user-friendly message *)
  let parse_error_response response_body status_code =
    try
      let json = Yojson.Basic.from_string response_body in
      let open Yojson.Basic.Util in
      let error_obj = json |> member "error" in
      let error_code = 
        try error_obj |> member "code" |> to_int
        with _ -> 0
      in
      let error_message = 
        try error_obj |> member "message" |> to_string
        with _ -> response_body
      in
      
      (* Helper to check if string contains substring (case-insensitive) *)
      let string_contains_s str sub =
        try
          let _ = Str.search_forward (Str.regexp_case_fold (Str.quote sub)) str 0 in
          true
        with Not_found -> false
      in
      
      (* Map common error codes to user-friendly messages *)
      let friendly_message = match error_code with
        (* OAuth/Authentication errors *)
        | 190 -> "Instagram access token expired or invalid. Please reconnect your Instagram account."
        | 102 -> "Instagram session expired. Please reconnect your account."
        
        (* Rate limit errors *)
        | 4 -> "Instagram rate limit exceeded. You can post up to 25 times per day. Please try again later."
        | 32 -> "Instagram page rate limit exceeded. Please wait a few minutes before posting again."
        | 613 -> "Too many API calls. Please wait a few minutes and try again."
        
        (* Content errors *)
        | 100 when string_contains_s (String.lowercase_ascii error_message) "business" ->
            "This Instagram account is not a Business or Creator account. Please convert your account: Instagram Settings → Account → Switch to Professional Account"
        | 100 when string_contains_s (String.lowercase_ascii error_message) "image_url" ->
            "Instagram couldn't access the image URL. Make sure the image is publicly accessible via HTTPS."
        | 100 when string_contains_s (String.lowercase_ascii error_message) "caption" ->
            "Caption is too long. Instagram captions must be 2,200 characters or less."
        | 100 when string_contains_s (String.lowercase_ascii error_message) "creation_id" ->
            "Container not ready for publishing. The image is still being processed. Please wait a moment and try again."
        
        (* Media errors *)
        | 9004 -> "Instagram couldn't download the image. Ensure the URL is publicly accessible via HTTPS."
        | 9005 -> "Invalid image format. Please use JPEG or PNG images."
        | 352 when string_contains_s (String.lowercase_ascii error_message) "size" ->
            "Image file is too large. Instagram images must be 8 MB or less."
        
        (* Permission errors *)
        | 10 -> "Missing Instagram permissions. Please reconnect your account and grant all requested permissions."
        | 200 -> "Missing Instagram content publishing permission. Please reconnect and grant instagram_content_publish."
        
        (* Generic fallback *)
        | _ -> Printf.sprintf "Instagram API error (%d): %s" error_code error_message
      in
      
      friendly_message
    with _ ->
      (* Failed to parse JSON error - return raw response *)
      Printf.sprintf "Instagram API error (%d): %s" status_code response_body
  
  (** Check if token is expired or expiring soon *)
  let is_token_expired_buffer ~buffer_seconds expires_at_opt =
    match expires_at_opt with
    | None -> false
    | Some expires_at_str ->
        try
          match Ptime.of_rfc3339 expires_at_str with
          | Ok (expires_at, _, _) ->
              let now = Ptime_clock.now () in
              let buffer = Ptime.Span.of_int_s buffer_seconds in
              (match Ptime.add_span now buffer with
               | Some future -> not (Ptime.is_later expires_at ~than:future)
               | None -> false)
          | Error _ -> true
        with _ -> true
  
  (** Refresh long-lived token (extends validity by 60 days) *)
  let refresh_token ~access_token on_success on_error =
    let params = [
      ("grant_type", ["ig_refresh_token"]);
    ] @
    (match compute_app_secret_proof ~access_token with
     | Some proof -> [("appsecret_proof", [proof])]
     | None -> [])
    in
    
    let query = Uri.encoded_of_query params in
    let url = Printf.sprintf "https://graph.instagram.com/refresh_access_token?%s" query in
    
    let headers = [
      ("Authorization", Printf.sprintf "Bearer %s" access_token);
    ] in
    
    Config.Http.get ~headers url
      (fun response ->
        update_rate_limits response;
        if response.status >= 200 && response.status < 300 then
          try
            let json = Yojson.Basic.from_string response.body in
            let open Yojson.Basic.Util in
            let refreshed_token = json |> member "access_token" |> to_string in
            let expires_in = json |> member "expires_in" |> to_int in
            let expires_at = 
              let now = Ptime_clock.now () in
              match Ptime.add_span now (Ptime.Span.of_int_s expires_in) with
              | Some exp -> Ptime.to_rfc3339 exp
              | None -> Ptime.to_rfc3339 now
            in
            let credentials = {
              access_token = refreshed_token;
              refresh_token = None;
              expires_at = Some expires_at;
              token_type = "Bearer";
            } in
            on_success credentials
          with e ->
            on_error (Printf.sprintf "Failed to parse refresh response: %s" (Printexc.to_string e))
        else
          on_error (parse_error_response response.body response.status))
      on_error
  
  (** Ensure valid access token, refreshing if needed *)
  let ensure_valid_token ~account_id on_success on_error =
    Config.get_credentials ~account_id
      (fun creds ->
        (* Check if token needs refresh (7 day buffer before expiry) *)
        if is_token_expired_buffer ~buffer_seconds:(7 * 86400) creds.expires_at then
          (* Token is expired or expiring soon - try to refresh it *)
          refresh_token ~access_token:creds.access_token
            (fun refreshed_creds ->
              (* Update stored credentials with refreshed token *)
              Config.update_credentials ~account_id ~credentials:refreshed_creds
                (fun () ->
                  Config.update_health_status ~account_id ~status:"healthy" ~error_message:None
                    (fun () -> on_success refreshed_creds.access_token)
                    on_error)
                (fun err ->
                  (* Failed to update credentials in DB *)
                  on_error (Printf.sprintf "Failed to save refreshed token: %s" err)))
            (fun refresh_err ->
              (* Token refresh failed - mark as expired and ask user to reconnect *)
              Config.update_health_status ~account_id ~status:"token_expired" 
                ~error_message:(Some "Access token expired - please reconnect")
                (fun () -> on_error (Printf.sprintf "Token refresh failed: %s. Please reconnect your Instagram account." refresh_err))
                on_error)
        else
          (* Token is still valid *)
          Config.update_health_status ~account_id ~status:"healthy" ~error_message:None
            (fun () -> on_success creds.access_token)
            on_error)
      on_error
  
  (** Media type detection from URL *)
  let detect_media_type url =
    let url_lower = String.lowercase_ascii url in
    if Str.string_match (Str.regexp ".*\\.\\(mp4\\|mov\\|avi\\)$") url_lower 0 then
      "VIDEO"
    else if Str.string_match (Str.regexp ".*\\.\\(jpg\\|jpeg\\|png\\|gif\\)$") url_lower 0 then
      "IMAGE"
    else
      "IMAGE" (* Default to image *)
  
  (** Step 1a: Create single image container *)
  let create_image_container ~ig_user_id ~access_token ~image_url ~caption ~alt_text ~is_carousel_item on_success on_error =
    let url = Printf.sprintf "%s/%s/media" graph_api_base ig_user_id in
    
    let base_params = [
      ("image_url", [image_url]);
    ] in
    
    (* Add alt text if provided *)
    let base_with_alt = match alt_text with
      | Some alt when String.length alt > 0 ->
          ("custom_accessibility_caption", [alt]) :: base_params
      | _ -> base_params
    in
    
    let params = 
      (if is_carousel_item then
        (* Carousel items don't include caption, and mark as carousel item *)
        ("is_carousel_item", ["true"]) :: base_with_alt
      else
        (* Regular posts include caption *)
        ("caption", [caption]) :: base_with_alt) @
      (* Add app secret proof if available *)
      (match compute_app_secret_proof ~access_token with
       | Some proof -> [("appsecret_proof", [proof])]
       | None -> [])
    in
    
    let body = Uri.encoded_of_query params in
    let headers = [
      ("Content-Type", "application/x-www-form-urlencoded");
      ("Authorization", Printf.sprintf "Bearer %s" access_token);
    ] in
    
    Config.Http.post ~headers ~body url
      (fun response ->
        update_rate_limits response;
        if response.status >= 200 && response.status < 300 then
          try
            let open Yojson.Basic.Util in
            let json = Yojson.Basic.from_string response.body in
            let container_id = json |> member "id" |> to_string in
            on_success container_id
          with e ->
            on_error (Printf.sprintf "Failed to parse container response: %s" (Printexc.to_string e))
        else
          on_error (parse_error_response response.body response.status))
      on_error
  
  (** Step 1b: Create video container *)
  let create_video_container ~ig_user_id ~access_token ~video_url ~caption ~alt_text ~media_type ~is_carousel_item on_success on_error =
    let url = Printf.sprintf "%s/%s/media" graph_api_base ig_user_id in
    
    let base_params = [
      ("media_type", [media_type]); (* "VIDEO" or "REELS" *)
      ("video_url", [video_url]);
    ] in
    
    (* Add alt text if provided *)
    let base_with_alt = match alt_text with
      | Some alt when String.length alt > 0 ->
          ("custom_accessibility_caption", [alt]) :: base_params
      | _ -> base_params
    in
    
    let params = 
      (if is_carousel_item then
        (* Carousel items don't include caption, and mark as carousel item *)
        ("is_carousel_item", ["true"]) :: base_with_alt
      else
        (* Regular posts include caption *)
        ("caption", [caption]) :: base_with_alt) @
      (* Add app secret proof if available *)
      (match compute_app_secret_proof ~access_token with
       | Some proof -> [("appsecret_proof", [proof])]
       | None -> [])
    in
    
    let body = Uri.encoded_of_query params in
    let headers = [
      ("Content-Type", "application/x-www-form-urlencoded");
      ("Authorization", Printf.sprintf "Bearer %s" access_token);
    ] in
    
    Config.Http.post ~headers ~body url
      (fun response ->
        update_rate_limits response;
        if response.status >= 200 && response.status < 300 then
          try
            let open Yojson.Basic.Util in
            let json = Yojson.Basic.from_string response.body in
            let container_id = json |> member "id" |> to_string in
            on_success container_id
          with e ->
            on_error (Printf.sprintf "Failed to parse video container response: %s" (Printexc.to_string e))
        else
          on_error (parse_error_response response.body response.status))
      on_error
  
  (** Step 1c: Create carousel container from child containers *)
  let create_carousel_container ~ig_user_id ~access_token ~children_ids ~caption on_success on_error =
    let url = Printf.sprintf "%s/%s/media" graph_api_base ig_user_id in
    
    let params = [
      ("media_type", ["CAROUSEL"]);
      ("children", [String.concat "," children_ids]);
      ("caption", [caption]);
    ] @
    (* Add app secret proof if available *)
    (match compute_app_secret_proof ~access_token with
     | Some proof -> [("appsecret_proof", [proof])]
     | None -> [])
    in
    
    let body = Uri.encoded_of_query params in
    let headers = [
      ("Content-Type", "application/x-www-form-urlencoded");
      ("Authorization", Printf.sprintf "Bearer %s" access_token);
    ] in
    
    Config.Http.post ~headers ~body url
      (fun response ->
        update_rate_limits response;
        if response.status >= 200 && response.status < 300 then
          try
            let open Yojson.Basic.Util in
            let json = Yojson.Basic.from_string response.body in
            let container_id = json |> member "id" |> to_string in
            on_success container_id
          with e ->
            on_error (Printf.sprintf "Failed to parse carousel container response: %s" (Printexc.to_string e))
        else
          on_error (parse_error_response response.body response.status))
      on_error
  
  (** Step 2: Publish container *)
  let publish_container ~ig_user_id ~access_token ~container_id on_success on_error =
    let url = Printf.sprintf "%s/%s/media_publish" graph_api_base ig_user_id in
    
    let params = [
      ("creation_id", [container_id]);
    ] @
    (* Add app secret proof if available *)
    (match compute_app_secret_proof ~access_token with
     | Some proof -> [("appsecret_proof", [proof])]
     | None -> [])
    in
    
    let body = Uri.encoded_of_query params in
    let headers = [
      ("Content-Type", "application/x-www-form-urlencoded");
      ("Authorization", Printf.sprintf "Bearer %s" access_token);
    ] in
    
    Config.Http.post ~headers ~body url
      (fun response ->
        update_rate_limits response;
        if response.status >= 200 && response.status < 300 then
          try
            let open Yojson.Basic.Util in
            let json = Yojson.Basic.from_string response.body in
            let media_id = json |> member "id" |> to_string in
            on_success media_id
          with e ->
            on_error (Printf.sprintf "Failed to parse publish response: %s" (Printexc.to_string e))
        else
          on_error (parse_error_response response.body response.status))
      on_error
  
  (** Check container status *)
  let check_container_status ~container_id ~access_token on_success on_error =
    let proof_params = match compute_app_secret_proof ~access_token with
      | Some proof -> [("appsecret_proof", [proof])]
      | None -> []
    in
    let query_params = [("fields", ["status_code,status"])] @ proof_params in
    let query = Uri.encoded_of_query query_params in
    let url = Printf.sprintf "%s/%s?%s" graph_api_base container_id query in
    
    let headers = [
      ("Authorization", Printf.sprintf "Bearer %s" access_token);
    ] in
    
    Config.Http.get ~headers url
      (fun response ->
        update_rate_limits response;
        if response.status >= 200 && response.status < 300 then
          try
            let json = Yojson.Basic.from_string response.body in
            let open Yojson.Basic.Util in
            let status_code = json |> member "status_code" |> to_string in
            let status = json |> member "status" |> to_string_option |> Option.value ~default:"UNKNOWN" in
            on_success (status_code, status)
          with e ->
            on_error (Printf.sprintf "Failed to parse status: %s" (Printexc.to_string e))
        else
          on_error (Printf.sprintf "Status check failed (%d): %s" response.status response.body))
      on_error
  
  (** Poll container status with exponential backoff *)
  let rec poll_container_status ~container_id ~access_token ~ig_user_id ~attempt ~max_attempts on_success on_error =
    if attempt > max_attempts then
      on_error (Printf.sprintf "Container still processing after %d attempts. Try publishing again in a few minutes." max_attempts)
    else
      (* Exponential backoff: 2s, 3s, 5s, 8s, 13s *)
      let delay = match attempt with
        | 1 -> 2.0
        | 2 -> 3.0
        | 3 -> 5.0
        | 4 -> 8.0
        | _ -> 13.0
      in
      
      Config.sleep delay (fun () ->
        check_container_status ~container_id ~access_token
          (fun (status_code, status) ->
            match status_code with
            | "FINISHED" ->
                (* Container ready - publish it *)
                publish_container ~ig_user_id ~access_token ~container_id
                  on_success on_error
            | "ERROR" ->
                (* Container processing failed *)
                let error_detail = if status <> "UNKNOWN" && status <> "" 
                  then Printf.sprintf ": %s" status 
                  else "" in
                on_error (Printf.sprintf "Instagram container processing failed%s" error_detail)
            | "IN_PROGRESS" ->
                (* Still processing - retry with next attempt *)
                poll_container_status ~container_id ~access_token ~ig_user_id 
                  ~attempt:(attempt + 1) ~max_attempts on_success on_error
            | _ ->
                (* Unknown status code - try publishing anyway after a few attempts *)
                if attempt >= 3 then
                  publish_container ~ig_user_id ~access_token ~container_id
                    on_success on_error
                else
                  poll_container_status ~container_id ~access_token ~ig_user_id 
                    ~attempt:(attempt + 1) ~max_attempts on_success on_error)
                (fun _err ->
                  (* Status check failed - try publishing if we've waited long enough *)
                  if attempt >= 2 then
              publish_container ~ig_user_id ~access_token ~container_id
                on_success on_error
            else
              poll_container_status ~container_id ~access_token ~ig_user_id 
                ~attempt:(attempt + 1) ~max_attempts on_success on_error))
  
  (** Create child containers for carousel (recursive) *)
  let rec create_carousel_children ~ig_user_id ~access_token ~media_urls_with_alt ~index ~acc on_success on_error =
    match media_urls_with_alt with
    | [] -> on_success (List.rev acc)
    | (url, alt_text) :: rest ->
        let media_type = detect_media_type url in
        
        (match media_type with
        | "VIDEO" ->
            (* Create video carousel item *)
            create_video_container ~ig_user_id ~access_token ~video_url:url 
              ~caption:"" ~alt_text ~media_type:"VIDEO" ~is_carousel_item:true
              (fun child_id ->
                create_carousel_children ~ig_user_id ~access_token 
                  ~media_urls_with_alt:rest ~index:(index + 1) ~acc:(child_id :: acc)
                  on_success on_error)
              on_error
        | _ ->
            (* Create image carousel item *)
            create_image_container ~ig_user_id ~access_token ~image_url:url 
              ~caption:"" ~alt_text ~is_carousel_item:true
              (fun child_id ->
                create_carousel_children ~ig_user_id ~access_token 
                  ~media_urls_with_alt:rest ~index:(index + 1) ~acc:(child_id :: acc)
                  on_success on_error)
              on_error)
  
  (** Post to Instagram with two-step process *)
  let post_single ~account_id ~text ~media_urls ?(alt_texts=[]) on_success on_error =
    if List.length media_urls = 0 then
      on_error "Instagram posts require at least one image or video"
    else if List.length media_urls > 10 then
      on_error "Instagram allows maximum 10 items in a carousel post"
    else if List.length media_urls > 1 then
      (* Carousel post with 2-10 items *)
      ensure_valid_token ~account_id
        (fun access_token ->
          Config.get_ig_user_id ~account_id
            (fun ig_user_id ->
              (* Pair URLs with alt text *)
              let media_urls_with_alt = List.mapi (fun i url ->
                let alt_text = try List.nth alt_texts i with _ -> None in
                (url, alt_text)
              ) media_urls in
              (* Step 1: Create child containers for each media item *)
              create_carousel_children ~ig_user_id ~access_token ~media_urls_with_alt 
                ~index:0 ~acc:[]
                (fun children_ids ->
                  (* Step 2: Create parent carousel container *)
                  create_carousel_container ~ig_user_id ~access_token 
                    ~children_ids ~caption:text
                    (fun carousel_id ->
                      (* Step 3: Poll carousel status and publish when ready *)
                      poll_container_status ~container_id:carousel_id 
                        ~access_token ~ig_user_id ~attempt:1 ~max_attempts:5 
                        on_success on_error)
                    on_error)
                on_error)
            on_error)
        on_error
    else
      (* Single image or video post *)
      ensure_valid_token ~account_id
        (fun access_token ->
          Config.get_ig_user_id ~account_id
            (fun ig_user_id ->
              let media_url = List.hd media_urls in
              let media_type = detect_media_type media_url in
              let alt_text = try List.nth alt_texts 0 with _ -> None in
              
              (* Step 1: Create container based on media type *)
              (match media_type with
              | "VIDEO" ->
                  create_video_container ~ig_user_id ~access_token ~video_url:media_url 
                    ~caption:text ~alt_text ~media_type:"VIDEO" ~is_carousel_item:false
                    (fun container_id ->
                      (* Step 2: Poll container status and publish when ready *)
                      poll_container_status ~container_id ~access_token ~ig_user_id 
                        ~attempt:1 ~max_attempts:5 on_success on_error)
                    on_error
              | _ ->
                  create_image_container ~ig_user_id ~access_token ~image_url:media_url 
                    ~caption:text ~alt_text ~is_carousel_item:false
                    (fun container_id ->
                      (* Step 2: Poll container status and publish when ready *)
                      poll_container_status ~container_id ~access_token ~ig_user_id 
                        ~attempt:1 ~max_attempts:5 on_success on_error)
                    on_error))
            on_error)
        on_error
  
  (** Post Reel (short-form video) *)
  let post_reel ~account_id ~text ~video_url ?(alt_text=None) on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        Config.get_ig_user_id ~account_id
          (fun ig_user_id ->
            (* Create Reel container with REELS media type *)
            create_video_container ~ig_user_id ~access_token ~video_url 
              ~caption:text ~alt_text ~media_type:"REELS" ~is_carousel_item:false
              (fun container_id ->
                (* Poll and publish *)
                poll_container_status ~container_id ~access_token ~ig_user_id 
                  ~attempt:1 ~max_attempts:5 on_success on_error)
              on_error)
          on_error)
      on_error
  
  (** Post thread (Instagram doesn't support threads, posts only first item) *)
  let post_thread ~account_id ~texts ~media_urls_per_post ?(alt_texts_per_post=[]) on_success on_error =
    if List.length texts = 0 then
      on_error "No content to post"
    else
      let first_text = List.hd texts in
      let first_media = if List.length media_urls_per_post > 0 then List.hd media_urls_per_post else [] in
      let first_alt_texts = if List.length alt_texts_per_post > 0 then List.hd alt_texts_per_post else [] in
      post_single ~account_id ~text:first_text ~media_urls:first_media ~alt_texts:first_alt_texts
        (fun post_id -> on_success [post_id])
        on_error
  
  (** OAuth authorization URL *)
  let get_oauth_url ~redirect_uri ~state on_success on_error =
    let client_id = Config.get_env "FACEBOOK_APP_ID" |> Option.value ~default:"" in
    
    if client_id = "" then
      on_error "Facebook App ID not configured"
    else (
      (* Instagram OAuth via Facebook - requires both Instagram and Pages permissions *)
      let scopes = [
        "instagram_basic";
        "instagram_content_publish";
        "pages_read_engagement";
        "pages_show_list";
      ] in
      
      let scope_str = String.concat "," scopes in
      let params = [
        ("client_id", client_id);
        ("redirect_uri", redirect_uri);
        ("state", state);
        ("scope", scope_str);
        ("response_type", "code");
        ("auth_type", "rerequest");
      ] in
      
      let query = Uri.encoded_of_query (List.map (fun (k, v) -> (k, [v])) params) in
      let url = Printf.sprintf "https://www.facebook.com/v21.0/dialog/oauth?%s" query in
      on_success url
    )
  
  (** Exchange OAuth code for short-lived access token *)
  let rec exchange_code ~code ~redirect_uri on_success on_error =
    let client_id = Config.get_env "FACEBOOK_APP_ID" |> Option.value ~default:"" in
    let client_secret = Config.get_env "FACEBOOK_APP_SECRET" |> Option.value ~default:"" in
    
    if client_id = "" || client_secret = "" then
      on_error "Facebook OAuth credentials not configured"
    else (
      let params = [
        ("client_id", [client_id]);
        ("client_secret", [client_secret]);
        ("redirect_uri", [redirect_uri]);
        ("code", [code]);
      ] in
      
      let query = Uri.encoded_of_query params in
      let url = Printf.sprintf "%s/oauth/access_token?%s" graph_api_base query in
      
      Config.Http.get ~headers:[] url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              let open Yojson.Basic.Util in
              let short_lived_token = json |> member "access_token" |> to_string in
              
              (* Immediately exchange for long-lived token (60 days) *)
              exchange_for_long_lived_token ~short_lived_token
                on_success on_error
            with e ->
              on_error (Printf.sprintf "Failed to parse token response: %s" (Printexc.to_string e))
          else
            on_error (Printf.sprintf "Token exchange failed (%d): %s" response.status response.body))
        on_error
    )
  
  (** Exchange short-lived token for long-lived token (60 days) *)
  and exchange_for_long_lived_token ~short_lived_token on_success on_error =
    let client_secret = Config.get_env "FACEBOOK_APP_SECRET" |> Option.value ~default:"" in
    
    if client_secret = "" then
      on_error "Facebook App Secret not configured"
    else (
      let params = [
        ("grant_type", ["ig_exchange_token"]);
        ("client_secret", [client_secret]);
        ("access_token", [short_lived_token]);
      ] in
      
      let query = Uri.encoded_of_query params in
      let url = Printf.sprintf "https://graph.instagram.com/access_token?%s" query in
      
      Config.Http.get ~headers:[] url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              let open Yojson.Basic.Util in
              let long_lived_token = json |> member "access_token" |> to_string in
              let expires_in = json |> member "expires_in" |> to_int in
              let expires_at = 
                let now = Ptime_clock.now () in
                match Ptime.add_span now (Ptime.Span.of_int_s expires_in) with
                | Some exp -> Ptime.to_rfc3339 exp
                | None -> Ptime.to_rfc3339 now
              in
              let credentials = {
                access_token = long_lived_token;
                refresh_token = None;
                expires_at = Some expires_at;
                token_type = "Bearer";
              } in
              on_success credentials
            with e ->
              on_error (Printf.sprintf "Failed to parse long-lived token response: %s" (Printexc.to_string e))
          else
            on_error (Printf.sprintf "Long-lived token exchange failed (%d): %s" response.status response.body))
        on_error
    )
  
  (** Validate content length and hashtags *)
  let validate_content ~text =
    let len = String.length text in
    if len > 2200 then
      Error (Printf.sprintf "Instagram captions must be 2,200 characters or less (current: %d)" len)
    else
      (* Count hashtags *)
      let hashtag_count = 
        let rec count_hashtags str pos acc =
          try
            let idx = String.index_from str pos '#' in
            count_hashtags str (idx + 1) (acc + 1)
          with Not_found -> acc
        in
        count_hashtags text 0 0
      in
      if hashtag_count > 30 then
        Error (Printf.sprintf "Instagram allows maximum 30 hashtags (current: %d)" hashtag_count)
      else
        Ok ()
  
  (** Validate carousel post *)
  let validate_carousel ~media_urls =
    let count = List.length media_urls in
    if count < 2 then
      Error "Instagram carousel posts require at least 2 media items"
    else if count > 10 then
      Error (Printf.sprintf "Instagram carousel posts allow maximum 10 items (current: %d)" count)
    else
      Ok ()
  
  (** Validate video URL *)
  let validate_video ~video_url ~media_type =
    let url_lower = String.lowercase_ascii video_url in
    (* Check if URL has video extension *)
    if not (Str.string_match (Str.regexp ".*\\.\\(mp4\\|mov\\)$") url_lower 0) then
      Error "Instagram videos must be MP4 or MOV format"
    else
      match media_type with
      | "REELS" -> Ok () (* Reels: 3-90 seconds, validated by Instagram *)
      | "VIDEO" -> Ok () (* Feed videos: 3-60 seconds, validated by Instagram *)
      | _ -> Error "Invalid video media type"
  
  (** Validate media URLs for carousel *)
  let validate_carousel_items ~media_urls =
    (* All items must be accessible URLs *)
    let all_valid = List.for_all (fun url ->
      String.length url > 0 && 
      (String.starts_with ~prefix:"http://" url || String.starts_with ~prefix:"https://" url)
    ) media_urls in
    
    if not all_valid then
      Error "All carousel media items must be publicly accessible HTTP(S) URLs"
    else
      Ok ()
end
