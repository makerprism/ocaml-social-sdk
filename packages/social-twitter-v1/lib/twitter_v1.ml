(** Twitter API v1.1 Provider
    
    This implementation focuses on Twitter v1.1-specific features:
    - OAuth 1.0a authentication (signature-based)
    - Streaming API (statuses/filter, statuses/sample)
    - Legacy endpoints not available in v2
    
    For modern features, use social-twitter-v2 instead.
    This package is designed to complement v2, not replace it.
*)

open Social_provider_core

(** Configuration module type for Twitter v1.1 provider *)
module type CONFIG = sig
  module Http : HTTP_CLIENT
  
  val get_env : string -> string option
  val get_credentials : account_id:string -> (credentials -> unit) -> (string -> unit) -> unit
  val update_credentials : account_id:string -> credentials:credentials -> (unit -> unit) -> (string -> unit) -> unit
  val update_health_status : account_id:string -> status:string -> error_message:string option -> (unit -> unit) -> (string -> unit) -> unit
end

(** Make functor to create Twitter v1.1 provider with given configuration *)
module Make (Config : CONFIG) = struct
  let twitter_api_base = "https://api.twitter.com/1.1"
  let twitter_stream_base = "https://stream.twitter.com/1.1"
  let twitter_upload_base = "https://upload.twitter.com/1.1"
  
  (** OAuth 1.0a signature generation *)
  
  (** Generate nonce for OAuth *)
  let generate_nonce () =
    let random_bytes = Random.int 0x3FFFFFFF in
    Printf.sprintf "%d%f" random_bytes (Unix.gettimeofday ())
  
  (** Get current Unix timestamp *)
  let get_timestamp () =
    Printf.sprintf "%.0f" (Unix.gettimeofday ())
  
  (** URL encode a string per OAuth spec *)
  let url_encode s =
    let buffer = Buffer.create (String.length s * 2) in
    String.iter (fun c ->
      match c with
      | 'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '.' | '_' | '~' -> 
          Buffer.add_char buffer c
      | _ -> 
          Printf.bprintf buffer "%%%02X" (Char.code c)
    ) s;
    Buffer.contents buffer
  
  (** Create query string from parameters *)
  let params_to_query params =
    params
    |> List.map (fun (k, v) -> Printf.sprintf "%s=%s" (url_encode k) (url_encode v))
    |> String.concat "&"
  
  (** Create signature base string for OAuth 1.0a *)
  let create_signature_base_string ~http_method ~url ~parameters =
    let sorted_params = List.sort (fun (k1, _) (k2, _) -> String.compare k1 k2) parameters in
    let param_string = sorted_params
      |> List.map (fun (k, v) -> Printf.sprintf "%s=%s" (url_encode k) (url_encode v))
      |> String.concat "&" in
    
    Printf.sprintf "%s&%s&%s"
      (String.uppercase_ascii http_method)
      (url_encode url)
      (url_encode param_string)
  
  (** Generate HMAC-SHA1 signature *)
  let generate_signature ~consumer_secret ~token_secret ~base_string =
    let signing_key = Printf.sprintf "%s&%s" 
      (url_encode consumer_secret) 
      (url_encode token_secret) in
    
    let hmac = Cryptokit.MAC.hmac_sha1 signing_key in
    Cryptokit.hash_string hmac base_string
    |> Base64.encode_exn
  
  (** Create OAuth 1.0a Authorization header *)
  let create_oauth_header ~consumer_key ~consumer_secret ~access_token ~token_secret ~http_method ~url ?(extra_params=[]) () =
    let nonce = generate_nonce () in
    let timestamp = get_timestamp () in
    
    let oauth_params = [
      ("oauth_consumer_key", consumer_key);
      ("oauth_nonce", nonce);
      ("oauth_signature_method", "HMAC-SHA1");
      ("oauth_timestamp", timestamp);
      ("oauth_token", access_token);
      ("oauth_version", "1.0");
    ] in
    
    (* Combine OAuth params with extra params for signature *)
    let all_params = oauth_params @ extra_params in
    
    (* Generate signature *)
    let base_string = create_signature_base_string ~http_method ~url ~parameters:all_params in
    let signature = generate_signature ~consumer_secret ~token_secret ~base_string in
    
    (* Build Authorization header *)
    let oauth_params_with_sig = ("oauth_signature", signature) :: oauth_params in
    let header_value = oauth_params_with_sig
      |> List.map (fun (k, v) -> Printf.sprintf "%s=\"%s\"" k (url_encode v))
      |> String.concat ", " in
    
    Printf.sprintf "OAuth %s" header_value
  
  (** Get OAuth credentials from storage *)
  let get_oauth_credentials ~account_id on_success on_error =
    Config.get_credentials ~account_id
      (fun creds ->
        (* Extract OAuth 1.0a tokens from credentials *)
        match creds.refresh_token with
        | Some token_secret ->
            on_success (creds.access_token, token_secret)
        | None ->
            on_error "No OAuth token secret found - OAuth 1.0a requires token + secret")
      on_error
  
  (** Streaming API - Filter stream with track keywords *)
  let stream_filter ~account_id ~track ~on_tweet ~on_error =
    let consumer_key = Config.get_env "TWITTER_CONSUMER_KEY" |> Option.value ~default:"" in
    let consumer_secret = Config.get_env "TWITTER_CONSUMER_SECRET" |> Option.value ~default:"" in
    
    get_oauth_credentials ~account_id
      (fun (access_token, token_secret) ->
        let url = Printf.sprintf "%s/statuses/filter.json" twitter_stream_base in
        
        (* Build parameters *)
        let track_param = String.concat "," track in
        let post_params = [("track", track_param)] in
        
        (* Create OAuth header *)
        let auth_header = create_oauth_header
          ~consumer_key
          ~consumer_secret
          ~access_token
          ~token_secret
          ~http_method:"POST"
          ~url
          ~extra_params:post_params
          () in
        
        let headers = [
          ("Authorization", auth_header);
          ("Content-Type", "application/x-www-form-urlencoded");
        ] in
        
        let body = params_to_query post_params in
        
        (* Note: Streaming requires special handling - this is a simplified version *)
        (* Production implementation would need streaming HTTP client *)
        Config.Http.post ~headers ~body url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              (* In real implementation, this would parse streaming JSON *)
              (* For now, we just indicate success *)
              on_tweet response.body
            else
              on_error (Printf.sprintf "Stream error (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Streaming API - Sample stream (1% of all tweets) *)
  let stream_sample ~account_id ~on_tweet ~on_error =
    let consumer_key = Config.get_env "TWITTER_CONSUMER_KEY" |> Option.value ~default:"" in
    let consumer_secret = Config.get_env "TWITTER_CONSUMER_SECRET" |> Option.value ~default:"" in
    
    get_oauth_credentials ~account_id
      (fun (access_token, token_secret) ->
        let url = Printf.sprintf "%s/statuses/sample.json" twitter_stream_base in
        
        let auth_header = create_oauth_header
          ~consumer_key
          ~consumer_secret
          ~access_token
          ~token_secret
          ~http_method:"GET"
          ~url
          () in
        
        let headers = [("Authorization", auth_header)] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_tweet response.body
            else
              on_error (Printf.sprintf "Stream error (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Collections API - Create a collection *)
  let create_collection ~account_id ~name ~description ~url on_success on_error =
    let consumer_key = Config.get_env "TWITTER_CONSUMER_KEY" |> Option.value ~default:"" in
    let consumer_secret = Config.get_env "TWITTER_CONSUMER_SECRET" |> Option.value ~default:"" in
    
    get_oauth_credentials ~account_id
      (fun (access_token, token_secret) ->
        let api_url = Printf.sprintf "%s/collections/create.json" twitter_api_base in
        
        let post_params = [
          ("name", name);
        ] in
        let post_params = match description with
          | Some desc -> ("description", desc) :: post_params
          | None -> post_params in
        let post_params = match url with
          | Some u -> ("url", u) :: post_params
          | None -> post_params in
        
        let auth_header = create_oauth_header
          ~consumer_key
          ~consumer_secret
          ~access_token
          ~token_secret
          ~http_method:"POST"
          ~url:api_url
          ~extra_params:post_params
          () in
        
        let headers = [
          ("Authorization", auth_header);
          ("Content-Type", "application/x-www-form-urlencoded");
        ] in
        
        let body = params_to_query post_params in
        
        Config.Http.post ~headers ~body api_url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_success json
              with e ->
                on_error (Printf.sprintf "Failed to parse response: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "Collection creation failed (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Collections API - Add tweet to collection *)
  let add_to_collection ~account_id ~collection_id ~tweet_id on_success on_error =
    let consumer_key = Config.get_env "TWITTER_CONSUMER_KEY" |> Option.value ~default:"" in
    let consumer_secret = Config.get_env "TWITTER_CONSUMER_SECRET" |> Option.value ~default:"" in
    
    get_oauth_credentials ~account_id
      (fun (access_token, token_secret) ->
        let url = Printf.sprintf "%s/collections/entries/add.json" twitter_api_base in
        
        let post_params = [
          ("id", collection_id);
          ("tweet_id", tweet_id);
        ] in
        
        let auth_header = create_oauth_header
          ~consumer_key
          ~consumer_secret
          ~access_token
          ~token_secret
          ~http_method:"POST"
          ~url
          ~extra_params:post_params
          () in
        
        let headers = [
          ("Authorization", auth_header);
          ("Content-Type", "application/x-www-form-urlencoded");
        ] in
        
        let body = params_to_query post_params in
        
        Config.Http.post ~headers ~body url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_success ()
            else
              on_error (Printf.sprintf "Add to collection failed (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Saved Searches API - Create saved search *)
  let create_saved_search ~account_id ~query on_success on_error =
    let consumer_key = Config.get_env "TWITTER_CONSUMER_KEY" |> Option.value ~default:"" in
    let consumer_secret = Config.get_env "TWITTER_CONSUMER_SECRET" |> Option.value ~default:"" in
    
    get_oauth_credentials ~account_id
      (fun (access_token, token_secret) ->
        let url = Printf.sprintf "%s/saved_searches/create.json" twitter_api_base in
        
        let post_params = [("query", query)] in
        
        let auth_header = create_oauth_header
          ~consumer_key
          ~consumer_secret
          ~access_token
          ~token_secret
          ~http_method:"POST"
          ~url
          ~extra_params:post_params
          () in
        
        let headers = [
          ("Authorization", auth_header);
          ("Content-Type", "application/x-www-form-urlencoded");
        ] in
        
        let body = params_to_query post_params in
        
        Config.Http.post ~headers ~body url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_success json
              with e ->
                on_error (Printf.sprintf "Failed to parse response: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "Saved search creation failed (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Get embeddable HTML for a tweet (oEmbed) *)
  let get_oembed ~tweet_id ?(max_width=None) ?(hide_media=false) () on_success on_error =
    (* oEmbed endpoint doesn't require authentication *)
    let params = [("url", Printf.sprintf "https://twitter.com/i/status/%s" tweet_id)] in
    let params = match max_width with
      | Some w -> ("maxwidth", string_of_int w) :: params
      | None -> params in
    let params = if hide_media then ("hide_media", "true") :: params else params in
    
    let query = params_to_query params in
    let url = Printf.sprintf "%s/statuses/oembed.json?%s" twitter_api_base query in
    
    Config.Http.get url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let json = Yojson.Basic.from_string response.body in
            on_success json
          with e ->
            on_error (Printf.sprintf "Failed to parse oEmbed response: %s" (Printexc.to_string e))
        else
          on_error (Printf.sprintf "oEmbed request failed (%d): %s" response.status response.body))
      on_error
  
  (** Geo API - Reverse geocode coordinates to place *)
  let reverse_geocode ~lat ~long ?(granularity="neighborhood") () on_success on_error =
    (* Geo endpoint doesn't require authentication for basic queries *)
    let params = [
      ("lat", Printf.sprintf "%.6f" lat);
      ("long", Printf.sprintf "%.6f" long);
      ("granularity", granularity);
    ] in
    
    let query = params_to_query params in
    let url = Printf.sprintf "%s/geo/reverse_geocode.json?%s" twitter_api_base query in
    
    Config.Http.get url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let json = Yojson.Basic.from_string response.body in
            on_success json
          with e ->
            on_error (Printf.sprintf "Failed to parse geo response: %s" (Printexc.to_string e))
        else
          on_error (Printf.sprintf "Reverse geocode failed (%d): %s" response.status response.body))
      on_error
  
  (** Chunked media upload - INIT phase *)
  let upload_media_init ~account_id ~total_bytes ~media_type on_success on_error =
    let consumer_key = Config.get_env "TWITTER_CONSUMER_KEY" |> Option.value ~default:"" in
    let consumer_secret = Config.get_env "TWITTER_CONSUMER_SECRET" |> Option.value ~default:"" in
    
    get_oauth_credentials ~account_id
      (fun (access_token, token_secret) ->
        let url = Printf.sprintf "%s/media/upload.json" twitter_upload_base in
        
        let post_params = [
          ("command", "INIT");
          ("total_bytes", string_of_int total_bytes);
          ("media_type", media_type);
        ] in
        
        let auth_header = create_oauth_header
          ~consumer_key
          ~consumer_secret
          ~access_token
          ~token_secret
          ~http_method:"POST"
          ~url
          ~extra_params:post_params
          () in
        
        let headers = [
          ("Authorization", auth_header);
          ("Content-Type", "application/x-www-form-urlencoded");
        ] in
        
        let body = params_to_query post_params in
        
        Config.Http.post ~headers ~body url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let media_id = json
                  |> Yojson.Basic.Util.member "media_id_string"
                  |> Yojson.Basic.Util.to_string in
                on_success media_id
              with e ->
                on_error (Printf.sprintf "Failed to parse INIT response: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "Media INIT failed (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Chunked media upload - APPEND phase *)
  let upload_media_append ~account_id ~media_id ~media_data ~segment_index on_success on_error =
    let consumer_key = Config.get_env "TWITTER_CONSUMER_KEY" |> Option.value ~default:"" in
    let consumer_secret = Config.get_env "TWITTER_CONSUMER_SECRET" |> Option.value ~default:"" in
    
    get_oauth_credentials ~account_id
      (fun (access_token, token_secret) ->
        let url = Printf.sprintf "%s/media/upload.json" twitter_upload_base in
        
        (* Base64 encode the media chunk *)
        let media_data_base64 = Base64.encode_exn media_data in
        
        let post_params = [
          ("command", "APPEND");
          ("media_id", media_id);
          ("media_data", media_data_base64);
          ("segment_index", string_of_int segment_index);
        ] in
        
        let auth_header = create_oauth_header
          ~consumer_key
          ~consumer_secret
          ~access_token
          ~token_secret
          ~http_method:"POST"
          ~url
          ~extra_params:post_params
          () in
        
        let headers = [
          ("Authorization", auth_header);
          ("Content-Type", "application/x-www-form-urlencoded");
        ] in
        
        let body = params_to_query post_params in
        
        Config.Http.post ~headers ~body url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_success ()
            else
              on_error (Printf.sprintf "Media APPEND failed (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Chunked media upload - FINALIZE phase *)
  let upload_media_finalize ~account_id ~media_id on_success on_error =
    let consumer_key = Config.get_env "TWITTER_CONSUMER_KEY" |> Option.value ~default:"" in
    let consumer_secret = Config.get_env "TWITTER_CONSUMER_SECRET" |> Option.value ~default:"" in
    
    get_oauth_credentials ~account_id
      (fun (access_token, token_secret) ->
        let url = Printf.sprintf "%s/media/upload.json" twitter_upload_base in
        
        let post_params = [
          ("command", "FINALIZE");
          ("media_id", media_id);
        ] in
        
        let auth_header = create_oauth_header
          ~consumer_key
          ~consumer_secret
          ~access_token
          ~token_secret
          ~http_method:"POST"
          ~url
          ~extra_params:post_params
          () in
        
        let headers = [
          ("Authorization", auth_header);
          ("Content-Type", "application/x-www-form-urlencoded");
        ] in
        
        let body = params_to_query post_params in
        
        Config.Http.post ~headers ~body url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                (* Check if processing is required *)
                let processing_info = try
                  Some (Yojson.Basic.Util.member "processing_info" json)
                with _ -> None in
                on_success (json, processing_info)
              with e ->
                on_error (Printf.sprintf "Failed to parse FINALIZE response: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "Media FINALIZE failed (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Chunked media upload - STATUS check for async processing *)
  let upload_media_status ~account_id ~media_id on_success on_error =
    let consumer_key = Config.get_env "TWITTER_CONSUMER_KEY" |> Option.value ~default:"" in
    let consumer_secret = Config.get_env "TWITTER_CONSUMER_SECRET" |> Option.value ~default:"" in
    
    get_oauth_credentials ~account_id
      (fun (access_token, token_secret) ->
        let params = [
          ("command", "STATUS");
          ("media_id", media_id);
        ] in
        
        let query = params_to_query params in
        let url = Printf.sprintf "%s/media/upload.json?%s" twitter_upload_base query in
        
        let auth_header = create_oauth_header
          ~consumer_key
          ~consumer_secret
          ~access_token
          ~token_secret
          ~http_method:"GET"
          ~url
          ~extra_params:params
          () in
        
        let headers = [("Authorization", auth_header)] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_success json
              with e ->
                on_error (Printf.sprintf "Failed to parse STATUS response: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "Media STATUS check failed (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Helper: Complete chunked media upload with automatic chunking *)
  let upload_media_chunked ~account_id ~media_data ~media_type ?(chunk_size=5_000_000) () on_success on_error =
    let total_bytes = String.length media_data in
    let num_chunks = (total_bytes + chunk_size - 1) / chunk_size in
    
    (* Step 1: INIT *)
    upload_media_init ~account_id ~total_bytes ~media_type
      (fun media_id ->
        (* Step 2: APPEND chunks *)
        let rec append_chunk segment_index =
          if segment_index >= num_chunks then
            (* Step 3: FINALIZE *)
            upload_media_finalize ~account_id ~media_id
              (fun (json, processing_info) ->
                match processing_info with
                | Some _ ->
                    (* Media requires async processing - return media_id and processing info *)
                    on_success (media_id, Some json)
                | None ->
                    (* Media ready immediately *)
                    on_success (media_id, None))
              on_error
          else
            let start_pos = segment_index * chunk_size in
            let end_pos = min (start_pos + chunk_size) total_bytes in
            let chunk = String.sub media_data start_pos (end_pos - start_pos) in
            
            upload_media_append ~account_id ~media_id ~media_data:chunk ~segment_index
              (fun () -> append_chunk (segment_index + 1))
              on_error
        in
        append_chunk 0)
      on_error
end
