(** Bluesky AT Protocol v1 Provider
    
    This implementation uses app password authentication and creates sessions
    for each API call. No OAuth refresh tokens needed.
*)

open Social_provider_core

(** Configuration module type for Bluesky provider *)
module type CONFIG = sig
  module Http : HTTP_CLIENT
  
  val get_env : string -> string option
  val get_credentials : account_id:string -> (credentials -> unit) -> (string -> unit) -> unit
  val update_credentials : account_id:string -> credentials:credentials -> (unit -> unit) -> (string -> unit) -> unit
  val encrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val decrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val update_health_status : account_id:string -> status:string -> error_message:string option -> (unit -> unit) -> (string -> unit) -> unit
end

(** Make functor to create Bluesky provider with given configuration *)
module Make (Config : CONFIG) = struct
  let pds_url = "https://bsky.social"
  
  (** Resolve handle to DID *)
  let resolve_handle ~handle on_success on_error =
    let url = Printf.sprintf "%s/xrpc/com.atproto.identity.resolveHandle?handle=%s" 
      pds_url (Uri.pct_encode handle) in
    Config.Http.get url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let json = Yojson.Basic.from_string response.body in
            let did = json |> Yojson.Basic.Util.member "did" |> Yojson.Basic.Util.to_string in
            on_success did
          with e ->
            on_error (Printf.sprintf "Failed to parse DID: %s" (Printexc.to_string e))
        else
          on_error (Printf.sprintf "Handle resolution failed: %s" response.body))
      on_error
  
  (** Extract facets from text (URLs, mentions, hashtags) *)
  let extract_facets text on_success on_error =
    (* URL pattern *)
    let url_pattern = Re.Pcre.regexp 
      "https?://[a-zA-Z0-9][-a-zA-Z0-9@:%._\\+~#=]{0,256}\\.[a-zA-Z0-9()]{1,6}\\b[-a-zA-Z0-9()@:%_\\+.~#?&/=]*"
    in
    
    (* Mention pattern: @handle.bsky.social or @username.com *)
    let mention_pattern = Re.Pcre.regexp "@([a-zA-Z0-9]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?\\.)+[a-zA-Z]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?" in
    
    (* Hashtag pattern: #hashtag *)
    let hashtag_pattern = Re.Pcre.regexp "#[a-zA-Z0-9_]+" in
    
    (* Extract all URL facets *)
    let url_matches = Re.all url_pattern text in
    let url_facets = List.map (fun group ->
      let url = Re.Group.get group 0 in
      let start_pos = Re.Group.start group 0 in
      let end_pos = Re.Group.stop group 0 in
      let prefix = String.sub text 0 start_pos in
      let matched = String.sub text start_pos (end_pos - start_pos) in
      let byte_start = String.length prefix in
      let byte_end = byte_start + String.length matched in
      (byte_start, byte_end, `Assoc [
        ("$type", `String "app.bsky.richtext.facet#link");
        ("uri", `String url);
      ])
    ) url_matches in
    
    (* Extract all hashtag facets *)
    let hashtag_matches = Re.all hashtag_pattern text in
    let hashtag_facets = List.map (fun group ->
      let tag = Re.Group.get group 0 in
      let start_pos = Re.Group.start group 0 in
      let end_pos = Re.Group.stop group 0 in
      let prefix = String.sub text 0 start_pos in
      let matched = String.sub text start_pos (end_pos - start_pos) in
      let byte_start = String.length prefix in
      let byte_end = byte_start + String.length matched in
      (* Remove the # prefix for the tag value *)
      let tag_value = String.sub tag 1 (String.length tag - 1) in
      (byte_start, byte_end, `Assoc [
        ("$type", `String "app.bsky.richtext.facet#tag");
        ("tag", `String tag_value);
      ])
    ) hashtag_matches in
    
    (* Extract mention handles *)
    let mention_matches = Re.all mention_pattern text in
    let mention_handles = List.map (fun group ->
      let mention = Re.Group.get group 0 in
      let start_pos = Re.Group.start group 0 in
      let end_pos = Re.Group.stop group 0 in
      let prefix = String.sub text 0 start_pos in
      let matched = String.sub text start_pos (end_pos - start_pos) in
      let byte_start = String.length prefix in
      let byte_end = byte_start + String.length matched in
      (* Remove @ prefix *)
      let handle = String.sub mention 1 (String.length mention - 1) in
      (byte_start, byte_end, handle)
    ) mention_matches in
    
    (* Resolve DIDs for all mentions *)
    let rec resolve_mentions mentions acc on_complete on_err =
      match mentions with
      | [] -> on_complete (List.rev acc)
      | (byte_start, byte_end, handle) :: rest ->
          resolve_handle ~handle
            (fun did ->
              let facet = (byte_start, byte_end, `Assoc [
                ("$type", `String "app.bsky.richtext.facet#mention");
                ("did", `String did);
              ]) in
              resolve_mentions rest (facet :: acc) on_complete on_err)
            (fun _err ->
              (* Skip mentions that fail to resolve *)
              resolve_mentions rest acc on_complete on_err)
    in
    
    resolve_mentions mention_handles []
      (fun mention_facets ->
        (* Combine all facets and format them *)
        let all_facets = url_facets @ hashtag_facets @ mention_facets in
        let formatted_facets = List.map (fun (byte_start, byte_end, feature) ->
          `Assoc [
            ("index", `Assoc [
              ("byteStart", `Int byte_start);
              ("byteEnd", `Int byte_end);
            ]);
            ("features", `List [feature]);
          ]
        ) all_facets in
        on_success formatted_facets)
      on_error
  
  (** Create session with Bluesky using app password *)
  let create_session ~identifier ~password on_success on_error =
    let url = Printf.sprintf "%s/xrpc/com.atproto.server.createSession" pds_url in
    let body = `Assoc [
      ("identifier", `String identifier);
      ("password", `String password);
    ] in
    let body_str = Yojson.Basic.to_string body in
    let headers = [("Content-Type", "application/json")] in
    
    Config.Http.post ~headers ~body:body_str url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let json = Yojson.Basic.from_string response.body in
            let open Yojson.Basic.Util in
            let did = json |> member "did" |> to_string in
            let access_jwt = json |> member "accessJwt" |> to_string in
            on_success (did, access_jwt)
          with e ->
            on_error (Printf.sprintf "Failed to parse session: %s" (Printexc.to_string e))
        else
          on_error (Printf.sprintf "Session creation failed (%d): %s" response.status response.body))
      on_error
  
  (** Upload blob to Bluesky with optional alt text *)
  let upload_blob ~access_jwt ~blob_data ~mime_type ~alt_text on_success on_error =
    let url = Printf.sprintf "%s/xrpc/com.atproto.repo.uploadBlob" pds_url in
    let headers = [
      ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
      ("Content-Type", mime_type);
    ] in
    
    Config.Http.post ~headers ~body:blob_data url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let open Yojson.Basic.Util in
            let json = Yojson.Basic.from_string response.body in
            let blob = json |> member "blob" in
            (* Return blob with alt text *)
            on_success (blob, alt_text)
          with e ->
            on_error (Printf.sprintf "Failed to parse blob response: %s" (Printexc.to_string e))
        else
          on_error (Printf.sprintf "Bluesky blob upload error (%d): %s" response.status response.body))
      on_error
  
  (** Extract OpenGraph meta tag content from HTML *)
  let extract_og_tag html property =
    (* Match: <meta property="og:title" content="..."> or <meta content="..." property="og:title"> *)
    let patterns = [
      Printf.sprintf "<meta[^>]*property=['\"]og:%s['\"][^>]*content=['\"]([^'\"]*)['\"]" property;
      Printf.sprintf "<meta[^>]*content=['\"]([^'\"]*)['\"][^>]*property=['\"]og:%s['\"]" property;
    ] in
    let rec try_patterns pats =
      match pats with
      | [] -> None
      | pattern :: rest ->
          try
            let regex = Re.Pcre.regexp ~flags:[`CASELESS] pattern in
            let group = Re.exec regex html in
            Some (Re.Group.get group 1)
          with Not_found -> try_patterns rest
    in
    try_patterns patterns

  (** Fetch link card metadata by scraping OpenGraph tags *)
  let fetch_link_card ~access_jwt ~url on_success _on_error =
    (* Log link card fetch attempt *)
    Printf.eprintf "[Bluesky] Attempting to fetch link card for URL: %s\n%!" url;
    
    (* Fetch the HTML content of the URL *)
    Config.Http.get url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let html = response.body in
            
            (* Extract OpenGraph metadata *)
            let title = extract_og_tag html "title" in
            let description = extract_og_tag html "description" in
            let image_url = extract_og_tag html "image" in
            
            (* Log extracted metadata *)
            Printf.eprintf "[Bluesky] Link card metadata - title: %s, description: %s, image: %s\n%!"
              (Option.value title ~default:"(none)")
              (Option.value description ~default:"(none)")
              (Option.value image_url ~default:"(none)");
            
            (* Only create card if we have at least a title *)
            match title with
            | None -> 
                Printf.eprintf "[Bluesky] No og:title found, skipping link card for %s\n%!" url;
                on_success None
            | Some title_str ->
                Printf.eprintf "[Bluesky] Creating link card with title: %s\n%!" title_str;
                let description_str = match description with
                  | Some d -> d
                  | None -> ""
                in
                
                (* Helper to complete card creation *)
                let create_card thumb_blob =
                  let external_fields = [
                    ("uri", `String url);
                    ("title", `String title_str);
                    ("description", `String description_str);
                  ] in
                  let external_with_thumb = match thumb_blob with
                    | Some blob -> external_fields @ [("thumb", blob)]
                    | None -> external_fields
                  in
                  let card = `Assoc [
                    ("$type", `String "app.bsky.embed.external");
                    ("external", `Assoc external_with_thumb);
                  ] in
                  Printf.eprintf "[Bluesky] Link card created successfully (with%s thumbnail)\n%!" 
                    (if thumb_blob = None then "out" else "");
                  on_success (Some card)
                in
                
                (* If there's an image, fetch and upload it *)
                match image_url with
                | None -> 
                    Printf.eprintf "[Bluesky] No og:image found, creating card without thumbnail\n%!";
                    create_card None
                | Some img_url ->
                    (* Handle relative URLs *)
                    let full_img_url = 
                      if String.contains img_url ':' then img_url
                      else
                        (* Parse base URL and append relative path *)
                        let base_uri = Uri.of_string url in
                        let scheme = Uri.scheme base_uri |> Option.value ~default:"https" in
                        let host = Uri.host base_uri |> Option.value ~default:"" in
                        if String.length img_url > 0 && img_url.[0] = '/' then
                          Printf.sprintf "%s://%s%s" scheme host img_url
                        else
                          Printf.sprintf "%s://%s/%s" scheme host img_url
                    in
                    
                    Printf.eprintf "[Bluesky] Fetching thumbnail image: %s\n%!" full_img_url;
                    
                    (* Fetch the image *)
                    Config.Http.get full_img_url
                      (fun img_response ->
                        if img_response.status >= 200 && img_response.status < 300 then
                          (* Get mime type from response headers *)
                          let mime_type = 
                            List.assoc_opt "content-type" img_response.headers
                            |> Option.value ~default:"image/jpeg"
                          in
                          
                          (* Check size limit (1MB max for images) *)
                          let img_size = String.length img_response.body in
                          Printf.eprintf "[Bluesky] Thumbnail downloaded: %d bytes, mime: %s\n%!" img_size mime_type;
                          
                          if img_size > 1000000 then
                            (* Image too large, skip it *)
                            (Printf.eprintf "[Bluesky] Thumbnail too large (%d bytes > 1MB), creating card without it\n%!" img_size;
                             create_card None)
                          else
                            (* Upload the image as a blob *)
                            (Printf.eprintf "[Bluesky] Uploading thumbnail as blob...\n%!";
                             upload_blob ~access_jwt ~blob_data:img_response.body 
                               ~mime_type ~alt_text:None
                               (fun (blob, _) -> 
                                 Printf.eprintf "[Bluesky] Thumbnail uploaded successfully\n%!";
                                 create_card (Some blob))
                               (fun err -> 
                                 Printf.eprintf "[Bluesky] Thumbnail upload failed: %s, creating card without it\n%!" err;
                                 create_card None))
                        else
                          (* Failed to fetch image, create card without it *)
                          (Printf.eprintf "[Bluesky] Failed to fetch thumbnail (HTTP %d), creating card without it\n%!" img_response.status;
                           create_card None))
                      (fun err -> 
                        Printf.eprintf "[Bluesky] HTTP error fetching thumbnail: %s, creating card without it\n%!" err;
                        create_card None)
          with e ->
            (* Don't fail post if link card parsing fails *)
            Printf.eprintf "[Bluesky] Exception parsing link card: %s, skipping card\n%!" (Printexc.to_string e);
            on_success None
        else
          (* Don't fail post if URL fetch fails *)
          Printf.eprintf "[Bluesky] Failed to fetch URL (HTTP %d), skipping link card\n%!" response.status;
          on_success None)
      (fun err -> 
        Printf.eprintf "[Bluesky] HTTP error fetching URL: %s, skipping link card\n%!" err;
        on_success None)
  
  (** Ensure valid session token *)
  let ensure_valid_token ~account_id on_success on_error =
    Config.get_credentials ~account_id
      (fun creds ->
        (* Bluesky uses identifier (handle/email) as access_token and password as refresh_token *)
        match creds.refresh_token with
        | None ->
            Config.update_health_status ~account_id ~status:"refresh_failed" 
              ~error_message:(Some "No app password available")
              (fun () -> on_error "No app password available - please reconnect")
              on_error
        | Some password ->
            (* Create new session *)
            create_session ~identifier:creds.access_token ~password
              (fun (_did, access_jwt) ->
                Config.update_health_status ~account_id ~status:"healthy" ~error_message:None
                  (fun () -> on_success access_jwt)
                  on_error)
              (fun err ->
                Config.update_health_status ~account_id ~status:"refresh_failed" 
                  ~error_message:(Some ("Session creation failed: " ^ err))
                  (fun () -> on_error err)
                  on_error))
      on_error
  
  (** Post with optional reply references *)
  let post_with_reply ~account_id ~text ~media_urls ?(alt_texts=[]) ~reply_refs on_success on_error =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        Config.get_credentials ~account_id
          (fun creds ->
            let identifier = creds.access_token in
            
            (* Pair URLs with alt text - use None if alt text list is shorter *)
            let urls_with_alt = List.mapi (fun i url ->
              let alt_text = try List.nth alt_texts i with _ -> None in
              (url, alt_text)
            ) media_urls in
            
            (* Helper to upload multiple blobs in sequence *)
            let rec upload_blobs_seq urls_with_alt acc on_complete on_err =
              match urls_with_alt with
              | [] -> on_complete (List.rev acc)
              | (url, alt_text) :: rest ->
                  (* Fetch media from URL *)
                  Config.Http.get url
                    (fun media_resp ->
                      if media_resp.status >= 200 && media_resp.status < 300 then
                        let mime_type = 
                          List.assoc_opt "content-type" media_resp.headers 
                          |> Option.value ~default:"application/octet-stream"
                        in
                        (* Upload blob with alt text *)
                        upload_blob ~access_jwt ~blob_data:media_resp.body ~mime_type ~alt_text
                          (fun (blob, alt) -> upload_blobs_seq rest ((blob, alt) :: acc) on_complete on_err)
                          on_err
                      else
                        on_err (Printf.sprintf "Failed to fetch media from %s" url))
                    on_err
            in
            
            (* Upload media if provided (max 4 images) *)
            let media_to_upload = List.filteri (fun i _ -> i < 4) urls_with_alt in
            upload_blobs_seq media_to_upload []
              (fun blobs ->
                (* Extract facets from text *)
                extract_facets text
                  (fun facets ->
                    (* Create post record *)
                    let now = Ptime.to_rfc3339 ~frac_s:6 ~tz_offset_s:0 (Ptime_clock.now ()) in
                    
                    let base_record = [
                      ("$type", `String "app.bsky.feed.post");
                      ("text", `String text);
                      ("createdAt", `String now);
                    ] in
                    
                    let base_with_facets = 
                      if List.length facets > 0 then
                        base_record @ [("facets", `List facets)]
                      else
                        base_record
                    in
                    
                    (* Add reply references if provided *)
                    let base_with_reply = match reply_refs with
                      | None -> base_with_facets
                      | Some (root_uri, root_cid, parent_uri, parent_cid) ->
                          base_with_facets @ [
                            ("reply", `Assoc [
                              ("root", `Assoc [
                                ("uri", `String root_uri);
                                ("cid", `String root_cid);
                              ]);
                              ("parent", `Assoc [
                                ("uri", `String parent_uri);
                                ("cid", `String parent_cid);
                              ]);
                            ])
                          ]
                    in
                
                (* Add embed based on content *)
                let post_record_cont =
                  if List.length blobs > 0 then
                    (* Images present *)
                    (Printf.eprintf "[Bluesky] Post has %d images, skipping link card extraction\n%!" (List.length blobs);
                     let images_json = `List (List.map (fun (blob, alt_text_opt) ->
                       let alt_text = match alt_text_opt with
                         | Some alt when String.length alt > 0 -> alt
                         | _ -> ""
                       in
                       `Assoc [
                         ("alt", `String alt_text);
                         ("image", blob);
                       ]
                     ) blobs) in
                     fun on_rec_success ->
                       on_rec_success (`Assoc (base_with_reply @ [
                         ("embed", `Assoc [
                           ("$type", `String "app.bsky.embed.images");
                           ("images", images_json);
                         ])
                       ])))
                  else
                    (* Try external link card *)
                    (Printf.eprintf "[Bluesky] No images attached, checking for URLs in text...\n%!";
                     let first_url =
                       try
                         let url_pattern = Re.Pcre.regexp 
                           "https?://[a-zA-Z0-9][-a-zA-Z0-9@:%._\\+~#=]{0,256}\\.[a-zA-Z0-9()]{1,6}\\b[-a-zA-Z0-9()@:%_\\+.~#?&/=]*"
                         in
                         let group = Re.exec url_pattern text in
                         Some (Re.Group.get group 0)
                       with Not_found -> None
                     in
                     match first_url with
                     | None -> 
                         (Printf.eprintf "[Bluesky] No URLs found in text, posting without embed\n%!";
                          fun on_rec_success -> on_rec_success (`Assoc base_with_reply))
                     | Some url ->
                         (Printf.eprintf "[Bluesky] Found URL in text: %s\n%!" url;
                          fun on_rec_success ->
                            fetch_link_card ~access_jwt ~url
                              (fun card_opt ->
                                match card_opt with
                                | None -> on_rec_success (`Assoc base_with_reply)
                                | Some card_json ->
                                    (* card_json is already the complete embed structure *)
                                    try
                                      let embed = card_json in
                                      on_rec_success (`Assoc (base_with_reply @ [("embed", embed)]))
                                    with _ ->
                                      on_rec_success (`Assoc base_with_reply))
                              (fun _ -> on_rec_success (`Assoc base_with_reply))))
                in
                
                (* Continue with post creation *)
                post_record_cont
                  (fun post_record ->
                    let url = Printf.sprintf "%s/xrpc/com.atproto.repo.createRecord" pds_url in
                    let body = `Assoc [
                      ("repo", `String identifier);
                      ("collection", `String "app.bsky.feed.post");
                      ("record", post_record);
                    ] in
                    let body_str = Yojson.Basic.to_string body in
                    let headers = [
                      ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
                      ("Content-Type", "application/json");
                    ] in
                    
                    Config.Http.post ~headers ~body:body_str url
                      (fun response ->
                        if response.status >= 200 && response.status < 300 then
                          try
                            let json = Yojson.Basic.from_string response.body in
                            let open Yojson.Basic.Util in
                            let post_uri = json |> member "uri" |> to_string in
                            let post_cid = json |> member "cid" |> to_string in
                            (* Return both URI and CID as "uri|cid" *)
                            on_success (Printf.sprintf "%s|%s" post_uri post_cid)
                          with e ->
                            on_error (Printf.sprintf "Failed to parse response: %s" (Printexc.to_string e))
                        else
                          on_error (Printf.sprintf "Bluesky API error (%d): %s" response.status response.body))
                      on_error))
                  on_error) (* Close extract_facets callback *)
              on_error)
          on_error)
      on_error
  
  (** Post single post without reply refs *)
  let post_single ~account_id ~text ~media_urls ?(alt_texts=[]) on_success on_error =
    post_with_reply ~account_id ~text ~media_urls ~alt_texts ~reply_refs:None on_success on_error
  
  (** Post thread to Bluesky *)
  let post_thread ~account_id ~texts ~media_urls_per_post ?(alt_texts_per_post=[]) on_success on_error =
    if List.length texts = 0 then
      on_error "No posts in thread"
    else
      (* Pair media URLs with alt text for each post *)
      let media_with_alt_per_post = List.map2 (fun media_urls alt_texts ->
        (media_urls, alt_texts)
      ) media_urls_per_post (alt_texts_per_post @ List.init (List.length media_urls_per_post - List.length alt_texts_per_post) (fun _ -> [])) in
      
      (* Helper to post thread items sequentially *)
      let rec post_thread_items remaining_texts remaining_media root_ref parent_ref acc_uris =
        match remaining_texts with
        | [] -> on_success (List.rev acc_uris)
        | text :: rest_texts ->
            let (media, alt_texts) = match remaining_media with
              | [] -> ([], [])
              | (m, a) :: _ -> (m, a)
            in
            let rest_media = match remaining_media with
              | [] -> []
              | _ :: r -> r
            in
            
            let reply_refs = match root_ref with
              | None -> None  (* First post, no reply *)
              | Some (root_uri, root_cid) ->
                  (* Subsequent posts are replies *)
                  match parent_ref with
                  | Some (parent_uri, parent_cid) ->
                      Some (root_uri, root_cid, parent_uri, parent_cid)
                  | None -> 
                      (* Should not happen, but use root as parent *)
                      Some (root_uri, root_cid, root_uri, root_cid)
            in
            
            post_with_reply ~account_id ~text ~media_urls:media ~alt_texts ~reply_refs
              (fun uri_cid ->
                (* Parse URI and CID from "uri|cid" format *)
                match String.split_on_char '|' uri_cid with
                | [uri; cid] ->
                    let new_root_ref = match root_ref with
                      | None -> Some (uri, cid)  (* First post becomes root *)
                      | Some r -> Some r  (* Keep existing root *)
                    in
                    let new_parent_ref = Some (uri, cid) in
                    post_thread_items rest_texts rest_media new_root_ref new_parent_ref (uri_cid :: acc_uris)
                | _ ->
                    on_error "Failed to parse post response (expected uri|cid format)")
              on_error
      in
      
      post_thread_items texts media_with_alt_per_post None None []
  
  (** Delete a post from Bluesky *)
  let delete_post ~account_id ~post_uri on_success on_error =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        Config.get_credentials ~account_id
          (fun creds ->
            let identifier = creds.access_token in
            let uri_parts = String.split_on_char '/' post_uri in
            let rkey = List.nth uri_parts (List.length uri_parts - 1) in
            
            let url = Printf.sprintf "%s/xrpc/com.atproto.repo.deleteRecord" pds_url in
            let body = `Assoc [
              ("repo", `String identifier);
              ("collection", `String "app.bsky.feed.post");
              ("rkey", `String rkey);
            ] in
            let body_str = Yojson.Basic.to_string body in
            let headers = [
              ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
              ("Content-Type", "application/json");
            ] in
            
            Config.Http.post ~headers ~body:body_str url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  on_success ()
                else
                  on_error (Printf.sprintf "Delete failed (%d): %s" response.status response.body))
              on_error)
          on_error)
      on_error
  
  (** Like a post *)
  let like_post ~account_id ~post_uri ~post_cid on_success on_error =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        Config.get_credentials ~account_id
          (fun creds ->
            let identifier = creds.access_token in
            let now = Ptime.to_rfc3339 ~frac_s:6 ~tz_offset_s:0 (Ptime_clock.now ()) in
            
            let url = Printf.sprintf "%s/xrpc/com.atproto.repo.createRecord" pds_url in
            let body = `Assoc [
              ("repo", `String identifier);
              ("collection", `String "app.bsky.feed.like");
              ("record", `Assoc [
                ("$type", `String "app.bsky.feed.like");
                ("subject", `Assoc [
                  ("uri", `String post_uri);
                  ("cid", `String post_cid);
                ]);
                ("createdAt", `String now);
              ]);
            ] in
            let body_str = Yojson.Basic.to_string body in
            let headers = [
              ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
              ("Content-Type", "application/json");
            ] in
            
            Config.Http.post ~headers ~body:body_str url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  try
                    let json = Yojson.Basic.from_string response.body in
                    let like_uri = json 
                      |> Yojson.Basic.Util.member "uri" 
                      |> Yojson.Basic.Util.to_string in
                    on_success like_uri
                  with e ->
                    on_error (Printf.sprintf "Failed to parse like response: %s" (Printexc.to_string e))
                else
                  on_error (Printf.sprintf "Like failed (%d): %s" response.status response.body))
              on_error)
          on_error)
      on_error
  
  (** Unlike a post *)
  let unlike_post ~account_id ~like_uri on_success on_error =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        Config.get_credentials ~account_id
          (fun creds ->
            let identifier = creds.access_token in
            let uri_parts = String.split_on_char '/' like_uri in
            let rkey = List.nth uri_parts (List.length uri_parts - 1) in
            
            let url = Printf.sprintf "%s/xrpc/com.atproto.repo.deleteRecord" pds_url in
            let body = `Assoc [
              ("repo", `String identifier);
              ("collection", `String "app.bsky.feed.like");
              ("rkey", `String rkey);
            ] in
            let body_str = Yojson.Basic.to_string body in
            let headers = [
              ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
              ("Content-Type", "application/json");
            ] in
            
            Config.Http.post ~headers ~body:body_str url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  on_success ()
                else
                  on_error (Printf.sprintf "Unlike failed (%d): %s" response.status response.body))
              on_error)
          on_error)
      on_error
  
  (** Repost a post *)
  let repost ~account_id ~post_uri ~post_cid on_success on_error =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        Config.get_credentials ~account_id
          (fun creds ->
            let identifier = creds.access_token in
            let now = Ptime.to_rfc3339 ~frac_s:6 ~tz_offset_s:0 (Ptime_clock.now ()) in
            
            let url = Printf.sprintf "%s/xrpc/com.atproto.repo.createRecord" pds_url in
            let body = `Assoc [
              ("repo", `String identifier);
              ("collection", `String "app.bsky.feed.repost");
              ("record", `Assoc [
                ("$type", `String "app.bsky.feed.repost");
                ("subject", `Assoc [
                  ("uri", `String post_uri);
                  ("cid", `String post_cid);
                ]);
                ("createdAt", `String now);
              ]);
            ] in
            let body_str = Yojson.Basic.to_string body in
            let headers = [
              ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
              ("Content-Type", "application/json");
            ] in
            
            Config.Http.post ~headers ~body:body_str url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  try
                    let json = Yojson.Basic.from_string response.body in
                    let repost_uri = json 
                      |> Yojson.Basic.Util.member "uri" 
                      |> Yojson.Basic.Util.to_string in
                    on_success repost_uri
                  with e ->
                    on_error (Printf.sprintf "Failed to parse repost response: %s" (Printexc.to_string e))
                else
                  on_error (Printf.sprintf "Repost failed (%d): %s" response.status response.body))
              on_error)
          on_error)
      on_error
  
  (** Unrepost *)
  let unrepost ~account_id ~repost_uri on_success on_error =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        Config.get_credentials ~account_id
          (fun creds ->
            let identifier = creds.access_token in
            let uri_parts = String.split_on_char '/' repost_uri in
            let rkey = List.nth uri_parts (List.length uri_parts - 1) in
            
            let url = Printf.sprintf "%s/xrpc/com.atproto.repo.deleteRecord" pds_url in
            let body = `Assoc [
              ("repo", `String identifier);
              ("collection", `String "app.bsky.feed.repost");
              ("rkey", `String rkey);
            ] in
            let body_str = Yojson.Basic.to_string body in
            let headers = [
              ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
              ("Content-Type", "application/json");
            ] in
            
            Config.Http.post ~headers ~body:body_str url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  on_success ()
                else
                  on_error (Printf.sprintf "Unrepost failed (%d): %s" response.status response.body))
              on_error)
          on_error)
      on_error
  
  (** Follow a user *)
  let follow ~account_id ~did on_success on_error =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        Config.get_credentials ~account_id
          (fun creds ->
            let identifier = creds.access_token in
            let now = Ptime.to_rfc3339 ~frac_s:6 ~tz_offset_s:0 (Ptime_clock.now ()) in
            
            let url = Printf.sprintf "%s/xrpc/com.atproto.repo.createRecord" pds_url in
            let body = `Assoc [
              ("repo", `String identifier);
              ("collection", `String "app.bsky.graph.follow");
              ("record", `Assoc [
                ("$type", `String "app.bsky.graph.follow");
                ("subject", `String did);
                ("createdAt", `String now);
              ]);
            ] in
            let body_str = Yojson.Basic.to_string body in
            let headers = [
              ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
              ("Content-Type", "application/json");
            ] in
            
            Config.Http.post ~headers ~body:body_str url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  try
                    let json = Yojson.Basic.from_string response.body in
                    let follow_uri = json 
                      |> Yojson.Basic.Util.member "uri" 
                      |> Yojson.Basic.Util.to_string in
                    on_success follow_uri
                  with e ->
                    on_error (Printf.sprintf "Failed to parse follow response: %s" (Printexc.to_string e))
                else
                  on_error (Printf.sprintf "Follow failed (%d): %s" response.status response.body))
              on_error)
          on_error)
      on_error
  
  (** Unfollow a user *)
  let unfollow ~account_id ~follow_uri on_success on_error =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        Config.get_credentials ~account_id
          (fun creds ->
            let identifier = creds.access_token in
            let uri_parts = String.split_on_char '/' follow_uri in
            let rkey = List.nth uri_parts (List.length uri_parts - 1) in
            
            let url = Printf.sprintf "%s/xrpc/com.atproto.repo.deleteRecord" pds_url in
            let body = `Assoc [
              ("repo", `String identifier);
              ("collection", `String "app.bsky.graph.follow");
              ("rkey", `String rkey);
            ] in
            let body_str = Yojson.Basic.to_string body in
            let headers = [
              ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
              ("Content-Type", "application/json");
            ] in
            
            Config.Http.post ~headers ~body:body_str url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  on_success ()
                else
                  on_error (Printf.sprintf "Unfollow failed (%d): %s" response.status response.body))
              on_error)
          on_error)
      on_error
  
  (** Get a user profile *)
  let get_profile ~account_id ~actor on_success on_error =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        let url = Printf.sprintf "%s/xrpc/app.bsky.actor.getProfile?actor=%s" 
          pds_url (Uri.pct_encode actor) in
        let headers = [("Authorization", Printf.sprintf "Bearer %s" access_jwt)] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_success json
              with e ->
                on_error (Printf.sprintf "Failed to parse profile: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "Get profile failed (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Get a post thread *)
  let get_post_thread ~account_id ~post_uri on_success on_error =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        let url = Printf.sprintf "%s/xrpc/app.bsky.feed.getPostThread?uri=%s" 
          pds_url (Uri.pct_encode post_uri) in
        let headers = [("Authorization", Printf.sprintf "Bearer %s" access_jwt)] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_success json
              with e ->
                on_error (Printf.sprintf "Failed to parse thread: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "Get thread failed (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Get timeline *)
  let get_timeline ~account_id ?limit on_success on_error =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        let limit_param = match limit with
          | Some l -> Printf.sprintf "?limit=%d" l
          | None -> ""
        in
        let url = Printf.sprintf "%s/xrpc/app.bsky.feed.getTimeline%s" pds_url limit_param in
        let headers = [("Authorization", Printf.sprintf "Bearer %s" access_jwt)] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_success json
              with e ->
                on_error (Printf.sprintf "Failed to parse timeline: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "Get timeline failed (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Quote a post with optional text and media *)
  let quote_post ~account_id ~post_uri ~post_cid ~text ~media_urls ?(alt_texts=[]) on_success on_error =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        Config.get_credentials ~account_id
          (fun creds ->
            let identifier = creds.access_token in
            
            (* Pair URLs with alt text *)
            let urls_with_alt = List.mapi (fun i url ->
              let alt_text = try List.nth alt_texts i with _ -> None in
              (url, alt_text)
            ) media_urls in
            
            (* Helper to upload blobs *)
            let rec upload_blobs_seq urls_with_alt acc on_complete on_err =
              match urls_with_alt with
              | [] -> on_complete (List.rev acc)
              | (url, alt_text) :: rest ->
                  Config.Http.get url
                    (fun media_resp ->
                      if media_resp.status >= 200 && media_resp.status < 300 then
                        let mime_type = 
                          List.assoc_opt "content-type" media_resp.headers 
                          |> Option.value ~default:"application/octet-stream"
                        in
                        (* Upload with alt text *)
                        upload_blob ~access_jwt ~blob_data:media_resp.body ~mime_type ~alt_text
                          (fun (blob, alt) -> upload_blobs_seq rest ((blob, alt) :: acc) on_complete on_err)
                          on_err
                      else
                        on_err (Printf.sprintf "Failed to fetch media from %s" url))
                    on_err
            in
            
            let media_to_upload = List.filteri (fun i _ -> i < 4) urls_with_alt in
            upload_blobs_seq media_to_upload []
              (fun blobs ->
                extract_facets text
                  (fun facets ->
                    let now = Ptime.to_rfc3339 ~frac_s:6 ~tz_offset_s:0 (Ptime_clock.now ()) in
                    
                    let base_record = [
                      ("$type", `String "app.bsky.feed.post");
                      ("text", `String text);
                      ("createdAt", `String now);
                    ] in
                    
                    let base_with_facets = 
                      if List.length facets > 0 then
                        base_record @ [("facets", `List facets)]
                      else
                        base_record
                    in
                    
                    (* Create embed for quote post *)
                    let quote_embed = `Assoc [
                      ("$type", `String "app.bsky.embed.record");
                      ("record", `Assoc [
                        ("uri", `String post_uri);
                        ("cid", `String post_cid);
                      ]);
                    ] in
                    
                    (* If we have media, use recordWithMedia *)
                    let final_record = 
                      if List.length blobs > 0 then
                        let images_json = `List (List.map (fun (blob, alt_text_opt) ->
                          let alt_text = match alt_text_opt with
                            | Some alt when String.length alt > 0 -> alt
                            | _ -> ""
                          in
                          `Assoc [
                            ("alt", `String alt_text);
                            ("image", blob);
                          ]
                        ) blobs) in
                        base_with_facets @ [
                          ("embed", `Assoc [
                            ("$type", `String "app.bsky.embed.recordWithMedia");
                            ("record", `Assoc [
                              ("$type", `String "app.bsky.embed.record");
                              ("record", `Assoc [
                                ("uri", `String post_uri);
                                ("cid", `String post_cid);
                              ]);
                            ]);
                            ("media", `Assoc [
                              ("$type", `String "app.bsky.embed.images");
                              ("images", images_json);
                            ]);
                          ])
                        ]
                      else
                        base_with_facets @ [("embed", quote_embed)]
                    in
                    
                    let url = Printf.sprintf "%s/xrpc/com.atproto.repo.createRecord" pds_url in
                    let body = `Assoc [
                      ("repo", `String identifier);
                      ("collection", `String "app.bsky.feed.post");
                      ("record", `Assoc final_record);
                    ] in
                    let body_str = Yojson.Basic.to_string body in
                    let headers = [
                      ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
                      ("Content-Type", "application/json");
                    ] in
                    
                    Config.Http.post ~headers ~body:body_str url
                      (fun response ->
                        if response.status >= 200 && response.status < 300 then
                          try
                            let json = Yojson.Basic.from_string response.body in
                            let post_uri = json 
                              |> Yojson.Basic.Util.member "uri" 
                              |> Yojson.Basic.Util.to_string in
                            on_success post_uri
                          with e ->
                            on_error (Printf.sprintf "Failed to parse response: %s" (Printexc.to_string e))
                        else
                          on_error (Printf.sprintf "Quote post failed (%d): %s" response.status response.body))
                      on_error)
                  on_error)
              on_error)
          on_error)
      on_error
  
  (** List notifications *)
  let list_notifications ~account_id ?limit ?cursor on_success on_error =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        let params = [] in
        let params = match limit with
          | Some l -> ("limit", string_of_int l) :: params
          | None -> params
        in
        let params = match cursor with
          | Some c -> ("cursor", c) :: params
          | None -> params
        in
        let query_string = match params with
          | [] -> ""
          | _ -> "?" ^ (String.concat "&" (List.map (fun (k, v) -> 
              Printf.sprintf "%s=%s" k (Uri.pct_encode v)) params))
        in
        let url = Printf.sprintf "%s/xrpc/app.bsky.notification.listNotifications%s" 
          pds_url query_string in
        let headers = [("Authorization", Printf.sprintf "Bearer %s" access_jwt)] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_success json
              with e ->
                on_error (Printf.sprintf "Failed to parse notifications: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "List notifications failed (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Count unread notifications *)
  let count_unread_notifications ~account_id on_success on_error =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        let url = Printf.sprintf "%s/xrpc/app.bsky.notification.getUnreadCount" pds_url in
        let headers = [("Authorization", Printf.sprintf "Bearer %s" access_jwt)] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let count = json 
                  |> Yojson.Basic.Util.member "count" 
                  |> Yojson.Basic.Util.to_int in
                on_success count
              with e ->
                on_error (Printf.sprintf "Failed to parse unread count: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "Get unread count failed (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Update seen notifications *)
  let update_seen_notifications ~account_id on_success on_error =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        let now = Ptime.to_rfc3339 ~frac_s:6 ~tz_offset_s:0 (Ptime_clock.now ()) in
        let url = Printf.sprintf "%s/xrpc/app.bsky.notification.updateSeen" pds_url in
        let body = `Assoc [("seenAt", `String now)] in
        let body_str = Yojson.Basic.to_string body in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
          ("Content-Type", "application/json");
        ] in
        
        Config.Http.post ~headers ~body:body_str url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_success ()
            else
              on_error (Printf.sprintf "Update seen failed (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Search for actors *)
  let search_actors ~account_id ~query ?limit ?cursor on_success on_error =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        let params = [("q", query)] in
        let params = match limit with
          | Some l -> ("limit", string_of_int l) :: params
          | None -> params
        in
        let params = match cursor with
          | Some c -> ("cursor", c) :: params
          | None -> params
        in
        let query_string = "?" ^ (String.concat "&" (List.map (fun (k, v) -> 
          Printf.sprintf "%s=%s" k (Uri.pct_encode v)) params)) in
        let url = Printf.sprintf "%s/xrpc/app.bsky.actor.searchActors%s" 
          pds_url query_string in
        let headers = [("Authorization", Printf.sprintf "Bearer %s" access_jwt)] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_success json
              with e ->
                on_error (Printf.sprintf "Failed to parse search results: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "Search actors failed (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Search for posts *)
  let search_posts ~account_id ~query ?limit ?cursor on_success on_error =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        let params = [("q", query)] in
        let params = match limit with
          | Some l -> ("limit", string_of_int l) :: params
          | None -> params
        in
        let params = match cursor with
          | Some c -> ("cursor", c) :: params
          | None -> params
        in
        let query_string = "?" ^ (String.concat "&" (List.map (fun (k, v) -> 
          Printf.sprintf "%s=%s" k (Uri.pct_encode v)) params)) in
        let url = Printf.sprintf "%s/xrpc/app.bsky.feed.searchPosts%s" 
          pds_url query_string in
        let headers = [("Authorization", Printf.sprintf "Bearer %s" access_jwt)] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_success json
              with e ->
                on_error (Printf.sprintf "Failed to parse search results: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "Search posts failed (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Mute an actor *)
  let mute_actor ~account_id ~actor on_success on_error =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        let url = Printf.sprintf "%s/xrpc/app.bsky.graph.muteActor" pds_url in
        let body = `Assoc [("actor", `String actor)] in
        let body_str = Yojson.Basic.to_string body in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
          ("Content-Type", "application/json");
        ] in
        
        Config.Http.post ~headers ~body:body_str url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_success ()
            else
              on_error (Printf.sprintf "Mute failed (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Unmute an actor *)
  let unmute_actor ~account_id ~actor on_success on_error =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        let url = Printf.sprintf "%s/xrpc/app.bsky.graph.unmuteActor" pds_url in
        let body = `Assoc [("actor", `String actor)] in
        let body_str = Yojson.Basic.to_string body in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
          ("Content-Type", "application/json");
        ] in
        
        Config.Http.post ~headers ~body:body_str url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_success ()
            else
              on_error (Printf.sprintf "Unmute failed (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Block an actor *)
  let block_actor ~account_id ~actor on_success on_error =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        Config.get_credentials ~account_id
          (fun creds ->
            let identifier = creds.access_token in
            let now = Ptime.to_rfc3339 ~frac_s:6 ~tz_offset_s:0 (Ptime_clock.now ()) in
            
            let url = Printf.sprintf "%s/xrpc/com.atproto.repo.createRecord" pds_url in
            let body = `Assoc [
              ("repo", `String identifier);
              ("collection", `String "app.bsky.graph.block");
              ("record", `Assoc [
                ("$type", `String "app.bsky.graph.block");
                ("subject", `String actor);
                ("createdAt", `String now);
              ]);
            ] in
            let body_str = Yojson.Basic.to_string body in
            let headers = [
              ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
              ("Content-Type", "application/json");
            ] in
            
            Config.Http.post ~headers ~body:body_str url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  try
                    let json = Yojson.Basic.from_string response.body in
                    let block_uri = json 
                      |> Yojson.Basic.Util.member "uri" 
                      |> Yojson.Basic.Util.to_string in
                    on_success block_uri
                  with e ->
                    on_error (Printf.sprintf "Failed to parse block response: %s" (Printexc.to_string e))
                else
                  on_error (Printf.sprintf "Block failed (%d): %s" response.status response.body))
              on_error)
          on_error)
      on_error
  
  (** Unblock an actor *)
  let unblock_actor ~account_id ~block_uri on_success on_error =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        Config.get_credentials ~account_id
          (fun creds ->
            let identifier = creds.access_token in
            let uri_parts = String.split_on_char '/' block_uri in
            let rkey = List.nth uri_parts (List.length uri_parts - 1) in
            
            let url = Printf.sprintf "%s/xrpc/com.atproto.repo.deleteRecord" pds_url in
            let body = `Assoc [
              ("repo", `String identifier);
              ("collection", `String "app.bsky.graph.block");
              ("rkey", `String rkey);
            ] in
            let body_str = Yojson.Basic.to_string body in
            let headers = [
              ("Authorization", Printf.sprintf "Bearer %s" access_jwt);
              ("Content-Type", "application/json");
            ] in
            
            Config.Http.post ~headers ~body:body_str url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  on_success ()
                else
                  on_error (Printf.sprintf "Unblock failed (%d): %s" response.status response.body))
              on_error)
          on_error)
      on_error
  
  (** Get author feed *)
  let get_author_feed ~account_id ~actor ?limit ?cursor on_success on_error =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        let params = [("actor", actor)] in
        let params = match limit with
          | Some l -> ("limit", string_of_int l) :: params
          | None -> params
        in
        let params = match cursor with
          | Some c -> ("cursor", c) :: params
          | None -> params
        in
        let query_string = "?" ^ (String.concat "&" (List.map (fun (k, v) -> 
          Printf.sprintf "%s=%s" k (Uri.pct_encode v)) params)) in
        let url = Printf.sprintf "%s/xrpc/app.bsky.feed.getAuthorFeed%s" 
          pds_url query_string in
        let headers = [("Authorization", Printf.sprintf "Bearer %s" access_jwt)] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_success json
              with e ->
                on_error (Printf.sprintf "Failed to parse author feed: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "Get author feed failed (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Get likes for a post *)
  let get_likes ~account_id ~post_uri ?limit ?cursor on_success on_error =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        let params = [("uri", post_uri)] in
        let params = match limit with
          | Some l -> ("limit", string_of_int l) :: params
          | None -> params
        in
        let params = match cursor with
          | Some c -> ("cursor", c) :: params
          | None -> params
        in
        let query_string = "?" ^ (String.concat "&" (List.map (fun (k, v) -> 
          Printf.sprintf "%s=%s" k (Uri.pct_encode v)) params)) in
        let url = Printf.sprintf "%s/xrpc/app.bsky.feed.getLikes%s" 
          pds_url query_string in
        let headers = [("Authorization", Printf.sprintf "Bearer %s" access_jwt)] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_success json
              with e ->
                on_error (Printf.sprintf "Failed to parse likes: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "Get likes failed (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Get reposts for a post *)
  let get_reposted_by ~account_id ~post_uri ?limit ?cursor on_success on_error =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        let params = [("uri", post_uri)] in
        let params = match limit with
          | Some l -> ("limit", string_of_int l) :: params
          | None -> params
        in
        let params = match cursor with
          | Some c -> ("cursor", c) :: params
          | None -> params
        in
        let query_string = "?" ^ (String.concat "&" (List.map (fun (k, v) -> 
          Printf.sprintf "%s=%s" k (Uri.pct_encode v)) params)) in
        let url = Printf.sprintf "%s/xrpc/app.bsky.feed.getRepostedBy%s" 
          pds_url query_string in
        let headers = [("Authorization", Printf.sprintf "Bearer %s" access_jwt)] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_success json
              with e ->
                on_error (Printf.sprintf "Failed to parse reposts: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "Get reposts failed (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Get followers *)
  let get_followers ~account_id ~actor ?limit ?cursor on_success on_error =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        let params = [("actor", actor)] in
        let params = match limit with
          | Some l -> ("limit", string_of_int l) :: params
          | None -> params
        in
        let params = match cursor with
          | Some c -> ("cursor", c) :: params
          | None -> params
        in
        let query_string = "?" ^ (String.concat "&" (List.map (fun (k, v) -> 
          Printf.sprintf "%s=%s" k (Uri.pct_encode v)) params)) in
        let url = Printf.sprintf "%s/xrpc/app.bsky.graph.getFollowers%s" 
          pds_url query_string in
        let headers = [("Authorization", Printf.sprintf "Bearer %s" access_jwt)] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_success json
              with e ->
                on_error (Printf.sprintf "Failed to parse followers: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "Get followers failed (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Get follows *)
  let get_follows ~account_id ~actor ?limit ?cursor on_success on_error =
    ensure_valid_token ~account_id
      (fun access_jwt ->
        let params = [("actor", actor)] in
        let params = match limit with
          | Some l -> ("limit", string_of_int l) :: params
          | None -> params
        in
        let params = match cursor with
          | Some c -> ("cursor", c) :: params
          | None -> params
        in
        let query_string = "?" ^ (String.concat "&" (List.map (fun (k, v) -> 
          Printf.sprintf "%s=%s" k (Uri.pct_encode v)) params)) in
        let url = Printf.sprintf "%s/xrpc/app.bsky.graph.getFollows%s" 
          pds_url query_string in
        let headers = [("Authorization", Printf.sprintf "Bearer %s" access_jwt)] in
        
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_success json
              with e ->
                on_error (Printf.sprintf "Failed to parse follows: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "Get follows failed (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Validate content for Bluesky *)
  let validate_content ~text =
    let max_length = 300 in
    if String.length text > max_length then
      Error (Printf.sprintf "Post exceeds %d character limit" max_length)
    else
      Ok ()
  
  (** Validate media for Bluesky *)
  let validate_media ~(media : Platform_types.post_media) =
    match media.Platform_types.media_type with
    | Image ->
        if media.file_size_bytes > 1024 * 1024 then
          Error "Image exceeds 1MB limit"
        else
          Ok ()
    | Video ->
        if media.file_size_bytes > 50 * 1024 * 1024 then
          Error "Video exceeds 50MB limit"
        else
          (match media.duration_seconds with
          | Some duration when duration > 60.0 ->
              Error "Video exceeds 60 second limit"
          | _ -> Ok ())
    | Gif ->
        if media.file_size_bytes > 1024 * 1024 then
          Error "GIF exceeds 1MB limit"
        else
          Ok ()
end
