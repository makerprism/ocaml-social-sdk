(** Mastodon API v1/v2 Provider
    
    This implementation supports Mastodon instances with OAuth 2.0 authentication.
    Each instance has its own URL and tokens typically don't expire unless revoked.
*)

open Social_provider_core

(** Mastodon-specific credentials with instance URL *)
type mastodon_credentials = {
  access_token: string;
  refresh_token: string option;
  token_type: string;
  instance_url: string;  (** The Mastodon instance URL (e.g., https://mastodon.social) *)
}

(** Visibility levels for statuses *)
type visibility = 
  | Public      (** Visible to everyone, shown in public timelines *)
  | Unlisted    (** Visible to everyone, but not in public timelines *)
  | Private     (** Visible to followers only *)
  | Direct      (** Visible to mentioned users only *)

(** Convert visibility to API string *)
let visibility_to_string = function
  | Public -> "public"
  | Unlisted -> "unlisted"
  | Private -> "private"
  | Direct -> "direct"

(** Poll option for status polls *)
type poll_option = {
  title: string;
}

(** Poll configuration *)
type poll = {
  options: poll_option list;
  expires_in: int;  (** Duration in seconds *)
  multiple: bool;   (** Allow multiple choices *)
  hide_totals: bool; (** Hide vote counts until poll ends *)
}

(** Configuration module type for Mastodon provider *)
module type CONFIG = sig
  module Http : HTTP_CLIENT
  
  val get_env : string -> string option
  val get_credentials : account_id:string -> (credentials -> unit) -> (string -> unit) -> unit
  val update_credentials : account_id:string -> credentials:credentials -> (unit -> unit) -> (string -> unit) -> unit
  val encrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val decrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val update_health_status : account_id:string -> status:string -> error_message:string option -> (unit -> unit) -> (string -> unit) -> unit
end

(** Make functor to create Mastodon provider with given configuration *)
module Make (Config : CONFIG) = struct
  
  (** Parse Mastodon credentials from core credentials type *)
  let parse_mastodon_credentials (credentials : credentials) on_success on_error =
    try
      (* For Mastodon, the access_token field contains a JSON string with both
         access_token and instance_url (provided by Mastodon_config.get_credentials) *)
      let json = Yojson.Basic.from_string credentials.access_token in
      let open Yojson.Basic.Util in
      let instance_url = json |> member "instance_url" |> to_string in
      let actual_token = json |> member "access_token" |> to_string in
      let mastodon_creds = {
        access_token = actual_token;
        refresh_token = credentials.refresh_token;
        token_type = credentials.token_type;
        instance_url;
      } in
      on_success mastodon_creds
    with e ->
      on_error (Printf.sprintf "Failed to parse Mastodon credentials: %s" (Printexc.to_string e))
  
  (** Convert Mastodon credentials back to core credentials type *)
  let to_core_credentials (mastodon_creds : mastodon_credentials) : credentials =
    (* Store both access_token and instance_url as JSON in the access_token field
       This format is expected by parse_mastodon_credentials *)
    let creds_json = `Assoc [
      ("access_token", `String mastodon_creds.access_token);
      ("instance_url", `String mastodon_creds.instance_url);
    ] |> Yojson.Basic.to_string in
    {
      access_token = creds_json;
      refresh_token = mastodon_creds.refresh_token;
      expires_at = None; (* Mastodon tokens don't expire *)
      token_type = mastodon_creds.token_type;
    }
  
  (** Generate a UUID v4 for idempotency keys *)
  let generate_uuid () =
    let random_byte () = Random.int 256 in
    Printf.sprintf "%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x"
      (random_byte ()) (random_byte ()) (random_byte ()) (random_byte ())
      (random_byte ()) (random_byte ())
      ((random_byte () land 0x0f) lor 0x40) (random_byte ())
      ((random_byte () land 0x3f) lor 0x80) (random_byte ())
      (random_byte ()) (random_byte ()) (random_byte ()) (random_byte ()) (random_byte ()) (random_byte ())
  
  (** Verify credentials are valid *)
  let verify_credentials ~mastodon_creds on_success on_error =
    let url = Printf.sprintf "%s/api/v1/accounts/verify_credentials" mastodon_creds.instance_url in
    let headers = [
      ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
    ] in
    
    Config.Http.get ~headers url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          on_success ()
        else
          on_error (Printf.sprintf "Invalid credentials (%d): %s" response.status response.body))
      on_error
  
  (** Upload media to Mastodon *)
  let upload_media ~mastodon_creds ~media_data ~mime_type ~description ~focus on_success on_error =
    let url = Printf.sprintf "%s/api/v2/media" mastodon_creds.instance_url in
    
    (* Determine filename from mime type *)
    let filename = match mime_type with
      | s when String.starts_with ~prefix:"image/" s -> "media.jpg"
      | s when String.starts_with ~prefix:"video/" s -> "media.mp4"
      | s when String.starts_with ~prefix:"image/gif" s -> "media.gif"
      | _ -> "media.bin"
    in
    
    (* Create multipart form data *)
    let base_parts = [
      {
        name = "file";
        filename = Some filename;
        content_type = Some mime_type;
        content = media_data;
      };
    ] in
    
    let parts_with_desc = match description with
      | Some desc when String.length desc > 0 ->
          base_parts @ [{
            name = "description";
            filename = None;
            content_type = None;
            content = desc;
          }]
      | _ -> base_parts
    in
    
    let parts = match focus with
      | Some (x, y) ->
          parts_with_desc @ [{
            name = "focus";
            filename = None;
            content_type = None;
            content = Printf.sprintf "%f,%f" x y;
          }]
      | None -> parts_with_desc
    in
    
    let headers = [
      ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
    ] in
    
    Config.Http.post_multipart ~headers ~parts url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let json = Yojson.Basic.from_string response.body in
            let media_id = json |> Yojson.Basic.Util.member "id" |> Yojson.Basic.Util.to_string in
            on_success media_id
          with e ->
            on_error (Printf.sprintf "Failed to parse media response: %s" (Printexc.to_string e))
        else
          on_error (Printf.sprintf "Media upload failed (%d): %s" response.status response.body))
      on_error
  
  (** Update media with alt text and/or focus point *)
  let update_media ~mastodon_creds ~media_id ~alt_text ~focus on_success on_error =
    let url = Printf.sprintf "%s/api/v1/media/%s" mastodon_creds.instance_url media_id in
    
    let fields = [] in
    let fields = match alt_text with
      | Some text -> ("description", `String text) :: fields
      | None -> fields
    in
    let fields = match focus with
      | Some (x, y) -> ("focus", `String (Printf.sprintf "%f,%f" x y)) :: fields
      | None -> fields
    in
    
    let body_json = `Assoc fields in
    let body = Yojson.Basic.to_string body_json in
    
    let headers = [
      ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
      ("Content-Type", "application/json");
    ] in
    
    Config.Http.put ~headers ~body url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          on_success ()
        else
          on_error (Printf.sprintf "Media update failed (%d): %s" response.status response.body))
      on_error
  
  (** Ensure valid token (Mastodon tokens don't expire unless revoked) *)
  let ensure_valid_token ~account_id on_success on_error =
    Config.get_credentials ~account_id
      (fun creds ->
        parse_mastodon_credentials creds
          (fun mastodon_creds ->
            verify_credentials ~mastodon_creds
              (fun () ->
                Config.update_health_status ~account_id ~status:"healthy" ~error_message:None
                  (fun () -> on_success mastodon_creds)
                  on_error)
              (fun err ->
                Config.update_health_status ~account_id ~status:"invalid_token" 
                  ~error_message:(Some err)
                  (fun () -> on_error err)
                  on_error))
          on_error)
      on_error
  
  (** Post single status with full options *)
  let post_single 
      ~account_id 
      ~text 
      ~media_urls 
      ?(alt_texts=[])
      ?(visibility=Public)
      ?(sensitive=false)
      ?(spoiler_text=None)
      ?(in_reply_to_id=None)
      ?(language=None)
      ?(poll=None)
      ?(scheduled_at=None)
      ?(idempotency_key=None)
      on_success 
      on_error =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        (* Pair URLs with alt text - use None if alt text list is shorter *)
        let urls_with_alt = List.mapi (fun i url ->
          let alt_text = try List.nth alt_texts i with _ -> None in
          (url, alt_text)
        ) media_urls in
        
        (* Helper to upload media from URLs *)
        let rec upload_media_seq urls_with_alt acc on_complete on_err =
          match urls_with_alt with
          | [] -> on_complete (List.rev acc)
          | (url, alt_text) :: rest ->
              (* Fetch media from URL *)
              Config.Http.get url
                (fun media_resp ->
                  if media_resp.status >= 200 && media_resp.status < 300 then
                    let mime_type = 
                      List.assoc_opt "content-type" media_resp.headers 
                      |> Option.value ~default:"image/jpeg"
                    in
                    (* Upload to Mastodon with alt text *)
                    upload_media ~mastodon_creds 
                      ~media_data:media_resp.body ~mime_type ~description:alt_text ~focus:None
                      (fun media_id -> 
                        upload_media_seq rest (media_id :: acc) on_complete on_err)
                      on_err
                  else
                    on_err (Printf.sprintf "Failed to fetch media from %s" url))
                on_err
        in
        
        (* Upload media if provided (max 4) *)
        let media_to_upload = List.filteri (fun i _ -> i < 4) urls_with_alt in
        upload_media_seq media_to_upload []
          (fun media_ids ->
            (* Create status *)
            let url = Printf.sprintf "%s/api/v1/statuses" mastodon_creds.instance_url in
            
            (* Build request body *)
            let base_fields = [
              ("status", `String text);
              ("visibility", `String (visibility_to_string visibility));
              ("sensitive", `Bool sensitive);
            ] in
            
            let fields = match spoiler_text with
              | Some text when String.length text > 0 -> 
                  ("spoiler_text", `String text) :: base_fields
              | _ -> base_fields
            in
            
            let fields = match in_reply_to_id with
              | Some id -> ("in_reply_to_id", `String id) :: fields
              | None -> fields
            in
            
            let fields = match language with
              | Some lang -> ("language", `String lang) :: fields
              | None -> fields
            in
            
            let fields = match scheduled_at with
              | Some datetime -> ("scheduled_at", `String datetime) :: fields
              | None -> fields
            in
            
            let fields = if List.length media_ids > 0 then
              ("media_ids", `List (List.map (fun id -> `String id) media_ids)) :: fields
            else
              fields
            in
            
            let fields = match poll with
              | Some p ->
                  let poll_json = `Assoc [
                    ("options", `List (List.map (fun opt -> `String opt.title) p.options));
                    ("expires_in", `Int p.expires_in);
                    ("multiple", `Bool p.multiple);
                    ("hide_totals", `Bool p.hide_totals);
                  ] in
                  ("poll", poll_json) :: fields
              | None -> fields
            in
            
            let body = Yojson.Basic.to_string (`Assoc fields) in
            
            (* Generate or use provided idempotency key *)
            let idem_key = match idempotency_key with
              | Some key -> key
              | None -> generate_uuid ()
            in
            
            let headers = [
              ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
              ("Content-Type", "application/json");
              ("Idempotency-Key", idem_key);
            ] in
            
            Config.Http.post ~headers ~body url
              (fun response ->
                if response.status >= 200 && response.status < 300 then
                  try
                    let json = Yojson.Basic.from_string response.body in
                    let open Yojson.Basic.Util in
                    (* Try to get the URL from the response *)
                    let status_url = 
                      try
                        match json |> member "url" with
                        | `Null -> raise Not_found  (* URL is null, use fallback *)
                        | url_json -> url_json |> to_string
                      with _ ->
                        (* Fallback: construct URL from instance, username and status ID *)
                        let status_id = json |> member "id" |> to_string in
                        let username = try
                          json |> member "account" |> member "acct" |> to_string
                        with _ ->
                          (* If we can't get username, fall back to /statuses format *)
                          ""
                        in
                        if username <> "" then
                          Printf.sprintf "%s/@%s/%s" mastodon_creds.instance_url username status_id
                        else
                          Printf.sprintf "%s/statuses/%s" mastodon_creds.instance_url status_id
                    in
                    on_success status_url
                  with e ->
                    on_error (Printf.sprintf "Failed to parse response: %s" (Printexc.to_string e))
                else
                  on_error (Printf.sprintf "Mastodon API error (%d): %s" response.status response.body))
              on_error)
          on_error)
      on_error
  
  (** Post thread with full options *)
  let post_thread 
      ~account_id 
      ~texts 
      ~media_urls_per_post
      ?(alt_texts_per_post=[])
      ?(visibility=Public)
      ?(sensitive=false)
      ?(spoiler_text=None)
      on_success 
      on_error =
    if List.length texts = 0 then
      on_error "No statuses in thread"
    else
      ensure_valid_token ~account_id
        (fun mastodon_creds ->
          (* Helper to upload media from URLs for a single post *)
          let upload_post_media media_urls alt_texts on_complete on_err =
            (* Pair URLs with alt text *)
            let urls_with_alt = List.mapi (fun i url ->
              let alt_text = try List.nth alt_texts i with _ -> None in
              (url, alt_text)
            ) media_urls in
            
            let rec upload_seq urls_with_alt acc =
              match urls_with_alt with
              | [] -> on_complete (List.rev acc)
              | (url, alt_text) :: rest ->
                  Config.Http.get url
                    (fun media_resp ->
                      if media_resp.status >= 200 && media_resp.status < 300 then
                        let mime_type = 
                          List.assoc_opt "content-type" media_resp.headers 
                          |> Option.value ~default:"image/jpeg"
                        in
                        upload_media ~mastodon_creds 
                          ~media_data:media_resp.body ~mime_type ~description:alt_text ~focus:None
                          (fun media_id -> upload_seq rest (media_id :: acc))
                          on_err
                      else
                        on_err (Printf.sprintf "Failed to fetch media from %s" url))
                    on_err
            in
            let media_to_upload = List.filteri (fun i _ -> i < 4) urls_with_alt in
            upload_seq media_to_upload []
          in
          
          (* Pair texts with media and alt text - handle mismatched lengths *)
          let num_posts = List.length texts in
          let padded_media_urls = media_urls_per_post @ List.init (max 0 (num_posts - List.length media_urls_per_post)) (fun _ -> []) in
          let padded_alt_texts = alt_texts_per_post @ List.init (max 0 (num_posts - List.length alt_texts_per_post)) (fun _ -> []) in
          
          let posts_with_media_and_alt = List.mapi (fun i text ->
            let media_urls = try List.nth padded_media_urls i with _ -> [] in
            let alt_texts = try List.nth padded_alt_texts i with _ -> [] in
            (text, media_urls, alt_texts)
          ) texts in
          
          (* Helper to post statuses in sequence with reply references *)
          let rec post_statuses_seq posts_with_media_and_alt reply_to_id acc on_complete on_err =
            match posts_with_media_and_alt with
            | [] -> on_complete (List.rev acc)
            | (text, media_urls, alt_texts) :: rest ->
                (* Upload media for this post *)
                upload_post_media media_urls alt_texts
                  (fun media_ids ->
                    let url = Printf.sprintf "%s/api/v1/statuses" mastodon_creds.instance_url in
                    
                    let base_fields = [
                      ("status", `String text);
                      ("visibility", `String (visibility_to_string visibility));
                      ("sensitive", `Bool sensitive);
                    ] in
                    
                    let fields = match spoiler_text with
                      | Some text when String.length text > 0 -> 
                          ("spoiler_text", `String text) :: base_fields
                      | _ -> base_fields
                    in
                    
                    let fields = match reply_to_id with
                      | Some id -> ("in_reply_to_id", `String id) :: fields
                      | None -> fields
                    in
                    
                    let fields = if List.length media_ids > 0 then
                      ("media_ids", `List (List.map (fun id -> `String id) media_ids)) :: fields
                    else
                      fields
                    in
                    
                    let body = Yojson.Basic.to_string (`Assoc fields) in
                    
                    let headers = [
                      ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
                      ("Content-Type", "application/json");
                      ("Idempotency-Key", generate_uuid ());
                    ] in
                    
                    Config.Http.post ~headers ~body url
                      (fun response ->
                        if response.status >= 200 && response.status < 300 then
                          try
                            let json = Yojson.Basic.from_string response.body in
                            let open Yojson.Basic.Util in
                            let status_id = json |> member "id" |> to_string in
                            (* Get the full URL for the status *)
                            let status_url = 
                              try
                                match json |> member "url" with
                                | `Null -> raise Not_found  (* URL is null, use fallback *)
                                | url_json -> url_json |> to_string
                              with _ ->
                                (* Fallback: construct URL from instance, username and status ID *)
                                let username = try
                                  json |> member "account" |> member "acct" |> to_string
                                with _ ->
                                  (* If we can't get username, fall back to /statuses format *)
                                  ""
                                in
                                if username <> "" then
                                  Printf.sprintf "%s/@%s/%s" mastodon_creds.instance_url username status_id
                                else
                                  Printf.sprintf "%s/statuses/%s" mastodon_creds.instance_url status_id
                            in
                            (* Continue with next status in thread, use ID for reply but accumulate URLs *)
                            post_statuses_seq rest (Some status_id) (status_url :: acc) on_complete on_err
                          with e ->
                            on_err (Printf.sprintf "Failed to parse response: %s" (Printexc.to_string e))
                        else
                          on_err (Printf.sprintf "Mastodon API error (%d): %s" response.status response.body))
                      on_err)
                      on_err
          in
          
          post_statuses_seq posts_with_media_and_alt None [] on_success on_error)
        on_error
  
  (** Validate content for Mastodon 
      Note: The actual character limit may vary by instance. 
      Default is 500, but many instances use 1000, 5000, or more.
      Should ideally fetch from /api/v1/instance endpoint. *)
  let validate_content ~text ?(max_length=500) () =
    if String.length text > max_length then
      Error (Printf.sprintf "Status exceeds %d character limit" max_length)
    else
      Ok ()
  
  (** Validate media for Mastodon 
      Note: These are default limits. Actual limits may vary by instance
      and should ideally be fetched from /api/v1/instance endpoint. *)
  let validate_media ~(media : Platform_types.post_media) =
    match media.Platform_types.media_type with
    | Platform_types.Image ->
        if media.file_size_bytes > 10 * 1024 * 1024 then
          Error "Image exceeds 10MB limit (default)"
        else
          Ok ()
    | Platform_types.Video ->
        if media.file_size_bytes > 100 * 1024 * 1024 then
          Error "Video exceeds 100MB limit (default)"
        else if Option.value ~default:0.0 media.duration_seconds > 7200.0 then
          Error "Video exceeds 2 hour duration limit"
        else
          Ok ()
    | Platform_types.Gif ->
        if media.file_size_bytes > 10 * 1024 * 1024 then
          Error "GIF exceeds 10MB limit (default)"
        else
          Ok ()
  
  (** Validate poll options *)
  let validate_poll ~(poll : poll) =
    if List.length poll.options < 2 then
      Error "Poll must have at least 2 options"
    else if List.length poll.options > 4 then
      Error "Poll can have at most 4 options"
    else if List.exists (fun opt -> String.length opt.title = 0) poll.options then
      Error "Poll options cannot be empty"
    else if List.exists (fun opt -> String.length opt.title > 50) poll.options then
      Error "Poll option exceeds 50 character limit"
    else if poll.expires_in < 300 then
      Error "Poll must be open for at least 5 minutes"
    else if poll.expires_in > 2592000 then
      Error "Poll cannot be open for more than 30 days"
    else
      Ok ()
  
  (** Delete a status *)
  let delete_status ~account_id ~status_id on_success on_error =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v1/statuses/%s" mastodon_creds.instance_url status_id in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        
        Config.Http.delete ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_success ()
            else
              on_error (Printf.sprintf "Delete failed (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Edit a status *)
  let edit_status
      ~account_id
      ~status_id
      ~text
      ?(media_ids=None)
      ?(visibility=None)
      ?(sensitive=None)
      ?(spoiler_text=None)
      ?(language=None)
      ?(poll=None)
      on_success
      on_error =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v1/statuses/%s" mastodon_creds.instance_url status_id in
        
        let base_fields = [("status", `String text)] in
        
        let fields = match visibility with
          | Some v -> ("visibility", `String (visibility_to_string v)) :: base_fields
          | None -> base_fields
        in
        
        let fields = match sensitive with
          | Some s -> ("sensitive", `Bool s) :: fields
          | None -> fields
        in
        
        let fields = match spoiler_text with
          | Some text when String.length text > 0 -> 
              ("spoiler_text", `String text) :: fields
          | _ -> fields
        in
        
        let fields = match language with
          | Some lang -> ("language", `String lang) :: fields
          | None -> fields
        in
        
        let fields = match media_ids with
          | Some ids when List.length ids > 0 ->
              ("media_ids", `List (List.map (fun id -> `String id) ids)) :: fields
          | _ -> fields
        in
        
        let fields = match poll with
          | Some p ->
              let poll_json = `Assoc [
                ("options", `List (List.map (fun opt -> `String opt.title) p.options));
                ("expires_in", `Int p.expires_in);
                ("multiple", `Bool p.multiple);
                ("hide_totals", `Bool p.hide_totals);
              ] in
              ("poll", poll_json) :: fields
          | None -> fields
        in
        
        let body = Yojson.Basic.to_string (`Assoc fields) in
        
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
          ("Content-Type", "application/json");
        ] in
        
        Config.Http.put ~headers ~body url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let edited_id = json |> Yojson.Basic.Util.member "id" |> Yojson.Basic.Util.to_string in
                on_success edited_id
              with e ->
                on_error (Printf.sprintf "Failed to parse edit response: %s" (Printexc.to_string e))
            else
              on_error (Printf.sprintf "Edit failed (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Favorite a status *)
  let favorite_status ~account_id ~status_id on_success on_error =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v1/statuses/%s/favourite" mastodon_creds.instance_url status_id in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        
        Config.Http.post ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_success ()
            else
              on_error (Printf.sprintf "Favorite failed (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Unfavorite a status *)
  let unfavorite_status ~account_id ~status_id on_success on_error =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v1/statuses/%s/unfavourite" mastodon_creds.instance_url status_id in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        
        Config.Http.post ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_success ()
            else
              on_error (Printf.sprintf "Unfavorite failed (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Boost (reblog) a status *)
  let boost_status ~account_id ~status_id ?(visibility=None) on_success on_error =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v1/statuses/%s/reblog" mastodon_creds.instance_url status_id in
        
        let body = match visibility with
          | Some v -> 
              Yojson.Basic.to_string (`Assoc [("visibility", `String (visibility_to_string v))])
          | None -> "{}"
        in
        
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
          ("Content-Type", "application/json");
        ] in
        
        Config.Http.post ~headers ~body url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_success ()
            else
              on_error (Printf.sprintf "Boost failed (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Unboost (unreblog) a status *)
  let unboost_status ~account_id ~status_id on_success on_error =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v1/statuses/%s/unreblog" mastodon_creds.instance_url status_id in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        
        Config.Http.post ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_success ()
            else
              on_error (Printf.sprintf "Unboost failed (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Bookmark a status *)
  let bookmark_status ~account_id ~status_id on_success on_error =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v1/statuses/%s/bookmark" mastodon_creds.instance_url status_id in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        
        Config.Http.post ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_success ()
            else
              on_error (Printf.sprintf "Bookmark failed (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Unbookmark a status *)
  let unbookmark_status ~account_id ~status_id on_success on_error =
    ensure_valid_token ~account_id
      (fun mastodon_creds ->
        let url = Printf.sprintf "%s/api/v1/statuses/%s/unbookmark" mastodon_creds.instance_url status_id in
        let headers = [
          ("Authorization", Printf.sprintf "Bearer %s" mastodon_creds.access_token);
        ] in
        
        Config.Http.post ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              on_success ()
            else
              on_error (Printf.sprintf "Unbookmark failed (%d): %s" response.status response.body))
          on_error)
      on_error
  
  (** Register an application with a Mastodon instance *)
  let register_app ~instance_url ~client_name ~redirect_uris ~scopes ~website on_success on_error =
    let url = Printf.sprintf "%s/api/v1/apps" instance_url in
    
    let body_json = `Assoc [
      ("client_name", `String client_name);
      ("redirect_uris", `String redirect_uris);
      ("scopes", `String scopes);
      ("website", `String website);
    ] in
    let body = Yojson.Basic.to_string body_json in
    
    let headers = [("Content-Type", "application/json")] in
    
    Config.Http.post ~headers ~body url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let json = Yojson.Basic.from_string response.body in
            let open Yojson.Basic.Util in
            let client_id = json |> member "client_id" |> to_string in
            let client_secret = json |> member "client_secret" |> to_string in
            on_success (client_id, client_secret)
          with e ->
            on_error (Printf.sprintf "Failed to parse app registration: %s" (Printexc.to_string e))
        else
          on_error (Printf.sprintf "App registration failed (%d): %s" response.status response.body))
      on_error
  
  (** Generate PKCE code verifier (43-128 characters) *)
  let generate_code_verifier () =
    let chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~" in
    let chars_len = String.length chars in
    String.init 128 (fun _ -> String.get chars (Random.int chars_len))
  
  (** Generate PKCE code challenge from verifier using SHA256 *)
  let generate_code_challenge verifier =
    let hash = Digestif.SHA256.digest_string verifier in
    let raw_hash = Digestif.SHA256.to_raw_string hash in
    (* Base64 URL encode without padding *)
    Base64.encode_exn ~pad:false ~alphabet:Base64.uri_safe_alphabet raw_hash
  
  (** Get OAuth authorization URL with PKCE support *)
  let get_oauth_url ~instance_url ~client_id ~redirect_uri ~scopes ?(state=None) ?(code_challenge=None) () =
    let state_param = match state with
      | Some s -> Printf.sprintf "&state=%s" s
      | None -> ""
    in
    let pkce_params = match code_challenge with
      | Some challenge -> 
          Printf.sprintf "&code_challenge=%s&code_challenge_method=S256" challenge
      | None -> ""
    in
    Printf.sprintf "%s/oauth/authorize?client_id=%s&redirect_uri=%s&response_type=code&scope=%s%s%s"
      instance_url client_id redirect_uri scopes state_param pkce_params
  
  (** Exchange authorization code for access token with optional PKCE verifier *)
  let exchange_code ~instance_url ~client_id ~client_secret ~redirect_uri ~code ?(code_verifier=None) on_success on_error =
    let url = Printf.sprintf "%s/oauth/token" instance_url in
    
    let base_fields = [
      ("client_id", `String client_id);
      ("client_secret", `String client_secret);
      ("redirect_uri", `String redirect_uri);
      ("grant_type", `String "authorization_code");
      ("code", `String code);
      ("scope", `String "read write follow");
    ] in
    
    (* Add code_verifier if using PKCE *)
    let fields = match code_verifier with
      | Some verifier -> ("code_verifier", `String verifier) :: base_fields
      | None -> base_fields
    in
    
    let body_json = `Assoc fields in
    let body = Yojson.Basic.to_string body_json in
    
    let headers = [("Content-Type", "application/json")] in
    
    Config.Http.post ~headers ~body url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let json = Yojson.Basic.from_string response.body in
            let open Yojson.Basic.Util in
            let access_token = json |> member "access_token" |> to_string in
            let token_type = try json |> member "token_type" |> to_string with _ -> "Bearer" in
            
            (* Validate granted scopes match requested *)
            let granted_scope = try json |> member "scope" |> to_string with _ -> "" in
            let granted_scopes = String.split_on_char ' ' granted_scope 
              |> List.filter (fun s -> s <> "") 
              |> List.sort String.compare in
            let requested_scopes = ["follow"; "read"; "write"] |> List.sort String.compare in
            
            (* Check if all requested scopes were granted *)
            let all_granted = List.for_all (fun req -> List.mem req granted_scopes) requested_scopes in
            
            if not all_granted then (
              let missing = List.filter (fun req -> not (List.mem req granted_scopes)) requested_scopes in
              on_error (Printf.sprintf "Missing required scopes: %s (granted: %s)" 
                (String.concat ", " missing) granted_scope)
            ) else (
              (* Return raw credentials - let the caller wrap with instance_url *)
              let core_creds = {
                access_token;  (* Return actual token, not JSON-wrapped *)
                refresh_token = None;
                expires_at = None;
                token_type;
              } in
              on_success core_creds
            )
          with e ->
            on_error (Printf.sprintf "Failed to parse token response: %s" (Printexc.to_string e))
        else
          on_error (Printf.sprintf "Token exchange failed (%d): %s" response.status response.body))
      on_error
  
  (** Revoke access token on logout/disconnect *)
  let revoke_token ~account_id on_success on_error =
    Config.get_credentials ~account_id
      (fun creds ->
        parse_mastodon_credentials creds
          (fun mastodon_creds ->
            let url = Printf.sprintf "%s/oauth/revoke" mastodon_creds.instance_url in
            let body_json = `Assoc [
              ("token", `String mastodon_creds.access_token);
            ] in
            let body = Yojson.Basic.to_string body_json in
            let headers = [("Content-Type", "application/json")] in
            
            Config.Http.post ~headers ~body url
              (fun response ->
                (* OAuth2 revocation endpoint returns 200 for both valid and invalid tokens
                   This is by design - it's idempotent and safe to call multiple times *)
                if response.status >= 200 && response.status < 300 then
                  on_success ()
                else
                  (* Don't fail - token might already be revoked or invalid *)
                  on_success ()  (* Still consider it a success since the goal is achieved *)
              )
              (fun _err ->
                (* Network errors should also not fail the disconnect flow *)
                on_success ()  (* Continue with disconnect even if revocation failed *)
              ))
          on_error)
      on_error
end