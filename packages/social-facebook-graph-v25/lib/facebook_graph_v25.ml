(** Facebook Graph API v25 Provider
    
    This implementation supports Facebook Pages posting via Graph API.
    Page access tokens are long-lived (60 days) and require re-authentication to refresh.
*)

open Social_core

(** {1 API version and endpoints}

    The single source of truth for the Graph API version this package
    targets. Use these instead of writing ["v25.0"] in application code, so
    that a version bump is a dependency bump. *)

(** Graph API version, e.g. ["v25.0"]. *)
let api_version = "v25.0"

(** Versioned Graph API base URL, ["https://graph.facebook.com/v25.0"]. *)
let graph_api_base = "https://graph.facebook.com/" ^ api_version

(** Host for resumable (Reels) video uploads, versioned with the Graph API. *)
let rupload_base = "https://rupload.facebook.com/video-upload/" ^ api_version

(** Facebook Login dialog for this API version. *)
let oauth_authorize_url = "https://www.facebook.com/" ^ api_version ^ "/dialog/oauth"

(** OAuth token endpoint (code exchange, long-lived token exchange). *)
let token_endpoint = graph_api_base ^ "/oauth/access_token"

(** Typed Page post comments ([GET /{object-id}/comments]); see
    {!Facebook_graph_v25_comments}. *)
module Comments = Facebook_graph_v25_comments

(** Page, post and video insights metric sets and response types; see
    {!Facebook_graph_v25_insights}. *)
module Insights = Facebook_graph_v25_insights

(** Typed Page search results used as places ([GET /pages/search]); see
    {!Facebook_graph_v25_places}. The request is [Make.search_places]. *)
module Places = Facebook_graph_v25_places

(** Typed Page feed posts ([/{page-id}/posts], [/feed],
    [/published_posts]); see {!Facebook_graph_v25_page_posts}. The request
    is [Make.get_page_posts_page]. *)
module Page_posts = Facebook_graph_v25_page_posts

(** Hosts a [paging.next] / [paging.previous] URL may point at before a
    reader sends the caller's token to it
    ({!Social_meta_graph_paging.resolve_page_url}). *)
let paging_url_hosts = [ "graph.facebook.com" ]

(** OAuth 2.0 module for Facebook
    
    Facebook uses OAuth 2.0 WITHOUT PKCE support.
    
    Token types:
    - Short-lived user tokens: ~1-2 hours (returned by code exchange)
    - Long-lived user tokens: ~60 days (obtained by exchanging short-lived tokens)
    - Page access tokens: Derived from user tokens, can be made permanent
    
    IMPORTANT: For posting to Facebook Pages, you need:
    1. User authenticates with pages_manage_posts permission
    2. Exchange short-lived token for long-lived token
    3. Get Page access token via /me/accounts endpoint
    
    Required environment variables (or pass directly to functions):
    - FACEBOOK_APP_ID: App ID from Facebook Developer Portal
    - FACEBOOK_APP_SECRET: App Secret
    - FACEBOOK_REDIRECT_URI: Registered callback URL
*)
module OAuth = struct
  (** Scope definitions for Facebook Graph API *)
  module Scopes = struct
    (** Facebook OAuth scopes as an ADT with string fallback for future scopes.
        Use [Custom s] for scopes not yet modeled in the ADT.

        Full list from Meta Developer Portal:
        - pages_manage_posts: Create, edit, and delete page posts
        - pages_manage_engagement: Manage comments on page posts
        - pages_read_engagement: Read page content and engagement data
        - pages_read_user_content: Read user-generated content on page
        - pages_show_list: Access pages managed by user
        - pages_manage_metadata: Manage webhooks and page settings
        - read_insights: Read insights data for pages, apps, domains
        - public_profile: Access public profile fields (shared across Meta)
        - business_management: Business asset management (shared across Meta)
        - email: Access primary email address (shared across Meta)
        - facebook_branded_content_ads_brand: Branded content ads for brands
        - facebook_creator_marketplace_discovery: Discover creators on marketplace
        - ads_management: Manage ad accounts
        - ads_read: Read ad insights
        - catalog_management: Manage product catalogs
    *)
    type scope =
      | Pages_manage_posts
      | Pages_manage_engagement
      | Pages_read_engagement
      | Pages_read_user_content
      | Pages_show_list
      | Pages_manage_metadata
      | Read_insights
      | Public_profile
      | Business_management
      | Email
      | Facebook_branded_content_ads_brand
      | Facebook_creator_marketplace_discovery
      | Ads_management
      | Ads_read
      | Catalog_management
      | Custom of string

    (** Convert a scope to its string representation. *)
    let scope_to_string = function
      | Pages_manage_posts -> "pages_manage_posts"
      | Pages_manage_engagement -> "pages_manage_engagement"
      | Pages_read_engagement -> "pages_read_engagement"
      | Pages_read_user_content -> "pages_read_user_content"
      | Pages_show_list -> "pages_show_list"
      | Pages_manage_metadata -> "pages_manage_metadata"
      | Read_insights -> "read_insights"
      | Public_profile -> "public_profile"
      | Business_management -> "business_management"
      | Email -> "email"
      | Facebook_branded_content_ads_brand -> "facebook_branded_content_ads_brand"
      | Facebook_creator_marketplace_discovery -> "facebook_creator_marketplace_discovery"
      | Ads_management -> "ads_management"
      | Ads_read -> "ads_read"
      | Catalog_management -> "catalog_management"
      | Custom s -> s

    (** Convert a scope list to the new Meta params[scope] JSON array format.
        Meta changed from ?scope=xxx to ?params[scope]=["xxx","yyy"] *)
    let scopes_to_json_array scopes =
      scopes
      |> List.map scope_to_string
      |> List.map (fun s -> Printf.sprintf "\"%s\"" s)
      |> String.concat ","
      |> Printf.sprintf "[%s]"

    (** Helper to create custom scopes from strings. *)
    let custom_scope s = Custom s


    (** Scopes required for basic read operations *)
    let read = [Public_profile; Email]

    (** Scopes required for Facebook Page posting *)
    let write = [
      Pages_read_engagement;
      Pages_manage_posts;
      Pages_show_list;
    ]

    (** All commonly used scopes for Pages management *)
    let all = [
      Public_profile; Email;
      Pages_read_engagement; Pages_manage_posts;
      Pages_show_list; Pages_read_user_content;
      Pages_manage_metadata; Pages_manage_engagement;
    ]

    (** Operations that can be performed with Facebook API *)
    type operation =
      | Post_text
      | Post_media
      | Post_video
      | Post_story
      | Read_profile
      | Read_posts
      | Delete_post
      | Manage_pages

    (** Get scopes required for specific operations *)
    let for_operations ops =
      let base = [Public_profile] in
      if List.exists (fun o -> o = Post_text || o = Post_media || o = Post_video || o = Post_story || o = Delete_post || o = Manage_pages) ops
      then base @ write
      else if List.exists (fun o -> o = Read_profile || o = Read_posts) ops
      then base @ [Pages_read_engagement; Pages_show_list]
      else base
  end
  
  (** Platform metadata for Facebook OAuth *)
  module Metadata = struct
    (** Facebook does NOT support PKCE *)
    let supports_pkce = false
    
    (** Facebook doesn't use traditional refresh tokens - use long-lived token exchange *)
    let supports_refresh = false
    
    (** Short-lived tokens last ~1-2 hours *)
    let short_lived_token_seconds = Some 3600
    
    (** Long-lived tokens last ~60 days *)
    let long_lived_token_seconds = Some 5184000
    
    (** Recommended buffer before expiry (7 days) *)
    let refresh_buffer_seconds = 604800
    
    (** Maximum retry attempts *)
    let max_refresh_attempts = 5
    
    (** Authorization endpoint *)
    let authorization_endpoint = oauth_authorize_url
    
    (** Token endpoint *)
    let token_endpoint = token_endpoint
    
    (** Graph API base URL *)
    let api_base = graph_api_base
  end
  
  (** Generate authorization URL for Facebook OAuth 2.0 flow

      {b Deprecated}: use {!authorize_url}. This builder sends the scopes as
      [params[scope]=["..."]], a form Meta's Login documentation does not
      describe, and always forces [auth_type=rerequest]. Kept unchanged for
      existing callers.

      Note: Facebook does NOT support PKCE.

      @param client_id Facebook App ID
      @param redirect_uri Registered callback URL
      @param state CSRF protection state parameter
      @param scopes OAuth scopes to request explicitly
      @return Full authorization URL to redirect user to
  *)
  let get_authorization_url ~client_id ~redirect_uri ~state ~scopes () =
    let scopes_json = Scopes.scopes_to_json_array scopes in
    let params = [
      ("client_id", client_id);
      ("redirect_uri", redirect_uri);
      ("state", state);
      ("params[scope]", scopes_json);
      ("response_type", "code");
      ("auth_type", "rerequest");
    ] in
    let query = Uri.encoded_of_query (List.map (fun (k, v) -> (k, [v])) params) in
    Printf.sprintf "%s?%s" Metadata.authorization_endpoint query

  (** [auth_type] of the Login dialog. *)
  type auth_type =
    | Rerequest
        (** Ask again for permissions the user declined
            ({{:https://developers.facebook.com/docs/facebook-login/guides/advanced/manual-flow}}). *)
    | Reauthenticate
        (** Ask for the password again
            ({{:https://developers.facebook.com/docs/facebook-login/guides/advanced/re-authentication}}). *)
    | Reauthorize
        (** Renew data access after it expired
            ({{:https://developers.facebook.com/docs/facebook-login/auth-vs-data}}). *)

  let auth_type_to_string = function
    | Rerequest -> "rerequest"
    | Reauthenticate -> "reauthenticate"
    | Reauthorize -> "reauthorize"

  (** The Facebook Login dialog URL: the one supported builder.

      [GET {!oauth_authorize_url}?client_id&redirect_uri&state&response_type=code&scope=a,b]
      as documented for the manual Login flow
      ({{:https://developers.facebook.com/docs/facebook-login/guides/advanced/manual-flow}}:
      "[scope]: A comma or space separated list of Permissions"). Scopes go
      out comma-separated in the plain [scope] parameter; Facebook does not
      support PKCE.

      @param scopes Permission names, e.g. [["pages_show_list"]]; use
        [List.map Scopes.scope_to_string] for typed scopes. Empty names are
        dropped.
      @param auth_type Omitted by default (Meta's default dialog).
      @param extra_params Appended as given, e.g. [("config_id", id)] for
        Facebook Login for Business. *)
  let authorize_url ~client_id ~redirect_uri ~state ~scopes ?auth_type ?(extra_params = []) () =
    let params =
      [ ("client_id", client_id);
        ("redirect_uri", redirect_uri);
        ("state", state);
        ("response_type", "code");
        ("scope", Social_oauth_scope.join Social_oauth_scope.Comma scopes) ]
      @ (match auth_type with
         | Some auth_type -> [ ("auth_type", auth_type_to_string auth_type) ]
         | None -> [])
      @ extra_params
    in
    let query = Uri.encoded_of_query (List.map (fun (k, v) -> (k, [ v ])) params) in
    Printf.sprintf "%s?%s" Metadata.authorization_endpoint query
  
  (** {2 Token tooling helpers} *)

  (** [app_secret_proof ~app_secret ~access_token] is the [appsecret_proof]
      Graph expects when the app enables "Require App Secret": the
      lowercase-hex HMAC-SHA256 of the access token keyed with the app
      secret ({{:https://developers.facebook.com/docs/graph-api/securing-requests}}). *)
  let app_secret_proof ~app_secret ~access_token =
    Digestif.SHA256.to_hex (Digestif.SHA256.hmac_string ~key:app_secret access_token)

  (** Query parameters carrying [access_token] and, when [app_secret] is
      given, its [appsecret_proof]. *)
  let token_query ?app_secret access_token =
    ("access_token", [ access_token ])
    ::
    (match app_secret with
     | Some app_secret -> [ ("appsecret_proof", [ app_secret_proof ~app_secret ~access_token ]) ]
     | None -> [])

  (** A transport failure message can echo the request URL, which carries
      tokens; keep only messages that do not. *)
  let redact_transport_message message =
    let lower = String.lowercase_ascii message in
    let mentions needle =
      let n = String.length needle and h = String.length lower in
      let rec scan i = i + n <= h && (String.sub lower i n = needle || scan (i + 1)) in
      scan 0
    in
    if List.exists mentions [ "token"; "secret"; "appsecret_proof" ] then
      "[REDACTED_SENSITIVE_TEXT]"
    else message

  (** Status of one entry of [/me/permissions]. *)
  type permission_status =
    | Granted
    | Declined
    | Expired  (** The user granted it, but the grant has lapsed (re-request it). *)
    | Unknown_status of string  (** A value Meta's reference does not list. *)

  type permission = {
    permission : string;
    status : permission_status;
  }

  let permission_status_of_string = function
    | "granted" -> Granted
    | "declined" -> Declined
    | "expired" -> Expired
    | other -> Unknown_status other

  (** Parses a [/me/permissions] body: [{"data": [{"permission", "status"}]}].
      Entries without both members are skipped. *)
  let parse_permissions body : (permission list, string) Stdlib.result =
    match Yojson.Basic.from_string body with
    | exception Yojson.Json_error message -> Error ("permissions response is not JSON: " ^ message)
    | `Assoc fields -> (
        match List.assoc_opt "data" fields with
        | Some (`List entries) ->
            Ok
              (List.filter_map
                 (function
                   | `Assoc entry -> (
                       match List.assoc_opt "permission" entry, List.assoc_opt "status" entry with
                       | Some (`String permission), Some (`String status) ->
                           Some { permission; status = permission_status_of_string status }
                       | _, _ -> None)
                   | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ -> None)
                 entries)
        | Some (`Null | `Bool _ | `Int _ | `Float _ | `String _ | `Assoc _) | None ->
            Error "permissions response has no data array")
    | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ ->
        Error "permissions response is not an object"

  (** The names with status [Granted], and with status [Declined], in
      response order. *)
  let granted_permissions permissions =
    List.filter_map
      (fun p -> match p.status with Granted -> Some p.permission | Declined | Expired | Unknown_status _ -> None)
      permissions

  let declined_permissions permissions =
    List.filter_map
      (fun p -> match p.status with Declined -> Some p.permission | Granted | Expired | Unknown_status _ -> None)
      permissions

  (** {2 Login discovery types} *)

  (** A Facebook Page as returned by [/me/accounts] and the Business
      [owned_pages] / [client_pages] edges
      ({{:https://developers.facebook.com/docs/graph-api/reference/page/}}). *)
  type page = {
    id : string;
    name : string;
    access_token : string option;
        (** The Page access token. Returned only when the user has a role on
            the Page (other than Live Contributor); Business edges can list
            Pages the user cannot act for, without a token. *)
    category : string option;
    about : string option;
    picture_url : string option;  (** [picture{url}] *)
    tasks : string list;
        (** The user's tasks on the Page ([MANAGE], [CREATE_CONTENT], ...).
            Present on [/me/accounts] when requested. *)
  }

  (** A Business the user belongs to ([/me/businesses]). *)
  type business = {
    business_id : string;
    business_name : string option;
  }

  (** Which Business edge a Page came from. *)
  type business_page_edge =
    | Owned_pages  (** [/{business_id}/owned_pages] *)
    | Client_pages  (** [/{business_id}/client_pages] *)

  type business_page_failure = {
    failed_business_id : string;
    failed_edge : business_page_edge;
    failure : Error_types.error;
  }

  type business_pages = {
    pages : page list;  (** Deduplicated by Page id, first occurrence kept. *)
    failures : business_page_failure list;
        (** Edges that could not be read. A user is often an employee of a
            Business whose Pages they may not list; one such edge does not
            hide the Pages of the others. *)
  }

  let business_page_edge_path = function
    | Owned_pages -> "owned_pages"
    | Client_pages -> "client_pages"

  (** Fields requested by default for {!page}. [tasks] is not among them:
      it exists only on [/me/accounts], and Graph rejects unknown fields. *)
  let default_page_fields = [ "id"; "name"; "access_token"; "category"; "about"; "picture{url}" ]

  let json_string key json =
    match json with
    | `Assoc fields -> (
        match List.assoc_opt key fields with
        | Some (`String value) -> Some value
        | Some (`Int value) -> Some (string_of_int value)
        | Some (`Null | `Bool _ | `Float _ | `List _ | `Assoc _) | None -> None)
    | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ -> None

  let json_member key json =
    match json with
    | `Assoc fields -> List.assoc_opt key fields
    | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ -> None

  let non_empty = function
    | Some "" -> None
    | (Some _ | None) as value -> value

  let page_of_json json =
    match json_string "id" json, json_string "name" json with
    | Some id, Some name ->
        Some
          {
            id;
            name;
            access_token = non_empty (json_string "access_token" json);
            category = non_empty (json_string "category" json);
            about = non_empty (json_string "about" json);
            picture_url =
              Option.bind (json_member "picture" json) (fun picture ->
                  Option.bind (json_member "data" picture) (fun data ->
                      non_empty (json_string "url" data)));
            tasks =
              (match json_member "tasks" json with
               | Some (`List tasks) ->
                   List.filter_map
                     (function
                       | `String task -> Some task
                       | `Null | `Bool _ | `Int _ | `Float _ | `List _ | `Assoc _ -> None)
                     tasks
               | Some (`Null | `Bool _ | `Int _ | `Float _ | `String _ | `Assoc _) | None -> []);
          }
    | (Some _ | None), _ -> None

  let business_of_json json =
    Option.map
      (fun business_id -> { business_id; business_name = json_string "name" json })
      (json_string "id" json)

  (** Parses one page of a Graph list: the items of [data] that [item_of]
      accepts, and the [after] cursor when [paging.next] says more pages
      follow. *)
  let parse_graph_list item_of body : ('a list * string option, string) Stdlib.result =
    match Yojson.Basic.from_string body with
    | exception Yojson.Json_error message -> Error ("Graph list response is not JSON: " ^ message)
    | json -> (
        match json_member "data" json with
        | Some (`List items) ->
            let paging = json_member "paging" json in
            let has_next = Option.bind paging (json_string "next") <> None in
            let after =
              Option.bind paging (fun paging ->
                  Option.bind (json_member "cursors" paging) (json_string "after"))
            in
            Ok (List.filter_map item_of items, if has_next then after else None)
        | Some (`Null | `Bool _ | `Int _ | `Float _ | `String _ | `Assoc _) | None ->
            Error "Graph list response has no data array")

  let dedup_pages pages =
    List.fold_left
      (fun (seen, acc) page ->
        if List.mem page.id seen then (seen, acc) else (page.id :: seen, page :: acc))
      ([], []) pages
    |> snd |> List.rev

  (** Make functor for OAuth operations that need HTTP client *)
  module Make (Http : HTTP_CLIENT) = struct
    (** Exchange authorization code for short-lived access token
        
        Note: The returned token is SHORT-LIVED (~1-2 hours).
        Call exchange_for_long_lived_token to get a 60-day token.
        
        @param client_id Facebook App ID
        @param client_secret Facebook App Secret
        @param redirect_uri Registered callback URL
        @param code Authorization code from callback
        @param on_success Continuation receiving credentials
        @param on_error Continuation receiving error message
    *)
    let exchange_code ~client_id ~client_secret ~redirect_uri ~code on_success on_error =
      let params = [
        ("client_id", [client_id]);
        ("client_secret", [client_secret]);
        ("redirect_uri", [redirect_uri]);
        ("code", [code]);
      ] in
      let query = Uri.encoded_of_query params in
      let url = Printf.sprintf "%s?%s" Metadata.token_endpoint query in
      
      Http.get url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              let open Yojson.Basic.Util in
              let access_token = json |> member "access_token" |> to_string in
              let expires_in = 
                try json |> member "expires_in" |> to_int
                with _ -> 3600 (* Default to 1 hour if not provided *)
              in
              let expires_at = 
                let now = Ptime_clock.now () in
                match Ptime.add_span now (Ptime.Span.of_int_s expires_in) with
                | Some exp -> Some (Ptime.to_rfc3339 exp)
                | None -> None in
              let token_type_str =
                try json |> member "token_type" |> to_string
                with _ -> "Bearer" in
              let scope =
                try Some (json |> member "scope" |> to_string)
                with _ -> None in
              let creds : credentials = {
                access_token;
                refresh_token = None;  (* Facebook doesn't use refresh tokens *)
                expires_at;
                auth_type = auth_type_of_string token_type_str;
                scope;
              } in
              on_success creds
            with e ->
              on_error (Printf.sprintf "Failed to parse token response: %s" (Printexc.to_string e))
          else
            on_error (Printf.sprintf "Token exchange failed (%d): %s" response.status response.body))
        on_error

    (** Exchange short-lived token for long-lived token (60 days)
        
        IMPORTANT: Always call this after exchange_code to get a usable token.
        
        @param client_id Facebook App ID
        @param client_secret Facebook App Secret
        @param short_lived_token The short-lived token from exchange_code
        @param on_success Continuation receiving long-lived credentials
        @param on_error Continuation receiving error message
    *)
    let exchange_for_long_lived_token ~client_id ~client_secret ~short_lived_token on_success on_error =
      let params = [
        ("grant_type", ["fb_exchange_token"]);
        ("client_id", [client_id]);
        ("client_secret", [client_secret]);
        ("fb_exchange_token", [short_lived_token]);
      ] in
      let query = Uri.encoded_of_query params in
      let url = Printf.sprintf "%s?%s" Metadata.token_endpoint query in
      
      Http.get url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              let open Yojson.Basic.Util in
              let access_token = json |> member "access_token" |> to_string in
              let expires_in = 
                try json |> member "expires_in" |> to_int
                with _ -> 5184000 (* Default to 60 days *)
              in
              let expires_at = 
                let now = Ptime_clock.now () in
                match Ptime.add_span now (Ptime.Span.of_int_s expires_in) with
                | Some exp -> Some (Ptime.to_rfc3339 exp)
                | None -> None in
              let token_type_str =
                try json |> member "token_type" |> to_string
                with _ -> "Bearer" in
              let scope =
                try Some (json |> member "scope" |> to_string)
                with _ -> None in
              let creds : credentials = {
                access_token;
                refresh_token = None;
                expires_at;
                auth_type = auth_type_of_string token_type_str;
                scope;
              } in
              on_success creds
            with e ->
              on_error (Printf.sprintf "Failed to parse long-lived token response: %s" (Printexc.to_string e))
          else
            on_error (Printf.sprintf "Long-lived token exchange failed (%d): %s" response.status response.body))
        on_error
    
    (** Page token information *)
    type page_info = {
      page_id: string;
      page_name: string;
      page_access_token: string;
      page_category: string option;
    }
    
    (** Get pages the user manages with their Page access tokens
        
        @param user_access_token Long-lived user access token
        @param on_success Continuation receiving list of page info
        @param on_error Continuation receiving error message
    *)
    let get_user_pages ~user_access_token on_success on_error =
      let query =
        Uri.encoded_of_query
          [ ("fields", ["id,name,access_token,category"]);
            ("access_token", [user_access_token]) ]
      in
      let url = Printf.sprintf "%s/me/accounts?%s" Metadata.api_base query in
      
      Http.get url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              let open Yojson.Basic.Util in
              let pages_data = json |> member "data" |> to_list in
              let pages = List.map (fun page ->
                {
                  page_id = page |> member "id" |> to_string;
                  page_name = page |> member "name" |> to_string;
                  page_access_token = page |> member "access_token" |> to_string;
                  page_category = page |> member "category" |> to_string_option;
                }
              ) pages_data in
              on_success pages
            with e ->
              on_error (Printf.sprintf "Failed to parse pages response: %s" (Printexc.to_string e))
          else
            on_error (Printf.sprintf "Get pages failed (%d): %s" response.status response.body))
        on_error
    
    (** Debug/inspect a token to check its validity and permissions
        
        @param access_token The token to inspect
        @param app_token App access token (client_id|client_secret)
        @param on_success Continuation receiving token info as JSON
        @param on_error Continuation receiving error message
    *)
    let debug_token ~access_token ~app_token on_success on_error =
      (* The app token is "app_id|app_secret"; both tokens must be
         percent-encoded or a reserved character in either one changes the
         query. *)
      let query =
        Uri.encoded_of_query
          [ ("input_token", [access_token]); ("access_token", [app_token]) ]
      in
      let url = Printf.sprintf "%s/debug_token?%s" Metadata.api_base query in
      
      Http.get url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              on_success json
            with e ->
              on_error (Printf.sprintf "Failed to parse debug response: %s" (Printexc.to_string e))
          else
            on_error (Printf.sprintf "Debug token failed (%d): %s" response.status response.body))
        on_error

    (** {2 Typed token reads} *)

    let graph_error ~status_code body =
      Social_meta_graph_error.to_error ~platform:Platform_types.FacebookPage ~status_code
        ~required_permissions:[] body

    (** GET [url]; a 2xx body goes through [parse], anything else becomes the
        shared Graph error, and a transport failure a [Network_error]. *)
    let get_parsed url parse on_result =
      Http.get url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            match (parse response.body : (_, string) Stdlib.result) with
            | Stdlib.Ok value -> on_result (Stdlib.Ok value)
            | Stdlib.Error message -> on_result (Stdlib.Error (Error_types.Internal_error message))
          else on_result (Stdlib.Error (graph_error ~status_code:response.status response.body)))
        (fun err ->
          on_result (Stdlib.Error (Social_meta_graph_error.transport_error (redact_transport_message err))))

    (** Inspect a token with [GET /debug_token]
        ({{:https://developers.facebook.com/docs/graph-api/reference/debug_token/}}).

        Typed replacement for {!debug_token}, which returns raw JSON through
        string continuations and stays for compatibility.

        @param app_access_token An app access token, e.g.
          {!Social_meta_debug_token.app_access_token} ["app_id|app_secret"].
          Keep it server-side.
        @param input_token The user or Page token to inspect.

        A token Meta considers invalid is [Ok] with [is_valid = false] (and
        usually [error]); [Error] means the call itself failed. *)
    let debug_token_info ~app_access_token ~input_token on_result =
      let query =
        Uri.encoded_of_query
          [ ("input_token", [ input_token ]); ("access_token", [ app_access_token ]) ]
      in
      get_parsed
        (Printf.sprintf "%s/debug_token?%s" Metadata.api_base query)
        Social_meta_debug_token.of_body on_result

    (** The permissions a user granted, declined or let expire, from
        [GET /me/permissions]
        ({{:https://developers.facebook.com/docs/graph-api/reference/user/permissions/}}).
        Use {!granted_permissions} / {!declined_permissions} to split them.

        @param access_token A user access token.
        @param app_secret Sends [appsecret_proof] when given. *)
    let get_permissions ~access_token ?app_secret on_result =
      let query = Uri.encoded_of_query (token_query ?app_secret access_token) in
      get_parsed
        (Printf.sprintf "%s/me/permissions?%s" Metadata.api_base query)
        parse_permissions on_result

    (** {2 Login discovery} *)

    (** The Facebook user id of a user token ([GET /me?fields=id]). *)
    let get_me_id ~access_token ?app_secret on_result =
      let query = Uri.encoded_of_query (("fields", [ "id" ]) :: token_query ?app_secret access_token) in
      get_parsed
        (Printf.sprintf "%s/me?%s" Metadata.api_base query)
        (fun body ->
          match Yojson.Basic.from_string body with
          | exception Yojson.Json_error message -> Stdlib.Error ("/me response is not JSON: " ^ message)
          | json -> (
              match json_string "id" json with
              | Some id -> Stdlib.Ok id
              | None -> Stdlib.Error "/me response has no id"))
        on_result

    (** Reads every page of a Graph list edge, following [paging.cursors.after]
        while Graph reports a [paging.next], up to [max_pages] requests. *)
    let get_graph_list ~path ~access_token ?app_secret ~fields ~max_pages item_of on_result =
      let rec fetch ~remaining ~after acc =
        let query =
          Uri.encoded_of_query
            ((("fields", [ String.concat "," fields ]) :: token_query ?app_secret access_token)
             @ match after with Some cursor -> [ ("after", [ cursor ]) ] | None -> [])
        in
        get_parsed
          (Printf.sprintf "%s/%s?%s" Metadata.api_base path query)
          (parse_graph_list item_of)
          (function
            | Stdlib.Error err -> on_result (Stdlib.Error err)
            | Stdlib.Ok (items, next) ->
                let acc = List.rev_append items acc in
                (match next with
                 | Some cursor when remaining > 1 -> fetch ~remaining:(remaining - 1) ~after:(Some cursor) acc
                 | Some _ | None -> on_result (Stdlib.Ok (List.rev acc))))
      in
      fetch ~remaining:(max 1 max_pages) ~after:None []

    (** The Pages the user has a role on, with their Page access tokens, from
        [GET /me/accounts]
        ({{:https://developers.facebook.com/docs/graph-api/reference/user/accounts/}}),
        following pagination. Needs [pages_show_list].

        Typed, paginated successor of {!get_user_pages}, which reads only the
        first page and stays for compatibility.

        @param fields Defaults to {!default_page_fields}; add ["tasks"] for
          the user's tasks per Page.
        @param max_pages Request cap (default 50 pages of results). *)
    let get_managed_pages ~access_token ?app_secret ?(fields = default_page_fields)
        ?(max_pages = 50) on_result =
      get_graph_list ~path:"me/accounts" ~access_token ?app_secret ~fields ~max_pages
        page_of_json on_result

    (** The Businesses the user belongs to ([GET /me/businesses],
        {{:https://developers.facebook.com/docs/graph-api/reference/user/businesses/}}).
        Needs [business_management]. *)
    let get_businesses ~access_token ?app_secret ?(max_pages = 50) on_result =
      get_graph_list ~path:"me/businesses" ~access_token ?app_secret ~fields:[ "id"; "name" ]
        ~max_pages business_of_json on_result

    (** The Pages of one Business edge: [owned_pages]
        ({{:https://developers.facebook.com/docs/marketing-api/reference/business/owned_pages/}})
        or [client_pages]
        ({{:https://developers.facebook.com/docs/marketing-api/reference/business/client_pages/}}). *)
    let get_business_pages ~access_token ?app_secret ~business_id ~edge
        ?(fields = default_page_fields) ?(max_pages = 50) on_result =
      get_graph_list
        ~path:(Uri.pct_encode business_id ^ "/" ^ business_page_edge_path edge)
        ~access_token ?app_secret ~fields ~max_pages page_of_json on_result

    (** Pages reachable through Business Manager: [owned_pages] and
        [client_pages] of every Business from {!get_businesses}, deduplicated
        by Page id. Pages a user manages only through a Business do not appear
        in [/me/accounts].

        [Error] only when the Business list itself fails; a failing edge is
        reported in [failures] and the other edges are still read. Pages
        without an [access_token] are kept: filter them if you need to act
        as the Page. *)
    let get_all_business_pages ~access_token ?app_secret ?fields ?max_pages on_result =
      get_businesses ~access_token ?app_secret ?max_pages (function
        | Stdlib.Error err -> on_result (Stdlib.Error err)
        | Stdlib.Ok businesses ->
            let edges =
              List.concat_map
                (fun business -> [ (business.business_id, Owned_pages); (business.business_id, Client_pages) ])
                businesses
            in
            let rec next edges pages failures =
              match edges with
              | [] ->
                  on_result
                    (Stdlib.Ok { pages = dedup_pages (List.rev pages); failures = List.rev failures })
              | (business_id, edge) :: rest ->
                  get_business_pages ~access_token ?app_secret ~business_id ~edge ?fields ?max_pages
                    (function
                      | Stdlib.Ok found -> next rest (List.rev_append found pages) failures
                      | Stdlib.Error failure ->
                          next rest pages
                            ({ failed_business_id = business_id; failed_edge = edge; failure } :: failures))
            in
            next edges [] [])
  end
end

(** {1 Error Types} *)

(** Facebook error codes *)
type facebook_error_code = 
  | Invalid_token (* 102, 190 - Token expired/invalid *)
  | Rate_limit_exceeded (* 4, 9, 17, 32, 341, 368, 613, 80000-80014 *)
  | Permission_denied (* 3, 10, 200-299 - Permission issues *)
  | Invalid_parameter (* 100 - Invalid API parameter *)
  | Temporarily_unavailable (* 1, 2 - Temporary failure *)
  | Duplicate_post (* 506 - Duplicate content *)
  | Unknown of int

(** Structured Facebook API error *)
type facebook_error = {
  message : string;
  error_type : string;
  code : facebook_error_code;
  subcode : int option;
  fbtrace_id : string option;
  should_retry : bool;
  retry_after_seconds : int option;
}

(** {1 Rate Limiting Types} *)

(** Rate limit usage information from X-App-Usage header *)
type rate_limit_info = {
  call_count : int;
  total_cputime : int;
  total_time : int;
  percentage_used : float;
}

(** {1 Pagination Types} *)

(** Pagination cursors. The same type as {!Social_meta_graph_paging.cursors},
    shared with the Instagram and Threads packages. *)
type paging_cursors = Social_meta_graph_paging.cursors = {
  before : string option;
  after : string option;
}

(** Paginated response. The same type as {!Social_meta_graph_paging.page}. *)
type 'a page_result = 'a Social_meta_graph_paging.page = {
  data : 'a list;
  paging : paging_cursors option;
  next_url : string option;
  previous_url : string option;
}

(** Reads [permalink_url] from a [GET /{id}?fields=permalink_url] response body.
    [Ok None] when the object exists but Meta returns no permalink_url for it
    (absent, [null] or empty); [Error] when the body is not a JSON object
    or the field is not a string. *)
let parse_permalink_response body : (string option, string) Stdlib.result =
  match Yojson.Basic.from_string body with
  | exception Yojson.Json_error msg -> Error msg
  | `Assoc fields -> (
      match List.assoc_opt "permalink_url" fields with
      | None | Some `Null | Some (`String "") -> Ok None
      | Some (`String permalink) -> Ok (Some permalink)
      | Some (`Bool _ | `Int _ | `Float _ | `List _ | `Assoc _) ->
          Error "permalink_url is not a string")
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ ->
      Error "expected a JSON object"

(** Configuration module type for Facebook provider *)
module type CONFIG = sig
  module Http : HTTP_CLIENT
  
  val get_env : string -> string option
  val get_credentials : account_id:string -> (credentials -> unit) -> (string -> unit) -> unit
  val update_credentials : account_id:string -> credentials:credentials -> (unit -> unit) -> (string -> unit) -> unit
  val encrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val decrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val update_health_status : account_id:string -> status:string -> error_message:string option -> (unit -> unit) -> (string -> unit) -> unit
  val get_page_id : account_id:string -> (string -> unit) -> (string -> unit) -> unit
  
  (** Optional: Called when rate limit info is updated *)
  val on_rate_limit_update : rate_limit_info -> unit
end

(** Make functor to create Facebook provider with given configuration *)
module Make (Config : CONFIG) = struct
  (* Aliases of the top-level constants, kept for code that referenced them
     through the functor. *)
  let graph_api_version = api_version
  let graph_api_base = graph_api_base
  let rupload_base = rupload_base
  module OAuth_http = OAuth.Make(Config.Http)
  
  (** {1 Platform Constants} *)
  
  let max_post_length = 63206  (* Facebook's actual limit *)
  let recommended_post_length = 5000  (* Recommended for engagement *)
  let max_photos_per_post = 10
  
  (** {1 Validation Functions} *)
  
  (** Validate a single post's content *)
  let validate_post ~text ?(media_count=0) () =
    let errors = ref [] in
    
    (* Check text length *)
    let text_len = String.length text in
    if text_len > max_post_length then
      errors := Error_types.Text_too_long { length = text_len; max = max_post_length } :: !errors;
    
    (* Check media count *)
    if media_count > max_photos_per_post then
      errors := Error_types.Too_many_media { count = media_count; max = max_photos_per_post } :: !errors;
    
    if !errors = [] then Ok ()
    else Error (List.rev !errors)
  
  (** Validate thread content *)
  let validate_thread ~texts ?(media_counts=[]) () =
    if List.length texts = 0 then
      Error [Error_types.Thread_empty]
    else
      let errors = ref [] in
      List.iteri (fun i text ->
        let media_count = 
          try List.nth media_counts i 
          with _ -> 0 
        in
        match validate_post ~text ~media_count () with
        | Error post_errors ->
            errors := Error_types.Thread_post_invalid { index = i; errors = post_errors } :: !errors
        | Ok () -> ()
      ) texts;
      if !errors = [] then Ok ()
      else Error (List.rev !errors)
  
  (** Validate media URLs *)
  let validate_media_urls ~media_urls =
    let errors = ref [] in
    List.iter (fun url ->
      if not (String.starts_with ~prefix:"http://" url || String.starts_with ~prefix:"https://" url) then
        errors := Error_types.Invalid_url url :: !errors
    ) media_urls;
    if !errors = [] then Ok ()
    else Error (List.rev !errors)
  
  (** Validate media file for Facebook
      
      Facebook limits:
      - Images: 4MB for regular posts
      - Videos: ~1GB for Reels, 10GB for regular video
      - Reels: 90 seconds max duration
  *)
  let validate_media_file ~(media : Platform_types.post_media) =
    match media.Platform_types.media_type with
    | Platform_types.Image ->
        let max_image_bytes = 4 * 1024 * 1024 in (* 4MB *)
        if media.file_size_bytes > max_image_bytes then
          Error [Error_types.Media_too_large { 
            size_bytes = media.file_size_bytes; 
            max_bytes = max_image_bytes 
          }]
        else
          Ok ()
    | Platform_types.Video ->
        let max_video_bytes = 1024 * 1024 * 1024 in (* 1GB for Reels *)
        if media.file_size_bytes > max_video_bytes then
          Error [Error_types.Media_too_large { 
            size_bytes = media.file_size_bytes; 
            max_bytes = max_video_bytes 
          }]
        else
          Ok ()
    | Platform_types.Gif ->
        let max_gif_bytes = 8 * 1024 * 1024 in (* 8MB *)
        if media.file_size_bytes > max_gif_bytes then
          Error [Error_types.Media_too_large { 
            size_bytes = media.file_size_bytes; 
            max_bytes = max_gif_bytes 
          }]
        else
          Ok ()
  
  (* Keep old name for backward compatibility *)
  let validate_media = validate_media_urls
  
  (** {1 Error Handling} *)
  
  (* Projects the shared classification onto the older variant. *)
  let facebook_error_code_of_classification ~code = function
    | Social_meta_graph_error.Token_invalid _ -> Invalid_token
    | Social_meta_graph_error.Rate_limited -> Rate_limit_exceeded
    | Social_meta_graph_error.Permission_denied -> Permission_denied
    | Social_meta_graph_error.Resource_not_found | Social_meta_graph_error.Invalid_request ->
        Invalid_parameter
    | Social_meta_graph_error.Transient -> Temporarily_unavailable
    | Social_meta_graph_error.Duplicate_content -> Duplicate_post
    | Social_meta_graph_error.Unclassified -> Unknown code

  (** Graph error code as a [facebook_error_code]. The table is
      {!Social_meta_graph_error.classify}'s, shared with Instagram and Threads. A
      bare code has no subcode or [is_transient] flag; {!parse_facebook_error}
      classifies the whole error instead. *)
  let parse_error_code code =
    Social_meta_graph_error.classify
      { Social_meta_graph_error.code = Some code; subcode = None; error_type = None;
        message = None; user_title = None; user_message = None;
        fbtrace_id = None; is_transient = None }
    |> facebook_error_code_of_classification ~code
  
  (** Determine if error is retryable *)
  let is_retryable = function
    | Rate_limit_exceeded | Temporarily_unavailable -> true
    | Invalid_token | Permission_denied | Invalid_parameter | Duplicate_post
    | Unknown _ ->
        false
  
  (** Get recommended retry delay in seconds *)
  let get_retry_delay = function
    | Rate_limit_exceeded -> Some 300 (* 5 minutes *)
    | Temporarily_unavailable -> Some 60 (* 1 minute *)
    | Invalid_token | Permission_denied | Invalid_parameter | Duplicate_post
    | Unknown _ ->
        None
  
  (** Parse Facebook API error from response *)
  let parse_facebook_error response_body =
    match Social_meta_graph_error.of_body response_body with
    | None -> None
    | Some graph ->
        let code =
          facebook_error_code_of_classification
            ~code:(Option.value ~default:0 graph.code)
            (Social_meta_graph_error.classify graph)
        in
        Some {
          message = Option.value ~default:"Unknown error" graph.message;
          error_type = Option.value ~default:"UnknownError" graph.error_type;
          code;
          subcode = graph.subcode;
          fbtrace_id = graph.fbtrace_id;
          should_retry = is_retryable code;
          retry_after_seconds = get_retry_delay code;
        }
  
  (** Parse API error response and return structured Error_types.error *)
  let contains_substring s sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false

  (* Note: publish_video is NOT a real Facebook permission. Video and Reel
     publishing on Pages requires only pages_manage_posts with a Page access
     token. See: https://developers.facebook.com/docs/video-api/guides/publishing *)
  let provider_required_permissions = [
    "pages_read_engagement";
    "pages_manage_posts";
    "pages_show_list";
  ]

  let permissions_for_path path =
    let p = String.lowercase_ascii path in
    if contains_substring p "me/accounts" then
      ["pages_show_list"]
    else if contains_substring p "video_reels" || contains_substring p "video_stories"
         || contains_substring p "/videos" then
      ["pages_manage_posts"]
    else if contains_substring p "photo_stories" || contains_substring p "photos" || contains_substring p "feed" then
      ["pages_manage_posts"]
    else
      provider_required_permissions

  let parse_api_error_with_permissions ~required_permissions ~status_code ~response_body =
    Social_meta_graph_error.to_error
      ~platform:Platform_types.FacebookPage
      ~status_code
      ~required_permissions
      ?retry_after_seconds:(get_retry_delay Rate_limit_exceeded)
      response_body

  let parse_api_error ~status_code ~response_body =
    parse_api_error_with_permissions
      ~required_permissions:provider_required_permissions
      ~status_code
      ~response_body

  let contains_substring haystack needle =
    let h_len = String.length haystack in
    let n_len = String.length needle in
    if n_len = 0 then true
    else if n_len > h_len then false
    else
      let rec loop i =
        if i + n_len > h_len then false
        else if String.sub haystack i n_len = needle then true
        else loop (i + 1)
      in
      loop 0

  let redact_sensitive_text_if_needed text =
    let lower = String.lowercase_ascii text in
    if List.exists (contains_substring lower)
         [ "access_token=";
           "access_token";
           "client_secret";
           "authorization" ]
    then "[REDACTED_SENSITIVE_TEXT]"
    else text
  
  (** A request that never got an HTTP response (connection refused, DNS,
      TLS, timeout) is an outage: not an SDK bug ([Internal_error]) and not
      evidence about the credentials. Consumers routing on the error
      variant, and {!Health_status.of_error}, rely on this distinction. *)
  let transport_error err =
    Social_meta_graph_error.transport_error (redact_sensitive_text_if_needed err)

  (** {1 Rate Limiting} *)
  
  (** Parse X-App-Usage header *)
  let parse_rate_limit_header headers =
    let parse_usage_number json field =
      try json |> Yojson.Basic.Util.member field |> Yojson.Basic.Util.to_int
      with _ ->
        try
          json
          |> Yojson.Basic.Util.member field
          |> Yojson.Basic.Util.to_float
          |> int_of_float
        with _ -> 0
    in
    try
      let usage_header = 
        List.find_opt (fun (k, _) -> 
          String.lowercase_ascii k = "x-app-usage"
        ) headers 
      in
      match usage_header with
      | Some (_, value) ->
          let json = Yojson.Basic.from_string value in
          let call_count = parse_usage_number json "call_count" in
          let total_cputime = parse_usage_number json "total_cputime" in
          let total_time = parse_usage_number json "total_time" in
          (* Facebook uses percentage-based limits *)
          let percentage_used = float_of_int (max call_count (max total_cputime total_time)) in
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
  
  (** {1 Pagination} *)
  
  (** Parse pagination info from response *)
  let parse_paging json =
    try
      let open Yojson.Basic.Util in
      let paging_obj = json |> member "paging" in
      let cursors = paging_obj |> member "cursors" in
      let before = cursors |> member "before" |> to_string_option in
      let after = cursors |> member "after" |> to_string_option in
      let next_url = paging_obj |> member "next" |> to_string_option in
      let previous_url = paging_obj |> member "previous" |> to_string_option in
      Some {
        before;
        after;
      }, next_url, previous_url
    with _ -> None, None, None
  
  (** GET an absolute Graph URL with the token as a Bearer header and, when
      an app secret is configured, [appsecret_proof] added to the query. *)
  let get_url ~url ~access_token ~required_permissions on_result =
    let final_url =
      match compute_app_secret_proof ~access_token with
      | Some proof ->
          let existing_query = if String.contains url '?' then "&" else "?" in
          url ^ existing_query ^ Uri.encoded_of_query [ ("appsecret_proof", [ proof ]) ]
      | None -> url
    in
    let headers = [ ("Authorization", Printf.sprintf "Bearer %s" access_token) ] in
    Config.Http.get ~headers final_url
      (fun response ->
        update_rate_limits response;
        if response.status >= 200 && response.status < 300 then
          on_result (Ok response)
        else
          on_result
            (Error
               (parse_api_error_with_permissions ~required_permissions
                  ~status_code:response.status ~response_body:response.body)))
      (fun err -> on_result (Error (transport_error err)))

  (** Generic GET request with pagination support.

      [?query] accepts arbitrary additional query-string params for
      endpoints that need more than [fields] / [cursor]. Most insights
      endpoints, for example, take [metric] / [period] / [since] /
      [until] as plain query params; passing them through here keeps
      callers off the hand-rolled URL-construction path and routes
      everything through the SDK's auth / appsecret_proof / rate-limit
      / error-classification stack. *)
  let get_paginated ~path ~access_token ?fields ?cursor ?(query=[]) on_result =
    let field_params = match fields with
      | Some f -> [("fields", String.concat "," f)]
      | None -> []
    in
    let cursor_params = match cursor with
      | Some c -> [("after", c)]
      | None -> []
    in
    let params = field_params @ cursor_params @ query in
    
    let url = 
      if List.length params > 0 then
        let query = Uri.encoded_of_query (List.map (fun (k, v) -> (k, [v])) params) in
        Printf.sprintf "%s/%s?%s" graph_api_base path query
      else
        Printf.sprintf "%s/%s" graph_api_base path
    in
    
    get_url ~url ~access_token ~required_permissions:(permissions_for_path path)
      on_result

  (** {1 Token Management} *)
  
  (** Check if token is expired or expiring soon *)
  let is_token_expired_buffer ~buffer_seconds expires_at_opt =
    Social_token_expiry.is_expired ~buffer_seconds expires_at_opt
  
  (** Ensure valid access token *)
  let ensure_valid_token ~account_id on_success on_error =
    Config.get_credentials ~account_id
      (fun creds ->
        (* Check if token needs refresh (24 hour buffer) *)
        if is_token_expired_buffer ~buffer_seconds:86400 creds.expires_at then
          (* Token expiring soon - Facebook requires re-authentication *)
          Config.update_health_status ~account_id
            ~status:Health_status.(to_string Token_expired)
            ~error_message:(Some "Access token expired - please reconnect")
            (fun () -> on_error (Error_types.Auth_error Error_types.Token_expired))
            (fun _ -> on_error (Error_types.Auth_error Error_types.Token_expired))
        else
          (* Token still valid *)
          Config.update_health_status ~account_id
            ~status:Health_status.(to_string Healthy) ~error_message:None
            (fun () -> on_success creds.access_token)
            (fun err -> on_error (Error_types.Network_error (Error_types.Connection_failed err))))
      (fun err -> on_error (Error_types.Network_error (Error_types.Connection_failed err)))

  (* Token_revoked is included because Graph subcodes 458 (app not
     installed) and 460 (password changed) map to it; before the shared
     Meta classifier they arrived as Token_invalid and went through
     recovery, which also records the account's health. *)
  let is_page_token_auth_error = function
    | Error_types.Auth_error
        ( Error_types.Insufficient_permissions _ | Error_types.Token_invalid
        | Error_types.Token_expired | Error_types.Token_revoked ) ->
        true
    | Error_types.Auth_error (Error_types.Refresh_failed _ | Error_types.Missing_credentials)
    | Error_types.Validation_error _ | Error_types.Rate_limited _
    | Error_types.Api_error _ | Error_types.Network_error _
    | Error_types.Duplicate_content | Error_types.Content_policy_violation _
    | Error_types.Resource_not_found _ | Error_types.Internal_error _ ->
        false

  let is_recoverable_posting_auth_error = is_page_token_auth_error

  let should_try_next_recovery_token = is_page_token_auth_error

  let recover_page_access_token ~account_id ~current_access_token on_success on_error =
    Config.get_page_id ~account_id
      (fun page_id ->
        let report_recovery_status status message cont =
          Config.update_health_status ~account_id ~status ~error_message:(Some message)
            cont
            (fun _ -> cont ())
        in
        let dedupe_non_empty tokens =
          let rec loop seen = function
            | [] -> List.rev seen
            | t :: rest when t = "" || List.mem t seen -> loop seen rest
            | t :: rest -> loop (t :: seen) rest
          in
          loop [] tokens
        in
        let fetch_pages_with_user_token ~user_token on_success_fetch on_error_fetch =
          let params = [
            ("fields", ["id,access_token"]);
            ("access_token", [user_token]);
          ] @
          (match compute_app_secret_proof ~access_token:user_token with
           | Some proof -> [("appsecret_proof", [proof])]
           | None -> []) in
          let url = Printf.sprintf "%s/me/accounts?%s" graph_api_base (Uri.encoded_of_query params) in
          Config.Http.get url
            (fun response ->
              if response.status >= 200 && response.status < 300 then
                try
                  let json = Yojson.Basic.from_string response.body in
                  let open Yojson.Basic.Util in
                  let pages = json |> member "data" |> to_list in
                  let maybe_page =
                    List.find_opt (fun page ->
                      try page |> member "id" |> to_string = page_id
                      with _ -> false
                    ) pages
                  in
                  match maybe_page with
                  | Some page ->
                      let page_access_token = page |> member "access_token" |> to_string in
                      if page_access_token = "" then
                        on_error_fetch (Error_types.Auth_error (Error_types.Insufficient_permissions {
                          required = ["pages_show_list"; "pages_manage_posts"];
                          platform_message = Some "Page access token missing from /me/accounts response";
                        }))
                      else
                        on_success_fetch (user_token, page_access_token)
                  | None ->
                      on_error_fetch (Error_types.Auth_error (Error_types.Insufficient_permissions {
                        required = ["pages_show_list"; "pages_manage_posts"];
                        platform_message = Some "Page not visible in /me/accounts — user may lack admin access";
                      }))
                with _ ->
                  on_error_fetch (Error_types.Auth_error Error_types.Token_invalid)
              else
                on_error_fetch (parse_api_error_with_permissions
                  ~required_permissions:["pages_show_list"]
                  ~status_code:response.status
                  ~response_body:response.body))
            (* No response is no evidence about the token. Reporting it as
               Token_invalid sent a connectivity blip down the "every token
               was rejected" path, which records token_revoked. *)
            (fun err -> on_error_fetch (transport_error err))
        in
        let rec try_candidate_tokens ~stored_creds ~last_error = function
          | [] ->
              let final_error =
                match last_error with
                | Some e -> e
                | None -> Error_types.Auth_error Error_types.Token_invalid
              in
              (* Every candidate user token was rejected by Facebook, so the
                 stored credentials really are dead. Which kind of dead is
                 [final_error]'s business, not this branch's: an expired user
                 token is [token_expired], which automatic refresh can still
                 recover, and only a revoked or scope-stripped one is
                 [token_revoked]. Hardcoding [token_revoked] agreed with
                 [of_error] for most auth errors, but only because
                 [should_try_next_recovery_token] recurses solely on
                 [Auth_error]; nothing pins that invariant.

                 [of_error] returns [None] only for errors that are not
                 credential evidence, which this loop cannot currently produce.
                 Reaching this branch at all means Facebook answered and
                 rejected every token, so [token_revoked] is the honest
                 fallback if that ever changes. *)
              let recovery_status =
                match Health_status.of_error final_error with
                | Some status -> status
                | None -> Health_status.Token_revoked
              in
              report_recovery_status (Health_status.to_string recovery_status)
                "Failed to recover Page token from /me/accounts for configured page_id"
                (fun () -> on_error final_error)
          | candidate_user_token :: rest ->
              fetch_pages_with_user_token ~user_token:candidate_user_token
                (fun (user_token_used, page_access_token) ->
                  let complete_success () =
                    report_recovery_status Health_status.(to_string Healthy)
                      "Recovered Page access token from /me/accounts after posting auth failure"
                      (fun () -> on_success page_id page_access_token)
                  in
                  match stored_creds with
                  | Some creds ->
                      let preserved_user_token =
                        if user_token_used = page_access_token then creds.refresh_token
                        else Some user_token_used
                      in
                      let updated_credentials = {
                        creds with
                        access_token = page_access_token;
                        refresh_token = preserved_user_token;
                        auth_type = Bearer;
                      } in
                      Config.update_credentials ~account_id ~credentials:updated_credentials
                        complete_success
                        (fun _ -> complete_success ())
                  | None ->
                      complete_success ())
                (fun err ->
                  if should_try_next_recovery_token err then
                    try_candidate_tokens ~stored_creds ~last_error:(Some err) rest
                  else
                    (* Recovery stopped on a non-auth failure, which by
                       definition says nothing about the stored credentials.
                       Leave the health status as it was and let the caller
                       surface the error. *)
                    on_error err)
        in
        Config.get_credentials ~account_id
          (fun creds ->
            let candidate_user_tokens =
              dedupe_non_empty (current_access_token :: (match creds.refresh_token with Some t -> [t] | None -> []))
            in
            try_candidate_tokens ~stored_creds:(Some creds) ~last_error:None candidate_user_tokens)
          (fun _ ->
            let candidate_user_tokens = dedupe_non_empty [current_access_token] in
            try_candidate_tokens ~stored_creds:None ~last_error:None candidate_user_tokens))
      (fun err -> on_error (Error_types.Internal_error err))

  let with_posting_page_context ~account_id on_success on_error =
    ensure_valid_token ~account_id
      (fun access_token ->
        Config.get_page_id ~account_id
          (fun page_id -> on_success page_id access_token)
          (fun err -> on_error (Error_types.Internal_error err)))
      on_error

  (** Get published posts from a Facebook Page's feed, including inline
      engagement and insights metrics for each post.

      Prefer {!get_page_posts_page_for_account} with
      [~fields:Page_posts.analytics_fields ~edge:Feed]: the same request,
      typed ({!Page_posts.page_post}), with Meta paging. This function is
      kept unchanged for existing callers.

      Fetches posts using cursor-based pagination via the Graph API
      /{page_id}/feed endpoint. Returns raw JSON with post data,
      pagination cursors, and per-post metrics nested under [insights],
      [comments_summary], [reactions_summary], and [shares].

      Why field-expansion instead of per-post /{post_id}/insights:
      Meta deprecated the singular-status endpoint in v2.4 (April 2019).
      Calls of the form [GET /{post_id}/insights] return
      [(#12) singular statuses API is deprecated] for status-type posts
      (text-only, no attachments). Reaching the insights via field
      expansion on the parent /{page_id}/posts edge bypasses the
      deprecation entirely — Meta serves the data because the request
      is rooted at the Page, not the singular post node. This restores
      analytics for status posts and drops API quota usage from
      [N posts × per-post calls] to [1 call per pagination page].

      The insights metric set includes [post_reactions_by_type_total]
      for reaction breakdowns and [post_clicks] / [post_clicks_by_type]
      for clicks. Deprecated impression metrics are excluded. Status posts
      get [reactions_summary.summary.total_count] /
      [comments_summary.summary.total_count] / [shares.count] even
      when their insights array is empty.

      Fields returned: id, message, created_time, full_picture,
      permalink_url, attachments, insights, comments_summary,
      reactions_summary, shares. Callers needing a post-level "type"
      classification should derive it from [attachments[0].media_type]
      — Meta deprecated the post-level [type] aggregation field in
      Graph API v3.3.

      @param account_id Social account ID for credential resolution
      @param limit Number of posts per page (default 25, max 100)
      @param after Pagination cursor from previous response
      @param on_result Continuation receiving Ok(json) or Error(error)
  *)
  let get_page_posts ~account_id ?(limit=25) ?(after=None) () on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        Config.get_page_id ~account_id
          (fun page_id ->
            (* Insights metric set is bounded by Meta's
               silent-omit-on-any-invalid-metric behaviour: if ANY
               metric in the list is invalid (or unavailable for a
               given post type), Meta drops the entire [insights]
               field from the response — no error, just absent. So
               the metric set has to be every-metric-valid-for-every-
               post-type. Empirically, [post_impressions_unique] /
               [post_reactions_by_type_total] / [post_clicks] /
               [post_clicks_by_type] survive across post types in v25
               in 2026; the v25 successor names
               [post_total_media_view_unique] and
               [post_impressions_organic_unique] are documented but
               appear to be the silent-omit triggers (verified
               empirically — including them on a real Page response
               drops the whole insights field).

               Reels are a separate pipeline at
               /{video_id}/video_insights with reels-specific
               metrics (fb_reels_total_plays etc.) — handled by a
               separate code path in the consumer, not via this
               feed-expansion call. *)
            let fields = Page_posts.analytics_fields in
            let params = [
              ("fields", [fields]);
              ("limit", [string_of_int limit]);
            ] @ (match after with Some c -> [("after", [c])] | None -> []) in
            let query = Uri.encoded_of_query params in
            let url = Printf.sprintf "%s/%s/feed?%s" graph_api_base page_id query in
            let proof = compute_app_secret_proof ~access_token in
            let auth_params = match proof with
              | Some p -> [("appsecret_proof", [p])]
              | None -> []
            in
            let final_url =
              if List.length auth_params > 0 then
                url ^ "&" ^ Uri.encoded_of_query auth_params
              else url
            in
            let headers = [
              ("Authorization", Printf.sprintf "Bearer %s" access_token);
            ] in
            Config.Http.get ~headers final_url
              (fun response ->
                update_rate_limits response;
                if response.status >= 200 && response.status < 300 then
                  (try
                    let json = Yojson.Basic.from_string response.body in
                    on_result (Ok json)
                  with e ->
                    on_result (Error (Error_types.Internal_error
                      (Printf.sprintf "Failed to parse page posts response: %s"
                        (Printexc.to_string e)))))
                else
                  on_result (Error (parse_api_error_with_permissions
                    ~required_permissions:["pages_read_engagement"]
                    ~status_code:response.status
                    ~response_body:response.body)))
              (fun err ->
                on_result (Error (transport_error err))))
          (fun err -> on_result (Error (Error_types.Internal_error err))))
      (fun err -> on_result (Error err))

  (** Upload photo to Facebook Page with optional alt text *)
  let upload_photo ~page_id ~page_access_token ~image_url ~alt_text ?(validate_before_upload=false) on_result =
    (* Download image first *)
    Config.Http.get ~headers:[] image_url
      (fun image_response ->
        if image_response.status >= 200 && image_response.status < 300 then
          let file_size = String.length image_response.body in
          
          (* Validate if requested *)
          let validation_result =
            if validate_before_upload then
              let media : Platform_types.post_media = {
                media_type = Platform_types.Image;
                mime_type = List.assoc_opt "content-type" image_response.headers 
                            |> Option.value ~default:"image/jpeg";
                file_size_bytes = file_size;
                width = None;
                height = None;
                duration_seconds = None;
                alt_text = alt_text;
              } in
              validate_media_file ~media
            else
              Ok ()
          in
          
          (match validation_result with
          | Error errs -> on_result (Error (Error_types.Validation_error errs))
          | Ok () ->
              let url = Printf.sprintf "%s/%s/photos" graph_api_base page_id in
              
              (* Add app secret proof if available *)
              let proof_params = match compute_app_secret_proof ~access_token:page_access_token with
                | Some proof -> [("appsecret_proof", [proof])]
                | None -> []
              in
              
              let final_url = 
                if List.length proof_params > 0 then
                  url ^ "?" ^ Uri.encoded_of_query proof_params
                else url
              in
              
              (* Create multipart form data *)
              let base_parts = [
                {
                  name = "source";
                  filename = Some "image.jpg";
                  content_type = Some "image/jpeg";
                  content = image_response.body;
                };
                {
                  name = "published";
                  filename = None;
                  content_type = None;
                  content = "false";  (* Upload unpublished, attach to post later *)
                };
              ] in
              
              (* Add alt text if provided *)
              let parts = match alt_text with
                | Some alt when String.length alt > 0 ->
                    base_parts @ [{
                      name = "alt_text_custom";
                      filename = None;
                      content_type = None;
                      content = alt;
                    }]
                | _ -> base_parts
              in
              
              let headers = [
                ("Authorization", Printf.sprintf "Bearer %s" page_access_token);
              ] in
              
              Config.Http.post_multipart ~headers ~parts final_url
                (fun response ->
                  update_rate_limits response;
                  if response.status >= 200 && response.status < 300 then
                    try
                      let open Yojson.Basic.Util in
                      let json = Yojson.Basic.from_string response.body in
                      let photo_id = json |> member "id" |> to_string in
                      on_result (Ok photo_id)
                    with e ->
                      on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse photo response: %s" (Printexc.to_string e))))
                  else
                    on_result (Error (parse_api_error_with_permissions
                      ~required_permissions:["pages_manage_posts"]
                      ~status_code:response.status
                      ~response_body:response.body)))
                (fun err -> on_result (Error (transport_error err))))
        else
          on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to download image from %s (%d)" image_url image_response.status))))
      (fun err ->
        on_result
          (Error (transport_error err)))
  
  (** {1 Page video upload phases (video_reels, video_stories)} *)

  (** Form params for one Page video upload phase: [params] followed by the
      Page token and, when FACEBOOK_APP_SECRET is set, its appsecret_proof. *)
  let page_token_form_params ~page_access_token params =
    params
    @ [("access_token", page_access_token)]
    @ (match compute_app_secret_proof ~access_token:page_access_token with
       | Some proof -> [("appsecret_proof", proof)]
       | None -> [])

  (** POST one upload phase to [/{page_id}/{edge}] as a urlencoded form.

      A 2xx hands the raw response body to [on_success]; any other status is
      mapped through [parse_api_error_with_permissions], because both edges
      need [pages_manage_posts]. *)
  let post_video_upload_phase ~page_id ~edge ~page_access_token ~params on_success on_error =
    let url = Printf.sprintf "%s/%s/%s" graph_api_base page_id edge in
    let body = Social_core.form_urlencode_kvs (page_token_form_params ~page_access_token params) in
    let headers = [("Content-Type", "application/x-www-form-urlencoded")] in
    Config.Http.post ~headers ~body url
      (fun response ->
        update_rate_limits response;
        if response.status >= 200 && response.status < 300 then
          on_success response.body
        else
          on_error (parse_api_error_with_permissions
            ~required_permissions:["pages_manage_posts"]
            ~status_code:response.status
            ~response_body:response.body))
      (fun err -> on_error (transport_error err))

  (** The Reels upload and finish phases answer [{"success": true}]. A 2xx
      with [success: false], or with no such flag, means Facebook did not
      accept the phase, so the reel must not be reported as published. *)
  let require_reel_phase_success ~phase body on_success on_error =
    let succeeded =
      try Yojson.Basic.Util.(Yojson.Basic.from_string body |> member "success" |> to_bool) with
      | Yojson.Json_error _ | Yojson.Basic.Util.Type_error _ -> false
    in
    if succeeded then on_success ()
    else
      on_error (Error_types.Internal_error
        (Printf.sprintf "Facebook reel %s phase did not succeed: %s" phase body))

  (** Upload and publish a Reel on a Facebook Page.

      Follows the Reels Publishing API with a hosted file:
      + [POST /{page_id}/video_reels] with [upload_phase=start] returns a [video_id].
      + [POST https://rupload.facebook.com/video-upload/{version}/{video_id}]
        with headers [Authorization: OAuth {page_token}] and [file_url: {video_url}]
        makes Facebook fetch the video itself.
      + [POST /{page_id}/video_reels] with [upload_phase=finish],
        [video_state=PUBLISHED], [description] and optional [place] publishes it.

      Facebook processes the video after the finish phase, so the reel can take
      a few minutes to appear on the Page.

      @param place Facebook Page ID of a location to tag the reel with. Only sent when given.
      @param on_success Receives the [video_id], which is also the reel's post id.
      @see <https://developers.facebook.com/docs/video-api/guides/reels-publishing>
  *)
  let upload_video_reel_typed ~page_id ~page_access_token ~video_url ~description ?place on_success on_error =
    let finish video_id =
      let params =
        [ ("upload_phase", "finish");
          ("video_id", video_id);
          ("video_state", "PUBLISHED");
          ("description", description) ]
        @ (match place with
           | Some p -> [("place", p)]
           | None -> [])
      in
      post_video_upload_phase ~page_id ~edge:"video_reels" ~page_access_token ~params
        (fun body ->
          require_reel_phase_success ~phase:"finish" body
            (fun () -> on_success video_id)
            on_error)
        on_error
    in
    let upload video_id =
      let url = Printf.sprintf "%s/%s" rupload_base video_id in
      let headers = [
        ("Authorization", Printf.sprintf "OAuth %s" page_access_token);
        ("file_url", video_url);
      ] in
      Config.Http.post ~headers ~body:"" url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            require_reel_phase_success ~phase:"upload" response.body
              (fun () -> finish video_id)
              on_error
          else
            on_error (parse_api_error_with_permissions
              ~required_permissions:["pages_manage_posts"]
              ~status_code:response.status
              ~response_body:response.body))
        (fun err -> on_error (transport_error err))
    in
    post_video_upload_phase ~page_id ~edge:"video_reels" ~page_access_token
      ~params:[("upload_phase", "start")]
      (fun body ->
        match Yojson.Basic.Util.(Yojson.Basic.from_string body |> member "video_id" |> to_string) with
        | video_id -> upload video_id
        | exception (Yojson.Json_error _ | Yojson.Basic.Util.Type_error _) ->
            on_error (Error_types.Internal_error
              (Printf.sprintf "Failed to parse reel start response: %s" body)))
      on_error

  let upload_video_reel ~page_id ~page_access_token ~video_url ~description ?place on_success on_error =
    upload_video_reel_typed ~page_id ~page_access_token ~video_url ~description ?place
      on_success
      (fun err -> on_error (Error_types.error_to_string err))

  (** Post Reel (short-form video) to Facebook Page.

      Runs {!upload_video_reel_typed}. An auth failure in any phase triggers
      one Page-token recovery and a retry of the whole flow.

      @param place Facebook Page ID of a location to tag the reel with.
  *)
  let post_reel ~account_id ~text ~video_url ?place on_result =
    (* Validate caption *)
    match validate_post ~text ~media_count:1 () with
    | Error errs -> on_result (Error_types.Failure (Error_types.Validation_error errs))
    | Ok () ->
        with_posting_page_context ~account_id
          (fun page_id page_access_token ->
            let rec attempt_post ~can_recover token =
              upload_video_reel_typed ~page_id ~page_access_token:token ~video_url ~description:text ?place
                (fun video_id -> on_result (Error_types.Success video_id))
                (fun parsed ->
                  if can_recover && is_recoverable_posting_auth_error parsed then
                    recover_page_access_token ~account_id ~current_access_token:token
                      (fun _resolved_page_id resolved_page_token ->
                        attempt_post ~can_recover:false resolved_page_token)
                      (fun _ -> on_result (Error_types.Failure parsed))
                  else
                    on_result (Error_types.Failure parsed))
            in
            attempt_post ~can_recover:true page_access_token)
          (fun err -> on_result (Error_types.Failure err))
  
  (** {1 Facebook Page Stories} *)
  
  (** Post photo story to Facebook Page
      
      Facebook Page Stories are full-screen vertical content that expires after 24 hours.
      This function posts a photo as a Page story.
      
      Requirements:
      - Page access token with pages_manage_posts permission
      - Image must be publicly accessible via HTTPS
      - Recommended: 9:16 aspect ratio (1080x1920 pixels)
      
      @see <https://developers.facebook.com/docs/graph-api/reference/page/photo_stories/>
      
      @param page_id Facebook Page ID
      @param page_access_token Page access token
      @param image_url Publicly accessible URL of the image
      @param on_result Continuation receiving api_result with story ID
  *)
  let upload_photo_story ~page_id ~page_access_token ~image_url on_result =
    (* Download image first *)
    Config.Http.get ~headers:[] image_url
      (fun image_response ->
        if image_response.status >= 200 && image_response.status < 300 then
          let url = Printf.sprintf "%s/%s/photo_stories" graph_api_base page_id in
          
          (* Add app secret proof if available *)
          let proof_params = match compute_app_secret_proof ~access_token:page_access_token with
            | Some proof -> [("appsecret_proof", [proof])]
            | None -> []
          in
          
          let final_url = 
            if List.length proof_params > 0 then
              url ^ "?" ^ Uri.encoded_of_query proof_params
            else url
          in
          
          (* Create multipart form data with photo *)
          let parts = [
            {
              name = "photo";
              filename = Some "story.jpg";
              content_type = Some "image/jpeg";
              content = image_response.body;
            };
          ] in
          
          let headers = [
            ("Authorization", Printf.sprintf "Bearer %s" page_access_token);
          ] in
          
          Config.Http.post_multipart ~headers ~parts final_url
            (fun response ->
              update_rate_limits response;
              if response.status >= 200 && response.status < 300 then
                try
                  let open Yojson.Basic.Util in
                  let json = Yojson.Basic.from_string response.body in
                  (* Response contains post_id for the story *)
                  let story_id = 
                    try json |> member "post_id" |> to_string
                    with _ -> json |> member "id" |> to_string
                  in
                  on_result (Ok story_id)
                with e ->
                  on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse photo story response: %s" (Printexc.to_string e))))
              else
                on_result (Error (parse_api_error_with_permissions
                  ~required_permissions:["pages_manage_posts"]
                  ~status_code:response.status
                  ~response_body:response.body)))
            (fun err -> on_result (Error (transport_error err)))
        else
          on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to download image from %s (%d)" image_url image_response.status))))
      (fun err ->
        on_result
          (Error (transport_error err)))
  
  (** Post video story to Facebook Page
      
      Facebook Page Stories are full-screen vertical content that expires after 24 hours.
      This function posts a video as a Page story using the resumable upload API.
      
      Requirements:
      - Page access token with pages_manage_posts permission
      - Video must be publicly accessible via HTTPS
      - Duration: 1-120 seconds
      - Recommended: 9:16 aspect ratio (1080x1920 pixels)
      - Supported formats: MP4, MOV
      
      @see <https://developers.facebook.com/docs/graph-api/reference/page/video_stories/>
      
      @param page_id Facebook Page ID
      @param page_access_token Page access token
      @param video_url Publicly accessible URL of the video
      @param on_result Continuation receiving api_result with story ID
  *)
  let upload_video_story ~page_id ~page_access_token ~video_url on_result =
    let on_error err = on_result (Error err) in
    (* Download video first *)
    Config.Http.get ~headers:[] video_url
      (fun video_response ->
        if video_response.status >= 200 && video_response.status < 300 then
          let video_content = video_response.body in
          let video_size = String.length video_content in

          (* Step 3: Finish the upload and publish the story *)
          let finish video_id =
            post_video_upload_phase ~page_id ~edge:"video_stories" ~page_access_token
              ~params:[("upload_phase", "finish"); ("video_id", video_id)]
              (fun body ->
                try
                  let open Yojson.Basic.Util in
                  let json = Yojson.Basic.from_string body in
                  (* Check for success and extract post_id *)
                  let success =
                    try json |> member "success" |> to_bool
                    with _ -> true
                  in
                  if success then
                    let story_id =
                      try json |> member "post_id" |> to_string
                      with _ -> video_id
                    in
                    on_result (Ok story_id)
                  else
                    on_result (Error (Error_types.Internal_error "Facebook video story upload failed: success=false"))
                with _e ->
                  (* If we can't parse but got 2xx, consider it a success *)
                  on_result (Ok video_id))
              on_error
          in

          (* Step 1: Initialize the upload session for video story *)
          post_video_upload_phase ~page_id ~edge:"video_stories" ~page_access_token
            ~params:[("upload_phase", "start")]
            (fun body ->
              try
                let open Yojson.Basic.Util in
                let json = Yojson.Basic.from_string body in
                let video_id = json |> member "video_id" |> to_string in
                let upload_url = json |> member "upload_url" |> to_string in

                (* Step 2: Upload video file to the upload_url *)
                let upload_headers = [
                  ("Authorization", Printf.sprintf "OAuth %s" page_access_token);
                  ("file_size", string_of_int video_size);
                ] in

                Config.Http.post ~headers:upload_headers ~body:video_content upload_url
                  (fun upload_response ->
                    if upload_response.status >= 200 && upload_response.status < 300 then
                      finish video_id
                    else
                      on_error (parse_api_error_with_permissions
                        ~required_permissions:["pages_manage_posts"]
                        ~status_code:upload_response.status
                        ~response_body:upload_response.body))
                  (fun err -> on_error (transport_error err))
              with e ->
                on_error (Error_types.Internal_error (Printf.sprintf "Failed to parse video story init response: %s" (Printexc.to_string e))))
            on_error
        else
          on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to download video from %s (%d)" video_url video_response.status))))
      (fun err ->
        on_result
          (Error (transport_error err)))
  
  (** Post photo story to Facebook Page (high-level)
      
      @param account_id Internal account identifier
      @param image_url Publicly accessible URL of the image
      @param on_result Continuation receiving outcome with story ID
  *)
  let post_story_photo ~account_id ~image_url on_result =
    with_posting_page_context ~account_id
      (fun page_id page_access_token ->
        let rec attempt_post can_recover token =
          upload_photo_story ~page_id ~page_access_token:token ~image_url
            (function
              | Ok story_id -> on_result (Error_types.Success story_id)
              | Error e when can_recover && is_recoverable_posting_auth_error e ->
                  recover_page_access_token ~account_id ~current_access_token:token
                    (fun _resolved_page_id resolved_page_token ->
                      attempt_post false resolved_page_token)
                    (fun _ -> on_result (Error_types.Failure e))
              | Error e -> on_result (Error_types.Failure e))
        in
        attempt_post true page_access_token)
      (fun err -> on_result (Error_types.Failure err))
  
  (** Post video story to Facebook Page (high-level)
      
      @param account_id Internal account identifier
      @param video_url Publicly accessible URL of the video
      @param on_result Continuation receiving outcome with story ID
  *)
  let post_story_video ~account_id ~video_url on_result =
    with_posting_page_context ~account_id
      (fun page_id page_access_token ->
        let rec attempt_post can_recover token =
          upload_video_story ~page_id ~page_access_token:token ~video_url
            (function
              | Ok story_id -> on_result (Error_types.Success story_id)
              | Error e when can_recover && is_recoverable_posting_auth_error e ->
                  recover_page_access_token ~account_id ~current_access_token:token
                    (fun _resolved_page_id resolved_page_token ->
                      attempt_post false resolved_page_token)
                    (fun _ -> on_result (Error_types.Failure e))
              | Error e -> on_result (Error_types.Failure e))
        in
        attempt_post true page_access_token)
      (fun err -> on_result (Error_types.Failure err))
  
  (** Detect media type from URL extension *)
  let detect_media_type url =
    match Content_validator.url_file_extension url with
    | ".mp4" | ".mov" | ".avi" -> "VIDEO"
    | _ -> "IMAGE"
  
  (** Post story to Facebook Page (auto-detect media type)
      
      Automatically detects whether the media is an image or video based on
      the file extension and posts it as a Facebook Page Story.
      
      @param account_id Internal account identifier
      @param media_url Publicly accessible URL of the image or video
      @param on_result Continuation receiving outcome with story ID
  *)
  let post_story ~account_id ~media_url on_result =
    let media_type = detect_media_type media_url in
    match media_type with
    | "VIDEO" -> post_story_video ~account_id ~video_url:media_url on_result
    | _ -> post_story_photo ~account_id ~image_url:media_url on_result
  
  (** Validate story media
      
      Validates that the media URL is appropriate for Facebook Page Stories.
      
      @param media_url The URL to validate
      @return Ok () if valid, Error message otherwise
  *)
  let validate_story ~media_url =
    let url_lower = String.lowercase_ascii media_url in
    if not (String.starts_with ~prefix:"http://" url_lower || String.starts_with ~prefix:"https://" url_lower) then
      Error "Story media URL must be a publicly accessible HTTP(S) URL"
    else
      match Content_validator.url_file_extension media_url with
      | ".jpg" | ".jpeg" | ".png" | ".gif" | ".mp4" | ".mov" -> Ok ()
      | _ -> Error "Story media must be an image (JPEG, PNG) or video (MP4, MOV)"
  
  (** Post to Facebook Page
      
      @param validate_media_before_upload When true, validates media file size after 
             download but before upload. Facebook limits: 4MB images, 1GB video.
             Default: false
  *)
  let post_single ~account_id ~text ~media_urls ?(alt_texts=[]) ?link ?place ?scheduled_publish_time ?(validate_media_before_upload=false) on_result =
    let media_count = List.length media_urls in
    
    (* Validate content first *)
    match validate_post ~text ~media_count () with
    | Error errs -> on_result (Error_types.Failure (Error_types.Validation_error errs))
    | Ok () ->
        (* Validate media URLs *)
        match validate_media ~media_urls with
        | Error errs -> on_result (Error_types.Failure (Error_types.Validation_error errs))
        | Ok () ->
            with_posting_page_context ~account_id
              (fun page_id page_access_token ->
                  let rec attempt_post can_recover token =
                    let cleanup_uploaded_photos photo_ids on_done =
                      let rec loop = function
                        | [] -> on_done ()
                        | photo_id :: rest ->
                            let proof_params =
                              match compute_app_secret_proof ~access_token:token with
                              | Some proof -> [ ("appsecret_proof", [ proof ]) ]
                              | None -> []
                            in
                            let base_url = Printf.sprintf "%s/%s" graph_api_base photo_id in
                            let delete_url =
                              if List.length proof_params > 0 then
                                base_url ^ "?" ^ Uri.encoded_of_query proof_params
                              else
                                base_url
                            in
                            let headers = [ ("Authorization", Printf.sprintf "Bearer %s" token) ] in
                            Config.Http.delete ~headers delete_url
                              (fun _ -> loop rest)
                              (fun _ -> loop rest)
                      in
                      loop photo_ids
                    in
                    (* Upload all photos first *)
                    let rec upload_all_photos urls_with_alt acc on_complete =
                      match urls_with_alt with
                      | [] -> on_complete (Ok (List.rev acc))
                      | (url, alt_text) :: rest ->
                          upload_photo ~page_id ~page_access_token:token ~image_url:url ~alt_text
                            ~validate_before_upload:validate_media_before_upload
                            (function
                              | Ok photo_id -> upload_all_photos rest (photo_id :: acc) on_complete
                              | Error e -> on_complete (Error (e, List.rev acc)))
                    in
                    
                    (* Pair URLs with alt text - use None if alt text list is shorter *)
                    let urls_with_alt = List.mapi (fun i url ->
                      let alt_text = try List.nth alt_texts i with _ -> None in
                      (url, alt_text)
                    ) media_urls in
                    
                    upload_all_photos urls_with_alt [] 
                      (function
                      | Error (e, uploaded_photo_ids) ->
                        cleanup_uploaded_photos uploaded_photo_ids (fun () ->
                          if can_recover && is_recoverable_posting_auth_error e then
                            recover_page_access_token ~account_id ~current_access_token:token
                              (fun _resolved_page_id resolved_page_token ->
                                attempt_post false resolved_page_token)
                              (fun _ -> on_result (Error_types.Failure e))
                          else
                            on_result (Error_types.Failure e))
                      | Ok photo_ids ->
                        (* Create Facebook Page post *)
                        let url = Printf.sprintf "%s/%s/feed" graph_api_base page_id in
                        let attached_media_params =
                          List.mapi (fun i photo_id ->
                            (Printf.sprintf "attached_media[%d]" i,
                              Yojson.Basic.to_string (`Assoc [("media_fbid", `String photo_id)]))
                          ) photo_ids
                        in

                        let params =
                          [
                            ("message", text);
                          ] @
                          attached_media_params @
                          (* Add link parameter for link posts *)
                          (match link with
                           | Some l -> [("link", l)]
                           | None -> []) @
                          (* Geotag the feed post with a location Page id. The
                             /{page-id}/feed endpoint accepts [place] regardless
                             of whether the post also has attached media, so this
                             covers single-photo and carousel posts. *)
                          (match place with
                           | Some p -> [("place", p)]
                           | None -> []) @
                          (* Add scheduled publishing parameters *)
                          (match scheduled_publish_time with
                           | Some t -> [("scheduled_publish_time", string_of_int t); ("published", "false")]
                           | None -> []) @
                          (* Add app secret proof *)
                          (match compute_app_secret_proof ~access_token:token with
                           | Some proof -> [("appsecret_proof", proof)]
                           | None -> [])
                        in

                        let body = Social_core.form_urlencode_kvs params in
                        let headers = [
                          ("Content-Type", "application/x-www-form-urlencoded");
                          ("Authorization", Printf.sprintf "Bearer %s" token);
                        ] in
                        
                        Config.Http.post ~headers ~body url
                          (fun response ->
                            update_rate_limits response;
                            if response.status >= 200 && response.status < 300 then
                              try
                                let open Yojson.Basic.Util in
                                let json = Yojson.Basic.from_string response.body in
                                let post_id = json |> member "id" |> to_string in
                                on_result (Error_types.Success post_id)
                              with _e ->
                                (* Post succeeded but couldn't parse ID *)
                                on_result (Error_types.Success "unknown")
                            else
                              let parsed_error =
                                parse_api_error_with_permissions
                                  ~required_permissions:["pages_manage_posts"]
                                  ~status_code:response.status
                                  ~response_body:response.body
                              in
                              cleanup_uploaded_photos photo_ids (fun () ->
                                if can_recover && is_recoverable_posting_auth_error parsed_error then
                                  recover_page_access_token ~account_id ~current_access_token:token
                                    (fun _resolved_page_id resolved_page_token ->
                                      attempt_post false resolved_page_token)
                                    (fun _ -> on_result (Error_types.Failure parsed_error))
                                else
                                  on_result (Error_types.Failure parsed_error)))
                          (fun err ->
                            let parsed_error = transport_error err in
                            cleanup_uploaded_photos photo_ids (fun () ->
                              on_result (Error_types.Failure parsed_error))))
                  in
                  attempt_post true page_access_token)
              (fun err -> on_result (Error_types.Failure err))
  
  (** Post thread (Facebook doesn't support threads, posts only first item with warning)
      
      @param validate_media_before_upload When true, validates media file size after 
             download but before upload. Facebook limits: 4MB images, 1GB video.
             Default: false
  *)
  let post_thread ~account_id ~texts ~media_urls_per_post ?(alt_texts_per_post=[]) ?(validate_media_before_upload=false) on_result =
    if List.length texts = 0 then
      on_result (Error_types.Failure (Error_types.Validation_error [Error_types.Thread_empty]))
    else
      let first_text = List.hd texts in
      let first_media = if List.length media_urls_per_post > 0 then List.hd media_urls_per_post else [] in
      let first_alt_texts = if List.length alt_texts_per_post > 0 then List.hd alt_texts_per_post else [] in
      let total_posts = List.length texts in
      
      post_single ~account_id ~text:first_text ~media_urls:first_media ~alt_texts:first_alt_texts
        ~validate_media_before_upload
        (fun outcome ->
          match outcome with
          | Error_types.Success post_id ->
              let thread_result = {
                Error_types.posted_ids = [post_id];
                failed_at_index = None;
                total_requested = total_posts;
              } in
              if total_posts > 1 then
                on_result (Error_types.Partial_success {
                  result = thread_result;
                  warnings = [Error_types.Enrichment_skipped 
                    "Facebook does not support threads - only the first post was published"];
                })
              else
                on_result (Error_types.Success thread_result)
          | Error_types.Partial_success { result = post_id; warnings } ->
              let thread_result = {
                Error_types.posted_ids = [post_id];
                failed_at_index = None;
                total_requested = total_posts;
              } in
              let all_warnings = 
                if total_posts > 1 then
                  Error_types.Enrichment_skipped 
                    "Facebook does not support threads - only the first post was published" :: warnings
                else warnings
              in
              on_result (Error_types.Partial_success { result = thread_result; warnings = all_warnings })
          | Error_types.Failure err ->
              on_result (Error_types.Failure err))
  
  (** OAuth authorization URL *)
  let get_oauth_url ~redirect_uri ~state ~scopes on_success on_error =
    let client_id = Config.get_env "FACEBOOK_APP_ID" |> Option.value ~default:"" in
    
    if client_id = "" then
      on_error "Facebook App ID not configured"
    else (
      (* Same URL as before: plain comma-separated scope, auth_type=rerequest. *)
      on_success
        (OAuth.authorize_url ~client_id ~redirect_uri ~state ~scopes
           ~auth_type:OAuth.Rerequest ())
    )
  
  (** Exchange OAuth code for a long-lived access token (~60 days).

      Internally performs two steps: exchanges the code for a short-lived token,
      then exchanges that for a long-lived token. Callers do not need to perform
      a separate long-lived token exchange. *)
  let exchange_code ~code ~redirect_uri on_success on_error =
    let client_id = Config.get_env "FACEBOOK_APP_ID" |> Option.value ~default:"" in
    let client_secret = Config.get_env "FACEBOOK_APP_SECRET" |> Option.value ~default:"" in
    
    if client_id = "" || client_secret = "" then
      on_error "Facebook OAuth credentials not configured"
    else
      OAuth_http.exchange_code ~client_id ~client_secret ~redirect_uri ~code
        (fun short_lived_creds ->
          OAuth_http.exchange_for_long_lived_token
            ~client_id
            ~client_secret
            ~short_lived_token:short_lived_creds.access_token
            (fun long_lived_creds -> on_success long_lived_creds)
            (fun err ->
              on_error (Printf.sprintf
                "Failed to exchange for long-lived token: %s" err)))
        on_error

  (** Exchange OAuth code and return pages with Page access tokens.

      This helper performs the full server-side onboarding flow:
      1) Exchange code for short-lived user token
      2) Exchange for long-lived user token
      3) Retrieve managed pages via /me/accounts
  *)
  let exchange_code_and_get_pages ~code ~redirect_uri on_success on_error =
    let client_id = Config.get_env "FACEBOOK_APP_ID" |> Option.value ~default:"" in
    let client_secret = Config.get_env "FACEBOOK_APP_SECRET" |> Option.value ~default:"" in
    if client_id = "" || client_secret = "" then
      on_error "Facebook OAuth credentials not configured"
    else
      OAuth_http.exchange_code ~client_id ~client_secret ~redirect_uri ~code
        (fun short_lived_creds ->
          let continue_with_user_creds user_creds =
            OAuth_http.get_user_pages ~user_access_token:user_creds.access_token
              (fun pages -> on_success (user_creds, pages))
              on_error
          in
          OAuth_http.exchange_for_long_lived_token
            ~client_id
            ~client_secret
            ~short_lived_token:short_lived_creds.access_token
            continue_with_user_creds
            (fun err ->
              on_error (Printf.sprintf
                "Failed to exchange for long-lived token before page discovery: %s" err)))
        on_error
  
  (** Validate content length *)
  let validate_content ~text =
    let len = String.length text in
    if len = 0 then
      Error "Text cannot be empty"
    else if len > 5000 then
      Error (Printf.sprintf "Facebook posts should be under 5000 characters for best engagement (current: %d)" len)
    else
      Ok ()

  (** {1 Insights / Analytics}

      Two paths to per-post analytics. Pick based on what data you need:

      - [get_page_posts] (recommended for new code): walks
        /{page_id}/posts with field expansion. The response carries
        per-post [insights], [comments_summary], [reactions_summary],
        and [shares] inline. Works for ALL post types — including
        text-only status posts, where Meta has blocked direct insights
        access since v2.4. One round-trip per pagination page covers
        N posts. Production-tested connectors (Airbyte, Mage) all use
        this form.

      - [get_post_analytics] / [get_post_analytics_canonical]: the
        legacy per-post path. Hits [GET /{post_id}/insights] for one
        post at a time. Returns
        [(#12) singular statuses API is deprecated] for status posts;
        works fine for posts with media. Retained for callers that
        only deal with media posts and prefer the simpler typed
        [post_analytics] result over parsing the page-feed response.

      Account-level analytics (page reach, follower deltas, etc.)
      lives in [get_account_analytics] / [get_account_analytics_canonical]
      — that path is unaffected by the singular-status deprecation. *)

  type insight_timeseries_point = {
    end_time : string option;
    value : int;
  }

  type account_analytics = {
    page_impressions_unique : insight_timeseries_point list;
    page_posts_impressions_unique : insight_timeseries_point list;
    page_post_engagements : insight_timeseries_point list;
    page_daily_follows : insight_timeseries_point list;
    page_video_views : insight_timeseries_point list;
  }

  type post_analytics = {
    post_impressions_unique : int option;
    post_reactions_by_type_total : (string * int) list;
    post_clicks : int option;
    post_clicks_by_type : (string * int) list;
  }

  let account_analytics_metrics =
    "page_impressions_unique,page_posts_impressions_unique,page_post_engagements,page_daily_follows,page_video_views"

  (* Metric set for per-post insights via [get_post_analytics]. We
     request [post_impressions_unique] (the legacy v24 / pre-v25 name
     for unique reach) rather than the documented v25 successor
     [post_total_media_view_unique], because the v25 name appears to
     be a silent-omit trigger on real Page responses — including it
     in the metric list drops the entire [insights] field from the
     reply with no error. The legacy name still works in v25 in 2026
     (Meta hasn't actually removed it; the public deprecation date
     for [post_impressions] — note: NOT [_unique] — is November 2025,
     and [post_impressions_unique] survives unchanged). When Meta
     ships a working v26 successor we'll add it back here.

     Note: this metric set is requested by [get_post_analytics] which
     hits [GET /{post_id}/insights]. That endpoint is blocked for
     status-type posts since v2.4 (returns
     [(#12) singular statuses API is deprecated]). For analytics that
     covers ALL post types, callers should prefer [get_page_posts],
     which expands the same insights inline on the parent edge — see
     the docstring on that function for details. *)
  let post_analytics_metrics =
    "post_impressions_unique,\
     post_reactions_by_type_total,post_clicks,post_clicks_by_type"

  let account_analytics_canonical_metric_keys =
    Analytics_normalization.canonical_metric_keys_of_provider_metrics
      ~provider:Analytics_normalization.Facebook
      [ "page_impressions_unique";
        "page_posts_impressions_unique";
        "page_post_engagements";
        "page_daily_follows";
        "page_video_views" ]

  let post_analytics_canonical_metric_keys =
    Analytics_normalization.canonical_metric_keys_of_provider_metrics
      ~provider:Analytics_normalization.Facebook
      [ "post_total_media_view_unique";
        "post_impressions_organic_unique";
        "post_reactions_by_type_total";
        "post_clicks";
        "post_clicks_by_type" ]

  let to_canonical_timeseries_points points =
    List.map
      (fun point -> Analytics_types.make_datapoint ?timestamp:point.end_time point.value)
      points

  let to_canonical_optional_point value_opt =
    match value_opt with
    | Some value -> [ Analytics_types.make_datapoint value ]
    | None -> []

  let to_canonical_assoc_total_points values =
    if values = [] then
      []
    else
      let total = List.fold_left (fun acc (_, value) -> acc + value) 0 values in
      [ Analytics_types.make_datapoint total ]

  let to_canonical_facebook_series ?time_range ~scope ~provider_metric points =
    match Analytics_normalization.facebook_metric_to_canonical provider_metric with
    | Some metric ->
        Some
          (Analytics_types.make_series
             ?time_range
             ~metric
             ~scope
             ~provider_metric
             points)
    | None -> None

  let to_canonical_account_analytics_series ?time_range account_analytics =
    [ ("page_impressions_unique",
       to_canonical_timeseries_points account_analytics.page_impressions_unique);
      ("page_posts_impressions_unique",
       to_canonical_timeseries_points account_analytics.page_posts_impressions_unique);
      ("page_post_engagements",
       to_canonical_timeseries_points account_analytics.page_post_engagements);
      ("page_daily_follows",
       to_canonical_timeseries_points account_analytics.page_daily_follows);
      ("page_video_views",
       to_canonical_timeseries_points account_analytics.page_video_views) ]
    |> List.filter_map (fun (provider_metric, points) ->
           to_canonical_facebook_series
             ?time_range
             ~scope:Analytics_types.Page
             ~provider_metric
             points)

  let to_canonical_post_analytics_series ?time_range post_analytics =
    (* Surface the unique-views value under the v25 name
       [post_total_media_view_unique] so canonical-series consumers see
       the current Meta naming. The record field is still called
       [post_impressions_unique] for backwards-compatible record
       construction; the two names refer to the same metric. *)
    [ ("post_total_media_view_unique",
       to_canonical_optional_point post_analytics.post_impressions_unique);
      ("post_reactions_by_type_total",
       to_canonical_assoc_total_points post_analytics.post_reactions_by_type_total);
      ("post_clicks",
       to_canonical_optional_point post_analytics.post_clicks);
      ("post_clicks_by_type",
       to_canonical_assoc_total_points post_analytics.post_clicks_by_type) ]
    |> List.filter_map (fun (provider_metric, points) ->
           to_canonical_facebook_series
             ?time_range
             ~scope:Analytics_types.Post
             ~provider_metric
             points)

  let int_of_json_safe json =
    try Some (Yojson.Basic.Util.to_int json)
    with _ ->
      try Some (int_of_float (Yojson.Basic.Util.to_float json))
      with _ -> None

  let int_assoc_of_json json =
    try
      json
      |> Yojson.Basic.Util.to_assoc
      |> List.filter_map (fun (name, value_json) ->
          match int_of_json_safe value_json with
          | Some value -> Some (name, value)
          | None -> None)
    with _ -> []

  let find_insight_by_name ~name insights =
    List.find_opt
      (fun metric_json ->
        try
          let metric_name =
            metric_json |> Yojson.Basic.Util.member "name" |> Yojson.Basic.Util.to_string
          in
          metric_name = name
        with _ -> false)
      insights

  let parse_timeseries_points metric_json =
    let open Yojson.Basic.Util in
    metric_json
    |> member "values"
    |> to_list
    |> List.map (fun value_json ->
           let value =
             match int_of_json_safe (value_json |> member "value") with
             | Some v -> v
             | None -> 0
           in
           {
             end_time = value_json |> member "end_time" |> to_string_option;
             value;
           })

  let parse_account_analytics json =
    let open Yojson.Basic.Util in
    let insights = json |> member "data" |> to_list in
    let metric_points metric_name =
      match find_insight_by_name ~name:metric_name insights with
      | Some metric_json -> parse_timeseries_points metric_json
      | None -> []
    in
    {
      page_impressions_unique = metric_points "page_impressions_unique";
      page_posts_impressions_unique = metric_points "page_posts_impressions_unique";
      page_post_engagements = metric_points "page_post_engagements";
      page_daily_follows = metric_points "page_daily_follows";
      page_video_views = metric_points "page_video_views";
    }

  let parse_post_analytics json =
    let open Yojson.Basic.Util in
    let insights = json |> member "data" |> to_list in
    let first_metric_value metric_name =
      match find_insight_by_name ~name:metric_name insights with
      | None -> None
      | Some metric_json ->
          (match metric_json |> member "values" |> to_list with
          | value_json :: _ -> Some (value_json |> member "value")
          | [] -> None)
    in
    (* Prefer the v25 name [post_total_media_view_unique]; fall back to
       the legacy name [post_impressions_unique] for older response
       payloads or callers requesting the legacy metric set. The two
       are aliases for the same number — Meta renamed it in v25 to
       reflect that it actually measures unique viewers. *)
    let unique_views =
      match first_metric_value "post_total_media_view_unique" with
      | Some json_value -> int_of_json_safe json_value
      | None ->
          (match first_metric_value "post_impressions_unique" with
          | Some json_value -> int_of_json_safe json_value
          | None -> None)
    in
    {
      post_impressions_unique = unique_views;
      post_reactions_by_type_total =
        (match first_metric_value "post_reactions_by_type_total" with
        | Some json_value -> int_assoc_of_json json_value
        | None -> []);
      post_clicks =
        (match first_metric_value "post_clicks" with
        | Some json_value -> int_of_json_safe json_value
        | None -> None);
      post_clicks_by_type =
        (match first_metric_value "post_clicks_by_type" with
        | Some json_value -> int_assoc_of_json json_value
        | None -> []);
    }

  (** Account analytics via page insights endpoint.

      Deprecated: [page_impressions_unique] and
      [page_posts_impressions_unique] are on Meta's list of Page metrics
      that return an invalid-metric error on every API version
      ({!Insights}). New code should use {!get_page_insights}, which
      requests the v25 metrics ({!Insights.page_metrics}). This function
      keeps its metric set so its record keeps its meaning.

      Contract:
      GET https://graph.facebook.com/v25.0/{id}/insights
          ?metric=page_impressions_unique,page_posts_impressions_unique,page_post_engagements,page_daily_follows,page_video_views
          &period=day
          &since=...
          &until=...
          &access_token=...
          [&appsecret_proof=...]
      Authorization: Bearer <token>
  *)
  let get_account_analytics ~id ~since ~until ~access_token on_result =
    (* The query is percent-encoded, the token also travels as a Bearer
       header and, when FACEBOOK_APP_SECRET is set, with appsecret_proof
       ({!get_url}). The token stays in the query because Page insights
       have been seen to need it there even with the header. *)
    let query =
      Uri.encoded_of_query
        [ ("metric", [ account_analytics_metrics ]);
          ("period", [ "day" ]);
          ("since", [ since ]);
          ("until", [ until ]);
          ("access_token", [ access_token ]) ]
    in
    let url = Printf.sprintf "%s/%s/insights?%s" graph_api_base id query in
    get_url ~url ~access_token ~required_permissions:[ "pages_read_engagement" ]
      (function
        | Error err -> on_result (Error err)
        | Ok response ->
            on_result
              (try Ok (parse_account_analytics (Yojson.Basic.from_string response.body))
               with e ->
                 Error
                   (Error_types.Internal_error
                      (Printf.sprintf "Failed to parse account analytics response: %s"
                         (Printexc.to_string e)))))

  let get_account_analytics_canonical ~id ~since ~until ~access_token on_result =
    let time_range =
      {
        Analytics_types.since = Some since;
        until_ = Some until;
        granularity = Some Analytics_types.Day;
      }
    in
    get_account_analytics ~id ~since ~until ~access_token
      (function
        | Ok analytics ->
            on_result
              (Ok
                 (to_canonical_account_analytics_series
                    ~time_range
                    analytics))
        | Error err -> on_result (Error err))

  (** Post analytics via per-post insights endpoint. Legacy path —
      see [get_page_posts] for the recommended page-feed-expansion
      flow.

      ⚠️ This endpoint returns [(#12) singular statuses API is
      deprecated for versions v2.4 and higher] for text-only status
      posts (no attachments). Meta retired the singular-status node
      in April 2019 and never restored it. Calls of this form work
      for posts with media (photo, video, link, share) but silently
      fail for status posts.

      For analytics that covers ALL post types, prefer
      [get_page_posts]: it traverses /{page_id}/posts with field
      expansion, which Meta still serves because the request is
      rooted at the Page node. The expanded response carries
      per-post [insights] (same metric set), [comments_summary],
      [reactions_summary], and [shares] inline. Production-tested
      connectors (Airbyte, Mage) use that path for the same reason.

      This function is retained for callers that only deal with
      media posts and want the simpler per-post API. New code should
      prefer [get_page_posts] unless there's a specific reason not to.

      Deprecated: it requests [post_impressions_unique], which Meta lists
      as deprecated (alternative [post_total_media_view_unique]). New code
      should use {!get_post_insights}.

      Contract:
      GET https://graph.facebook.com/v25.0/{post_id}/insights
          ?metric=post_impressions_unique,post_reactions_by_type_total,post_clicks,post_clicks_by_type
          &access_token=...
          [&appsecret_proof=...]
      Authorization: Bearer <token>
  *)
  let get_post_analytics ~post_id ~access_token on_result =
    let query =
      Uri.encoded_of_query
        [ ("metric", [ post_analytics_metrics ]);
          ("access_token", [ access_token ]) ]
    in
    let url = Printf.sprintf "%s/%s/insights?%s" graph_api_base post_id query in
    get_url ~url ~access_token ~required_permissions:[ "pages_read_engagement" ]
      (function
        | Error err -> on_result (Error err)
        | Ok response ->
            on_result
              (try Ok (parse_post_analytics (Yojson.Basic.from_string response.body))
               with e ->
                 Error
                   (Error_types.Internal_error
                      (Printf.sprintf "Failed to parse post analytics response: %s"
                         (Printexc.to_string e)))))

  (** Canonical-series wrapper around [get_post_analytics]. Same
      caveats apply: this is the legacy per-post path that fails
      with (#12) for status posts. Use [get_page_posts] when you
      need analytics for arbitrary post types — see that function's
      docstring for the recommended flow. *)
  let get_post_analytics_canonical ~post_id ~access_token on_result =
    get_post_analytics ~post_id ~access_token
      (function
        | Ok analytics ->
            on_result (Ok (to_canonical_post_analytics_series analytics))
        | Error err -> on_result (Error err))
  
  (** {1 Typed insights (v25 metrics)}

      [GET /{object-id}/insights] and [GET /{video-id}/video_insights]
      with a caller-chosen metric list, parsed into {!Insights.metric}.
      The token travels as a Bearer header and in the query (Page
      insights have been seen to need it there), with [appsecret_proof]
      when FACEBOOK_APP_SECRET is set; every value is percent-encoded.
      One invalid metric fails the whole call, so check custom lists
      against {!Insights.is_deprecated_metric}.
      {{:https://developers.facebook.com/docs/graph-api/reference/insights}} *)

  let insights_read_permissions = [ "read_insights"; "pages_read_engagement" ]

  let get_insights_edge ?(required_permissions = insights_read_permissions) ~path ~access_token ~metrics ?period ?since ?until on_result =
    let optional key = function Some value -> [ (key, [ value ]) ] | None -> [] in
    let query =
      Uri.encoded_of_query
        ([ ("metric", [ String.concat "," metrics ]) ]
        @ optional "period" (Option.map Insights.period_to_string period)
        @ optional "since" since @ optional "until" until
        @ [ ("access_token", [ access_token ]) ])
    in
    let url = Printf.sprintf "%s/%s?%s" graph_api_base path query in
    get_url ~url ~access_token ~required_permissions (function
      | Error err -> on_result (Error err)
      | Ok response -> (
          match Insights.parse response.body with
          | Ok metrics -> on_result (Ok metrics)
          | Error msg ->
              on_result
                (Error
                   (Error_types.Internal_error
                      (Printf.sprintf "Failed to parse insights response: %s" msg)))))

  (** Page insights, [period=day] and {!Insights.page_metrics} by default.
      [since] / [until] are Unix timestamps or dates; one window spans at
      most 90 days. [page_follows] is the follower total per day; the
      daily gain is [page_daily_follows_unique]. *)
  let get_page_insights ~page_id ~access_token ?(metrics = Insights.page_metrics)
      ?(period = Insights.Day) ?since ?until on_result =
    get_insights_edge
      ~path:(Uri.pct_encode page_id ^ "/insights")
      ~access_token ~metrics ~period ?since ?until on_result

  (** {!get_page_insights} for the account's Page, with its stored token. *)
  let get_page_insights_for_account ~account_id ?metrics ?period ?since ?until on_result =
    with_posting_page_context ~account_id
      (fun page_id access_token ->
        get_page_insights ~page_id ~access_token ?metrics ?period ?since ?until on_result)
      (fun err -> on_result (Error err))

  (** Post insights, {!Insights.post_metrics} by default (lifetime
      values). Meta answers [(#12) singular statuses API is deprecated] for
      text-only status posts; read those through {!get_page_posts}. *)
  let get_post_insights ~post_id ~access_token ?(metrics = Insights.post_metrics) ?period
      on_result =
    get_insights_edge
      ~path:(Uri.pct_encode post_id ^ "/insights")
      ~access_token ~metrics ?period on_result

  (** {!get_post_insights} with the account's stored Page token. *)
  let get_post_insights_for_account ~account_id ~post_id ?metrics ?period on_result =
    ensure_valid_token ~account_id
      (fun access_token -> get_post_insights ~post_id ~access_token ?metrics ?period on_result)
      (fun err -> on_result (Error err))

  (** Video and Reels insights, [GET /{video_id}/video_insights]:
      {!Insights.reels_metrics} and [period=lifetime] by default; pass
      {!Insights.video_metrics} for a regular video. A Reel posted to a
      Page appears in its feed with the video id at
      [attachments.data[0].target.id].
      {{:https://developers.facebook.com/docs/graph-api/reference/video/video_insights}} *)
  let get_video_insights ~video_id ~access_token ?(metrics = Insights.reels_metrics)
      ?(period = Insights.Lifetime) on_result =
    get_insights_edge ~required_permissions:[ "read_insights"; "pages_manage_engagement" ]
      ~path:(Uri.pct_encode video_id ^ "/video_insights")
      ~access_token ~metrics ~period on_result

  (** {!get_video_insights} with the account's stored Page token. *)
  let get_video_insights_for_account ~account_id ~video_id ?metrics ?period on_result =
    ensure_valid_token ~account_id
      (fun access_token -> get_video_insights ~video_id ~access_token ?metrics ?period on_result)
      (fun err -> on_result (Error err))

  (** {1 Generic API Methods} *)
  
  (** Generic GET request to any Graph API endpoint.

      Pass [?query] for arbitrary query-string params (e.g.
      [metric], [period], [since], [until] on insights endpoints).
      [?fields] is a convenience shortcut for the common [fields=...]
      pattern; [?query] is the escape hatch for everything else. *)
  let get ~path ~access_token ?fields ?(query=[]) ?required_permissions on_result =
    get_paginated ~path ~access_token ?fields ~query ?cursor:None
      (function
        | Ok response -> on_result (Ok response)
        | Error (Error_types.Auth_error (Error_types.Insufficient_permissions { platform_message; _ })) ->
            let required = match required_permissions with
              | Some perms -> perms
              | None -> permissions_for_path path
            in
            on_result (Error (Error_types.Auth_error (Error_types.Insufficient_permissions {
              required;
              platform_message;
            })))
        | Error e -> on_result (Error e))
  
  (** Permalink of a post (or any object with [permalink_url]) by id:
      [GET /{post_id}?fields=permalink_url], with [appsecret_proof] when an
      app secret is configured. [Ok None] when Meta returns no permalink. *)
  let get_permalink ~post_id ~access_token on_result =
    get ~path:post_id ~access_token ~fields:[ "permalink_url" ]
      ~required_permissions:[ "pages_read_engagement" ]
      (function
        | Error err -> on_result (Error err)
        | Ok response -> (
            match parse_permalink_response response.body with
            | Ok permalink -> on_result (Ok permalink)
            | Error msg ->
                on_result
                  (Error
                     (Error_types.Internal_error
                        (Printf.sprintf "Failed to parse permalink response: %s"
                           msg)))))

  (** {!get_permalink} with the account's stored Page token. *)
  let get_permalink_for_account ~account_id ~post_id on_result =
    ensure_valid_token ~account_id
      (fun access_token -> get_permalink ~post_id ~access_token on_result)
      (fun err -> on_result (Error err))

  (** Get a page of results from a collection endpoint *)
  let get_page ~path ~access_token ?fields ?cursor (parse_data : Yojson.Basic.t -> 'a list) on_result =
    get_paginated ~path ~access_token ?fields ?cursor
      (function
        | Error e -> on_result (Error e)
        | Ok response ->
            try
              let json = Yojson.Basic.from_string response.body in
              let open Yojson.Basic.Util in
              let data = json |> member "data" |> parse_data in
              let paging, next_url, previous_url = parse_paging json in
              on_result (Ok {
                data;
                paging;
                next_url;
                previous_url;
              })
            with e ->
              on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse response: %s" (Printexc.to_string e)))))
  
  (** Fetch next page using cursor *)
  let get_next_page ~path ~access_token ?fields ~cursor parse_data on_result =
    get_page ~path ~access_token ?fields ~cursor parse_data on_result
  
  (** Generic POST request to any Graph API endpoint *)
  let post ~path ~access_token ~params ?required_permissions on_result =
    let url = Printf.sprintf "%s/%s" graph_api_base path in

    let all_params = params @
      (match compute_app_secret_proof ~access_token with
       | Some proof -> [("appsecret_proof", proof)]
       | None -> [])
    in

    let body = Social_core.form_urlencode_kvs all_params in
    let headers = [
      ("Content-Type", "application/x-www-form-urlencoded");
      ("Authorization", Printf.sprintf "Bearer %s" access_token);
    ] in
    
    Config.Http.post ~headers ~body url
      (fun response ->
        update_rate_limits response;
        if response.status >= 200 && response.status < 300 then
          on_result (Ok response)
        else
          let required_permissions =
            match required_permissions with
            | Some p -> p
            | None -> permissions_for_path path
          in
          on_result (Error (parse_api_error_with_permissions
            ~required_permissions
            ~status_code:response.status
            ~response_body:response.body)))
      (fun err -> on_result (Error (transport_error err)))
  
  (** Generic DELETE request to any Graph API endpoint *)
  let delete ~path ~access_token ?required_permissions on_result =
    let proof_params = match compute_app_secret_proof ~access_token with
      | Some proof -> [("appsecret_proof", [proof])]
      | None -> []
    in
    
    let url = Printf.sprintf "%s/%s" graph_api_base path in
    let final_url = 
      if List.length proof_params > 0 then
        url ^ "?" ^ Uri.encoded_of_query proof_params
      else url
    in
    
    let headers = [
      ("Authorization", Printf.sprintf "Bearer %s" access_token);
    ] in
    
    Config.Http.delete ~headers final_url
      (fun response ->
        update_rate_limits response;
        if response.status >= 200 && response.status < 300 then
          on_result (Ok response)
        else
          let required_permissions =
            match required_permissions with
            | Some p -> p
            | None -> permissions_for_path path
          in
          on_result (Error (parse_api_error_with_permissions
            ~required_permissions
            ~status_code:response.status
            ~response_body:response.body)))
      (fun err -> on_result (Error (transport_error err)))
  
  (** {1 Batch Requests} *)
  
  (** Batch request item *)
  type batch_request_item = {
    method_ : [`GET | `POST | `DELETE];
    relative_url : string;
    body : string option;
    name : string option;  (* For referencing in dependent requests *)
  }
  
  (** Batch response item *)
  type batch_response_item = {
    code : int;
    headers : (string * string) list;
    body : string;
  }
  
  (** Execute batch requests (up to 50 per batch) *)
  let batch_request ~requests ~access_token ?required_permissions on_result =
    if List.length requests = 0 then
      on_result (Error (Error_types.Internal_error "Batch request list cannot be empty"))
    else if List.length requests > 50 then
      on_result (Error (Error_types.Internal_error "Facebook allows maximum 50 requests per batch"))
    else
      let url = Printf.sprintf "%s/" graph_api_base in
      
      (* Build batch JSON *)
      let batch_items = List.map (fun req ->
        let method_str = match req.method_ with
          | `GET -> "GET"
          | `POST -> "POST"
          | `DELETE -> "DELETE"
        in
        let fields = [
          ("method", `String method_str);
          ("relative_url", `String req.relative_url);
        ] in
        let fields = match req.body with
          | Some body -> fields @ [("body", `String body)]
          | None -> fields
        in
        let fields = match req.name with
          | Some name -> fields @ [("name", `String name)]
          | None -> fields
        in
        `Assoc fields
      ) requests in
      
      let batch_json = Yojson.Basic.to_string (`List batch_items) in
      
      let params = [
        ("batch", batch_json);
      ] @
      (match compute_app_secret_proof ~access_token with
       | Some proof -> [("appsecret_proof", proof)]
       | None -> [])
      in

      let body = Social_core.form_urlencode_kvs params in
      let headers = [
        ("Content-Type", "application/x-www-form-urlencoded");
        ("Authorization", Printf.sprintf "Bearer %s" access_token);
      ] in
      
      Config.Http.post ~headers ~body url
        (fun response ->
          update_rate_limits response;
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              let open Yojson.Basic.Util in
              let results = json |> to_list |> List.map (fun item ->
                let code = item |> member "code" |> to_int in
                let headers_json = item |> member "headers" |> to_list in
                let headers = List.filter_map (fun h ->
                  try
                    let name = h |> member "name" |> to_string in
                    let value = h |> member "value" |> to_string in
                    Some (name, value)
                  with _ -> None
                ) headers_json in
                let body = item |> member "body" |> to_string in
                { code; headers; body }
              ) in
              on_result (Ok results)
            with e ->
              on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse batch response: %s" (Printexc.to_string e))))
          else
            let required_permissions =
              match required_permissions with
              | Some p -> p
              | None -> provider_required_permissions
            in
            on_result (Error (parse_api_error_with_permissions
              ~required_permissions
              ~status_code:response.status
              ~response_body:response.body)))
        (fun err -> on_result (Error (transport_error err)))

  (** {1 Comment Management} *)

  (** Comment on a post *)
  type comment = {
    comment_id : string;
    comment_message : string;
    comment_created_time : string option;
    comment_from : string option;
  }

  (** Post a comment on a post

      @param post_id The ID of the post to comment on
      @param access_token Page access token
      @param message The comment text
      @param on_result Continuation receiving Ok with new comment ID or Error
  *)
  let post_comment ~post_id ~access_token ~message on_result =
    let path = Printf.sprintf "%s/comments" post_id in
    let params = [("message", message)] in
    post ~path ~access_token ~params
      (function
        | Ok response ->
            (try
              let json = Yojson.Basic.from_string response.body in
              let open Yojson.Basic.Util in
              let comment_id = json |> member "id" |> to_string in
              on_result (Ok comment_id)
            with e ->
              on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse comment response: %s" (Printexc.to_string e)))))
        | Error e -> on_result (Error e))

  (** Get comments on a post

      @param post_id The ID of the post to read comments from
      @param access_token Page access token
      @param on_result Continuation receiving Ok with list of comments or Error
  *)
  let get_comments ~post_id ~access_token on_result =
    let path = Printf.sprintf "%s/comments" post_id in
    get ~path ~access_token ~fields:["id"; "message"; "created_time"; "from"]
      (function
        | Ok response ->
            (try
              let json = Yojson.Basic.from_string response.body in
              let open Yojson.Basic.Util in
              let data = json |> member "data" |> to_list in
              let comments = List.map (fun item ->
                {
                  comment_id = item |> member "id" |> to_string;
                  comment_message = item |> member "message" |> to_string_option |> Option.value ~default:"";
                  comment_created_time = item |> member "created_time" |> to_string_option;
                  comment_from =
                    (try Some (item |> member "from" |> member "name" |> to_string)
                     with _ -> None);
                }
              ) data in
              on_result (Ok comments)
            with e ->
              on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse comments response: %s" (Printexc.to_string e)))))
        | Error e -> on_result (Error e))

  (* Reading Page post comments needs pages_read_engagement, and other
     people's comments pages_read_user_content; replying, liking, hiding and
     deleting need pages_manage_engagement. *)
  let comment_read_permissions = [ "pages_read_engagement"; "pages_read_user_content" ]
  let comment_manage_permissions = [ "pages_manage_engagement" ]

  let invalid_input message =
    Error_types.Validation_error [ Error_types.Invalid_url message ]

  let parse_failure what msg =
    Error_types.Internal_error (Printf.sprintf "Failed to parse %s response: %s" what msg)

  (* URL of one page of [/{object_id}/comments]: a checked [Page_url] as Meta
     returned it (credentials are added by [get_url]), or a request rebuilt
     from the id and options. *)
  let comments_page_url ~object_id ?filter ?order ?limit position =
    match (position : Social_meta_graph_paging.position) with
    | Page_url url ->
        Social_meta_graph_paging.resolve_page_url ~allowed_hosts:paging_url_hosts
          ~edge:"comments" url
        |> Result.map Uri.to_string
        |> Result.map_error invalid_input
    | First | After _ | Before _ ->
        if String.trim object_id = "" then
          Error (invalid_input "object_id must not be empty")
        else
          let params =
            [ ("fields", [ Comments.fields ]) ]
            @ (match filter with
               | Some f -> [ ("filter", [ Comments.string_of_filter f ]) ]
               | None -> [])
            @ (match order with
               | Some o -> [ ("order", [ Comments.string_of_order o ]) ]
               | None -> [])
            @ (match limit with
               | Some n when n > 0 -> [ ("limit", [ string_of_int n ]) ]
               | Some _ | None -> [])
            @ Social_meta_graph_paging.cursor_query position
          in
          Ok
            (Printf.sprintf "%s/%s/comments?%s" graph_api_base
               (Uri.pct_encode (String.trim object_id))
               (Uri.encoded_of_query params))

  (** One page of comments on a Page post, photo, video or comment
      ([GET /{object_id}/comments], fields {!Comments.fields}), with a Page
      access token and [appsecret_proof] when [FACEBOOK_APP_SECRET] is set.

      - [filter]: [Stream] lists every level as one flat list in which
        [parent_id] places replies; omitted, the API default ([Toplevel]).
      - [order]: omitted, the API default (chronological).
      - [position]: a cursor or a [paging.next] URL on [graph.facebook.com]
        (see {!Social_meta_graph_paging}); with [Page_url] the other options come
        from the URL.

      Needs [pages_read_engagement] and [pages_read_user_content]; [from]
      can be missing (see {!Comments}).

      @see <https://developers.facebook.com/docs/graph-api/reference/object/comments/>
      @see <https://developers.facebook.com/docs/graph-api/results> *)
  let get_comments_page ~object_id ~access_token ?filter ?order
      ?(position = Social_meta_graph_paging.First) ?limit on_result =
    match comments_page_url ~object_id ?filter ?order ?limit position with
    | Error err -> on_result (Error err)
    | Ok url ->
        get_url ~url ~access_token ~required_permissions:comment_read_permissions
          (function
            | Error err -> on_result (Error err)
            | Ok response -> (
                match
                  Social_meta_graph_paging.of_body ~item:Comments.parse_comment response.body
                with
                | Ok page -> on_result (Ok page)
                | Error msg -> on_result (Error (parse_failure "comments" msg))))

  (** {!get_comments_page} with the account's stored Page token. *)
  let get_comments_page_for_account ~account_id ~object_id ?filter ?order
      ?position ?limit on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        get_comments_page ~object_id ~access_token ?filter ?order ?position
          ?limit on_result)
      (fun err -> on_result (Error err))

  (** Pages that can serve as places: [GET /pages/search?q=..&fields=]
      {!Places.fields}, with the token as a Bearer header and
      [appsecret_proof] when [FACEBOOK_APP_SECRET] is set. Pages without a
      location are returned too ({!Places.coordinates} is [None]); Meta
      rejects such a [place] on a post. A blank [query] is a
      [Validation_error [Text_empty]] and sends no request.

      Takes a User or Page access token with the app secret, or an App
      token with Page Public Metadata Access (see {!Places}).

      @see <https://developers.facebook.com/docs/pages-api/search-pages> *)
  let search_places ~access_token ~query on_result =
    let query = String.trim query in
    if query = "" then on_result (Error (Error_types.Validation_error [ Error_types.Text_empty ]))
    else
      let url =
        Printf.sprintf "%s/pages/search?%s" graph_api_base
          (Uri.encoded_of_query [ ("q", [ query ]); ("fields", [ Places.fields ]) ])
      in
      get_url ~url ~access_token ~required_permissions:[]
        (function
          | Error err -> on_result (Error err)
          | Ok response -> (
              match Places.parse_response response.body with
              | Ok places -> on_result (Ok places)
              | Error msg -> on_result (Error (parse_failure "place search" msg))))

  (* Page feed reads need pages_read_engagement and pages_read_user_content. *)
  let page_posts_read_permissions = [ "pages_read_engagement"; "pages_read_user_content" ]

  (* URL of one page of a Page feed edge: a checked [Page_url] as Meta
     returned it (credentials are added by [get_url]), or a request rebuilt
     from the id and options. *)
  let page_posts_page_url ~page_id ~edge ~fields ?limit position =
    let edge_name = Page_posts.string_of_edge edge in
    match (position : Social_meta_graph_paging.position) with
    | Page_url url ->
        Social_meta_graph_paging.resolve_page_url ~allowed_hosts:paging_url_hosts
          ~edge:edge_name url
        |> Result.map Uri.to_string
        |> Result.map_error invalid_input
    | First | After _ | Before _ ->
        if String.trim page_id = "" then Error (invalid_input "page_id must not be empty")
        else
          let params =
            [ ("fields", [ fields ]) ]
            @ (match limit with
               | Some n when n > 0 -> [ ("limit", [ string_of_int (min n 100) ]) ]
               | Some _ | None -> [])
            @ Social_meta_graph_paging.cursor_query position
          in
          Ok
            (Printf.sprintf "%s/%s/%s?%s" graph_api_base
               (Uri.pct_encode (String.trim page_id))
               edge_name (Uri.encoded_of_query params))

  (** One page of a Page's posts, typed ({!Page_posts.page_post}), with a
      Page access token and [appsecret_proof] when [FACEBOOK_APP_SECRET] is
      set.

      - [edge]: [Posts] (default), [Feed] or [Published_posts]; see
        {!Page_posts} for what each lists.
      - [fields]: {!Page_posts.basic_fields} (default) or
        {!Page_posts.analytics_fields}; the parser reads the aliases the
        latter sets.
      - [limit]: clamped to 100, the documented maximum.
      - [position]: a cursor or a [paging.next] URL on [graph.facebook.com]
        for the same edge (see {!Social_meta_graph_paging}); with
        [Page_url] the other options come from the URL.

      Needs [pages_read_engagement] and [pages_read_user_content].

      @see <https://developers.facebook.com/docs/graph-api/reference/page/feed/> *)
  let get_page_posts_page ~page_id ~access_token ?(edge = Page_posts.Posts)
      ?(fields = Page_posts.basic_fields) ?(position = Social_meta_graph_paging.First) ?limit
      on_result =
    match page_posts_page_url ~page_id ~edge ~fields ?limit position with
    | Error err -> on_result (Error err)
    | Ok url ->
        get_url ~url ~access_token ~required_permissions:page_posts_read_permissions
          (function
            | Error err -> on_result (Error err)
            | Ok response -> (
                match
                  Social_meta_graph_paging.of_body ~item:Page_posts.parse_page_post response.body
                with
                | Ok page -> on_result (Ok page)
                | Error msg -> on_result (Error (parse_failure "page posts" msg))))

  (** {!get_page_posts_page} for the account's Page ([Config.get_page_id])
      with its stored Page token. *)
  let get_page_posts_page_for_account ~account_id ?edge ?fields ?position ?limit on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        Config.get_page_id ~account_id
          (fun page_id ->
            get_page_posts_page ~page_id ~access_token ?edge ?fields ?position ?limit on_result)
          (fun err -> on_result (Error (Error_types.Internal_error err))))
      (fun err -> on_result (Error err))

  (* Graph answers moderation calls with {"success": true|false}. *)
  let expect_success ~what (response : Social_core.response) =
    match Yojson.Basic.from_string response.body with
    | `Assoc fields -> (
        match List.assoc_opt "success" fields with
        | Some (`Bool true) -> Ok ()
        | Some (`Bool false) ->
            Error
              (Error_types.Api_error
                 { status_code = response.status;
                   message = Printf.sprintf "Facebook did not %s" what;
                   platform = Platform_types.FacebookPage;
                   raw_response = Some response.body;
                   request_id = None })
        | Some _ | None -> Error (parse_failure what "no success flag"))
    | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ ->
        Error (parse_failure what "expected a JSON object")
    | exception Yojson.Json_error msg -> Error (parse_failure what msg)

  let require_id ~name id on_result k =
    if String.trim id = "" then
      on_result (Error (invalid_input (name ^ " must not be empty")))
    else k (Uri.pct_encode (String.trim id))

  (** Reply to a comment as the Page ([POST /{comment_id}/comments] with
      [message]); returns the new comment's id. Facebook shows comments two
      levels deep, so reply to a top-level comment (see {!Comments}). Needs
      [pages_manage_engagement].

      @see <https://developers.facebook.com/docs/graph-api/reference/object/comments/> *)
  let reply_to_comment ~comment_id ~access_token ~message on_result =
    require_id ~name:"comment_id" comment_id on_result (fun comment_id ->
        post ~path:(comment_id ^ "/comments") ~access_token
          ~params:[ ("message", message) ]
          ~required_permissions:comment_manage_permissions
          (function
            | Error err -> on_result (Error err)
            | Ok response -> (
                match Yojson.Basic.from_string response.body with
                | `Assoc fields -> (
                    match List.assoc_opt "id" fields with
                    | Some (`String id) -> on_result (Ok id)
                    | Some _ | None -> on_result (Error (parse_failure "reply" "no id")))
                | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ ->
                    on_result (Error (parse_failure "reply" "expected a JSON object"))
                | exception Yojson.Json_error msg ->
                    on_result (Error (parse_failure "reply" msg)))))

  (** Hide ([hidden:true]) or unhide a comment on a Page's post
      ([POST /{comment_id}] with [is_hidden]; Page comments only). Needs
      [pages_manage_engagement].

      @see <https://developers.facebook.com/docs/graph-api/reference/comment/> *)
  let set_comment_hidden ~comment_id ~access_token ~hidden on_result =
    require_id ~name:"comment_id" comment_id on_result (fun comment_id ->
        post ~path:comment_id ~access_token
          ~params:[ ("is_hidden", string_of_bool hidden) ]
          ~required_permissions:comment_manage_permissions
          (function
            | Error err -> on_result (Error err)
            | Ok response ->
                on_result
                  (expect_success
                     ~what:(if hidden then "hide the comment" else "unhide the comment")
                     response)))

  (** Delete a comment ([DELETE /{comment_id}]). Needs
      [pages_manage_engagement].

      @see <https://developers.facebook.com/docs/graph-api/reference/comment/> *)
  let delete_comment ~comment_id ~access_token on_result =
    require_id ~name:"comment_id" comment_id on_result (fun comment_id ->
        delete ~path:comment_id ~access_token
          ~required_permissions:comment_manage_permissions
          (function
            | Error err -> on_result (Error err)
            | Ok response ->
                on_result (expect_success ~what:"delete the comment" response)))

  (** Like a comment or Page post as the Page ([POST /{object_id}/likes]).
      The Page must not already like it. Needs [pages_manage_engagement].

      @see <https://developers.facebook.com/docs/graph-api/reference/object/likes/> *)
  let like_object ~object_id ~access_token on_result =
    require_id ~name:"object_id" object_id on_result (fun object_id ->
        post ~path:(object_id ^ "/likes") ~access_token ~params:[]
          ~required_permissions:comment_manage_permissions
          (function
            | Error err -> on_result (Error err)
            | Ok response -> on_result (expect_success ~what:"like the object" response)))

  (** Remove the Page's like ([DELETE /{object_id}/likes]). Needs
      [pages_manage_engagement].

      @see <https://developers.facebook.com/docs/graph-api/reference/object/likes/> *)
  let unlike_object ~object_id ~access_token on_result =
    require_id ~name:"object_id" object_id on_result (fun object_id ->
        delete ~path:(object_id ^ "/likes") ~access_token
          ~required_permissions:comment_manage_permissions
          (function
            | Error err -> on_result (Error err)
            | Ok response -> on_result (expect_success ~what:"unlike the object" response)))

  (** {1 Page Info} *)

  (** Page detail information *)
  type page_detail = {
    page_detail_id : string;
    page_detail_name : string option;
    page_detail_about : string option;
    page_detail_fan_count : int option;
    page_detail_followers_count : int option;
  }

  (** Fetch page info

      @param page_id The Facebook Page ID
      @param access_token Page access token
      @param on_result Continuation receiving Ok with page detail or Error
  *)
  let get_page_info ~page_id ~access_token on_result =
    get ~path:page_id ~access_token ~fields:["id"; "name"; "about"; "fan_count"; "followers_count"]
      (function
        | Ok response ->
            (try
              let json = Yojson.Basic.from_string response.body in
              let open Yojson.Basic.Util in
              let detail = {
                page_detail_id = json |> member "id" |> to_string;
                page_detail_name = json |> member "name" |> to_string_option;
                page_detail_about = json |> member "about" |> to_string_option;
                page_detail_fan_count = json |> member "fan_count" |> to_int_option;
                page_detail_followers_count = json |> member "followers_count" |> to_int_option;
              } in
              on_result (Ok detail)
            with e ->
              on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse page info response: %s" (Printexc.to_string e)))))
        | Error e -> on_result (Error e))
end
