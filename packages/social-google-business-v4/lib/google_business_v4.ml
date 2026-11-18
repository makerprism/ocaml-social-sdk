(** Google Business Profile API v4 Provider

    This implementation supports posting updates, events, and offers
    to Google Business Profile locations.

    - Google OAuth 2.0 with PKCE (same infrastructure as YouTube)
    - Access tokens expire after 1 hour
    - Refresh tokens don't expire (unless revoked)
    - Uses My Business v4 API for posting (localPosts)
    - Uses My Business Account Management v1 for account/location discovery
*)

open Social_core

(** OAuth 2.0 module for Google Business Profile

    Uses the same Google OAuth 2.0 infrastructure as YouTube.

    Required environment variables (or pass directly to functions):
    - GOOGLE_BUSINESS_CLIENT_ID: OAuth 2.0 Client ID from Google Cloud Console
    - GOOGLE_BUSINESS_CLIENT_SECRET: OAuth 2.0 Client Secret
    - GOOGLE_BUSINESS_REDIRECT_URI: Registered callback URL
*)
module OAuth = struct
  (** Scope definitions for Google Business Profile API *)
  module Scopes = struct
    (** Scope for managing business profile *)
    let business_manage = [
      "https://www.googleapis.com/auth/business.manage";
    ]

    (** Scope for user profile info *)
    let userinfo = [
      "https://www.googleapis.com/auth/userinfo.profile";
      "https://www.googleapis.com/auth/userinfo.email";
    ]

    (** All scopes needed for full Google Business Profile management *)
    let all =
      business_manage @ userinfo

    (** Operations that can be performed with Google Business Profile API *)
    type operation =
      | Post
      | List_locations

    (** Get scopes required for specific operations *)
    let for_operations ops =
      let needs_post = List.exists (fun o -> o = Post) ops in
      let needs_list = List.exists (fun o -> o = List_locations) ops in
      (if needs_post || needs_list then business_manage else []) @
      userinfo
  end

  (** Platform metadata for Google Business Profile OAuth *)
  module Metadata = struct
    (** Google supports PKCE with S256 method *)
    let supports_pkce = true

    (** Google supports token refresh *)
    let supports_refresh = true

    (** Access tokens expire after 1 hour *)
    let access_token_seconds = Some 3600

    (** Refresh tokens never expire (unless revoked) *)
    let refresh_token_seconds = None

    (** Recommended buffer before expiry (10 minutes) *)
    let refresh_buffer_seconds = 600

    (** Maximum retry attempts for token operations *)
    let max_refresh_attempts = 5

    (** My Business Account Management API v1 base URL *)
    let account_management_base = "https://mybusinessaccountmanagement.googleapis.com/v1"

    (** My Business Business Information API v1 base URL *)
    let business_information_base = "https://mybusinessbusinessinformation.googleapis.com/v1"

    (** My Business API v4 base URL (for localPosts) *)
    let mybusiness_v4_base = "https://mybusiness.googleapis.com/v4"
  end

  module Pkce = Social_google_oauth.Pkce

  (** Generate authorization URL for Google Business Profile OAuth 2.0 flow with PKCE *)
  let get_authorization_url ~client_id ~redirect_uri ~state ~code_verifier ?(scopes=Scopes.all) () =
    Social_google_oauth.get_authorization_url ~client_id ~redirect_uri ~state ~code_verifier ~scopes

  (** {2 Token-based discovery types} *)

  (** [type] of a Business Profile account
      ({{:https://developers.google.com/my-business/reference/accountmanagement/rest/v1/accounts}}). *)
  type account_type =
    | Personal
    | Location_group
    | User_group
    | Organization
    | Unknown_account_type of string  (** Includes [ACCOUNT_TYPE_UNSPECIFIED]. *)

  (** The caller's [role] on an account. *)
  type account_role =
    | Primary_owner
    | Owner
    | Manager
    | Site_manager
    | Unknown_role of string  (** Includes [ACCOUNT_ROLE_UNSPECIFIED]. *)

  type business_account = {
    account_resource_name : string;  (** [name]: ["accounts/{accountId}"] *)
    account_name : string option;  (** [accountName], the display name. *)
    account_type : account_type option;
    role : account_role option;
    verification_state : string option;  (** [VERIFIED], [UNVERIFIED], ... *)
  }

  (** A location from [accounts.locations.list]
      ({{:https://developers.google.com/my-business/reference/businessinformation/rest/v1/accounts.locations}}).
      Fields outside the [readMask] are [None] / empty. *)
  type business_location = {
    location_resource_name : string;  (** [name]: ["locations/{locationId}"] *)
    title : string option;
    store_code : string option;
    website_uri : string option;
    address_lines : string list;  (** [storefrontAddress.addressLines] *)
  }

  (** One page of a Google list call. [next_page_token] is [None] on the
      last page. *)
  type 'a page = {
    items : 'a list;
    next_page_token : string option;
  }

  (** The [readMask] {!Make.list_locations_page} sends by default. Google
      requires one. *)
  let default_location_read_mask = "name,title,storeCode,websiteUri,storefrontAddress"

  (** The v4 resource name ["accounts/{a}/locations/{l}"] that the v4
      [localPosts] API needs, from the account name and the
      ["locations/{l}"] name the Business Information API returns. A
      [location_name] that already starts with ["accounts/"] is returned
      unchanged. *)
  let v4_location_name ~account_name ~location_name =
    if String.starts_with ~prefix:"accounts/" location_name then location_name
    else account_name ^ "/" ^ location_name

  let account_type_of_string = function
    | "PERSONAL" -> Personal
    | "LOCATION_GROUP" -> Location_group
    | "USER_GROUP" -> User_group
    | "ORGANIZATION" -> Organization
    | other -> Unknown_account_type other

  let account_role_of_string = function
    | "PRIMARY_OWNER" -> Primary_owner
    | "OWNER" -> Owner
    | "MANAGER" -> Manager
    | "SITE_MANAGER" -> Site_manager
    | other -> Unknown_role other

  let json_field key = function
    | `Assoc fields -> List.assoc_opt key fields
    | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ -> None

  let json_text key json =
    match json_field key json with
    | Some (`String value) when value <> "" -> Some value
    | Some (`String _ | `Null | `Bool _ | `Int _ | `Float _ | `List _ | `Assoc _) | None -> None

  let business_account_of_json json =
    Option.map
      (fun account_resource_name ->
        { account_resource_name;
          account_name = json_text "accountName" json;
          account_type = Option.map account_type_of_string (json_text "type" json);
          role = Option.map account_role_of_string (json_text "role" json);
          verification_state = json_text "verificationState" json })
      (json_text "name" json)

  let business_location_of_json json =
    Option.map
      (fun location_resource_name ->
        { location_resource_name;
          title = json_text "title" json;
          store_code = json_text "storeCode" json;
          website_uri = json_text "websiteUri" json;
          address_lines =
            (match Option.bind (json_field "storefrontAddress" json) (json_field "addressLines") with
             | Some (`List lines) ->
                 List.filter_map
                   (function
                     | `String line -> Some line
                     | `Null | `Bool _ | `Int _ | `Float _ | `List _ | `Assoc _ -> None)
                   lines
             | Some (`Null | `Bool _ | `Int _ | `Float _ | `String _ | `Assoc _) | None -> []) })
      (json_text "name" json)

  (** Parses one page of a list response: the entries of [key] that
      [item_of] accepts, and [nextPageToken]. *)
  let parse_page ~key item_of body : ('a page, string) Stdlib.result =
    match Yojson.Basic.from_string body with
    | exception Yojson.Json_error message -> Stdlib.Error ("list response is not JSON: " ^ message)
    | json ->
        let items =
          match json_field key json with
          | Some (`List items) -> List.filter_map item_of items
          | Some (`Null | `Bool _ | `Int _ | `Float _ | `String _ | `Assoc _) | None -> []
        in
        Stdlib.Ok { items; next_page_token = json_text "nextPageToken" json }

  (** Make functor for OAuth operations that need HTTP client *)
  module Make (Http : HTTP_CLIENT) = struct
    module Shared = Social_google_oauth.Make(Http)

    let exchange_code = Shared.exchange_code
    let refresh_token = Shared.refresh_token

    let revoke_token ~token on_result =
      Shared.revoke_token ~platform:Platform_types.GoogleBusinessProfile ~token on_result

    let get_user_info ~access_token on_result =
      Shared.get_user_info ~platform:Platform_types.GoogleBusinessProfile ~access_token on_result

    (** {!Social_google_oauth.Make.get_userinfo} tagged
        [GoogleBusinessProfile]. *)
    let get_userinfo ~access_token on_result =
      Shared.get_userinfo ~platform:Platform_types.GoogleBusinessProfile ~access_token on_result

    let get_page ~access_token url parse on_result =
      Http.get ~headers:[ ("Authorization", "Bearer " ^ access_token) ] url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            match parse response.body with
            | Stdlib.Ok page -> on_result (Stdlib.Ok page)
            | Stdlib.Error message -> on_result (Stdlib.Error (Error_types.Internal_error message))
          else
            on_result
              (Stdlib.Error
                 (Social_google_oauth.api_error ~platform:Platform_types.GoogleBusinessProfile
                    ~required_scopes:Scopes.business_manage ~status_code:response.status response.body)))
        (fun err -> on_result (Stdlib.Error (Error_types.Network_error (Error_types.Connection_failed err))))

    let page_query ?page_size ?page_token extra =
      let params =
        extra
        @ (match page_size with Some size -> [ ("pageSize", [ string_of_int size ]) ] | None -> [])
        @ (match page_token with Some token -> [ ("pageToken", [ token ]) ] | None -> [])
      in
      match params with
      | [] -> ""
      | _ :: _ -> "?" ^ Uri.encoded_of_query params

    (** Follows [nextPageToken] up to [max_pages] requests. Returns a
        [Validation_error] if the cap prevents a complete result. *)
    let collect_pages ~max_pages fetch on_result =
      let rec loop ~remaining ~page_token acc =
        fetch ?page_token (function
          | Stdlib.Error err -> on_result (Stdlib.Error err)
          | Stdlib.Ok { items; next_page_token } ->
              let acc = List.rev_append items acc in
              (match next_page_token with
               | Some token when remaining > 1 -> loop ~remaining:(remaining - 1) ~page_token:(Some token) acc
               | Some _ -> on_result (Stdlib.Error (Error_types.Validation_error
                   [ Error_types.Option_conflict
                       "Discovery exceeded max_pages; increase max_pages or use the paged API" ]))
               | None -> on_result (Stdlib.Ok (List.rev acc))))
      in
      loop ~remaining:(max 1 max_pages) ~page_token:None []

    (** One page of the token user's accounts, [GET /v1/accounts]
        ({{:https://developers.google.com/my-business/reference/accountmanagement/rest/v1/accounts/list}};
        [pageSize] at most 20). Needs [business.manage]. *)
    let list_accounts_page ~access_token ?page_size ?page_token on_result =
      get_page ~access_token
        (Metadata.account_management_base ^ "/accounts" ^ page_query ?page_size ?page_token [])
        (parse_page ~key:"accounts" business_account_of_json)
        on_result

    (** Every account, following [nextPageToken] (default 50 pages of 20).
        Returns [Validation_error] if more pages remain at the cap. *)
    let list_all_accounts ~access_token ?(max_pages = 50) on_result =
      collect_pages ~max_pages
        (fun ?page_token k -> list_accounts_page ~access_token ~page_size:20 ?page_token k)
        on_result

    (** One page of an account's locations,
        [GET /v1/{account_name}/locations?readMask=...]
        ({{:https://developers.google.com/my-business/reference/businessinformation/rest/v1/accounts.locations/list}};
        [pageSize] at most 100).

        @param account_name ["accounts/{accountId}"], or ["accounts/-"] for
          the token user.
        @param read_mask Defaults to {!default_location_read_mask}. *)
    let list_locations_page ~access_token ~account_name ?(read_mask = default_location_read_mask)
        ?page_size ?page_token on_result =
      get_page ~access_token
        (Printf.sprintf "%s/%s/locations%s" Metadata.business_information_base account_name
           (page_query ?page_size ?page_token [ ("readMask", [ read_mask ]) ]))
        (parse_page ~key:"locations" business_location_of_json)
        on_result

    (** Every location of an account, following [nextPageToken] (default
        50 pages of 100). Returns [Validation_error] when more pages remain
        at the cap. Retry and backoff on [Rate_limited] are left to the
        caller. *)
    let list_all_locations ~access_token ~account_name ?read_mask ?(max_pages = 50) on_result =
      collect_pages ~max_pages
        (fun ?page_token k -> list_locations_page ~access_token ~account_name ?read_mask ~page_size:100 ?page_token k)
        on_result
  end
end

(** {1 Domain Types} *)

(** Topic type for local posts *)
type topic_type =
  | Standard
  | Event
  | Offer

(** Call to action type *)
type call_to_action_type =
  | Book
  | Order
  | Shop
  | Learn_more
  | Sign_up
  | Get_offer
  | Call

(** Call to action *)
type call_to_action = {
  cta_type : call_to_action_type;
  url : string option;
}

(** Event schedule *)
type event_schedule = {
  start_date : string;  (** YYYY-MM-DD *)
  start_time : string option;  (** HH:MM *)
  end_date : string option;
  end_time : string option;
}

(** Event data *)
type event_data = {
  title : string;
  schedule : event_schedule;
}

(** Offer data *)
type offer_data = {
  coupon_code : string option;
  redeem_online_url : string option;
  terms_conditions : string option;
}

(** Post content for Google Business Profile *)
type post_content = {
  summary : string;
  topic : topic_type;
  event : event_data option;
  offer : offer_data option;
  call_to_action : call_to_action option;
  media_url : string option;
  language_code : string option;
}

(** Location record *)
type location = {
  name : string;  (** Resource name: locations/{locationId} *)
  title : string;
  address : string option;
  photo_url : string option;
}

(** Configuration module type for Google Business Profile provider *)
module type CONFIG = sig
  module Http : HTTP_CLIENT

  val get_env : string -> string option
  val get_credentials : account_id:string -> (credentials -> unit) -> (string -> unit) -> unit
  val update_credentials : account_id:string -> credentials:credentials -> (unit -> unit) -> (string -> unit) -> unit
  val encrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val decrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val update_health_status : account_id:string -> status:string -> error_message:string option -> (unit -> unit) -> (string -> unit) -> unit
end

(** Make functor to create Google Business Profile provider with given configuration *)
module Make (Config : CONFIG) = struct
  module Shared_oauth = Social_google_oauth.Make(Config.Http)

  let account_management_base = "https://mybusinessaccountmanagement.googleapis.com/v1"
  let business_information_base = "https://mybusinessbusinessinformation.googleapis.com/v1"
  let mybusiness_v4_base = "https://mybusiness.googleapis.com/v4"

  (** {1 Platform Constants} *)

  let max_post_length = 1500

  (** {1 String Conversion Helpers} *)

  let topic_type_to_string = function
    | Standard -> "STANDARD"
    | Event -> "EVENT"
    | Offer -> "OFFER"

  let call_to_action_type_to_string = function
    | Book -> "BOOK"
    | Order -> "ORDER"
    | Shop -> "SHOP"
    | Learn_more -> "LEARN_MORE"
    | Sign_up -> "SIGN_UP"
    | Get_offer -> "GET_OFFER"
    | Call -> "CALL"

  (** {1 Validation Functions} *)

  (** Validate a post's content *)
  let validate_post ~text =
    let errors = ref [] in
    let text_len = String.length text in
    if text_len > max_post_length then
      errors := Error_types.Text_too_long { length = text_len; max = max_post_length } :: !errors;
    if text_len = 0 then
      errors := Error_types.Text_empty :: !errors;
    if !errors = [] then Ok ()
    else Error (List.rev !errors)

  (** Validate a media URL *)
  let validate_media_url url =
    if String.length url = 0 then
      Error [Error_types.Invalid_url ""]
    else if not (String.sub url 0 (min 8 (String.length url)) = "https://" ||
                 String.sub url 0 (min 7 (String.length url)) = "http://") then
      Error [Error_types.Invalid_url url]
    else
      Ok ()

  (** Parse API error response and return structured Error_types.error *)
  let parse_api_error ~status_code ~response_body =
    try
      let json = Yojson.Basic.from_string response_body in
      let open Yojson.Basic.Util in
      let error_msg =
        try
          let error = json |> member "error" in
          try error |> member "message" |> to_string
          with _ -> response_body
        with _ -> response_body
      in

      if status_code = 401 then
        Error_types.Auth_error Error_types.Token_invalid
      else if status_code = 403 then
        Error_types.Auth_error (Error_types.Insufficient_permissions {
          required = ["business.manage"];
          platform_message = Some error_msg;
        })
      else if status_code = 429 then
        Error_types.Rate_limited {
          retry_after_seconds = Some 60;
          limit = None;
          remaining = Some 0;
          reset_at = None;
        }
      else
        Error_types.Api_error {
          status_code;
          message = error_msg;
          platform = Platform_types.GoogleBusinessProfile;
          raw_response = Some response_body;
          request_id = None;
        }
    with _ ->
      Error_types.Api_error {
        status_code;
        message = response_body;
        platform = Platform_types.GoogleBusinessProfile;
        raw_response = Some response_body;
        request_id = None;
      }

  (** Ensure valid OAuth 2.0 access token, refreshing if needed *)
  let ensure_valid_token ~account_id on_success on_error =
    let perform_refresh ~credentials on_refresh_success on_refresh_error =
      match credentials.Social_core.refresh_token with
      | None -> on_refresh_error (Error_types.Auth_error Error_types.Missing_credentials)
      | Some refresh_token ->
          let client_id = Config.get_env "GOOGLE_BUSINESS_CLIENT_ID" |> Option.value ~default:"" in
          let client_secret = Config.get_env "GOOGLE_BUSINESS_CLIENT_SECRET" |> Option.value ~default:"" in
          Shared_oauth.refresh_token
            ?prior_scope:credentials.Social_core.scope
            ~client_id ~client_secret ~refresh_token
            on_refresh_success
            (fun err -> on_refresh_error (Error_types.Auth_error (Error_types.Refresh_failed err)))
    in
    Social_refresh.Orchestrator.ensure_valid_access_token
      ~policy:{ Social_refresh.refresh_window_seconds = 600 }
      ~account_id
      ~load_credentials:Config.get_credentials
      ~perform_refresh
      ~persist_credentials:Config.update_credentials
      ~update_health:Config.update_health_status
      (fun credentials -> on_success credentials.Social_core.access_token)
      on_error

  (** {1 JSON Body Builders} *)

  (** Parse "YYYY-MM-DD" into JSON date fields. Raises on malformed input. *)
  let date_to_json date_str =
    `Assoc [
      ("year", `Int (int_of_string (String.sub date_str 0 4)));
      ("month", `Int (int_of_string (String.sub date_str 5 2)));
      ("day", `Int (int_of_string (String.sub date_str 8 2)));
    ]

  (** Parse "HH:MM" into JSON time fields. Raises on malformed input. *)
  let time_to_json time_str =
    `Assoc [
      ("hours", `Int (int_of_string (String.sub time_str 0 2)));
      ("minutes", `Int (int_of_string (String.sub time_str 3 2)));
    ]

  (** Helper to include an optional field *)
  let optional_field key f = function
    | Some v -> [(key, f v)]
    | None -> []

  (** Build JSON body for a local post *)
  let build_post_body (content : post_content) =
    let event_fields = match content.event with
      | None -> []
      | Some event ->
          let schedule =
            [("startDate", date_to_json event.schedule.start_date)]
            @ optional_field "startTime" time_to_json event.schedule.start_time
            @ optional_field "endDate" date_to_json event.schedule.end_date
            @ optional_field "endTime" time_to_json event.schedule.end_time
          in
          [("event", `Assoc [
            ("title", `String event.title);
            ("schedule", `Assoc schedule);
          ])]
    in
    let offer_fields = match content.offer with
      | None -> []
      | Some offer ->
          let fields =
            optional_field "couponCode" (fun s -> `String s) offer.coupon_code
            @ optional_field "redeemOnlineUrl" (fun s -> `String s) offer.redeem_online_url
            @ optional_field "termsConditions" (fun s -> `String s) offer.terms_conditions
          in
          if fields = [] then [] else [("offer", `Assoc fields)]
    in
    let cta_fields = match content.call_to_action with
      | None -> []
      | Some cta ->
          let fields =
            [("actionType", `String (call_to_action_type_to_string cta.cta_type))]
            @ optional_field "url" (fun s -> `String s) cta.url
          in
          [("callToAction", `Assoc fields)]
    in
    let media_fields = match content.media_url with
      | None -> []
      | Some url ->
          [("media", `List [
            `Assoc [
              ("mediaFormat", `String "PHOTO");
              ("sourceUrl", `String url);
            ]
          ])]
    in
    `Assoc (
      [("summary", `String content.summary);
       ("topicType", `String (topic_type_to_string content.topic))]
      @ optional_field "languageCode" (fun s -> `String s) content.language_code
      @ event_fields
      @ offer_fields
      @ cta_fields
      @ media_fields
    )

  (** {1 Location Discovery} *)

  (** List accounts for the authenticated user *)
  let list_accounts ~account_id on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let url = account_management_base ^ "/accounts" in
        let headers = [("Authorization", "Bearer " ^ access_token)] in
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_result (Ok json)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse accounts response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Internal_error err))))
      (fun err -> on_result (Error err))

  (** List locations for a given account *)
  let list_locations ~account_id ~account_name on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let url = Printf.sprintf "%s/%s/locations?readMask=name,title,storefrontAddress" business_information_base account_name in
        let headers = [("Authorization", "Bearer " ^ access_token)] in
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                on_result (Ok json)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse locations response: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Internal_error err))))
      (fun err -> on_result (Error err))

  (** Get a single location *)
  let get_location ~account_id ~location_name on_result =
    ensure_valid_token ~account_id
      (fun access_token ->
        let url = Printf.sprintf "%s/%s?readMask=name,title,storefrontAddress" business_information_base location_name in
        let headers = [("Authorization", "Bearer " ^ access_token)] in
        Config.Http.get ~headers url
          (fun response ->
            if response.status >= 200 && response.status < 300 then
              try
                let json = Yojson.Basic.from_string response.body in
                let open Yojson.Basic.Util in
                let loc : location = {
                  name = (try json |> member "name" |> to_string with _ -> location_name);
                  title = (try json |> member "title" |> to_string with _ -> "");
                  address = (try Some (json |> member "storefrontAddress" |> member "addressLines" |> to_list |> List.hd |> to_string) with _ -> None);
                  photo_url = None;
                } in
                on_result (Ok loc)
              with e ->
                on_result (Error (Error_types.Internal_error (Printf.sprintf "Failed to parse location: %s" (Printexc.to_string e))))
            else
              on_result (Error (parse_api_error ~status_code:response.status ~response_body:response.body)))
          (fun err -> on_result (Error (Error_types.Internal_error err))))
      (fun err -> on_result (Error err))

  (** {1 Posting} *)

  (** Post a single update to a location *)
  let post_single ~account_id ~location_name ~text ~media_urls on_result =
    let media_url = match media_urls with
      | url :: _ -> Some url
      | [] -> None
    in
    let content = {
      summary = text;
      topic = Standard;
      event = None;
      offer = None;
      call_to_action = None;
      media_url;
      language_code = None;
    } in
    if location_name = "" then
      on_result (Error_types.Failure (Error_types.Internal_error "Google Business location name not configured"))
    else
    match validate_post ~text with
    | Error errs -> on_result (Error_types.Failure (Error_types.Validation_error errs))
    | Ok () ->
        (* Validate media URL if provided *)
        let media_valid = match media_url with
          | None -> Ok ()
          | Some url -> validate_media_url url
        in
        (match media_valid with
         | Error errs -> on_result (Error_types.Failure (Error_types.Validation_error errs))
         | Ok () ->
             ensure_valid_token ~account_id
               (fun access_token ->
                   let url = Printf.sprintf "%s/%s/localPosts" mybusiness_v4_base location_name in
                   let body = Yojson.Basic.to_string (build_post_body content) in
                   let headers = [
                     ("Authorization", "Bearer " ^ access_token);
                     ("Content-Type", "application/json");
                   ] in

                   Config.Http.post ~headers ~body url
                     (fun response ->
                       if response.status >= 200 && response.status < 300 then
                         try
                           let open Yojson.Basic.Util in
                           let json = Yojson.Basic.from_string response.body in
                           let post_name = json |> member "name" |> to_string in
                           on_result (Error_types.Success post_name)
                         with _e ->
                           on_result (Error_types.Failure (Error_types.Internal_error (Printf.sprintf "Failed to parse response: %s" response.body)))
                       else
                         on_result (Error_types.Failure (parse_api_error ~status_code:response.status ~response_body:response.body)))
                     (fun err -> on_result (Error_types.Failure (Error_types.Internal_error err))))
               (fun err -> on_result (Error_types.Failure err)))

  (** Post thread - Google Business Profile doesn't support threads *)
  let post_thread ~account_id ~location_name ~texts ~media_urls_per_post on_result =
    if List.length texts = 0 then
      on_result (Error_types.Failure (Error_types.Validation_error [Error_types.Thread_empty]))
    else
      let first_text = List.hd texts in
      let first_media = try List.hd media_urls_per_post with _ -> [] in
      let total_requested = List.length texts in
      post_single ~account_id ~location_name ~text:first_text ~media_urls:first_media
        (fun outcome ->
          match outcome with
          | Error_types.Success post_name ->
              let thread_result = {
                Error_types.posted_ids = [post_name];
                failed_at_index = None;
                total_requested;
              } in
              if total_requested > 1 then
                on_result (Error_types.Partial_success {
                  result = thread_result;
                  warnings = [Error_types.Generic_warning {
                    code = "google_business_no_threads";
                    message = Printf.sprintf "Google Business Profile does not support threads. Only first of %d items posted." total_requested;
                    recoverable = false
                  }]
                })
              else
                on_result (Error_types.Success thread_result)
          | Error_types.Partial_success { result = post_name; warnings } ->
              let thread_result = {
                Error_types.posted_ids = [post_name];
                failed_at_index = None;
                total_requested;
              } in
              on_result (Error_types.Partial_success { result = thread_result; warnings })
          | Error_types.Failure err ->
              on_result (Error_types.Failure err))

  (** {1 OAuth Helpers (CPS)} *)

  (** OAuth authorization URL with PKCE *)
  let get_oauth_url ~redirect_uri ~state ~code_verifier on_success on_error =
    let client_id = Config.get_env "GOOGLE_BUSINESS_CLIENT_ID" |> Option.value ~default:"" in

    if client_id = "" then
      on_error "Google Business client ID not configured"
    else
      let url = OAuth.get_authorization_url
        ~client_id ~redirect_uri ~state ~code_verifier () in
      on_success url

  (** Exchange OAuth code for access token with PKCE *)
  let exchange_code ~code ~redirect_uri ~code_verifier on_success on_error =
    let client_id = Config.get_env "GOOGLE_BUSINESS_CLIENT_ID" |> Option.value ~default:"" in
    let client_secret = Config.get_env "GOOGLE_BUSINESS_CLIENT_SECRET" |> Option.value ~default:"" in

    if client_id = "" || client_secret = "" then
      on_error "Google Business OAuth credentials not configured"
    else
      Shared_oauth.exchange_code ~client_id ~client_secret ~redirect_uri ~code ~code_verifier
        on_success on_error

  (** Validate content length *)
  let validate_content ~text =
    let len = String.length text in
    if len = 0 then
      Error "Text cannot be empty"
    else if len > max_post_length then
      Error (Printf.sprintf "Post exceeds %d character limit (current: %d)" max_post_length len)
    else
      Ok ()
end
