(** Error Types - Structured errors for social media SDK *)

open Platform_types

(** {1 Validation Errors} *)

(** Specific validation error types *)
type validation_error =
  | Text_too_long of { length: int; max: int }
  | Text_empty
  | Media_required  (** Platform requires media (e.g., Pinterest, TikTok, YouTube Shorts) *)
  | Media_too_large of { size_bytes: int; max_bytes: int }
  | Media_unsupported_format of string
  | Media_dimensions_invalid of { width: int; height: int; reason: string }
  | Video_too_long of { duration_seconds: float; max_seconds: int }
  | Too_many_media of { count: int; max: int }
  | Too_few_items of { count: int; min: int; item_kind: string }
  | Invalid_url of string
  | Option_conflict of string  (** Incompatible option combination *)
  | Topic_tag_invalid of string  (** Topic tag content constraint violation *)
  | Thread_empty
  | Thread_post_invalid of { index: int; errors: validation_error list }

(** {1 Rate Limiting} *)

(** Rate limit information returned by platforms *)
type rate_limit_info = {
  retry_after_seconds: int option;
  limit: int option;
  remaining: int option;
  reset_at: string option;  (** ISO 8601 timestamp *)
}

(** {1 Authentication Errors} *)

(** Authentication-related error types *)
type auth_error =
  | Token_expired
  | Token_invalid
  | Token_revoked
  | Refresh_failed of string
  | Missing_credentials
  | Insufficient_permissions of {
      required: string list;
      (** Permissions/scopes the SDK believes are needed for the call.
          May be a heuristic (the SDK's best guess for the failed path)
          rather than what the platform specifically rejected. *)
      platform_message: string option;
      (** Verbatim error message text from the platform, when available.
          Use this to disambiguate which specific scope the platform
          rejected — the [required] list is the SDK's heuristic and may
          be broader than the actual cause. *)
    }

(** {1 API Errors} *)

(** Structured API error from a platform *)
type api_error = {
  status_code: int;
  message: string;
  platform: platform;
  raw_response: string option;
  request_id: string option;  (** For debugging/support *)
}

(** {1 Network Errors} *)

(** Network-level error types *)
type network_error =
  | Connection_failed of string
  | Timeout
  | Dns_resolution_failed
  | Ssl_error of string

(** {1 Unified Error Type} *)

(** The main error type for all SDK operations *)
type error =
  | Validation_error of validation_error list
  | Auth_error of auth_error
  | Rate_limited of rate_limit_info
  | Api_error of api_error
  | Network_error of network_error
  | Duplicate_content
  | Content_policy_violation of string
  | Resource_not_found of string
  | Internal_error of string  (** SDK bugs, JSON parse failures, etc. *)

(** {1 Warnings} *)

(** Warnings for soft failures that don't prevent the operation *)
type warning =
  | Link_card_failed of string      (** URL that couldn't be enriched *)
  | Alt_text_failed of string       (** Media ID that couldn't get alt text *)
  | Thumbnail_skipped of string     (** Reason thumbnail was skipped *)
  | Media_resized                   (** Media was auto-resized *)
  | Enrichment_skipped of string    (** Generic enrichment failure *)
  | Generic_warning of { code: string; message: string; recoverable: bool }

(** {1 Result Types} *)

(** Simple result type for operations that either succeed or fail.
    Used for read operations, engagement actions (like, follow), etc. *)
type 'a api_result = ('a, error) result

(** Result type for operations that can partially succeed.
    Used for posting operations where warnings can occur (e.g., link card failed). *)
type 'a outcome =
  | Success of 'a
  | Partial_success of { result: 'a; warnings: warning list }
  | Failure of error

(** {1 Thread Result} *)

(** Result of a thread posting operation *)
type thread_result = {
  posted_ids: string list;       (** IDs of successfully posted items *)
  failed_at_index: int option;   (** Index where failure occurred, if any *)
  total_requested: int;          (** Total number of posts requested *)
}

(** {1 String Conversion Functions} *)

(** Convert a validation error to human-readable string *)
let rec validation_error_to_string = function
  | Text_too_long { length; max } -> 
      Printf.sprintf "Text too long: %d characters (max %d)" length max
  | Text_empty -> 
      "Text cannot be empty"
  | Media_required ->
      "Media is required for this platform"
  | Media_too_large { size_bytes; max_bytes } ->
      Printf.sprintf "Media too large: %d bytes (max %d)" size_bytes max_bytes
  | Media_unsupported_format fmt ->
      Printf.sprintf "Unsupported media format: %s" fmt
  | Media_dimensions_invalid { width; height; reason } ->
      Printf.sprintf "Invalid media dimensions %dx%d: %s" width height reason
  | Video_too_long { duration_seconds; max_seconds } ->
      Printf.sprintf "Video too long: %.1fs (max %ds)" duration_seconds max_seconds
  | Too_many_media { count; max } ->
      Printf.sprintf "Too many media items: %d (max %d)" count max
  | Too_few_items { count; min; item_kind } ->
      Printf.sprintf "Too few %s: %d (min %d)" item_kind count min
  | Invalid_url url ->
      Printf.sprintf "Invalid URL: %s" url
  | Option_conflict reason ->
      Printf.sprintf "Incompatible options: %s" reason
  | Topic_tag_invalid reason ->
      Printf.sprintf "Invalid topic tag: %s" reason
  | Thread_empty ->
      "Thread cannot be empty"
  | Thread_post_invalid { index; errors } ->
      Printf.sprintf "Thread post %d invalid: %s" index 
        (String.concat "; " (List.map validation_error_to_string errors))

(** Convert an auth error to human-readable string *)
let auth_error_to_string = function
  | Token_expired -> "Access token has expired"
  | Token_invalid -> "Access token is invalid"
  | Token_revoked -> "Access token has been revoked"
  | Refresh_failed msg -> Printf.sprintf "Token refresh failed: %s" msg
  | Missing_credentials -> "No credentials available"
  | Insufficient_permissions { required; platform_message } ->
      let required_part =
        Printf.sprintf "Missing permissions: %s" (String.concat ", " required)
      in
      (match platform_message with
       | None -> required_part
       | Some msg -> Printf.sprintf "%s (platform: %s)" required_part msg)

(** Convert a network error to human-readable string *)
let network_error_to_string = function
  | Connection_failed msg -> Printf.sprintf "Connection failed: %s" msg
  | Timeout -> "Request timed out"
  | Dns_resolution_failed -> "DNS resolution failed"
  | Ssl_error msg -> Printf.sprintf "SSL error: %s" msg

(** Convert any error to human-readable string *)
let error_to_string = function
  | Validation_error errs ->
      Printf.sprintf "Validation failed: %s" 
        (String.concat "; " (List.map validation_error_to_string errs))
  | Auth_error err -> 
      auth_error_to_string err
  | Rate_limited info ->
      (match info.retry_after_seconds with
       | Some secs -> Printf.sprintf "Rate limited. Retry after %d seconds" secs
       | None -> "Rate limited")
  | Api_error { status_code; message; platform; _ } ->
      Printf.sprintf "%s API error (%d): %s" 
        (platform_to_string platform) status_code message
  | Network_error err -> 
      network_error_to_string err
  | Duplicate_content -> 
      "Duplicate content detected"
  | Content_policy_violation reason ->
      Printf.sprintf "Content policy violation: %s" reason
  | Resource_not_found id ->
      Printf.sprintf "Resource not found: %s" id
  | Internal_error msg ->
      Printf.sprintf "Internal error: %s" msg

(** Convert a warning to human-readable string *)
let warning_to_string = function
  | Link_card_failed url -> 
      Printf.sprintf "Link card failed for: %s" url
  | Alt_text_failed media_id -> 
      Printf.sprintf "Alt text failed for: %s" media_id
  | Thumbnail_skipped reason -> 
      Printf.sprintf "Thumbnail skipped: %s" reason
  | Media_resized -> 
      "Media was automatically resized"
  | Enrichment_skipped reason -> 
      Printf.sprintf "Enrichment skipped: %s" reason
  | Generic_warning { code; message; _ } ->
      Printf.sprintf "%s: %s" code message

(** {1 Error Classification} *)

(** Three-way error classification for queue/scheduler consumers.
    Distinguishes "re-authenticate" from "retry later" from "give up". *)
type error_kind =
  | Auth_failure       (** Account needs reconnection — token revoked/expired/invalid *)
  | Transient_failure  (** Worth retrying — network, rate limit, server 5xx *)
  | Permanent_failure  (** Non-recoverable — validation, duplicate, policy violation *)

(** Classify an error into auth/transient/permanent *)
let classify_error = function
  | Auth_error _ -> Auth_failure
  | Rate_limited _ -> Transient_failure
  | Network_error _ -> Transient_failure
  | Api_error { status_code; _ } when status_code >= 500 -> Transient_failure
  | _ -> Permanent_failure

(** Check if an error is retryable *)
let is_retryable = function
  | Rate_limited _ -> true
  | Network_error (Connection_failed _ | Timeout) -> true
  | Api_error { status_code; _ } when status_code >= 500 -> true
  | _ -> false

(** Get recommended retry delay in seconds *)
let get_retry_delay = function
  | Rate_limited { retry_after_seconds = Some secs; _ } -> Some secs
  | Rate_limited _ -> Some 60  (* Default 1 minute *)
  | Network_error _ -> Some 5  (* Quick retry for network issues *)
  | Api_error { status_code; _ } when status_code >= 500 -> Some 30
  | _ -> None

(** {1 Outcome Utilities} *)

(** Convert outcome to simple result, discarding warnings *)
let outcome_to_result = function
  | Success x -> Ok x
  | Partial_success { result; _ } -> Ok result
  | Failure e -> Error e

(** Map over the success value of an outcome *)
let outcome_map f = function
  | Success x -> Success (f x)
  | Partial_success { result; warnings } -> 
      Partial_success { result = f result; warnings }
  | Failure e -> Failure e

(** Add warnings to an outcome *)
let outcome_add_warnings new_warnings = function
  | Success x -> 
      if new_warnings = [] then Success x
      else Partial_success { result = x; warnings = new_warnings }
  | Partial_success { result; warnings } -> 
      Partial_success { result; warnings = warnings @ new_warnings }
  | Failure e -> Failure e

(** Check if outcome represents complete success (no warnings) *)
let is_complete_success = function
  | Success _ -> true
  | _ -> false

(** Check if outcome represents any kind of success *)
let is_success = function
  | Success _ | Partial_success _ -> true
  | Failure _ -> false

(** Get warnings from an outcome *)
let get_warnings = function
  | Success _ -> []
  | Partial_success { warnings; _ } -> warnings
  | Failure _ -> []

(** {1 Error Construction Helpers} *)

(** Create an API error *)
let make_api_error ~platform ~status_code ~message ?raw_response ?request_id () =
  Api_error { status_code; message; platform; raw_response; request_id }

(** Create a rate limit error with retry info *)
let make_rate_limited ?retry_after_seconds ?limit ?remaining ?reset_at () =
  Rate_limited { retry_after_seconds; limit; remaining; reset_at }

(** Create a validation error from a single validation issue *)
let make_validation_error err =
  Validation_error [err]

(** Create a text too long error *)
let text_too_long ~length ~max =
  make_validation_error (Text_too_long { length; max })

(** Create a media too large error *)
let media_too_large ~size_bytes ~max_bytes =
  make_validation_error (Media_too_large { size_bytes; max_bytes })

(** Create a too many media error *)
let too_many_media ~count ~max =
  make_validation_error (Too_many_media { count; max })

(** {1 API Result Utilities} *)

(** Create a successful api_result *)
let ok x = Ok x

(** Create a failed api_result *)
let fail e = Error e

(** Map over a successful api_result *)
let api_result_map f = function
  | Ok x -> Ok (f x)
  | Error e -> Error e

(** Check if api_result is successful *)
let api_result_is_ok = function
  | Ok _ -> true
  | Error _ -> false
