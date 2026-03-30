(** Social Provider Core - Runtime-agnostic social media API client interfaces *)

(** {1 Core Types} *)

(** Platform-agnostic response type *)
type response = {
  status: int;
  headers: (string * string) list;
  body: string;
}

(** Multipart form data part *)
type multipart_part = {
  name: string;
  filename: string option;
  content_type: string option;
  content: string;
}

(** Authentication type *)
type auth_type =
  | Bearer
  | OAuth1a
  | App_password
  | Custom of string

let auth_type_of_string = function
  | "Bearer" | "bearer" -> Bearer
  | "oauth1a" | "OAuth1a" -> OAuth1a
  | "app_password" | "AppPassword" -> App_password
  | s -> Custom s

let string_of_auth_type = function
  | Bearer -> "Bearer"
  | OAuth1a -> "oauth1a"
  | App_password -> "app_password"
  | Custom s -> s

(** Account credentials *)
type credentials = {
  access_token: string;
  refresh_token: string option;
  expires_at: string option;
  auth_type: auth_type;
}

(** {1 HTTP Client Interface} 

    CPS-style HTTP client interface that can be implemented with any HTTP library
    (Cohttp, Curly, Httpaf, etc.) and any async runtime (Lwt, Eio, synchronous).
*)
module type HTTP_CLIENT = sig
  (** Make a GET request
      @param headers Optional HTTP headers
      @param url The URL to request
      @param on_success Success continuation receiving the response
      @param on_error Error continuation receiving error message
  *)
  val get : 
    ?headers:(string * string) list -> 
    string -> 
    (response -> unit) -> 
    (string -> unit) -> 
    unit

  (** Make a POST request
      @param headers Optional HTTP headers
      @param body Optional request body
      @param url The URL to request
      @param on_success Success continuation receiving the response
      @param on_error Error continuation receiving error message
  *)
  val post : 
    ?headers:(string * string) list -> 
    ?body:string -> 
    string -> 
    (response -> unit) -> 
    (string -> unit) -> 
    unit

  (** Make a POST request with multipart form data
      @param headers Optional HTTP headers (Content-Type will be set automatically)
      @param parts List of multipart parts
      @param url The URL to request
      @param on_success Success continuation receiving the response
      @param on_error Error continuation receiving error message
  *)
  val post_multipart : 
    ?headers:(string * string) list -> 
    parts:multipart_part list -> 
    string -> 
    (response -> unit) -> 
    (string -> unit) -> 
    unit

  (** Make a PUT request
      @param headers Optional HTTP headers
      @param body Optional request body
      @param url The URL to request
      @param on_success Success continuation receiving the response
      @param on_error Error continuation receiving error message
  *)
  val put :
    ?headers:(string * string) list ->
    ?body:string ->
    string ->
    (response -> unit) ->
    (string -> unit) ->
    unit

  (** Make a DELETE request
      @param headers Optional HTTP headers
      @param url The URL to request
      @param on_success Success continuation receiving the response
      @param on_error Error continuation receiving error message
  *)
  val delete :
    ?headers:(string * string) list ->
    string ->
    (response -> unit) ->
    (string -> unit) ->
    unit
end

(** {1 Storage Interface} 

    Abstract storage operations for media files.
*)
module type STORAGE = sig
  (** Download media from storage
      @param media_id The media identifier
      @param on_success Success continuation receiving the media content
      @param on_error Error continuation receiving error message
  *)
  val download_media : 
    media_id:string -> 
    (string -> unit) -> 
    (string -> unit) -> 
    unit

  (** Upload media to public storage and get URL
      @param content The media content bytes
      @param filename The filename
      @param content_type The MIME type
      @param on_success Success continuation receiving the public URL
      @param on_error Error continuation receiving error message
  *)
  val upload_public_media : 
    content:string -> 
    filename:string -> 
    content_type:string ->
    (string -> unit) -> 
    (string -> unit) -> 
    unit
end

(** {1 Configuration Interface} 

    Abstract configuration and credential management.
*)
module type CONFIG = sig
  (** Get environment variable
      @param key The environment variable name
      @return Some value if set, None otherwise
  *)
  val get_env : string -> string option

  (** Get account credentials
      @param account_id The account identifier
      @param on_success Success continuation receiving the credentials
      @param on_error Error continuation receiving error message
  *)
  val get_credentials : 
    account_id:string ->
    (credentials -> unit) -> 
    (string -> unit) -> 
    unit

  (** Update account credentials
      @param account_id The account identifier
      @param credentials The new credentials
      @param on_success Success continuation
      @param on_error Error continuation receiving error message
  *)
  val update_credentials : 
    account_id:string -> 
    credentials:credentials ->
    (unit -> unit) -> 
    (string -> unit) -> 
    unit

  (** Encrypt sensitive data
      @param plaintext The data to encrypt
      @param on_success Success continuation receiving encrypted data
      @param on_error Error continuation receiving error message
  *)
  val encrypt : 
    string -> 
    (string -> unit) -> 
    (string -> unit) -> 
    unit

  (** Decrypt sensitive data
      @param ciphertext The encrypted data
      @param on_success Success continuation receiving decrypted data
      @param on_error Error continuation receiving error message
  *)
  val decrypt : 
    string -> 
    (string -> unit) -> 
    (string -> unit) -> 
    unit

  (** Update account health status
      @param account_id The account identifier
      @param status The health status (e.g., "healthy", "token_expired")
      @param error_message Optional error message
      @param on_success Success continuation
      @param on_error Error continuation receiving error message
  *)
  val update_health_status :
    account_id:string ->
    status:string ->
    error_message:string option ->
    (unit -> unit) ->
    (string -> unit) ->
    unit
end

(** {1 Provider Result Type} *)

(** Result type for provider operations *)
type ('ok, 'err) result = 
  | Ok of 'ok
  | Error of 'err

(** {1 Utility Functions} *)

(** Percent-encode a string for application/x-www-form-urlencoded.

    Unlike [Uri.encoded_of_query] which uses URI query-string encoding
    (leaving [/], [=], [@], [:], [?], [!], [*], [(], [)] unencoded),
    this function uses the stricter encoding required by
    application/x-www-form-urlencoded POST bodies where only unreserved
    characters are left raw and spaces become [+]. *)
let form_urlencode_value s =
  let buf = Buffer.create (String.length s) in
  String.iter (fun c ->
    match c with
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '_' | '.' | '~' ->
        Buffer.add_char buf c
    | ' ' ->
        Buffer.add_char buf '+'
    | _ ->
        Buffer.add_string buf (Printf.sprintf "%%%02X" (Char.code c))
  ) s;
  Buffer.contents buf

(** Encode a list of key-value pairs as an application/x-www-form-urlencoded
    string.  Each key and value is percent-encoded with {!form_urlencode_value}
    and pairs are joined with [&]. *)
let form_urlencode_kvs (pairs : (string * string) list) =
  pairs
  |> List.map (fun (k, v) ->
       form_urlencode_value k ^ "=" ^ form_urlencode_value v)
  |> String.concat "&"

(** Parse JSON credentials blob *)
let parse_credentials_json json_str =
  try
    let open Yojson.Basic.Util in
    let json = Yojson.Basic.from_string json_str in
    let access_token = json |> member "access_token" |> to_string in
    let refresh_token = try Some (json |> member "refresh_token" |> to_string) with _ -> None in
    let expires_at = try Some (json |> member "expires_at" |> to_string) with _ -> None in
    let auth_type =
      let s = try json |> member "token_type" |> to_string with _ -> "Bearer" in
      auth_type_of_string s
    in
    Ok { access_token; refresh_token; expires_at; auth_type }
  with e ->
    Error (Printf.sprintf "Failed to parse credentials: %s" (Printexc.to_string e))

(** Create JSON credentials blob *)
let create_credentials_json (creds : credentials) =
  `Assoc [
    ("access_token", `String creds.access_token);
    ("refresh_token", match creds.refresh_token with Some rt -> `String rt | None -> `Null);
    ("expires_at", match creds.expires_at with Some exp -> `String exp | None -> `Null);
    ("token_type", `String (string_of_auth_type creds.auth_type));
  ] |> Yojson.Basic.to_string
