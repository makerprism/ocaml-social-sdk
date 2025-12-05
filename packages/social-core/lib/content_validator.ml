(** Content Validator - Validate content for social media platforms *)

open Platform_types

(** Validate text length for a platform *)
let validate_text_length ~platform ~text ~max_length =
  if String.length text > max_length then
    Error (Printf.sprintf "%s post exceeds maximum length of %d characters" 
      (platform_to_string platform) max_length)
  else
    Ok ()

(** Validate image constraints *)
let validate_image ~mime_type ~file_size_bytes ~width ~height ~constraints =
  (* Check format *)
  if not (List.mem mime_type constraints.supported_image_formats) then
    Error (Printf.sprintf "Unsupported image format: %s" mime_type)
  (* Check file size *)
  else if float_of_int file_size_bytes > (constraints.max_image_size_mb *. 1024.0 *. 1024.0) then
    Error (Printf.sprintf "Image exceeds maximum size of %.1f MB" constraints.max_image_size_mb)
  (* Check dimensions *)
  else begin
    match constraints.max_width, constraints.max_height with
    | Some max_w, Some max_h when width > max_w || height > max_h ->
        Error (Printf.sprintf "Image dimensions %dx%d exceed maximum %dx%d" 
          width height max_w max_h)
    | _ -> 
        match constraints.min_width, constraints.min_height with
        | Some min_w, Some min_h when width < min_w || height < min_h ->
            Error (Printf.sprintf "Image dimensions %dx%d below minimum %dx%d" 
              width height min_w min_h)
        | _ -> Ok ()
  end

(** Validate video constraints *)
let validate_video ~mime_type ~file_size_bytes ~duration_seconds ~constraints =
  (* Check format *)
  if not (List.mem mime_type constraints.supported_video_formats) then
    Error (Printf.sprintf "Unsupported video format: %s" mime_type)
  (* Check file size *)
  else if float_of_int file_size_bytes > (constraints.max_video_size_mb *. 1024.0 *. 1024.0) then
    Error (Printf.sprintf "Video exceeds maximum size of %.1f MB" constraints.max_video_size_mb)
  (* Check duration *)
  else if int_of_float duration_seconds > constraints.max_video_duration_seconds then
    Error (Printf.sprintf "Video duration %.1fs exceeds maximum %ds" 
      duration_seconds constraints.max_video_duration_seconds)
  else
    Ok ()

(** Validate media for a platform *)
let validate_media ~platform ~media ~capability =
  match capability.media_constraints with
  | None -> Error (Printf.sprintf "%s does not support media" (platform_to_string platform))
  | Some constraints ->
      match media.media_type with
      | Image ->
          validate_image 
            ~mime_type:media.mime_type 
            ~file_size_bytes:media.file_size_bytes
            ~width:(Option.value ~default:0 media.width)
            ~height:(Option.value ~default:0 media.height)
            ~constraints
      | Video ->
          validate_video
            ~mime_type:media.mime_type
            ~file_size_bytes:media.file_size_bytes
            ~duration_seconds:(Option.value ~default:0.0 media.duration_seconds)
            ~constraints
      | Gif ->
          (* Treat GIF as image for validation *)
          validate_image 
            ~mime_type:media.mime_type 
            ~file_size_bytes:media.file_size_bytes
            ~width:(Option.value ~default:0 media.width)
            ~height:(Option.value ~default:0 media.height)
            ~constraints
