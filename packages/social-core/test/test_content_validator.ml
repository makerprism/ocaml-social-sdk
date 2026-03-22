let assert_eq ~context expected actual =
  if expected <> actual then
    failwith
      (Printf.sprintf "%s: expected %S but got %S" context expected actual)

let test_url_path () =
  let t ~context ~expected url =
    assert_eq ~context expected (Content_validator.url_path url)
  in
  t ~context:"plain URL"
    ~expected:"https://example.com/file.jpg"
    "https://example.com/file.jpg";
  t ~context:"with query params"
    ~expected:"https://s3.example.com/bucket/file.jpg"
    "https://s3.example.com/bucket/file.jpg?X-Amz-Algorithm=AWS4&X-Amz-Signature=abc";
  t ~context:"with fragment"
    ~expected:"https://example.com/file.jpg"
    "https://example.com/file.jpg#section";
  t ~context:"with query and fragment"
    ~expected:"https://example.com/file.jpg"
    "https://example.com/file.jpg?key=val#frag";
  t ~context:"no path"
    ~expected:"https://example.com"
    "https://example.com";
  t ~context:"empty string"
    ~expected:""
    "";
  t ~context:"presigned B2 URL"
    ~expected:"https://s3.eu-central-003.backblazeb2.com/feedmansion/media/user/post/img.jpg"
    "https://s3.eu-central-003.backblazeb2.com/feedmansion/media/user/post/img.jpg?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=key%2F20260322%2Feu-central-003%2Fs3%2Faws4_request&X-Amz-Date=20260322T001845Z&X-Amz-Expires=3600&X-Amz-SignedHeaders=host&X-Amz-Signature=deadbeef";
  Printf.printf "  ✓ url_path tests passed\n"

let test_url_file_extension () =
  let t ~context ~expected url =
    assert_eq ~context expected (Content_validator.url_file_extension url)
  in
  (* Basic extensions *)
  t ~context:"jpg" ~expected:".jpg" "https://example.com/photo.jpg";
  t ~context:"jpeg" ~expected:".jpeg" "https://example.com/photo.jpeg";
  t ~context:"png" ~expected:".png" "https://example.com/photo.png";
  t ~context:"gif" ~expected:".gif" "https://example.com/photo.gif";
  t ~context:"mp4" ~expected:".mp4" "https://example.com/video.mp4";
  t ~context:"mov" ~expected:".mov" "https://example.com/video.mov";
  t ~context:"avi" ~expected:".avi" "https://example.com/video.avi";
  (* Case insensitive *)
  t ~context:"uppercase JPG" ~expected:".jpg" "https://example.com/photo.JPG";
  t ~context:"mixed case Png" ~expected:".png" "https://example.com/photo.Png";
  (* Presigned URLs — the original bug *)
  t ~context:"presigned jpg"
    ~expected:".jpg"
    "https://s3.example.com/bucket/file.jpg?X-Amz-Signature=abc123";
  t ~context:"presigned mp4"
    ~expected:".mp4"
    "https://cdn.example.com/video.mp4?token=xyz&expires=123";
  t ~context:"presigned with fragment"
    ~expected:".png"
    "https://example.com/img.png?v=1#top";
  (* Full B2 presigned URL *)
  t ~context:"full B2 presigned URL"
    ~expected:".jpg"
    "https://s3.eu-central-003.backblazeb2.com/feedmansion/media/6c36dfb3/1146f118/1774138449-e5b735c4.jpg?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=key%2F20260322%2Feu-central-003%2Fs3%2Faws4_request&X-Amz-Date=20260322T001845Z&X-Amz-Expires=3600&X-Amz-SignedHeaders=host&X-Amz-Signature=75aaaa2efc310230ff4acfeef45fedcb01d27950";
  (* No extension *)
  t ~context:"no extension" ~expected:"" "https://example.com/media/12345";
  t ~context:"no extension with query" ~expected:"" "https://example.com/media?id=1";
  (* Dot in path but not an extension *)
  t ~context:"dot in directory" ~expected:".jpg"
    "https://example.com/v2.0/media/photo.jpg";
  Printf.printf "  ✓ url_file_extension tests passed\n"

let () =
  Printf.printf "\n=== Content Validator Tests ===\n\n";
  test_url_path ();
  test_url_file_extension ();
  Printf.printf "\n✅ All content validator tests passed!\n\n"
