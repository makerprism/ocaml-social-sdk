(** Response fields follow https://docs.joinmastodon.org/entities/Status/ and
    POST https://docs.joinmastodon.org/methods/statuses/#create. *)
let status_body =
  {|{"id":"109","uri":"https://social.example/users/alice/statuses/109","url":null,"created_at":"2026-11-18T01:02:03.000Z","account":{"id":"1","acct":"alice"},"content":"<p>reply</p>","spoiler_text":"","sensitive":false,"visibility":"private","in_reply_to_id":"108","in_reply_to_account_id":"2","mentions":[],"replies_count":0,"reblogs_count":0,"favourites_count":0}|}

let exact_text = "  no prefix\n@someone@example.org & \"quoted\" 🐫  "

module type Scenario = sig
  val response : (Social_core.response, string) result
  val visibility : Social_mastodon_v1.visibility
  val key : string option
end

module Test (Scenario : Scenario) = struct
  module Http = struct
    let get ?headers:_ url on_success _on_error =
      assert (url = "https://social.example/api/v1/accounts/verify_credentials");
      on_success
        {
          Social_core.status = 200;
          headers = [];
          body = {|{"id":"1","username":"alice","acct":"alice"}|};
        }

    let post ?(headers = []) ?(body = "") url on_success on_error =
      assert (url = "https://social.example/api/v1/statuses");
      assert (List.assoc_opt "Authorization" headers = Some "Bearer test-token");
      assert (List.assoc_opt "Content-Type" headers = Some "application/json");
      (match (Scenario.key, List.assoc_opt "Idempotency-Key" headers) with
      | Some expected, Some key -> assert (key = expected)
      | None, Some key -> assert (String.length key = 36)
      | (Some _ | None), None -> failwith "missing idempotency key");
      let json = Yojson.Basic.from_string body in
      assert (Yojson.Basic.Util.member "status" json = `String exact_text);
      assert (Yojson.Basic.Util.member "in_reply_to_id" json = `String "108");
      assert (
        Yojson.Basic.Util.member "visibility" json
        = `String (Social_mastodon_v1.visibility_to_string Scenario.visibility));
      match Scenario.response with
      | Ok response -> on_success response
      | Error message -> on_error message

    let put ?headers:_ ?body:_ _ _ _ = failwith "unexpected PUT"
    let delete ?headers:_ _ _ _ = failwith "unexpected DELETE"
    let post_multipart ?headers:_ ~parts:_ _ _ _ = failwith "unexpected upload"
  end

  module Config = struct
    module Http = Http

    let get_env _ = None

    let get_credentials ~account_id:_ callback _ =
      callback
        {
          Social_core.access_token =
            {|{"access_token":"test-token","instance_url":"https://social.example"}|};
          refresh_token = None;
          expires_at = None;
          auth_type = Social_core.Bearer;
          scope = None;
        }

    let update_credentials ~account_id:_ ~credentials:_ callback _ = callback ()
    let encrypt value callback _ = callback value
    let decrypt value callback _ = callback value
    let sleep ~seconds:_ callback _ = callback ()

    let update_health_status ~account_id:_ ~status:_ ~error_message:_ callback _
        =
      callback ()
  end

  module Client = Social_mastodon_v1.Make (Config)

  let run callback =
    Client.reply_to_status_exact ~account_id:"account" ~in_reply_to_id:"108"
      ~text:exact_text ~visibility:Scenario.visibility
      ?idempotency_key:Scenario.key callback
end

let () =
  List.iter
    (fun visibility ->
      List.iter
        (fun key ->
          let module T = Test (struct
            let response =
              Ok { Social_core.status = 200; headers = []; body = status_body }

            let visibility = visibility
            let key = key
          end) in
          T.run (function
            | Ok status ->
                assert (status.id = "109");
                assert (status.url = None);
                assert (
                  status.uri = "https://social.example/users/alice/statuses/109");
                assert (status.created_at = "2026-11-18T01:02:03.000Z")
            | Error err -> failwith (Error_types.error_to_string err)))
        [ None; Some "stable-reply-key" ])
    [ Social_mastodon_v1.Public; Unlisted; Private; Direct ];
  List.iter
    (fun (response, check) ->
      let module T = Test (struct
        let response = response
        let visibility = Social_mastodon_v1.Public
        let key = Some "retry-key"
      end) in
      T.run (function
        | Ok _ -> failwith "failure accepted"
        | Error err -> assert (check err)))
    [
      ( Ok { Social_core.status = 200; headers = []; body = "{}" },
        function
        | Error_types.Internal_error _ -> true
        | Error_types.Validation_error _ | Error_types.Auth_error _
        | Error_types.Rate_limited _ | Error_types.Api_error _
        | Error_types.Network_error _ | Error_types.Duplicate_content
        | Error_types.Content_policy_violation _
        | Error_types.Resource_not_found _ ->
            false );
      ( Ok { Social_core.status = 200; headers = []; body = "not JSON" },
        function
        | Error_types.Internal_error _ -> true
        | Error_types.Validation_error _ | Error_types.Auth_error _
        | Error_types.Rate_limited _ | Error_types.Api_error _
        | Error_types.Network_error _ | Error_types.Duplicate_content
        | Error_types.Content_policy_violation _
        | Error_types.Resource_not_found _ ->
            false );
      ( Ok
          {
            Social_core.status = 401;
            headers = [];
            body = {|{"error":"The access token is invalid"}|};
          },
        function
        | Error_types.Auth_error _ -> true
        | Error_types.Validation_error _ | Error_types.Rate_limited _
        | Error_types.Api_error _ | Error_types.Network_error _
        | Error_types.Duplicate_content | Error_types.Content_policy_violation _
        | Error_types.Resource_not_found _ | Error_types.Internal_error _ ->
            false );
      ( Error "connection failed",
        function
        | Error_types.Network_error _ -> true
        | Error_types.Validation_error _ | Error_types.Auth_error _
        | Error_types.Rate_limited _ | Error_types.Api_error _
        | Error_types.Duplicate_content | Error_types.Content_policy_violation _
        | Error_types.Resource_not_found _ | Error_types.Internal_error _ ->
            false );
    ]
