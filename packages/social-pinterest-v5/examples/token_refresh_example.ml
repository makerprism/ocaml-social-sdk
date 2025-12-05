(** Example: Automatic Token Refresh
    
    Shows how tokens are automatically refreshed when expired.
    Based on patterns from the official Pinterest Python SDK.
*)

open Pinterest_v5_enhanced

module MyConfig = struct
  module Http = Cohttp_lwt_unix.Client
  
  let get_env = Sys.getenv_opt
  
  (* Simulate stored credentials with expiry *)
  let stored_credentials = ref {
    access_token = "old_access_token";
    refresh_token = Some "refresh_token_abc123";
    expires_at = Some (Unix.time () -. 3600.0); (* Expired 1 hour ago *)
    token_type = "Bearer";
  }
  
  let get_credentials ~account_id on_success on_error =
    on_success !stored_credentials
  
  let update_credentials ~account_id ~credentials on_success on_error =
    Printf.printf "[Storage] Updating credentials:\n";
    Printf.printf "  New access token: %s...\n" 
      (String.sub credentials.access_token 0 10);
    Printf.printf "  Expires at: %s\n"
      (match credentials.expires_at with
       | Some exp -> 
           let tm = Unix.localtime exp in
           Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d"
             (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
             tm.tm_hour tm.tm_min tm.tm_sec
       | None -> "Never");
    
    stored_credentials := credentials;
    on_success ()
  
  let encrypt value on_success on_error =
    on_success ("encrypted:" ^ value)
  
  let decrypt value on_success on_error =
    on_success (String.sub value 10 (String.length value - 10))
  
  let update_health_status ~account_id ~status ~error_message on_success on_error =
    Printf.printf "[Health] Account %s: %s%s\n" 
      account_id 
      status
      (match error_message with
       | Some msg -> " - " ^ msg
       | None -> "");
    on_success ()
  
  (* Enhanced logging *)
  let log level message =
    let level_str = match level with
      | Debug -> "[DEBUG]"
      | Info -> "[INFO]"
      | Warning -> "[WARN]"
      | Error -> "[ERROR]"
    in
    Printf.printf "%s %s\n" level_str message
  
  let current_time () = Unix.time ()
  
  (* Simple in-memory cache *)
  let cache = Hashtbl.create 10
  
  let get_cache key =
    try Some (Hashtbl.find cache key) with Not_found -> None
  
  let set_cache key value ttl =
    Hashtbl.replace cache key value;
    Printf.printf "[Cache] Stored %s for %.0f seconds\n" key ttl
end

module Pinterest = Make(MyConfig)

let demonstrate_auto_refresh () =
  Printf.printf "\n=== Automatic Token Refresh Demo ===\n\n";
  
  (* First request - token is expired, will trigger refresh *)
  Printf.printf "1. Making API call with expired token...\n";
  
  Pinterest.ensure_valid_token ~account_id:"demo_user"
    (fun token ->
      Printf.printf "   ✓ Got valid token: %s...\n" (String.sub token 0 20);
      
      (* Second request - token is now fresh, no refresh needed *)
      Printf.printf "\n2. Making another API call...\n";
      
      Pinterest.ensure_valid_token ~account_id:"demo_user"
        (fun token2 ->
          Printf.printf "   ✓ Reused existing token (no refresh needed)\n";
          
          (* Simulate time passing - token about to expire *)
          Printf.printf "\n3. Simulating token near expiry (5 min before)...\n";
          MyConfig.stored_credentials := {
            !MyConfig.stored_credentials with
            expires_at = Some (Unix.time () +. 240.0) (* 4 minutes left *)
          };
          
          Pinterest.ensure_valid_token ~account_id:"demo_user"
            (fun token3 ->
              Printf.printf "   ✓ Token proactively refreshed before expiry!\n";
              Printf.printf "\n=== Demo Complete ===\n"))
        (fun err -> Printf.eprintf "Error: %s\n" err))
    (fun err -> Printf.eprintf "Error: %s\n" err)

let () =
  demonstrate_auto_refresh ()