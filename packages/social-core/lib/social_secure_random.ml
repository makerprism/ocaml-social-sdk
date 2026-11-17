let entropy_source = "/dev/urandom"

let bytes n =
  if n < 0 then invalid_arg "Social_secure_random.bytes: negative length"
  else if n = 0 then ""
  else
    try In_channel.with_open_bin entropy_source (fun ic -> really_input_string ic n)
    with (Sys_error _ | End_of_file) as exn ->
      failwith
        (Printf.sprintf "Social_secure_random.bytes: cannot read %s (%s)" entropy_source
           (Printexc.to_string exn))
