let entropy_source = "/dev/urandom"

let bytes n =
  if n < 0 then invalid_arg "Secure_random.bytes: negative length"
  else if n = 0 then ""
  else
    try In_channel.with_open_bin entropy_source (fun ic -> really_input_string ic n)
    with (Sys_error _ | End_of_file) as exn ->
      failwith
        (Printf.sprintf "Secure_random.bytes: cannot read %s (%s)" entropy_source
           (Printexc.to_string exn))
