let blocked_v4 =
  List.map Ipaddr.V4.Prefix.of_string_exn
    [ "0.0.0.0/8"; "10.0.0.0/8"; "100.64.0.0/10"; "127.0.0.0/8";
      "169.254.0.0/16"; "172.16.0.0/12"; "192.0.0.0/24";
      "192.0.2.0/24"; "192.88.99.0/24"; "192.168.0.0/16";
      "198.18.0.0/15"; "198.51.100.0/24"; "203.0.113.0/24";
      "224.0.0.0/3" ]

let blocked_v6 =
  List.map Ipaddr.V6.Prefix.of_string_exn
    [ "2001::/23"; "2001:db8::/32"; "2002::/16"; "3fff::/20" ]

let alphanumeric = function
  | 'a' .. 'z' | '0' .. '9' -> true
  | _ -> false

let dns_label label =
  let length = String.length label in
  length > 0 && length <= 63
  && alphanumeric label.[0] && alphanumeric label.[length - 1]
  && String.for_all (fun c -> alphanumeric c || c = '-') label

let public_dns host =
  let labels = String.split_on_char '.' host in
  let numeric label =
    String.for_all (function '0' .. '9' -> true | _ -> false) label
    || String.starts_with ~prefix:"0x" label
  in
  List.length labels >= 2 && List.for_all dns_label labels
  && (match List.rev labels with
      | last :: _ -> not (numeric last)
      | [] -> false)
  && not (List.exists (fun suffix ->
    String.equal host suffix || String.ends_with ~suffix:("." ^ suffix) host)
    [ "localhost"; "local"; "internal"; "home"; "lan"; "localdomain" ])

let is_public_host raw =
  let host = String.lowercase_ascii raw in
  let length = String.length host in
  let host =
    if length > 1 && host.[0] = '[' && host.[length - 1] = ']' then
      String.sub host 1 (length - 2)
    else if length > 0 && host.[length - 1] = '.' then
      String.sub host 0 (length - 1)
    else host
  in
  String.length host <= 253 && not (String.contains host '%')
  && (not (String.starts_with ~prefix:"[" raw) || String.contains host ':')
  && match Ipaddr.of_string host with
     | Ok (Ipaddr.V4 ip) ->
         String.equal host (Ipaddr.V4.to_string ip)
         && not (List.exists (Ipaddr.V4.Prefix.mem ip) blocked_v4)
     | Ok (Ipaddr.V6 ip) ->
         Ipaddr.V6.Prefix.mem ip Ipaddr.V6.Prefix.global_unicast_001
         && not (List.exists (Ipaddr.V6.Prefix.mem ip) blocked_v6)
     | Error (`Msg _) -> public_dns host
