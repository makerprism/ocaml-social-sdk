let () =
  List.iter (fun host -> assert (not (Social_public_host.is_public_host host)))
    [ ""; "localhost"; "localhost."; "x.local"; "x.internal"; "[example.org]";
      "2130706433"; "0x7f000001"; "127.1"; "0177.0.0.1"; "127.0.0.1";
      "0.0.0.0"; "0.1.2.3"; "10.0.0.1"; "100.64.0.1"; "100.127.255.255";
      "169.254.169.254"; "172.16.0.1"; "172.31.255.255"; "192.168.0.1";
      "192.0.2.1"; "198.18.0.1"; "198.51.100.1"; "203.0.113.1";
      "224.0.0.1"; "240.0.0.1"; "255.255.255.255"; "::"; "::1";
      "[fc00::1]"; "fdff::1"; "fe80::1"; "fe80::1%eth0"; "ff0e::1";
      "::ffff:127.0.0.1"; "2001:db8::1"; "2002:7f00:1::";
      "host.example/path"; "user@host.example"; "host.example:443" ];
  List.iter (fun host -> assert (Social_public_host.is_public_host host))
    [ "example.org"; "EXAMPLE.ORG."; "pds.example.org"; "8.8.8.8";
      "100.63.255.255"; "100.128.0.1"; "172.15.255.255"; "172.32.0.1";
      "2606:4700:4700::1111"; "[2606:4700:4700::1111]" ]
