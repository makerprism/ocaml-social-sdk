(** Pure checks for hosts supplied by remote documents or user input. *)
val is_public_host : string -> bool
(** Accepts syntactically valid public DNS names and canonical public IP
    literals. Rejects private/reserved ranges, alternate numeric IPv4 forms,
    local names, scoped IPv6 and transition addresses. IPv6 literals may be
    bracketed. This does not resolve DNS: HTTP adapters must validate resolved
    addresses and redirects as well, including protection against rebinding. *)
