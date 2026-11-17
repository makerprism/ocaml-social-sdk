(** Deprecated alias of {!Social_oauth_pkce}, kept so consumers using the pre-prefix
    name keep compiling. Use {!Social_oauth_pkce}: social-core is [(wrapped false)], so
    unprefixed module names clash with consumer modules.

    No [@@@deprecated] alert: dune's dev profile turns alerts into errors,
    which would break the consumers this alias exists to keep building.

    @deprecated use {!Social_oauth_pkce}. *)

include module type of struct
  include Social_oauth_pkce
end
