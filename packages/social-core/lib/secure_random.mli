(** Deprecated alias of {!Social_secure_random}, kept so consumers using the pre-prefix
    name keep compiling. Use {!Social_secure_random}: social-core is [(wrapped false)], so
    unprefixed module names clash with consumer modules.

    No [@@@deprecated] alert: dune's dev profile turns alerts into errors,
    which would break the consumers this alias exists to keep building.

    @deprecated use {!Social_secure_random}. *)

include module type of struct
  include Social_secure_random
end
