(** Deprecated alias of {!Social_provider_error_detail}, kept so consumers using the pre-prefix
    name keep compiling. Use {!Social_provider_error_detail}: social-core is [(wrapped false)], so
    unprefixed module names clash with consumer modules.

    No [@@@deprecated] alert: dune's dev profile turns alerts into errors,
    which would break the consumers this alias exists to keep building.

    @deprecated use {!Social_provider_error_detail}. *)

include module type of struct
  include Social_provider_error_detail
end
