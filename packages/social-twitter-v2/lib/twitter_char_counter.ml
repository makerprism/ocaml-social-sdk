(** Twitter-specific character counting module.

    @deprecated moved to [Social_text_counter] in social-core so the shared
    capability table can count text with each platform's strategy. This alias
    keeps the old name; the API is unchanged. *)

include Social_text_counter
