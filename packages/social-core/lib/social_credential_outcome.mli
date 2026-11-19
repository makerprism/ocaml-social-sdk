(** Acquisition facts, not account-health policy. Kept in social-core so OAuth
    and session-based providers can share the result without a refresh-package
    dependency. See {{:https://www.rfc-editor.org/rfc/rfc6749#section-6}OAuth
    refresh semantics} and {{:https://docs.bsky.app/docs/api/com-atproto-server-refresh-session}AT Protocol session refresh}.
    A provider may return the same token text after renewal; token equality is
    not evidence of reuse. *)
type 'a t = Reused of 'a | Renewed of 'a
(** [Reused] performed no credential write. [Renewed] follows successful
    renewal and persistence. Coordinated APIs deliver it only after their
    caller-supplied transaction boundary completes successfully. Neither
    constructor claims account health. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** Preserve the acquisition fact when projecting a token or auth context. *)

val value : 'a t -> 'a
(** Compatibility projection for callers that do not need the acquisition fact. *)
