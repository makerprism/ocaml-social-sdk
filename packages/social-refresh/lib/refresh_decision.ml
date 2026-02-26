open Refresh_types

let decide ~policy ~expires_at =
  match expires_at with
  | None ->
      if policy.treat_missing_expiry_as_valid then Skip else Refresh_required
  | Some _ when Refresh_time.is_expired_with_buffer ~buffer_seconds:policy.refresh_buffer_seconds expires_at ->
      Refresh_required
  | Some _ -> Skip
