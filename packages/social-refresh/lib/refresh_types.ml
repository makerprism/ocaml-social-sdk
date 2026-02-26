type refresh_decision =
  | Skip
  | Refresh_required

type refresh_error =
  | Missing_refresh_token
  | Missing_configuration of string
  | Credential_load_failed of string
  | Credential_persist_failed of string
  | Health_update_failed of string
  | Refresh_failed of string

type refresh_outcome =
  | Token_reused of string
  | Token_refreshed of {
      access_token : string;
      refresh_token : string option;
      expires_at : string option;
      token_type : string;
    }

type policy = {
  refresh_buffer_seconds : int;
  treat_missing_expiry_as_valid : bool;
}

type refreshed_token = {
  access_token : string;
  refresh_token : string option;
  expires_at : string option;
  token_type : string option;
}

let default_policy = {
  refresh_buffer_seconds = 600;
  treat_missing_expiry_as_valid = true;
}

let to_social_error = function
  | Missing_refresh_token -> Error_types.Auth_error Error_types.Missing_credentials
  | Missing_configuration msg -> Error_types.Auth_error (Error_types.Refresh_failed msg)
  | Refresh_failed msg -> Error_types.Auth_error (Error_types.Refresh_failed msg)
  | Credential_load_failed msg
  | Credential_persist_failed msg
  | Health_update_failed msg ->
      Error_types.Network_error (Error_types.Connection_failed msg)
