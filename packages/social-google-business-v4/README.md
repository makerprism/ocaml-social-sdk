# social-google-business-v4

Google Business Profile API client for OCaml. Supports posting updates, events, and offers to business locations.

## Features

- Google OAuth 2.0 with PKCE (S256)
- Post standard updates, events, and offers
- Call-to-action buttons (Book, Order, Shop, Learn More, Sign Up, Get Offer, Call)
- Photo attachments via source URL
- Account and location discovery
- Automatic token refresh via social-refresh

## OAuth Scopes

- `business.manage` - Required for posting and location management
- `userinfo.profile` - User profile information
- `userinfo.email` - User email address

## Post Types

- **Standard** - Text update with optional photo
- **Event** - Event with title, schedule, and optional CTA
- **Offer** - Promotional offer with coupon code and terms

## API Endpoints

- Account management: `mybusinessaccountmanagement.googleapis.com/v1`
- Business information: `mybusinessbusinessinformation.googleapis.com/v1`
- Local posts: `mybusiness.googleapis.com/v4/{location}/localPosts`

## Usage

```ocaml
module Config = struct
  module Http = My_http_client
  let get_env = Sys.getenv_opt
  (* ... other CONFIG functions ... *)
end

module GoogleBusiness = Social_google_business_v4.Make(Config)

(* Post an update *)
let () =
  GoogleBusiness.post_single
    ~account_id:"my_account"
    ~text:"Check out our new menu!"
    ~media_urls:["https://example.com/photo.jpg"]
    (fun outcome -> match outcome with
     | Error_types.Success post_name -> print_endline ("Posted: " ^ post_name)
     | Error_types.Failure err -> print_endline ("Error: " ^ Error_types.error_to_string err)
     | _ -> ())
```
