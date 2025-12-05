(lang dune 3.17)
(name ocaml-social-sdk)

(generate_opam_files true)

(source (github makerprism/ocaml-social-sdk))
(license MIT)
(authors "Sabine Schmaltz")
(maintainers "sabine@schmaltz.dev")
(documentation "https://github.com/makerprism/ocaml-social-sdk")
(bug_reports "https://github.com/makerprism/ocaml-social-sdk/issues")

(package
 (name social-core)
 (synopsis "Core interfaces for social media API clients")
 (description "Runtime-agnostic, HTTP-client-agnostic core interfaces for building social media API clients. This package provides CPS-style interfaces that can be implemented with any async runtime (Lwt, Eio, synchronous) and any HTTP client library.")
 (depends
  (ocaml (>= 4.14))
  (dune (>= 3.0))
  yojson
  re))

(package
 (name social-lwt)
 (synopsis "Lwt runtime adapter for social-core")
 (description "Lwt runtime adapter for social-core with Cohttp_lwt_unix HTTP client implementation.")
 (depends
  (ocaml (>= 4.14))
  (dune (>= 3.0))
  (social-core (= :version))
  lwt
  (lwt_ppx (>= 2.0.0))
  cohttp-lwt-unix))

(package
 (name social-twitter-v1)
 (synopsis "Twitter API v1.1 client (OAuth 1.0a)")
 (description "Twitter API v1.1 client with OAuth 1.0a authentication. Supports posting tweets, threads, and media upload.")
 (depends
  (ocaml (>= 4.14))
  (dune (>= 3.0))
  social-core
  yojson
  base64
  ptime
  (cryptokit (>= 1.16))))

(package
 (name social-twitter-v2)
 (synopsis "Twitter API v2 client")
 (description "Twitter API v2 client. Supports posting tweets, threads, media upload, and user lookup.")
 (depends
  (ocaml (>= 4.14))
  (dune (>= 3.0))
  (social-core (= :version))
  yojson
  base64
  digestif
  ptime
  uri))

(package
 (name social-bluesky-v1)
 (synopsis "Bluesky/AT Protocol client")
 (description "Bluesky social network client using the AT Protocol. Supports posting, threads, media upload, and facets (mentions, links, hashtags).")
 (depends
  (ocaml (>= 4.14))
  (dune (>= 3.0))
  (social-core (= :version))
  yojson
  re
  ptime
  uri))

(package
 (name social-linkedin-v2)
 (synopsis "LinkedIn API v2 client")
 (description "LinkedIn API v2 client. Supports posting to personal profiles and company pages, media upload, and article shares.")
 (depends
  (ocaml (>= 4.14))
  (dune (>= 3.0))
  (social-core (= :version))
  yojson
  uri
  ptime
  base64))

(package
 (name social-mastodon-v1)
 (synopsis "Mastodon API client")
 (description "Mastodon API client. Supports posting toots, threads, media upload, and visibility settings.")
 (depends
  (ocaml (>= 4.14))
  (dune (>= 3.0))
  (social-core (= :version))
  yojson
  digestif
  base64))

(package
 (name social-facebook-graph-v21)
 (synopsis "Facebook Graph API v21 client")
 (description "Facebook Graph API v21 client. Supports posting to pages, media upload, stories, and reels.")
 (depends
  (ocaml (>= 4.14))
  (dune (>= 3.0))
  (social-core (= :version))
  yojson
  uri
  ptime
  digestif))

(package
 (name social-instagram-graph-v21)
 (synopsis "Instagram Graph API v21 client")
 (description "Instagram Graph API v21 client. Supports posting to business/creator accounts, media upload, stories, reels, and carousels.")
 (depends
  (ocaml (>= 4.14))
  (dune (>= 3.0))
  (social-core (= :version))
  yojson
  uri
  ptime
  digestif))

(package
 (name social-youtube-data-v3)
 (synopsis "YouTube Data API v3 client")
 (description "YouTube Data API v3 client. Supports video upload, shorts, and channel management.")
 (depends
  (ocaml (>= 4.14))
  (dune (>= 3.0))
  (social-core (= :version))
  yojson
  uri
  ptime
  digestif
  base64))

(package
 (name social-pinterest-v5)
 (synopsis "Pinterest API v5 client")
 (description "Pinterest API v5 client. Supports creating pins, boards, and media upload.")
 (depends
  (ocaml (>= 4.14))
  (dune (>= 3.0))
  (social-core (= :version))
  yojson
  uri
  base64))

(package
 (name social-tiktok-v1)
 (synopsis "TikTok Content Posting API client")
 (description "TikTok Content Posting API client. Supports video upload and posting.")
 (depends
  (ocaml (>= 4.14))
  (dune (>= 3.0))
  (social-core (= :version))
  yojson
  uri
  ptime))
