# Mastodon Package Test Improvements

Based on analysis of popular Mastodon client libraries (Mastodon.py, masto.js), here are recommended test improvements for our implementation.

## Current Test Coverage

### ✅ Already Tested (12 tests)
1. Post simple status
2. Post status with visibility and spoiler options
3. Post thread
4. Delete status
5. Edit status
6. Favorite status
7. Bookmark status
8. Validate content
9. Validate poll
10. Register OAuth app
11. Get OAuth URL
12. Exchange authorization code for token

## Recommended Additional Tests

### High Priority (Integration & Edge Cases)

#### 1. **Status Context & Threading**
```ocaml
(** Test: Fetch status context - ancestors and descendants *)
let test_status_context () =
  (* Post s1, s2 (reply to s1), s3 (reply to s2) *)
  (* Fetch context of s2 *)
  (* Verify s1 is in ancestors, s3 is in descendants *)
```
**Reference:** masto.js `tests/rest/v1/statuses.spec.ts:62-82`
**Why:** Ensures threading works correctly - critical for conversation features

#### 2. **Idempotency Key Testing**
```ocaml
(** Test: Same idempotency key returns same status ID *)
let test_idempotency_key () =
  (* Post with UUID idempotency key twice *)
  (* Verify both return same status ID *)
```
**Reference:** masto.js `tests/rest/v1/statuses.spec.ts:39-61`
**Why:** Prevents duplicate posts on network errors - critical for reliability

#### 3. **Reblog (Boost) Operations**
```ocaml
(** Test: Reblog and unreblog a status *)
let test_reblog_status () =
  (* Alice posts status *)
  (* Bob reblogs it *)
  (* Verify status.reblogged = true *)
  (* Fetch reblogged_by list, verify Bob is in it *)
  (* Bob unreblogs *)
  (* Verify status.reblogged = false *)
```
**Reference:** masto.js `tests/rest/v1/statuses.spec.ts:162-184`
**Why:** We have boost functionality but no tests for it

#### 4. **Pin/Unpin Status**
```ocaml
(** Test: Pin and unpin a status to profile *)
let test_pin_status () =
  (* Post status with private visibility *)
  (* Pin it *)
  (* Verify status.pinned = true *)
  (* Unpin it *)
  (* Verify status.pinned = false *)
```
**Reference:** masto.js `tests/rest/v1/statuses.spec.ts:186-197`
**Why:** Common feature for highlighting important posts

#### 5. **Mute/Unmute Conversation**
```ocaml
(** Test: Mute and unmute a conversation *)
let test_mute_status () =
  (* Post status *)
  (* Mute conversation *)
  (* Verify status.muted = true *)
  (* Unmute conversation *)
  (* Verify status.muted = false *)
```
**Reference:** masto.js `tests/rest/v1/statuses.spec.ts:150-161`
**Why:** Users need to mute notifications from specific threads

#### 6. **Media Upload with Focus Points**
```ocaml
(** Test: Upload media with focus point for cropping *)
let test_media_focus_point () =
  (* Upload image with focus point (0.5, 0.3) *)
  (* Update media with new focus point (-0.2, 0.8) *)
  (* Verify focus point was updated *)
```
**Why:** We support focus points but don't test them

#### 7. **Poll Creation & Validation**
```ocaml
(** Test: Create status with poll *)
let test_post_with_poll () =
  (* Post status with 3-option poll, 3600s expiry *)
  (* Verify poll was created with correct options *)
  (* Vote on poll option *)
  (* Verify vote was recorded *)
```
**Why:** We have poll support but only validate, never test actual posting

#### 8. **Scheduled Status**
```ocaml
(** Test: Schedule status for future posting *)
let test_scheduled_status () =
  (* Schedule status for 1 hour from now *)
  (* Verify scheduled_at is correct *)
  (* Fetch scheduled statuses list *)
  (* Verify our status is in the list *)
  (* Cancel scheduled status *)
```
**Why:** We support scheduling but don't test it

### Medium Priority (Error Handling & Edge Cases)

#### 9. **Character Limit Validation**
```ocaml
(** Test: Reject posts exceeding character limit *)
let test_character_limit () =
  (* Attempt to post 501 characters (default limit is 500) *)
  (* Verify validation error is returned *)
```
**Why:** Prevents failed API calls

#### 10. **Empty Status Rejection**
```ocaml
(** Test: Reject empty status without media *)
let test_empty_status () =
  (* Attempt to post empty text with no media *)
  (* Verify validation error *)
```
**Why:** Mastodon API rejects empty statuses

#### 11. **Invalid Poll Options**
```ocaml
(** Test: Reject polls with < 2 or > 4 options *)
let test_invalid_poll () =
  (* Test poll with 1 option - should fail *)
  (* Test poll with 5 options - should fail *)
  (* Test poll with empty option text - should fail *)
```
**Why:** Prevents invalid API calls

#### 12. **Media Upload Failure Handling**
```ocaml
(** Test: Handle media upload failure gracefully *)
let test_media_upload_failure () =
  (* Mock HTTP failure for media upload *)
  (* Attempt to post with media *)
  (* Verify error callback is called *)
```
**Why:** Network errors happen - need graceful handling

#### 13. **OAuth Token Expiry**
```ocaml
(** Test: Handle expired OAuth token *)
let test_token_expiry () =
  (* Mock 401 Unauthorized response *)
  (* Attempt to post status *)
  (* Verify error callback mentions authentication *)
```
**Why:** Mastodon tokens don't expire, but instance issues can cause 401s

#### 14. **Rate Limiting**
```ocaml
(** Test: Handle 429 Too Many Requests *)
let test_rate_limit () =
  (* Mock 429 response with Retry-After header *)
  (* Attempt to post status *)
  (* Verify error mentions rate limiting *)
```
**Why:** All API clients should handle rate limits

### Low Priority (Nice to Have)

#### 15. **Status Source Fetching**
```ocaml
(** Test: Fetch original markdown/plaintext source of status *)
let test_status_source () =
  (* Post status with markdown "**bold**" *)
  (* Fetch status source *)
  (* Verify source.text contains original markdown *)
```
**Reference:** masto.js `tests/rest/v1/statuses.spec.ts:18-20`
**Why:** Useful for edit functionality

#### 16. **Status History**
```ocaml
(** Test: Fetch edit history of a status *)
let test_status_history () =
  (* Post status with text "v1" *)
  (* Edit to "v2" *)
  (* Fetch edit history *)
  (* Verify history[0].content contains "v1" *)
```
**Reference:** masto.js `tests/rest/v1/statuses.spec.ts:30-37`
**Why:** Shows edit audit trail

#### 17. **Visibility Validation**
```ocaml
(** Test: Verify each visibility level works *)
let test_all_visibility_levels () =
  (* Post with Public visibility *)
  (* Post with Unlisted visibility *)
  (* Post with Private visibility *)
  (* Post with Direct visibility *)
  (* Verify all succeed *)
```
**Why:** Ensure all visibility options actually work

#### 18. **Language Specification**
```ocaml
(** Test: Post with language code *)
let test_language_specification () =
  (* Post status with language = "ja" *)
  (* Verify status.language = "ja" *)
```
**Why:** Multi-language support testing

## Test Infrastructure Improvements

### 1. **Use Real Mastodon Instance (Like masto.js)**
masto.js tests run against a real Mastodon instance using Docker Compose:
```yaml
# compose.yml
services:
  mastodon:
    image: ghcr.io/mastodon/mastodon:latest
    # ... config
```

**Benefits:**
- Catches real API incompatibilities
- Tests actual network behavior
- Validates JSON serialization/deserialization
- Ensures OAuth flow works end-to-end

**Implementation:**
```bash
# In CI/CD or local testing
docker-compose up -d mastodon
# Wait for instance to be ready
# Run tests against http://localhost:3000
# Clean up
docker-compose down
```

### 2. **Test Fixtures & Factories**
Create reusable test data builders:
```ocaml
module TestData = struct
  let make_status ?(text="test status") ?(visibility=Public) () = {
    text;
    visibility;
    media_urls = [];
    sensitive = false;
    spoiler_text = None;
    (* ... defaults *)
  }
  
  let make_poll ?(options=["Yes"; "No"]) ?(expires_in=3600) () = {
    options = List.map (fun t -> {title = t}) options;
    expires_in;
    multiple = false;
    hide_totals = false;
  }
end
```

### 3. **Async Test Framework**
Use Alcotest or OUnit2 for better test organization:
```ocaml
(* Using Alcotest *)
let test_post_status () =
  Alcotest.(check string) "post_id is returned" 
    "54321" 
    (Mastodon.post_status ~text:"test" ())

let suite = [
  "status", [
    test_case "post status" `Quick test_post_status;
    test_case "edit status" `Quick test_edit_status;
    (* ... *)
  ];
  "oauth", [
    (* ... *)
  ];
]

let () = Alcotest.run "Mastodon" suite
```

### 4. **Coverage Reporting**
Add bisect_ppx for code coverage:
```ocaml
(* dune *)
(library
  (name social_mastodon_v1)
  (instrumentation (backend bisect_ppx)))
```

Then generate coverage reports:
```bash
dune runtest --instrument-with bisect_ppx
bisect-ppx-report html
# Opens coverage report in browser
```

## Priority Order for Implementation

1. **Quick Wins (1-2 hours):**
   - Add idempotency key test
   - Add reblog/unreblog test
   - Add pin/unpin test
   - Add character limit validation test

2. **Medium Effort (3-4 hours):**
   - Add status context test
   - Add poll creation test
   - Add scheduled status test
   - Add error handling tests (rate limit, auth failure)

3. **Infrastructure (1-2 days):**
   - Set up Docker Compose with real Mastodon instance
   - Migrate to Alcotest test framework
   - Add code coverage reporting
   - Create test fixtures/factories

4. **Complete Coverage (3-5 days):**
   - Implement all remaining tests
   - Achieve >90% code coverage
   - Add integration test suite
   - Document test patterns in README

## Example Test from masto.js (for reference)

```typescript
// tests/rest/v1/statuses.spec.ts
it("reblogs and unreblog a status", async () => {
  await using alice = await sessions.acquire();
  await using bob = await sessions.acquire();

  const { id: statusId } = await alice.rest.v1.statuses.create({
    status: "status",
  });

  try {
    let status = await bob.rest.v1.statuses.$select(statusId).reblog();
    expect(status.reblogged).toBe(true);

    const reblogs = await alice.rest.v1.statuses
      .$select(statusId)
      .rebloggedBy.list();
    expect(reblogs).toContainEqual(bob.account);

    status = await bob.rest.v1.statuses.$select(statusId).unreblog();
    expect(status.reblogged).toBe(false);
  } finally {
    await alice.rest.v1.statuses.$select(statusId).remove();
  }
});
```

## Summary

**Current:** 12 basic tests with mocked HTTP  
**Recommended:** 30+ tests including:
- Integration tests with real Mastodon instance
- Edge case handling (errors, rate limits)
- All major features (polls, scheduling, media, etc.)
- Comprehensive OAuth flow testing

**Expected Outcome:**
- Increased confidence in production reliability
- Catch breaking changes in Mastodon API updates
- Better documentation through test examples
- Easier onboarding for contributors
