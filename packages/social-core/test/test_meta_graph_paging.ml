(** Tests for {!Meta_graph_paging}: the page shape Facebook, Instagram and
    Threads list edges return, and the checks a [paging.next] URL must pass
    before a reader attaches credentials to it. *)

let failures = ref 0

let check name condition =
  if condition then Printf.printf "  ✓ %s\n" name
  else begin
    incr failures;
    Printf.printf "  ✗ %s\n" name
  end

let id_of json =
  match json with
  | `Assoc fields -> (
      match List.assoc_opt "id" fields with
      | Some (`String id) -> id
      | Some _ | None -> failwith "missing id")
  | _ -> failwith "not an object"

(* Shape of a Facebook /{post-id}/comments page (Graph API "Paginated
   results"): cursors plus next/previous URLs carrying the token. *)
let middle_page =
  {|{"data":[{"id":"1_2"},{"id":"1_3"}],
     "paging":{"cursors":{"before":"QVFIUm1","after":"QVFIUnd"},
               "next":"https://graph.facebook.com/v25.0/1/comments?access_token=EAAB&limit=2&after=QVFIUnd",
               "previous":"https://graph.facebook.com/v25.0/1/comments?access_token=EAAB&limit=2&before=QVFIUm1"}}|}

(* The last page still has cursors but no next link. *)
let last_page =
  {|{"data":[{"id":"1_4"}],"paging":{"cursors":{"before":"QVFIUm2","after":"QVFIUm2"}}}|}

let test_parse () =
  Printf.printf "parse\n";
  (match Meta_graph_paging.of_body ~item:id_of middle_page with
   | Ok page ->
       check "data" (page.data = [ "1_2"; "1_3" ]);
       check "cursors"
         (page.paging = Some { Meta_graph_paging.before = Some "QVFIUm1"; after = Some "QVFIUnd" });
       check "next url" (Option.is_some page.next_url);
       check "previous url" (Option.is_some page.previous_url);
       check "has_next" (Meta_graph_paging.has_next page);
       check "next_after" (Meta_graph_paging.next_after page = Some "QVFIUnd")
   | Error msg -> check ("middle page parses: " ^ msg) false);
  (match Meta_graph_paging.of_body ~item:id_of last_page with
   | Ok page ->
       check "last page has no next" (not (Meta_graph_paging.has_next page));
       check "last page after cursor is not offered" (Meta_graph_paging.next_after page = None);
       check "last page keeps its cursors" (Option.is_some page.paging)
   | Error msg -> check ("last page parses: " ^ msg) false);
  (match Meta_graph_paging.of_body ~item:id_of {|{"data":[]}|} with
   | Ok page -> check "no paging block" (page.paging = None && page.next_url = None)
   | Error _ -> check "empty page parses" false);
  check "missing data is an error"
    (Result.is_error (Meta_graph_paging.of_body ~item:id_of {|{"error":{}}|}));
  check "bad item is an error"
    (Result.is_error (Meta_graph_paging.of_body ~item:id_of {|{"data":[{"x":1}]}|}));
  check "not JSON is an error"
    (Result.is_error (Meta_graph_paging.of_body ~item:id_of "<html>"))

let test_cursor_query () =
  Printf.printf "cursor_query\n";
  check "first" (Meta_graph_paging.cursor_query First = []);
  check "after" (Meta_graph_paging.cursor_query (After "c") = [ ("after", [ "c" ]) ]);
  check "before" (Meta_graph_paging.cursor_query (Before "c") = [ ("before", [ "c" ]) ]);
  check "page url" (Meta_graph_paging.cursor_query (Page_url "https://x") = [])

let resolve url =
  Meta_graph_paging.resolve_page_url ~allowed_hosts:[ "graph.facebook.com" ]
    ~edge:"comments" url

let test_resolve_page_url () =
  Printf.printf "resolve_page_url\n";
  (match
     resolve
       "https://graph.facebook.com/v25.0/1_2/comments?access_token=EAAB&appsecret_proof=abc&limit=2&after=QVFIUnd&fields=id,message,from%7Bid,name%7D"
   with
   | Ok uri ->
       check "strips access_token" (Uri.get_query_param uri "access_token" = None);
       check "strips appsecret_proof" (Uri.get_query_param uri "appsecret_proof" = None);
       check "keeps the cursor" (Uri.get_query_param uri "after" = Some "QVFIUnd");
       check "keeps fields" (Uri.get_query_param uri "fields" = Some "id,message,from{id,name}");
       check "keeps the path" (Uri.path uri = "/v25.0/1_2/comments")
   | Error msg -> check ("accepts a Graph next URL: " ^ msg) false);
  check "host is case-insensitive"
    (Result.is_ok (resolve "https://GRAPH.facebook.com/v25.0/1/comments?after=x"));
  check "rejects another host"
    (Result.is_error (resolve "https://evil.example/v25.0/1/comments?after=x"));
  check "rejects a look-alike host"
    (Result.is_error (resolve "https://graph.facebook.com.evil.example/v25.0/1/comments"));
  check "rejects http" (Result.is_error (resolve "http://graph.facebook.com/v25.0/1/comments"));
  check "rejects user info"
    (Result.is_error (resolve "https://x@graph.facebook.com/v25.0/1/comments"));
  check "rejects a port" (Result.is_error (resolve "https://graph.facebook.com:8443/v25.0/1/comments"));
  check "rejects another edge"
    (Result.is_error (resolve "https://graph.facebook.com/v25.0/me/accounts?after=x"));
  check "rejects a relative URL" (Result.is_error (resolve "/v25.0/1/comments?after=x"))

let () =
  Printf.printf "Running Meta_graph_paging tests\n\n";
  test_parse ();
  test_cursor_query ();
  test_resolve_page_url ();
  if !failures > 0 then begin
    Printf.printf "\n%d check(s) failed\n" !failures;
    exit 1
  end
  else Printf.printf "\nAll Meta_graph_paging checks passed\n"
