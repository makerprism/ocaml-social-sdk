(** Example: Advanced Board Management
    
    Demonstrates board creation, selection by name/ID, and listing.
    Features inspired by py3-pinterest (353 stars).
*)

open Pinterest_v5_enhanced

let access_token = "your_access_token_here"

module Pinterest = Make(YourConfig)

(** Create a new board *)
let create_recipe_board () =
  Printf.printf "Creating new board...\n";
  
  Pinterest.create_board
    ~access_token
    ~name:"Healthy Recipes 2024"
    ~description:(Some "Collection of healthy recipes to try this year")
    ~privacy:"PUBLIC"
    (fun board_id ->
      Printf.printf "✓ Created board with ID: %s\n\n" board_id;
      board_id)
    (fun err ->
      Printf.eprintf "✗ Failed to create board: %s\n" err;
      "")

(** Find board by name *)
let find_board_by_name name =
  Printf.printf "Looking for board '%s'...\n" name;
  
  Pinterest.get_board
    ~access_token
    ~board_identifier:name  (* Can be name or ID *)
    (fun board ->
      Printf.printf "✓ Found board:\n";
      Printf.printf "  ID: %s\n" board.id;
      Printf.printf "  Name: %s\n" board.name;
      Printf.printf "  Privacy: %s\n" board.privacy;
      Printf.printf "  Owner: %s\n" board.owner_username;
      (match board.description with
       | Some desc -> Printf.printf "  Description: %s\n" desc
       | None -> ());
      Printf.printf "\n";
      Some board)
    (fun err ->
      Printf.eprintf "✗ Board not found: %s\n\n" err;
      None)

(** List all boards with details *)
let list_all_boards () =
  Printf.printf "Fetching all boards...\n";
  
  Pinterest.get_all_boards
    ~access_token
    ~page_size:25
    (fun boards ->
      Printf.printf "✓ Found %d boards:\n\n" (List.length boards);
      
      List.iteri (fun i board ->
        Printf.printf "%d. %s\n" (i + 1) board.name;
        Printf.printf "   ID: %s\n" board.id;
        Printf.printf "   Privacy: %s\n" board.privacy;
        (match board.created_at with
         | Some date -> Printf.printf "   Created: %s\n" date
         | None -> ());
        Printf.printf "\n"
      ) boards;
      
      boards)
    (fun err ->
      Printf.eprintf "✗ Failed to fetch boards: %s\n" err;
      [])

(** Smart board selection for pinning *)
let select_board_for_pin ~pin_title ~preferred_board_name =
  Printf.printf "Selecting board for pin '%s'...\n" pin_title;
  
  (* Try to find preferred board first *)
  match find_board_by_name preferred_board_name with
  | Some board ->
      Printf.printf "✓ Using preferred board: %s\n" board.name;
      Some board.id
  | None ->
      Printf.printf "Preferred board not found, checking alternatives...\n";
      
      (* Get all boards and select best match *)
      let boards = list_all_boards () in
      
      (* Look for boards with related keywords *)
      let keywords = ["recipe"; "food"; "cooking"; "general"] in
      let find_board_with_keyword keyword =
        List.find_opt (fun b ->
          String.lowercase_ascii b.name 
          |> String.contains keyword
        ) boards
      in
      
      let selected = 
        List.find_map find_board_with_keyword keywords
      in
      
      match selected with
      | Some board ->
          Printf.printf "✓ Selected alternative board: %s\n" board.name;
          Some board.id
      | None ->
          (* Use first board as fallback *)
          match boards with
          | first :: _ ->
              Printf.printf "✓ Using first available board: %s\n" first.name;
              Some first.id
          | [] ->
              Printf.eprintf "✗ No boards available!\n";
              None

(** Create pin with smart board selection *)
let create_pin_smart ~title ~description ~image_url ~preferred_board =
  Printf.printf "\n=== Creating Pin with Smart Board Selection ===\n";
  
  match select_board_for_pin ~pin_title:title ~preferred_board_name:preferred_board with
  | Some board_id ->
      Pinterest.create_pin
        ~access_token
        ~board_id
        ~title
        ~description
        ~media_url:image_url
        ?link:None
        ?alt_text:(Some "Delicious healthy recipe")
        (fun pin_id ->
          Printf.printf "\n✓ Successfully created pin!\n";
          Printf.printf "  Pin ID: %s\n" pin_id;
          Printf.printf "  Board ID: %s\n" board_id)
        (fun err ->
          Printf.eprintf "\n✗ Failed to create pin: %s\n" err)
  | None ->
      Printf.eprintf "Cannot create pin without a board!\n"

(** Main demo *)
let () =
  Printf.printf "\n====================================\n";
  Printf.printf "  Pinterest Board Management Demo\n";
  Printf.printf "====================================\n\n";
  
  (* 1. Create a new board *)
  let new_board_id = create_recipe_board () in
  
  (* 2. List all boards *)
  let _ = list_all_boards () in
  
  (* 3. Find specific board *)
  let _ = find_board_by_name "Healthy Recipes 2024" in
  
  (* 4. Create pin with smart board selection *)
  create_pin_smart
    ~title:"Green Smoothie Bowl"
    ~description:"Nutritious breakfast bowl packed with vitamins"
    ~image_url:"https://example.com/smoothie.jpg"
    ~preferred_board:"Healthy Recipes 2024";
  
  Printf.printf "\n=== Demo Complete ===\n"