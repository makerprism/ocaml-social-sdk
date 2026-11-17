type coordinates = {
  latitude : float;
  longitude : float;
}

type location = {
  id : string;
  name : string option;
  address : string option;
  city : string option;
  country : string option;
  postal_code : string option;
  coordinates : coordinates option;
}

let fields = "id,name,address,city,country,latitude,longitude,postal_code"

let member key = function
  | `Assoc pairs -> Option.value ~default:`Null (List.assoc_opt key pairs)
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ -> `Null

let string_opt = function
  | `String s when String.trim s <> "" -> Some s
  | `String _ | `Null | `Bool _ | `Int _ | `Float _ | `List _ | `Assoc _ -> None

let id_opt = function
  | `String s when String.trim s <> "" -> Some (String.trim s)
  | `Int i -> Some (string_of_int i)
  | `String _ | `Null | `Bool _ | `Float _ | `List _ | `Assoc _ -> None

let float_opt = function
  | `Float f -> Some f
  | `Int i -> Some (float_of_int i)
  | `String _ | `Null | `Bool _ | `List _ | `Assoc _ -> None

let parse_location json =
  match id_opt (member "id" json) with
  | None -> failwith "location without an id"
  | Some id ->
      let coordinates =
        match float_opt (member "latitude" json), float_opt (member "longitude" json) with
        | Some latitude, Some longitude -> Some { latitude; longitude }
        | None, _ | _, None -> None
      in
      {
        id;
        name = string_opt (member "name" json);
        address = string_opt (member "address" json);
        city = string_opt (member "city" json);
        country = string_opt (member "country" json);
        postal_code = string_opt (member "postal_code" json);
        coordinates;
      }

let parse_response body =
  match Yojson.Basic.from_string body with
  | exception Yojson.Json_error msg -> Error ("location search response is not JSON: " ^ msg)
  | json -> (
      match member "data" json with
      | `List items -> (
          match List.map parse_location items with
          | locations -> Ok locations
          | exception Failure msg -> Error msg)
      | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `Assoc _ ->
          Error "location search response has no data array")
