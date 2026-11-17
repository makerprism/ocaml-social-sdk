type location = {
  street : string option;
  city : string option;
  state : string option;
  country : string option;
  zip : string option;
  latitude : float option;
  longitude : float option;
}

type place = {
  id : string;
  name : string option;
  location : location option;
}

let fields = "id,name,location"

let member key = function
  | `Assoc pairs -> Option.value ~default:`Null (List.assoc_opt key pairs)
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ -> `Null

let string_opt = function
  | `String s when String.trim s <> "" -> Some s
  | `String _ | `Null | `Bool _ | `Int _ | `Float _ | `List _ | `Assoc _ -> None

let float_opt = function
  | `Float f -> Some f
  | `Int i -> Some (float_of_int i)
  | `String _ | `Null | `Bool _ | `List _ | `Assoc _ -> None

let coordinates place =
  match place.location with
  | Some { latitude = Some lat; longitude = Some lon; _ } -> Some (lat, lon)
  | Some _ | None -> None

let parse_location = function
  | `Assoc _ as json ->
      Some
        {
          street = string_opt (member "street" json);
          city = string_opt (member "city" json);
          state = string_opt (member "state" json);
          country = string_opt (member "country" json);
          zip = string_opt (member "zip" json);
          latitude = float_opt (member "latitude" json);
          longitude = float_opt (member "longitude" json);
        }
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ -> None

let parse_place json =
  match string_opt (member "id" json) with
  | None -> failwith "place without an id"
  | Some id ->
      {
        id;
        name = string_opt (member "name" json);
        location = parse_location (member "location" json);
      }

let parse_response body =
  match Yojson.Basic.from_string body with
  | exception Yojson.Json_error msg -> Error ("place search response is not JSON: " ^ msg)
  | json -> (
      match member "data" json with
      | `List items -> (
          match List.map parse_place items with
          | places -> Ok places
          | exception Failure msg -> Error msg)
      | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `Assoc _ ->
          Error "place search response has no data array")
