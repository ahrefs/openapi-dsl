[@@@warning "-30"] (* disable duplicate field name warnings *)

open Jsonkit.Primitives
exception Json_decode_error of string

type url = string [@@deriving json]

type json = Jsonkit.t [@@deriving json]

type ref_ = string [@@deriving json]

module Or_ref = struct
  type 'a t =
    | Obj of 'a
    | Ref of ref_
    | Ref_with_metadata of ref_ * (string * json) list
  [@@deriving json]

  let to_json a_to_json = function
    | Obj obj -> a_to_json obj
    | Ref ref_ -> `Assoc [ "$ref", ref__to_json ref_ ]
    | Ref_with_metadata (ref_, fields) -> `Assoc (("$ref", ref__to_json ref_) :: fields)

  let of_json a_of_json = function
    | `Assoc fields as obj ->
      (match List.assoc_opt "$ref" fields with
      | Some ref_ ->
        let metadata = List.remove_assoc "$ref" fields in
        if metadata = [] then Ref (ref__of_json ref_) else Ref_with_metadata (ref__of_json ref_, metadata)
      | None -> Obj (a_of_json obj))
    | obj -> Obj (a_of_json obj)

  let obj x = Obj x
  let ref s = Ref s
end

module JsonObject = struct
  type 'a t = (string * 'a) list

  let to_json value_to_json list = `Assoc (List.map (fun (k, v) -> k, value_to_json v) list)

  let of_json value_of_json = function
    | `Assoc pairs -> List.map (fun (k, v) -> k, value_of_json v) pairs
    | _ -> raise @@ Json_decode_error "Expected JSON object"
end

type 'a or_ref = 'a Or_ref.t [@@deriving json]
type 'a json_object = 'a JsonObject.t [@@deriving json]

type typ =
  | Integer [@name "integer"]
  | Number [@name "number"]
  | String [@name "string"]
  | Boolean [@name "boolean"]
  | Object [@name "object"]
  | Array [@name "array"]
  | Null [@name "null"]
[@@deriving json] [@@json.compact_variants]

(* serialize a single-element list as a bare string and a multi-element list as an array.
   The list form is typically only used for nullable types, e.g. ["string", "null"]. *)
let typ_list_to_json = function
  | [ t ] -> typ_to_json t
  | ts -> `List (List.map typ_to_json ts)

let typ_list_of_json = function
  | `List ts -> List.map typ_of_json ts
  | other -> [ typ_of_json other ]

type format =
  [ `Int32 [@name "int32"]
  | `Int64 [@name "int64"]
  | `Float [@name "float"]
  | `Double [@name "double"]
  | `Date [@name "date"]
  | `Datetime [@name "date-time"]
  | `Domain [@name "domain"]
  | `Url [@name "url"]
  ]
[@@deriving json] [@@json.compact_variants]

type parameter_location =
  [ `Query [@name "query"]
  | `Querystring [@name "querystring"]
  | `Header [@name "header"]
  | `Cookie [@name "cookie"]
  | `Path [@name "path"]
  ]
[@@deriving json] [@@json.compact_variants]

type location_base =
  [ `Query [@name "query"]
  | `Header [@name "header"]
  | `Cookie [@name "cookie"]
  ]
[@@deriving json] [@@json.compact_variants]

type style =
  | Matrix [@name "matrix"]
  | Label [@name "label"]
  | Form [@name "form"]
  | Simple [@name "simple"]
  | SpaceDelimited [@name "spaceDelimited"]
  | PipeDelimited [@name "pipeDelimited"]
  | DeepObject [@name "deepObject"]
  | Cookie [@name "cookie"]
[@@deriving json] [@@json.compact_variants]

let equal_bool = Bool.equal

type schema = Jsonkit.Jsonschema.t

let schema_to_json (s : schema) : json = s
let schema_of_json : json -> schema = function
  | (`Assoc _ | `Bool _) as s -> s
  | _ -> raise (Json_decode_error "Expected a schema object or boolean")

module Schema = struct
  let get key = function
    | `Assoc fields -> List.assoc_opt key fields
    | _ -> None
  let annotation_order =
    [
      "title", 0;
      "description", 1;
      "default", 2;
      "discriminator", 3;
      "readOnly", 4;
      "writeOnly", 5;
      "xml", 6;
      "externalDocs", 7;
      "examples", 8;
      "deprecated", 9;
    ]

  let set key value = function
    | `Assoc fields ->
      let fields =
        match value with
        | None -> List.remove_assoc key fields
        | Some value when List.mem_assoc key fields ->
          List.map (fun (name, old) -> name, if String.equal name key then value else old) fields
        | Some value ->
          (* New annotations follow the DSL's order; existing keywords stay in place. *)
          let rank = List.assoc_opt key annotation_order in
          let rec insert = function
            | [] -> [ key, value ]
            | (name, _) :: _ as fields
              when match rank, List.assoc_opt name annotation_order with
                   | Some rank, Some other -> rank < other
                   | _ -> false ->
              (key, value) :: fields
            | field :: rest -> field :: insert rest
          in
          insert fields
      in
      `Assoc fields
    | _ -> invalid_arg "Cannot set a keyword on a non-object schema"
  let remove key = function
    | `Assoc fields -> `Assoc (List.remove_assoc key fields)
    | s -> s
  let properties s =
    match get "properties" s with
    | Some (`Assoc p) -> p
    | _ -> []
  let resource s =
    match get "$defs" s, get "$id" s with
    | Some _, None ->
      let id = Digest.to_hex (Digest.string (Yojson.Basic.to_string s)) in
      set "$id" (Some (`String ("urn:ahrefs:jsonschema:" ^ id))) s
    | _ -> s

  let types s =
    match get "type" s with
    | Some t -> typ_list_of_json t
    | None -> []
  let string key s =
    match get key s with
    | Some (`String s) -> Some s
    | _ -> None
  let strings key s =
    match get key s with
    | Some (`List xs) ->
      List.filter_map
        (function
          | `String s -> Some s
          | _ -> None)
        xs
    | _ -> []
  let child_kind = function
    | "properties" | "patternProperties" | "$defs" | "dependentSchemas" -> `Map
    | "allOf" | "anyOf" | "oneOf" | "prefixItems" -> `List
    | "items" | "additionalProperties" | "unevaluatedProperties" | "unevaluatedItems" | "contains" | "propertyNames"
    | "not" | "if" | "then" | "else" ->
      `Single
    | _ -> `None

  let map_children f = function
    | `Assoc fields ->
      `Assoc
        (List.map
           (fun (key, value) ->
             let value =
               match child_kind key, value with
               | `Map, `Assoc schemas -> `Assoc (List.map (fun (name, s) -> name, f s) schemas)
               | `List, `List schemas -> `List (List.map f schemas)
               | `Single, s -> f s
               | _ -> value
             in
             key, value)
           fields)
    | s -> s

  let fold_children f acc = function
    | `Assoc fields ->
      List.fold_left
        (fun acc (key, value) ->
          match child_kind key, value with
          | `Map, `Assoc schemas -> List.fold_left (fun acc (_, s) -> f acc s) acc schemas
          | `List, `List schemas -> List.fold_left f acc schemas
          | `Single, s -> f acc s
          | _ -> acc)
        acc fields
    | _ -> acc
end

type extensions = (string * json) list

type 'a extensible = {
  value : 'a;
  extensions : extensions;
}

let validate_extensions extensions =
  let names = List.map fst extensions in
  if not (List.for_all (String.starts_with ~prefix:"x-") names) then
    raise (Json_decode_error "Specification extension names must start with x-");
  if List.length names <> List.length (List.sort_uniq String.compare names) then
    raise (Json_decode_error "Duplicate specification extension")

let extend ?(extensions = []) value =
  validate_extensions extensions;
  { value; extensions }

let merge_extensions left right = List.filter (fun (key, _) -> not (List.mem_assoc key right)) left @ right

let extensible_to_json to_json { value; extensions } =
  validate_extensions extensions;
  match to_json value with
  | `Assoc fields ->
    if extensions <> [] && List.exists (fun (key, _) -> List.mem_assoc key extensions) fields then
      invalid_arg "Specification extensions collide with object fields";
    `Assoc (fields @ extensions)
  | _ -> invalid_arg "An extensible OpenAPI object must encode as an object"

let extensible_of_json of_json = function
  | `Assoc fields ->
    let extensions, fields = List.partition (fun (key, _) -> String.starts_with ~prefix:"x-" key) fields in
    extend ~extensions (of_json (`Assoc fields))
  | _ -> raise (Json_decode_error "Expected an extensible OpenAPI object")

(* Unlike [json option], this preserves an explicitly present JSON null. *)
type optional_json =
  | Absent
  | Present of json
let optional_json_to_json = function
  | Absent -> `Null
  | Present value -> value
let optional_json_of_json value = Present value
let equal_optional_json a b =
  match a, b with
  | Absent, Absent -> true
  | Present a, Present b -> Yojson.Basic.equal a b
  | Absent, Present _ | Present _, Absent -> false

type content_type =
  | OctetStream
  | PlainText
  | Json
  | Custom of string

let content_type_to_json = function
  | OctetStream -> `String "application/octet-stream"
  | PlainText -> `String "text/plain"
  | Json -> `String "application/json"
  | Custom value -> `String value

let content_type_of_json = function
  | `String "application/octet-stream" -> OctetStream
  | `String "text/plain" -> PlainText
  | `String "application/json" -> Json
  | `String value -> Custom value
  | _ -> raise (Json_decode_error "Expected a content type string")

type xml_node_type =
  [ `Element [@name "element"]
  | `Attribute [@name "attribute"]
  | `Text [@name "text"]
  | `Cdata [@name "cdata"]
  | `None [@name "none"]
  ]
[@@deriving json] [@@json.compact_variants]

type path_item = path_item_fields extensible

and path_item_fields = {
  ref_ : ref_ option; [@option] [@key "$ref"] [@drop_default]
  summary : string option; [@option] [@drop_default]
  description : string option; [@option] [@drop_default]
  get : operation option; [@option] [@drop_default]
  put : operation option; [@option] [@drop_default]
  post : operation option; [@option] [@drop_default]
  delete : operation option; [@option] [@drop_default]
  options : operation option; [@option] [@drop_default]
  head : operation option; [@option] [@drop_default]
  patch : operation option; [@option] [@drop_default]
  trace : operation option; [@option] [@drop_default]
  query : operation option; [@option] [@drop_default]
  additionalOperations : operation json_object option; [@option] [@drop_default]
  servers : server list option; [@option] [@drop_default]
  parameters : parameter or_ref list option; [@option] [@drop_default]
}

and operation = operation_fields extensible

and operation_fields = {
  tags : string list option; [@option] [@drop_default]
  summary_ : string option; [@option] [@key "summary"] [@drop_default]
  description_ : string option; [@option] [@key "description"] [@drop_default]
  external_docs : external_docs option; [@option] [@key "externalDocs"] [@drop_default]
  operation_id : string option; [@option] [@key "operationId"] [@drop_default]
  parameters_ : parameter or_ref list option; [@option] [@key "parameters"] [@drop_default]
  request_body : request_body or_ref option; [@option] [@key "requestBody"] [@drop_default]
  responses : responses option; [@option] [@drop_default]
  callbacks : callback or_ref json_object option; [@option] [@drop_default]
  deprecated : bool; [@default false] [@drop_default]
  security : security_requirement list option; [@option] [@drop_default]
  servers_ : server list option; [@option] [@key "servers"] [@drop_default]
}

and external_docs = external_docs_fields extensible

and external_docs_fields = {
  description : string option; [@option] [@drop_default]
  url : url;
}

and parameter = parameter_fields extensible

and parameter_fields = {
  (* fixed fields *)
  description : string option; [@option] [@drop_default]
  required : bool; [@default false]
  deprecated : bool; [@default false] [@drop_default]
  allow_empty_value : bool option; [@option] [@key "allowEmptyValue"] [@drop_default]
  example : optional_json; [@default Absent] [@drop_default]
  examples : example or_ref json_object option; [@option] [@drop_default]
  (* for use with [schema] *)
  style : style option; [@option] [@drop_default]
  explode : bool option; [@option] [@drop_default]
  allow_reserved : bool; [@default false] [@key "allowReserved"] [@drop_default]
  schema : schema option; [@option] [@drop_default]
  content : media_type or_ref json_object option; [@option] [@drop_default]
  name : string;
  location : parameter_location; [@key "in"]
}

and request_body = request_body_fields extensible

and request_body_fields = {
  description : string option; [@option] [@drop_default]
  content : media_type or_ref json_object;
  required : bool; [@default false] [@drop_default]
}

and media_type = media_type_fields extensible

and media_type_fields = {
  schema : schema option; [@option] [@drop_default]
  example : optional_json; [@default Absent] [@drop_default]
  examples : example or_ref json_object option; [@option] [@drop_default]
  encoding : encoding json_object option; [@option] [@drop_default]
  prefix_encoding : encoding list option; [@option] [@key "prefixEncoding"] [@drop_default]
  item_encoding : encoding option; [@option] [@key "itemEncoding"] [@drop_default]
  item_schema : schema option; [@option] [@key "itemSchema"] [@drop_default]
}

and encoding = encoding_fields extensible

and encoding_fields = {
  content_type : content_type option; [@option] [@key "contentType"] [@drop_default]
  headers : header or_ref json_object option; [@option] [@drop_default]
  encoding : encoding json_object option; [@option] [@drop_default]
  prefix_encoding : encoding list option; [@option] [@key "prefixEncoding"] [@drop_default]
  item_encoding : encoding option; [@option] [@key "itemEncoding"] [@drop_default]
  style : style option; [@option] [@drop_default]
  explode : bool option; [@option] [@drop_default]
  allow_reserved : bool option; [@option] [@key "allowReserved"] [@drop_default]
}

and response = response_fields extensible

and response_fields = {
  summary : string option; [@option] [@drop_default]
  description : string option; [@option] [@drop_default]
  headers : header or_ref json_object option; [@option] [@drop_default]
  content : media_type or_ref json_object option; [@option] [@drop_default]
  links : link or_ref json_object option; [@option] [@drop_default]
}

and responses = response or_ref json_object extensible

and callback = path_item json_object extensible [@@deriving json]

and example = example_fields extensible

and example_fields = {
  summary : string option; [@option] [@drop_default]
  description : string option; [@option] [@drop_default]
  value : optional_json; [@default Absent] [@drop_default]
  data_value : optional_json; [@default Absent] [@key "dataValue"] [@drop_default]
  serialized_value : string option; [@option] [@key "serializedValue"] [@drop_default]
  external_value : string option; [@key "externalValue"] [@option] [@drop_default]
}

and link = link_fields extensible

and link_fields = {
  operation_ref : string option; [@option] [@key "operationRef"] [@drop_default]
  operation_id : string option; [@option] [@key "operationId"] [@drop_default]
  parameters : json json_object; [@default []] [@drop_default_if_json_equal]
  request_body : optional_json; [@default Absent] [@key "requestBody"] [@drop_default]
  description : string option; [@option] [@drop_default]
  server : server option; [@option] [@drop_default]
}

and header = header_fields extensible

and header_fields = {
  description : string option; [@option] [@drop_default]
  required : bool; [@default false] [@drop_default]
  deprecated : bool; [@default false] [@drop_default]
  style : style option; [@option] [@drop_default]
  explode : bool option; [@option] [@drop_default]
  schema : schema option; [@option] [@drop_default]
  content : media_type or_ref json_object option; [@option] [@drop_default]
  example : optional_json; [@default Absent] [@drop_default]
  examples : example or_ref json_object option; [@option] [@drop_default]
}

and tag = tag_fields extensible

and tag_fields = {
  name : string;
  summary : string option; [@option] [@drop_default]
  description : string option; [@option] [@drop_default]
  external_docs : external_docs option; [@option] [@key "externalDocs"] [@drop_default]
  parent : string option; [@option] [@drop_default]
  kind : string option; [@option] [@drop_default]
}

and discriminator = discriminator_fields extensible

and discriminator_fields = {
  property_name : string; [@key "propertyName"]
  default_mapping : string option; [@option] [@key "defaultMapping"] [@drop_default]
  mapping : string json_object option; [@option] [@drop_default]
}

and xml = xml_fields extensible

and xml_fields = {
  node_type : xml_node_type option; [@option] [@key "nodeType"] [@drop_default]
  name : string option; [@option] [@drop_default]
  namespace : string option; [@option] [@drop_default]
  prefix : string option; [@option] [@drop_default]
  attribute : bool; [@default false] [@drop_default]
  wrapped : bool; [@default false] [@drop_default]
}

and api_key_security_scheme = api_key_security_scheme_fields extensible

and api_key_security_scheme_fields = {
  deprecated : bool; [@default false] [@drop_default]
  description : string option; [@option] [@drop_default]
  name : string;
  in_ : location_base; [@key "in"]
}

and http_security_scheme = http_security_scheme_fields extensible

and http_security_scheme_fields = {
  deprecated : bool; [@default false] [@drop_default]
  description : string option; [@option] [@drop_default]
  scheme : string;
  bearer_format : string option; [@option] [@key "bearerFormat"] [@drop_default]
}

and oauth2_security_scheme = oauth2_security_scheme_fields extensible

and oauth2_security_scheme_fields = {
  deprecated : bool; [@default false] [@drop_default]
  description : string option; [@option] [@drop_default]
  flows : oauth_flows;
  oauth2_metadata_url : string option; [@option] [@key "oauth2MetadataUrl"] [@drop_default]
}

and open_id_connect_security_scheme = open_id_connect_security_scheme_fields extensible

and open_id_connect_security_scheme_fields = {
  deprecated : bool; [@default false] [@drop_default]
  description : string option; [@option] [@drop_default]
  open_id_connect_url : url; [@key "openIdConnectUrl"]
}

and mutual_tls_security_scheme = mutual_tls_security_scheme_fields extensible

and mutual_tls_security_scheme_fields = {
  deprecated : bool; [@default false] [@drop_default]
  description : string option; [@option] [@drop_default]
}

and oauth_flows = oauth_flows_fields extensible

and oauth_flows_fields = {
  implicit : oauth_flow option; [@option] [@drop_default]
  password : oauth_flow option; [@option] [@drop_default]
  client_credentials : oauth_flow option; [@option] [@key "clientCredentials"] [@drop_default]
  authorization_code : oauth_flow option; [@option] [@key "authorizationCode"] [@drop_default]
  device_authorization : oauth_flow option; [@option] [@key "deviceAuthorization"] [@drop_default]
}

and oauth_flow = oauth_flow_fields extensible

and oauth_flow_fields = {
  authorization_url : string option; [@option] [@key "authorizationUrl"] [@drop_default]
  device_authorization_url : string option; [@option] [@key "deviceAuthorizationUrl"] [@drop_default]
  token_url : string option; [@option] [@key "tokenUrl"] [@drop_default]
  refresh_url : string option; [@option] [@key "refreshUrl"] [@drop_default]
  scopes : string json_object;
}

and security_requirement = string list json_object

and server = server_fields extensible

and server_fields = {
  url : string;
  description : string option; [@option] [@drop_default]
  name : string option; [@option] [@drop_default]
  variables : server_variable json_object; [@default []] [@drop_default_if_json_equal]
}

and server_variable = server_variable_fields extensible

and server_variable_fields = {
  enum : string NonEmptyList.t option; [@option] [@drop_default]
  default : string;
  description : string option; [@option] [@drop_default]
}

type info = info_fields extensible

and info_fields = {
  title : string;
  description : string option; [@option] [@drop_default]
  summary : string option; [@option] [@drop_default]
  terms_of_service : url option; [@option] [@key "termsOfService"] [@drop_default]
  contact : contact option; [@option] [@drop_default]
  license : license option; [@option] [@drop_default]
  version : string;
}

and contact = contact_fields extensible

and contact_fields = {
  name : string option; [@option] [@drop_default]
  url : url option; [@option] [@drop_default]
  email : string option; [@option] [@drop_default]
}

and license = license_fields extensible

and license_fields = {
  name : string;
  identifier : string option; [@option] [@drop_default]
  url : string option; [@option] [@drop_default]
}
[@@deriving json]

type security_scheme =
  | ApiKey of api_key_security_scheme
  | Http of http_security_scheme
  | Oauth2 of oauth2_security_scheme
  | OpenIdConnect of open_id_connect_security_scheme
  | MutualTLS of mutual_tls_security_scheme

let security_scheme_to_json = function
  | ApiKey s ->
    begin match api_key_security_scheme_to_json s with
    | `Assoc fields -> `Assoc (("type", `String "apiKey") :: fields)
    | json -> json
    end
  | Http s ->
    begin match http_security_scheme_to_json s with
    | `Assoc fields -> `Assoc (("type", `String "http") :: fields)
    | json -> json
    end
  | Oauth2 s ->
    begin match oauth2_security_scheme_to_json s with
    | `Assoc fields -> `Assoc (("type", `String "oauth2") :: fields)
    | json -> json
    end
  | OpenIdConnect s ->
    begin match open_id_connect_security_scheme_to_json s with
    | `Assoc fields -> `Assoc (("type", `String "openIdConnect") :: fields)
    | json -> json
    end
  | MutualTLS s ->
    begin match mutual_tls_security_scheme_to_json s with
    | `Assoc fields -> `Assoc (("type", `String "mutualTLS") :: fields)
    | json -> json
    end

let security_scheme_of_json json =
  match json with
  | `Assoc fields ->
    begin match List.assoc_opt "type" fields with
    | Some (`String "apiKey") ->
      let fields' = List.remove_assoc "type" fields in
      ApiKey (api_key_security_scheme_of_json (`Assoc fields'))
    | Some (`String "http") ->
      let fields' = List.remove_assoc "type" fields in
      Http (http_security_scheme_of_json (`Assoc fields'))
    | Some (`String "oauth2") ->
      let fields' = List.remove_assoc "type" fields in
      Oauth2 (oauth2_security_scheme_of_json (`Assoc fields'))
    | Some (`String "openIdConnect") ->
      let fields' = List.remove_assoc "type" fields in
      OpenIdConnect (open_id_connect_security_scheme_of_json (`Assoc fields'))
    | Some (`String "mutualTLS") ->
      let fields' = List.remove_assoc "type" fields in
      MutualTLS (mutual_tls_security_scheme_of_json (`Assoc fields'))
    | _ -> raise (Json_decode_error "Invalid or missing type field in security_scheme")
    end
  | _ -> raise (Json_decode_error "Expected object for security_scheme")

type components = components_fields extensible

and components_fields = {
  schemas : schema json_object option; [@option] [@drop_default]
  responses : response or_ref json_object option; [@option] [@drop_default]
  parameters : parameter or_ref json_object option; [@option] [@drop_default]
  examples : example or_ref json_object option; [@option] [@drop_default]
  request_bodies : request_body or_ref json_object option; [@option] [@key "requestBodies"] [@drop_default]
  headers : header or_ref json_object option; [@option] [@drop_default]
  security_schemes : security_scheme or_ref json_object option; [@option] [@key "securitySchemes"] [@drop_default]
  links : link or_ref json_object option; [@option] [@drop_default]
  callbacks : callback or_ref json_object option; [@option] [@drop_default]
  path_items : path_item or_ref json_object option; [@option] [@key "pathItems"] [@drop_default]
  media_types : media_type or_ref json_object option; [@option] [@key "mediaTypes"] [@drop_default]
}
[@@deriving json]

type paths = path_item json_object extensible [@@deriving json]

type root = root_fields extensible

and root_fields = {
  openapi : string;
  self : url option; [@option] [@key "$self"] [@drop_default]
  info : info;
  json_schema_dialect : string option; [@option] [@key "jsonSchemaDialect"] [@drop_default]
  servers : server list; [@default []]
  paths : paths; [@default extend []]
  components : components option; [@option] [@drop_default]
  webhooks : path_item or_ref json_object option; [@option] [@drop_default]
  security : security_requirement list; [@default []] [@drop_default_if_json_equal]
  tags : tag list; [@default []]
  external_docs : external_docs option; [@option] [@key "externalDocs"] [@drop_default]
}
[@@deriving json]
