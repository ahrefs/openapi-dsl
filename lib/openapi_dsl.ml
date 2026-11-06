include Openapi

let openapi_version = "3.2.0"

let optional_json = function
  | None -> Absent
  | Some value -> Present value

module OrderedTbl : sig
  type 'value t
  val create : int -> 'value t
  val add : 'value t -> string -> 'value -> unit
  val find_opt : 'value t -> string -> 'value option
  val to_list : 'value t -> (string * 'value) list
end = struct
  type 'value t = int ref * (string, int * 'value) Hashtbl.t

  let create size = ref 0, Hashtbl.create size

  let add (next_id, tbl) k v =
    let id =
      match Hashtbl.find_opt tbl k with
      | Some (id, _) -> id
      | None ->
        let id = !next_id in
        incr next_id;
        id
    in
    Hashtbl.replace tbl k (id, v)

  let find_opt (_, tbl) k = Hashtbl.find_opt tbl k |> Option.map snd

  let to_list (_, tbl) =
    Hashtbl.to_seq tbl
    |> List.of_seq
    |> List.sort (fun (_, (a, _)) (_, (b, _)) -> Int.compare a b)
    |> List.map (fun (k, (_, v)) -> k, v)
end

type builder = {
  paths : path_item OrderedTbl.t;
  schemas : schema OrderedTbl.t;
  responses : response or_ref OrderedTbl.t;
  parameters : parameter or_ref OrderedTbl.t;
  examples : example or_ref OrderedTbl.t;
  request_bodies : request_body or_ref OrderedTbl.t;
  headers : header or_ref OrderedTbl.t;
  security_schemes : security_scheme or_ref OrderedTbl.t;
  links : link or_ref OrderedTbl.t;
  callbacks : callback or_ref OrderedTbl.t;
  path_items : path_item or_ref OrderedTbl.t;
  media_types : media_type or_ref OrderedTbl.t;
  root_proto : root;
  components_extensions : extensions;
}

exception Component_conflict of { section : string; name : string }

let () =
  Printexc.register_printer (function
    | Component_conflict { section; name } ->
      Some (Printf.sprintf "Conflicting component %s/%s (use ~replace:true to replace it)" section name)
    | _ -> None)

module Ref = struct
  let register ?(replace = false) tbl section name value =
    if name = "" || not (String.for_all (function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '-' | '_' -> true
      | _ -> false) name) then
      invalid_arg ("Invalid OpenAPI component name: " ^ name);
    (match OrderedTbl.find_opt tbl name with
    | Some existing when existing <> value && not replace ->
      raise (Component_conflict { section; name })
    | _ -> OrderedTbl.add tbl name value);
    Printf.sprintf "#/components/%s/%s" section name

  let add ?replace tbl section name value =
    Or_ref.ref (register ?replace tbl section name value)

  let add_schema ?replace t name schema =
    let ref_ = register ?replace t.schemas "schemas" name (Schema.resource schema) in
    `Assoc [ "$ref", `String ref_ ]
  let add_response ?replace t = add ?replace t.responses "responses"
  let add_parameter ?replace t = add ?replace t.parameters "parameters"
  let add_example ?replace t = add ?replace t.examples "examples"
  let add_request_body ?replace t = add ?replace t.request_bodies "requestBodies"
  let add_header ?replace t = add ?replace t.headers "headers"
  let add_security_scheme ?replace t = add ?replace t.security_schemes "securitySchemes"
  let add_link ?replace t = add ?replace t.links "links"
  let add_callback ?replace t = add ?replace t.callbacks "callbacks"
  let add_path_item ?replace t name item = add ?replace t.path_items "pathItems" name (Or_ref.obj item)
  let add_media_type ?replace t = add ?replace t.media_types "mediaTypes"
end

module Find = struct
  let schema t = OrderedTbl.find_opt t.schemas
  let response t = OrderedTbl.find_opt t.responses
  let parameter t = OrderedTbl.find_opt t.parameters
  let example t = OrderedTbl.find_opt t.examples
  let request_body t = OrderedTbl.find_opt t.request_bodies
  let header t = OrderedTbl.find_opt t.headers
  let security_scheme t = OrderedTbl.find_opt t.security_schemes
  let link t = OrderedTbl.find_opt t.links
  let callback t = OrderedTbl.find_opt t.callbacks
  let path_item t = OrderedTbl.find_opt t.path_items
  let media_type t = OrderedTbl.find_opt t.media_types
  let path t = OrderedTbl.find_opt t.paths
end

exception Operation_conflict of { path : string; meth : string }

let () =
  Printexc.register_printer (function
    | Operation_conflict { path; meth } ->
      Some (Printf.sprintf "Operation already registered: %s %s" meth path)
    | _ -> None)

(* Merge different methods, but reject a second registration of the same method. *)
let merge_path_item path (a : Openapi.path_item) (b : Openapi.path_item) : Openapi.path_item =
  let pick a b =
    match b with
    | Some _ -> b
    | None -> a
  in
  let method_ meth a b =
    match a, b with
    | Some _, Some _ -> raise (Operation_conflict { path; meth })
    | _ -> pick a b
  in
  let extensions = merge_extensions a.extensions b.extensions in
  let a = a.value
  and b = b.value in
  let additionalOperations =
    match a.additionalOperations, b.additionalOperations with
    | Some a, Some b -> Some (a @ b)
    | a, b -> pick a b
  in
  extend ~extensions
    {
      ref_ = pick a.ref_ b.ref_;
      summary = pick a.summary b.summary;
      description = pick a.description b.description;
      get = method_ "GET" a.get b.get;
      put = method_ "PUT" a.put b.put;
      post = method_ "POST" a.post b.post;
      delete = method_ "DELETE" a.delete b.delete;
      options = method_ "OPTIONS" a.options b.options;
      head = method_ "HEAD" a.head b.head;
      patch = method_ "PATCH" a.patch b.patch;
      trace = method_ "TRACE" a.trace b.trace;
      query = method_ "QUERY" a.query b.query;
      additionalOperations;
      servers = pick a.servers b.servers;
      parameters = pick a.parameters b.parameters;
    }

let add_path t id path_item =
  let seen = Hashtbl.create 12 in
  let check meth =
    let meth = String.uppercase_ascii meth in
    if Hashtbl.mem seen meth then raise (Operation_conflict { path = id; meth });
    Hashtbl.add seen meth ()
  in
  let check_item (item : Openapi.path_item) =
    let item = item.value in
    List.iter (fun (meth, operation) -> Option.iter (fun _ -> check meth) operation)
      [ "GET", item.get; "PUT", item.put; "POST", item.post; "DELETE", item.delete;
        "OPTIONS", item.options; "HEAD", item.head; "PATCH", item.patch;
        "TRACE", item.trace; "QUERY", item.query ];
    Option.iter (List.iter (fun (meth, _) -> check meth)) item.additionalOperations
  in
  let existing = OrderedTbl.find_opt t.paths id in
  Option.iter check_item existing;
  check_item path_item;
  match existing with
  | Some existing -> OrderedTbl.add t.paths id (merge_path_item id existing path_item)
  | None -> OrderedTbl.add t.paths id path_item

let create ?extensions ?path_extensions ?(components_extensions = []) ?(openapi = openapi_version) ?self
  ?json_schema_dialect ?webhooks ?external_docs ?(tags = []) ?(servers = []) ?(security = []) ~info () =
  {
    paths = OrderedTbl.create 10;
    schemas = OrderedTbl.create 10;
    responses = OrderedTbl.create 10;
    parameters = OrderedTbl.create 10;
    examples = OrderedTbl.create 10;
    request_bodies = OrderedTbl.create 10;
    headers = OrderedTbl.create 10;
    security_schemes = OrderedTbl.create 10;
    links = OrderedTbl.create 10;
    callbacks = OrderedTbl.create 10;
    path_items = OrderedTbl.create 10;
    media_types = OrderedTbl.create 10;
    components_extensions;
    root_proto =
      extend ?extensions
        {
          openapi;
          self;
          info;
          json_schema_dialect;
          servers;
          paths = extend ?extensions:path_extensions [];
          webhooks;
          components = None;
          security;
          tags;
          external_docs;
        };
  }

let export_components t =
    extend ~extensions:t.components_extensions
      {
        schemas = Some (OrderedTbl.to_list t.schemas);
        responses = Some (OrderedTbl.to_list t.responses);
        parameters = Some (OrderedTbl.to_list t.parameters);
        examples = Some (OrderedTbl.to_list t.examples);
        request_bodies = Some (OrderedTbl.to_list t.request_bodies);
        headers = Some (OrderedTbl.to_list t.headers);
        security_schemes = Some (OrderedTbl.to_list t.security_schemes);
        links = Some (OrderedTbl.to_list t.links);
        callbacks = Some (OrderedTbl.to_list t.callbacks);
        path_items = Some (OrderedTbl.to_list t.path_items);
        media_types = Some (OrderedTbl.to_list t.media_types);
      }

let export_paths t =
  { t.root_proto.value.paths with value = OrderedTbl.to_list t.paths }

let finalize t =
  { t.root_proto with
    value = { t.root_proto.value with components = Some (export_components t); paths = export_paths t } }

let tag ?extensions ~name ?summary ?description ?external_docs ?parent ?kind () =
  extend ?extensions { name; summary; description; external_docs; parent; kind }

let external_docs ?extensions ~url ?description () = extend ?extensions ({ url; description } : external_docs_fields)

let request_body ?extensions ~content ?description ?(required = false) () =
  extend ?extensions ({ content; description; required } : request_body_fields) |> Or_ref.obj

let example ?extensions ?summary ?description ?value ?data_value ?serialized_value ?external_value () =
  extend ?extensions
    ({
       summary;
       description;
       value = optional_json value;
       data_value = optional_json data_value;
       serialized_value;
       external_value;
     }
      : example_fields)
  |> Or_ref.obj

let link ?extensions ?operation_ref ?operation_id ?(parameters = []) ?request_body ?description ?server () =
  extend ?extensions
    ({ operation_ref; operation_id; parameters; request_body = optional_json request_body; description; server }
      : link_fields)
  |> Or_ref.obj

let header ?extensions ?description ?(required = false) ?(deprecated = false) ?style ?explode ?schema ?content ?example
  ?examples () =
  extend ?extensions
    ({ description; required; deprecated; style; explode; schema; content; example = optional_json example; examples }
      : header_fields)
  |> Or_ref.obj

let encoding ?extensions ?content_type ?headers ?encoding ?prefix_encoding ?item_encoding ?style ?explode
  ?allow_reserved () =
  extend ?extensions
    ({ content_type; headers; encoding; prefix_encoding; item_encoding; style; explode; allow_reserved }
      : encoding_fields)

let discriminator ?extensions ~property_name ?mapping ?default_mapping () =
  extend ?extensions ({ property_name; mapping; default_mapping } : discriminator_fields)

let xml ?extensions ?node_type ?name ?namespace ?prefix ?(attribute = false) ?(wrapped = false) () =
  extend ?extensions ({ node_type; name; namespace; prefix; attribute; wrapped } : xml_fields)

let api_key_security_scheme ?extensions ~name ~location ?description ?(deprecated = false) () =
  extend ?extensions ({ name; in_ = location; description; deprecated } : api_key_security_scheme_fields)
  |> fun scheme -> Or_ref.obj (ApiKey scheme)

let http_security_scheme ?extensions ~scheme ?description ?bearer_format ?(deprecated = false) () =
  extend ?extensions ({ scheme; description; bearer_format; deprecated } : http_security_scheme_fields)
  |> fun scheme -> Or_ref.obj (Http scheme)

let oauth2_security_scheme ?extensions ~flows ?description ?oauth2_metadata_url ?(deprecated = false) () =
  extend ?extensions ({ flows; description; oauth2_metadata_url; deprecated } : oauth2_security_scheme_fields)
  |> fun scheme -> Or_ref.obj (Oauth2 scheme)

let open_id_connect_security_scheme ?extensions ~open_id_connect_url ?description ?(deprecated = false) () =
  extend ?extensions ({ open_id_connect_url; description; deprecated } : open_id_connect_security_scheme_fields)
  |> fun scheme -> Or_ref.obj (OpenIdConnect scheme)

let mutual_tls_security_scheme ?extensions ?description ?(deprecated = false) () =
  extend ?extensions ({ description; deprecated } : mutual_tls_security_scheme_fields)
  |> fun scheme -> Or_ref.obj (MutualTLS scheme)

let oauth_flows ?extensions ?implicit ?password ?client_credentials ?authorization_code ?device_authorization () =
  extend ?extensions
    ({ implicit; password; client_credentials; authorization_code; device_authorization } : oauth_flows_fields)

let oauth_flow ?extensions ~scopes ?authorization_url ?device_authorization_url ?token_url ?refresh_url () =
  extend ?extensions
    ({ scopes; authorization_url; device_authorization_url; token_url; refresh_url } : oauth_flow_fields)

let server ?extensions ~url ?description ?name ?(variables = []) () =
  extend ?extensions ({ url; description; name; variables } : server_fields)

let server_variable ?extensions ~default ?enum ?description () =
  extend ?extensions ({ default; enum; description } : server_variable_fields)

let info ?extensions ~title ~version ?description ?summary ?terms_of_service ?contact ?license () =
  extend ?extensions ({ title; version; description; summary; terms_of_service; contact; license } : info_fields)

let contact ?extensions ?name ?url ?email () = extend ?extensions ({ name; url; email } : contact_fields)

let license ?extensions ~name ?identifier ?url () = extend ?extensions ({ name; identifier; url } : license_fields)

let paths ?extensions entries : paths = extend ?extensions entries

let responses ?extensions entries : responses = extend ?extensions entries

let callback ?extensions entries : callback or_ref = Or_ref.obj (extend ?extensions entries)

let schema_ref ref_ : schema = `Assoc [ "$ref", `String ref_ ]

let make_nullable s =
  match Schema.get "type" s with
  | Some typ
    when List.for_all
           (fun key -> Option.is_none (Schema.get key s))
           [ "const"; "allOf"; "anyOf"; "oneOf"; "not"; "if"; "then"; "else"; "$ref"; "$dynamicRef" ] ->
    let types =
      match typ with
      | `List ts -> ts
      | t -> [ t ]
    in
    let types = if List.mem (`String "null") types then types else types @ [ `String "null" ] in
    let s = Schema.set "type" (Some (`List types)) s in
    begin match Schema.get "enum" s with
    | Some (`List values) ->
      let values = if List.mem `Null values then values else values @ [ `Null ] in
      Schema.set "enum" (Some (`List values)) s
    | _ -> s
    end
  | _ -> `Assoc [ "anyOf", `List [ Schema.resource s; `Assoc [ "type", `String "null" ] ] ]

let schema ?all_of ?any_of ?one_of ?not ?items ?prefix_items ?properties ?additional_properties ?typs ?typ ?enum
  ?enum_values ?const ?multiple_of ?maximum ?exclusive_maximum ?minimum ?exclusive_minimum ?max_length ?min_length
  ?pattern ?max_items ?min_items ?unique_items ?max_contains ?min_contains ?max_properties ?min_properties
  ?(required = []) ?(dependent_required = []) ?format ?title ?description ?default ?(nullable = false) ?discriminator
  ?(read_only = false) ?(write_only = false) ?xml ?external_docs ?examples ?(deprecated = false) () : schema =
  let typ =
    match typs, typ with
    | Some ts, _ -> Some ts
    | None, Some t -> Some [ t ]
    | None, None -> None
  in
  let fields =
    [
      "allOf", Option.map (fun xs -> `List (NonEmptyList.to_list xs)) all_of;
      "anyOf", Option.map (fun xs -> `List (NonEmptyList.to_list xs)) any_of;
      "oneOf", Option.map (fun xs -> `List (NonEmptyList.to_list xs)) one_of;
      "not", Option.map Fun.id not;
      "items", Option.map Fun.id items;
      "prefixItems", Option.map (fun xs -> `List xs) prefix_items;
      "properties", Option.map (fun xs -> `Assoc xs) properties;
      "additionalProperties", Option.map Fun.id additional_properties;
      "type", Option.map typ_list_to_json typ;
      ( "enum",
        match enum_values with
        | Some xs -> Some (`List (NonEmptyList.to_list xs))
        | None -> Option.map (fun xs -> `List (List.map (fun s -> `String s) (NonEmptyList.to_list xs))) enum );
      "const", Option.map Fun.id const;
      "multipleOf", Option.map (fun n -> `Float n) multiple_of;
      "maximum", Option.map (fun n -> `Float n) maximum;
      "exclusiveMaximum", Option.map (fun n -> `Float n) exclusive_maximum;
      "minimum", Option.map (fun n -> `Float n) minimum;
      "exclusiveMinimum", Option.map (fun n -> `Float n) exclusive_minimum;
      "maxLength", Option.map (fun n -> `Int n) max_length;
      "minLength", Option.map (fun n -> `Int n) min_length;
      "pattern", Option.map (fun s -> `String s) pattern;
      "maxItems", Option.map (fun n -> `Int n) max_items;
      "minItems", Option.map (fun n -> `Int n) min_items;
      "uniqueItems", Option.map (fun b -> `Bool b) unique_items;
      "maxContains", Option.map (fun n -> `Int n) max_contains;
      "minContains", Option.map (fun n -> `Int n) min_contains;
      "maxProperties", Option.map (fun n -> `Int n) max_properties;
      "minProperties", Option.map (fun n -> `Int n) min_properties;
      ("required", if required = [] then None else Some (`List (List.map (fun s -> `String s) required)));
      ( "dependentRequired",
        if dependent_required = [] then None
        else Some (json_object_to_json (fun xs -> `List (List.map (fun s -> `String s) xs)) dependent_required) );
      "format", Option.map format_to_json format;
      "title", Option.map (fun s -> `String s) title;
      "description", Option.map (fun s -> `String s) description;
      "default", Option.map Fun.id default;
      "discriminator", Option.map discriminator_to_json discriminator;
      ("readOnly", if read_only then Some (`Bool true) else None);
      ("writeOnly", if write_only then Some (`Bool true) else None);
      "xml", Option.map xml_to_json xml;
      "externalDocs", Option.map external_docs_to_json external_docs;
      "examples", Option.map (fun xs -> `List xs) examples;
      ("deprecated", if deprecated then Some (`Bool true) else None);
    ]
    |> List.filter_map (fun (key, value) -> Option.map (fun value -> key, value) value)
  in
  let s = `Assoc fields in
  if nullable then make_nullable s else s

let nullable = make_nullable

let wrap_response_schema ~entity ~rows_mode ?xml row_schema =
  let wrapper_key = if rows_mode = `Multiple then entity ^ "s" else entity in
  let data =
    match rows_mode with
    | `Multiple -> schema ~typ:Array ~items:row_schema ()
    | `Single -> row_schema
  in
  schema ?xml ~typ:Object ~properties:[ wrapper_key, data ] ()

let parameter ?extensions ~name ~location ?description ~required ?(deprecated = false) ?allow_empty_value ?style
  ?explode ?(allow_reserved = false) ?schema ?content ?example ?examples () =
  extend ?extensions
    {
      name;
      location;
      description;
      required;
      deprecated;
      allow_empty_value;
      style;
      explode;
      allow_reserved;
      schema;
      content;
      example = optional_json example;
      examples;
    }
  |> Or_ref.obj

let media_type ?extensions ?schema ?example ?examples ?encoding ?prefix_encoding ?item_encoding ?item_schema () =
  extend ?extensions
    {
      schema = Option.map Schema.resource schema;
      item_schema;
      example = optional_json example;
      examples;
      encoding;
      prefix_encoding;
      item_encoding;
    }
  |> Or_ref.obj

let content content_type media_type = content_type, media_type

let event_stream ?extensions events =
  let event (name, data) =
    schema ~typ:Object
      ~properties:[ "event", schema ~const:(`String name) (); "data", Schema.resource data ]
      ~required:[ "event"; "data" ] ()
  in
  extend ?extensions
    {
      schema = None;
      item_schema = Some (schema ?one_of:(NonEmptyList.of_list_opt (List.map event events)) ());
      example = Absent;
      examples = None;
      encoding = None;
      prefix_encoding = None;
      item_encoding = None;
    }
  |> Or_ref.obj

let response ?extensions ?summary ?description ?headers ?content ?links () =
  extend ?extensions { summary; description; headers; content; links } |> Or_ref.obj

let operation ?extensions ?response_extensions ?tags ?summary ?description ?external_docs ?id ?parameters ?request_body
  ?responses ?callbacks ?(deprecated = false) ?security ?servers () =
  let responses =
    match responses, response_extensions with
    | None, None -> None
    | _ -> Some (extend ?extensions:response_extensions (Option.value ~default:[] responses))
  in
  extend ?extensions
    {
      tags;
      summary_ = summary;
      description_ = description;
      external_docs;
      operation_id = id;
      parameters_ = parameters;
      request_body;
      responses;
      callbacks;
      deprecated;
      security;
      servers_ = servers;
    }

let path_item ?extensions ?ref_ ?summary ?description ?get ?put ?post ?delete ?options ?head ?patch ?trace ?query
  ?additionalOperations ?servers ?parameters () =
  extend ?extensions
    {
      ref_;
      summary;
      description;
      get;
      put;
      post;
      delete;
      options;
      head;
      patch;
      trace;
      query;
      additionalOperations;
      servers;
      parameters;
    }

let components ?extensions ?schemas ?responses ?parameters ?examples ?request_bodies ?headers ?security_schemes ?links
  ?callbacks ?path_items ?media_types () =
  extend ?extensions
    {
      schemas;
      responses;
      parameters;
      examples;
      request_bodies;
      headers;
      security_schemes;
      links;
      callbacks;
      path_items;
      media_types;
    }

let root ?extensions ?path_extensions ?(openapi = openapi_version) ?self ~info ?json_schema_dialect ?(servers = [])
  ?(paths = []) ?webhooks ?components ?(security = []) ?(tags = []) ?external_docs () =
  extend ?extensions
    {
      openapi;
      self;
      info;
      json_schema_dialect;
      servers;
      paths = extend ?extensions:path_extensions paths;
      webhooks;
      components;
      security;
      tags;
      external_docs;
    }

type property = { name : string; schema : schema; required : bool }

let required name schema = { name; schema; required = true }
let optional name schema = { name; schema; required = false }

module Schema = struct
  include Schema

  let object_ ?title ?description ?additional_properties fields =
    let seen = Hashtbl.create (List.length fields) in
    List.iter (fun field ->
      if Hashtbl.mem seen field.name then
        invalid_arg ("Duplicate object property: " ^ field.name);
      Hashtbl.add seen field.name ()) fields;
    schema ~typ:Object ?title ?description ?additional_properties
      ~properties:(List.map (fun field -> field.name, field.schema) fields)
      ~required:(List.filter_map (fun field -> if field.required then Some field.name else None) fields)
      ()
end
