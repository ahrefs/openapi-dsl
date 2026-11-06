[@@@warning "-30"] (* disable duplicate field name warnings *)

exception Json_decode_error of string

(** A URI reference, including relative references and non-HTTP schemes. *)
type url = string [@@deriving json]

type json = Jsonkit.t [@@deriving json]

type ref_ = string [@@deriving json]

module Or_ref : sig
  type 'a t =
    | Obj of 'a
    | Ref of ref_
    | Ref_with_metadata of ref_ * (string * json) list

  val obj : 'a -> 'a t
  val ref : ref_ -> 'a t
end

module JsonObject : sig
  type 'a t = (ref_ * 'a) list
end

type 'a json_object = 'a JsonObject.t [@@deriving json]
type 'a or_ref = 'a Or_ref.t [@@deriving json]

type typ =
  | Integer
  | Number
  | String
  | Boolean
  | Object
  | Array
  | Null

val typ_list_to_json : typ list -> json

type format =
  [ `Int32
  | `Int64
  | `Float
  | `Double
  | `Date
  | `Datetime
  | `Domain
  | `Url
  ]
[@@deriving json]

type parameter_location =
  [ `Query
  | `Querystring
  | `Header
  | `Cookie
  | `Path
  ]

type location_base =
  [ `Query
  | `Header
  | `Cookie
  ]

type style =
  | Matrix
  | Label
  | Form
  | Simple
  | SpaceDelimited
  | PipeDelimited
  | DeepObject
  | Cookie

type schema = Jsonkit.Jsonschema.t
val schema_to_json : schema -> json
val schema_of_json : json -> schema
module Schema : sig
  val get : string -> schema -> json option
  val set : string -> json option -> schema -> schema
  val remove : string -> schema -> schema
  val properties : schema -> (string * schema) list

  (** Establish a resource boundary for local [$defs] references before embedding. *)
  val resource : schema -> schema

  val types : schema -> typ list
  val string : string -> schema -> string option
  val strings : string -> schema -> string list
  val map_children : (schema -> schema) -> schema -> schema
  val fold_children : ('a -> schema -> 'a) -> 'a -> schema -> 'a
end

type extensions = (string * json) list

(** HTTP objects keep their typed fields in [value] and emit extensions inline.
    Schema objects remain raw JSON Schema values. *)
type 'a extensible = {
  value : 'a;
  extensions : extensions;
}

(** Rejects duplicate names and names not starting with [x-]. *)
val extend : ?extensions:extensions -> 'a -> 'a extensible

(** The right-hand object's extensions take precedence. *)
val merge_extensions : extensions -> extensions -> extensions

val extensible_to_json : ('a -> json) -> 'a extensible -> json
val extensible_of_json : (json -> 'a) -> json -> 'a extensible

(** Distinguishes an absent JSON-valued field from an explicit JSON null. *)
type optional_json =
  | Absent
  | Present of json

type content_type =
  | OctetStream
  | PlainText
  | Json
  | Custom of string
[@@deriving json]

type xml_node_type =
  [ `Element
  | `Attribute
  | `Text
  | `Cdata
  | `None
  ]
[@@deriving json]

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
  (* fixed fields *)
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

val security_scheme_to_json : security_scheme -> json
val security_scheme_of_json : json -> security_scheme

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
