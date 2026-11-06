(** Constructors wrap inline values in [Or_ref.Obj] where containers accept
    references. Security-scheme constructors also select the scheme variant.
    Schemas and path items use their own [$ref] fields. *)

include module type of struct
  include Openapi
end

type builder

(** Registration raises this when a name already has a different definition.
    The builder keeps the existing definition. *)
exception Component_conflict of { section : string; name : string }

(** Register components and return typed references.

    Registering the same name and structurally equal value leaves the definition
    unchanged. A different value raises [Component_conflict]. Pass [~replace:true]
    to replace it without changing its position in the output.

    Names must contain at least one character. Only letters, digits, dots,
    hyphens and underscores are allowed. Invalid names raise [Invalid_argument]. *)
module Ref : sig
  val add_schema : ?replace:bool -> builder -> string -> schema -> schema
  val add_response : ?replace:bool -> builder -> string -> response or_ref -> response or_ref
  val add_parameter : ?replace:bool -> builder -> string -> parameter or_ref -> parameter or_ref
  val add_example : ?replace:bool -> builder -> string -> example or_ref -> example or_ref
  val add_request_body : ?replace:bool -> builder -> string -> request_body or_ref -> request_body or_ref
  val add_header : ?replace:bool -> builder -> string -> header or_ref -> header or_ref
  val add_security_scheme : ?replace:bool -> builder -> string -> security_scheme or_ref -> security_scheme or_ref
  val add_link : ?replace:bool -> builder -> string -> link or_ref -> link or_ref
  val add_callback : ?replace:bool -> builder -> string -> callback or_ref -> callback or_ref
  val add_path_item : ?replace:bool -> builder -> string -> path_item -> path_item or_ref
  val add_media_type : ?replace:bool -> builder -> string -> media_type or_ref -> media_type or_ref
end

(** Look up definitions by component name, not reference URI.
    Missing names return [None]. Lookups return stored aliases without resolving
    their references. *)
module Find : sig
  val schema : builder -> string -> schema option
  val response : builder -> string -> response or_ref option
  val parameter : builder -> string -> parameter or_ref option
  val example : builder -> string -> example or_ref option
  val request_body : builder -> string -> request_body or_ref option
  val header : builder -> string -> header or_ref option
  val security_scheme : builder -> string -> security_scheme or_ref option
  val link : builder -> string -> link or_ref option
  val callback : builder -> string -> callback or_ref option
  val path_item : builder -> string -> path_item or_ref option
  val media_type : builder -> string -> media_type or_ref option
  val path : builder -> string -> path_item option
end

(** Export snapshots in insertion order with the configured extensions.
    Later builder changes do not affect these values. *)
val export_components : builder -> components
val export_paths : builder -> paths

(** Raised when a path already has an operation for the method. *)
exception Operation_conflict of { path : string; meth : string }

(** Add a path or merge methods into an existing path. Registering the same
    method twice raises [Operation_conflict], even if the definitions match.
    A failed registration leaves the path unchanged. *)
val add_path : builder -> ref_ -> path_item -> unit

val create :
  ?extensions:extensions ->
  ?path_extensions:extensions ->
  ?components_extensions:extensions ->
  ?openapi:ref_ ->
  ?self:url ->
  ?json_schema_dialect:string ->
  ?webhooks:path_item or_ref json_object ->
  ?external_docs:external_docs ->
  ?tags:tag list ->
  ?servers:server list ->
  ?security:security_requirement list ->
  info:info ->
  unit ->
  builder

val finalize : builder -> root

val tag :
  ?extensions:extensions ->
  name:ref_ ->
  ?summary:ref_ ->
  ?description:ref_ ->
  ?external_docs:external_docs ->
  ?parent:ref_ ->
  ?kind:ref_ ->
  unit ->
  tag

val external_docs : ?extensions:extensions -> url:url -> ?description:string -> unit -> external_docs

val request_body :
  ?extensions:extensions ->
  content:media_type or_ref json_object ->
  ?description:string ->
  ?required:bool ->
  unit ->
  request_body or_ref

val example :
  ?extensions:extensions ->
  ?summary:string ->
  ?description:string ->
  ?value:json ->
  ?data_value:json ->
  ?serialized_value:string ->
  ?external_value:string ->
  unit ->
  example or_ref

val link :
  ?extensions:extensions ->
  ?operation_ref:string ->
  ?operation_id:string ->
  ?parameters:json json_object ->
  ?request_body:json ->
  ?description:string ->
  ?server:server ->
  unit ->
  link or_ref

val header :
  ?extensions:extensions ->
  ?description:string ->
  ?required:bool ->
  ?deprecated:bool ->
  ?style:style ->
  ?explode:bool ->
  ?schema:schema ->
  ?content:media_type or_ref json_object ->
  ?example:json ->
  ?examples:example or_ref json_object ->
  unit ->
  header or_ref

val encoding :
  ?extensions:extensions ->
  ?content_type:content_type ->
  ?headers:header or_ref json_object ->
  ?encoding:encoding json_object ->
  ?prefix_encoding:encoding list ->
  ?item_encoding:encoding ->
  ?style:style ->
  ?explode:bool ->
  ?allow_reserved:bool ->
  unit ->
  encoding

val discriminator :
  ?extensions:extensions ->
  property_name:string ->
  ?mapping:string json_object ->
  ?default_mapping:string ->
  unit ->
  discriminator

val xml :
  ?extensions:extensions ->
  ?node_type:xml_node_type ->
  ?name:string ->
  ?namespace:string ->
  ?prefix:string ->
  ?attribute:bool ->
  ?wrapped:bool ->
  unit ->
  xml

val api_key_security_scheme :
  ?extensions:extensions ->
  name:string ->
  location:location_base ->
  ?description:string ->
  ?deprecated:bool ->
  unit ->
  security_scheme or_ref

val http_security_scheme :
  ?extensions:extensions ->
  scheme:string ->
  ?description:string ->
  ?bearer_format:string ->
  ?deprecated:bool ->
  unit ->
  security_scheme or_ref

val oauth2_security_scheme :
  ?extensions:extensions ->
  flows:oauth_flows ->
  ?description:string ->
  ?oauth2_metadata_url:string ->
  ?deprecated:bool ->
  unit ->
  security_scheme or_ref

val open_id_connect_security_scheme :
  ?extensions:extensions ->
  open_id_connect_url:url ->
  ?description:string ->
  ?deprecated:bool ->
  unit ->
  security_scheme or_ref

val mutual_tls_security_scheme :
  ?extensions:extensions -> ?description:string -> ?deprecated:bool -> unit -> security_scheme or_ref

val oauth_flows :
  ?extensions:extensions ->
  ?implicit:oauth_flow ->
  ?password:oauth_flow ->
  ?client_credentials:oauth_flow ->
  ?authorization_code:oauth_flow ->
  ?device_authorization:oauth_flow ->
  unit ->
  oauth_flows

val oauth_flow :
  ?extensions:extensions ->
  scopes:string json_object ->
  ?authorization_url:string ->
  ?device_authorization_url:string ->
  ?token_url:string ->
  ?refresh_url:string ->
  unit ->
  oauth_flow

val server :
  ?extensions:extensions ->
  url:string ->
  ?description:string ->
  ?name:string ->
  ?variables:server_variable json_object ->
  unit ->
  server

val server_variable :
  ?extensions:extensions ->
  default:string ->
  ?enum:string NonEmptyList.t ->
  ?description:string ->
  unit ->
  server_variable

val info :
  ?extensions:extensions ->
  title:string ->
  version:string ->
  ?description:string ->
  ?summary:string ->
  ?terms_of_service:url ->
  ?contact:contact ->
  ?license:license ->
  unit ->
  info

val contact : ?extensions:extensions -> ?name:string -> ?url:url -> ?email:string -> unit -> contact

val license : ?extensions:extensions -> name:string -> ?identifier:string -> ?url:string -> unit -> license

val paths : ?extensions:extensions -> path_item json_object -> paths

val responses : ?extensions:extensions -> response or_ref json_object -> responses

val callback : ?extensions:extensions -> path_item json_object -> callback or_ref

val schema_ref : ref_ -> schema
val nullable : schema -> schema

(** [typs] overrides [typ] when both arguments are present. *)
val schema :
  ?all_of:schema NonEmptyList.t ->
  ?any_of:schema NonEmptyList.t ->
  ?one_of:schema NonEmptyList.t ->
  ?not:schema ->
  ?items:schema ->
  ?prefix_items:schema list ->
  ?properties:schema json_object ->
  ?additional_properties:schema ->
  ?typs:typ list ->
  ?typ:typ ->
  ?enum:ref_ NonEmptyList.t ->
  ?enum_values:json NonEmptyList.t ->
  ?const:json ->
  ?multiple_of:float ->
  ?maximum:float ->
  ?exclusive_maximum:float ->
  ?minimum:float ->
  ?exclusive_minimum:float ->
  ?max_length:int ->
  ?min_length:int ->
  ?pattern:ref_ ->
  ?max_items:int ->
  ?min_items:int ->
  ?unique_items:bool ->
  ?max_contains:int ->
  ?min_contains:int ->
  ?max_properties:int ->
  ?min_properties:int ->
  ?required:ref_ list ->
  ?dependent_required:ref_ list json_object ->
  ?format:format ->
  ?title:ref_ ->
  ?description:ref_ ->
  ?default:json ->
  ?nullable:bool ->
  ?discriminator:discriminator ->
  ?read_only:bool ->
  ?write_only:bool ->
  ?xml:xml ->
  ?external_docs:external_docs ->
  ?examples:json list ->
  ?deprecated:bool ->
  unit ->
  schema

type property

(** Property presence is independent of whether its schema accepts [null]. *)
val required : string -> schema -> property
val optional : string -> schema -> property

module Schema : sig
  include module type of struct include Openapi.Schema end

  (** Derive [properties] and [required] from the field list.
      Duplicate property names raise [Invalid_argument]. *)
  val object_ :
    ?title:string ->
    ?description:string ->
    ?additional_properties:schema ->
    property list -> schema
end

val wrap_response_schema : entity:ref_ -> rows_mode:[< `Multiple | `Single > `Multiple ] -> ?xml:xml -> schema -> schema

val parameter :
  ?extensions:extensions ->
  name:ref_ ->
  location:parameter_location ->
  ?description:ref_ ->
  required:bool ->
  ?deprecated:bool ->
  ?allow_empty_value:bool ->
  ?style:style ->
  ?explode:bool ->
  ?allow_reserved:bool ->
  ?schema:schema ->
  ?content:media_type or_ref json_object ->
  ?example:json ->
  ?examples:example or_ref json_object ->
  unit ->
  parameter or_ref

val media_type :
  ?extensions:extensions ->
  ?schema:schema ->
  ?example:json ->
  ?examples:example or_ref json_object ->
  ?encoding:encoding json_object ->
  ?prefix_encoding:encoding list ->
  ?item_encoding:encoding ->
  ?item_schema:schema ->
  unit ->
  media_type or_ref

(** Pair a content type with an inline media type or reference. *)
val content : string -> media_type or_ref -> string * media_type or_ref

(** Build [text/event-stream] content with one [itemSchema] variant for each
    [(event name, data schema)] pair. *)
val event_stream : ?extensions:extensions -> (ref_ * schema) list -> media_type or_ref

val response :
  ?extensions:extensions ->
  ?summary:string ->
  ?description:ref_ ->
  ?headers:header or_ref json_object ->
  ?content:media_type or_ref json_object ->
  ?links:link or_ref json_object ->
  unit ->
  response or_ref

val operation :
  ?extensions:extensions ->
  ?response_extensions:extensions ->
  ?tags:ref_ list ->
  ?summary:ref_ ->
  ?description:ref_ ->
  ?external_docs:external_docs ->
  ?id:ref_ ->
  ?parameters:parameter or_ref list ->
  ?request_body:request_body or_ref ->
  ?responses:response or_ref json_object ->
  ?callbacks:callback or_ref json_object ->
  ?deprecated:bool ->
  ?security:security_requirement list ->
  ?servers:server list ->
  unit ->
  operation

(** Return a plain path item. Path maps do not accept [Or_ref.Obj] wrappers. *)
val path_item :
  ?extensions:extensions ->
  ?ref_:ref_ ->
  ?summary:ref_ ->
  ?description:ref_ ->
  ?get:operation ->
  ?put:operation ->
  ?post:operation ->
  ?delete:operation ->
  ?options:operation ->
  ?head:operation ->
  ?patch:operation ->
  ?trace:operation ->
  ?query:operation ->
  ?additionalOperations:(ref_ * operation) list ->
  ?servers:server list ->
  ?parameters:parameter or_ref list ->
  unit ->
  path_item

val components :
  ?extensions:extensions ->
  ?schemas:schema json_object ->
  ?responses:response or_ref json_object ->
  ?parameters:parameter or_ref json_object ->
  ?examples:example or_ref json_object ->
  ?request_bodies:request_body or_ref json_object ->
  ?headers:header or_ref json_object ->
  ?security_schemes:security_scheme or_ref json_object ->
  ?links:link or_ref json_object ->
  ?callbacks:callback or_ref json_object ->
  ?path_items:path_item or_ref json_object ->
  ?media_types:media_type or_ref json_object ->
  unit ->
  components

val root :
  ?extensions:extensions ->
  ?path_extensions:extensions ->
  ?openapi:ref_ ->
  ?self:url ->
  info:info ->
  ?json_schema_dialect:ref_ ->
  ?servers:server list ->
  ?paths:path_item json_object ->
  ?webhooks:path_item or_ref json_object ->
  ?components:components ->
  ?security:security_requirement list ->
  ?tags:tag list ->
  ?external_docs:external_docs ->
  unit ->
  root
