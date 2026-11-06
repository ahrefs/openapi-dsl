(** OpenAPI types, JSON codecs, and document builders. *)

(** OpenAPI types and JSON codecs. *)
module Types = Openapi_dsl_internal.Openapi

(** Document builders and schema helpers. *)
module DSL = Openapi_dsl_internal.Openapi_dsl

(** Non-empty lists for enums and schema alternatives. *)
module NonEmptyList = Openapi_dsl_internal.NonEmptyList

(** Export DSL functions and types at the top level. *)
include module type of struct
  include DSL
end
