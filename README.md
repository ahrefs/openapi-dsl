# openapi-dsl

OCaml DSL for OpenAPI 3.2 documents, with JSON codecs from [jsonkit](https://github.com/melange-community/jsonkit).

```ocaml
open OpenAPI
open Jsonkit.Primitives

type user = {
  id : string;
  email : string option;
} [@@deriving jsonschema]

let api = create ~info:(info ~title:"Users" ~version:"1.0.0" ()) ()

let user = Ref.add_schema api "User" user_jsonschema
let json = media_type ~schema:user ()
let ok = Ref.add_response api "UserResponse" (response ~content:[content "application/json" json] ())
let bearer = Ref.add_security_scheme api "Bearer" (http_security_scheme ~scheme:"bearer" ())

let () = add_path api "/users" (path_item ~get:(operation ~responses:["200", ok] ()) ())
let document = root_to_json (finalize api)
```

You can also build a schema without an OCaml type:
```ocaml
let user_schema =
  OpenAPI.(Schema.object_ [ required "id" (schema ~typ:String ()); optional "email" (schema ~typ:String ()) ])
```

`optional` allows a property to be absent. Use `nullable` if its value may be `null`.

Other modules:
- `OpenAPI.Types` exposes the raw records, variants, and codecs.
- `OpenAPI.NonEmptyList` provides non-empty lists for enums and schema alternatives.
