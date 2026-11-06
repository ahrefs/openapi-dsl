(lang dune 3.23)

(name openapi-dsl)

(generate_opam_files true)

(source
 (github ahrefs/openapi-dsl))

(authors "Leonardo Santos <leonardo.santos@ahrefs.com>")

(maintainers "Leonardo Santos <leonardo.santos@ahrefs.com>")

(license MIT)

(package
 (name openapi-dsl)
 (synopsis "Small DSL for constructing OpenAPI documents.")
 (depends
  (ocaml (>= 4.13))
  (jsonkit (>= 1.1.0))
  yojson)
 (tags (openapi dsl)))
