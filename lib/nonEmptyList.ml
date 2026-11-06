type 'a t = { head : 'a; tail : 'a list }

let cons head tail = { head; tail }
let singleton head = cons head []

let of_list_opt = function
  | [] -> None
  | head :: tail -> Some (cons head tail)

let to_list { head; tail } = head :: tail
let map f { head; tail } = { head = f head; tail = List.map f tail }
let to_json encode xs = `List (List.map encode (to_list xs))

let of_json decode = function
  | `List (head :: tail) -> cons (decode head) (List.map decode tail)
  | json -> Jsonkit.of_json_error ~json "Expected a non-empty JSON array"
