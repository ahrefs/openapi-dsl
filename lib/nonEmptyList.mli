(** Lists containing at least one element. *)
type 'a t = private { head : 'a; tail : 'a list }

val cons : 'a -> 'a list -> 'a t
val singleton : 'a -> 'a t
val of_list_opt : 'a list -> 'a t option
val to_list : 'a t -> 'a list
val map : ('a -> 'b) -> 'a t -> 'b t

(** Encode as a JSON array. Decoding rejects empty arrays and non-arrays
    with [Jsonkit.Of_json_error]. *)
val to_json : ('a -> Jsonkit.t) -> 'a t -> Jsonkit.t
val of_json : (Jsonkit.t -> 'a) -> Jsonkit.t -> 'a t
