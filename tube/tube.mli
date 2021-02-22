
type 'a t

type 'a register

val create : unit -> 'a register

val append : 'a t -> 'a t -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t
val map_lwt : ('a -> 'b Lwt.t) -> 'a t -> 'b t
val filter : ('a -> bool) -> 'a t -> 'a t
val filter_map : ('a -> 'b option) -> 'a t -> 'b t
val take_while : ('a -> bool) -> 'a t -> 'b t
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a Lwt.t
val iter : ('a -> unit) -> 'a t -> unit Lwt.t
val hd : 'a t -> 'a option Lwt.t
val tl : 'a t -> 'a t
val interleave : 'a t list -> 'a t
val await : ('a -> 'b option) -> 'a t -> 'b Lwt.t

val add : 'a register -> 'a option -> unit
val deref : 'a register -> 'a t
