open Rtc_datachannel_generic

module PeerConnection : sig

    type t

    val create : rtc_configuration -> (t, string) Result.result Lwt.t

    val close : t -> unit

    val states : t -> rtc_state Lwt_stream.t

    val local_descriptions : t -> (string * string) Lwt_stream.t
    val local_candidates : t -> (string * string) Lwt_stream.t

    val set_local_description : t -> string -> unit
    val set_remote_description : t -> string -> unit
    val add_remote_candidate : t -> string -> string -> unit

    val get_local_description : t -> string
    val get_remote_description : t -> string

    val get_remote_address : t -> string
    val get_local_address : t -> string

end

module DataChannel : sig
    
    type t

    val create : PeerConnection.t -> string -> t Lwt.t
    val await : PeerConnection.t -> t Lwt_stream.t

    val source : t -> (bytes, string) Result.result Lwt_stream.t

    val send : t -> bytes -> unit Lwt.t

    val close : t -> unit

    val await_close : t -> unit Lwt.t

end

module WebSocket : sig

    type t

    val create : string -> (t, string) Result.result Lwt.t

    val send : t -> bytes -> unit

    val source : t -> (bytes, string) Result.result Lwt_stream.t

    val close : t -> unit

    val on_close : t -> (t -> unit) -> unit

end
