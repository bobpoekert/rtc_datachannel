open Rtc_datachannel_generic

module PeerConnection : sig

    type t

    val create : rtc_configuration -> t Lwt.t

    val close : t -> unit

    val get_state : t -> rtc_state

    val states : t -> rtc_state Lwt_stream.t

end

module DataChannel : sig
    
    type t

    val create : PeerConnection.t -> string -> t Lwt.t
    val await : PeerConnection.t -> t Lwt.t

    val source : t -> (bytes, string) Result.result Lwt_stream.t

    val send : t -> bytes -> unit

    val close : t -> unit

    val on_close : t -> (t -> unit) -> unit

end

module WebSocket : sig

    type t

    val create : string -> (t, string) Result.result Lwt.t

    val send : t -> bytes -> unit

    val source : t -> (bytes, string) Result.result Lwt_stream.t

    val close : t -> unit

    val on_close : t -> (t -> unit) -> unit

end
