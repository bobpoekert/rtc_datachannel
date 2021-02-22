open Rtc_datachannel_generic

(*
 * connection init state machine:
 * 1. init (PeerConnection.create)
 * 2. get sdp (create_offer)
 * 3. send sdp via signaling server
 * 4. receive remote sdp via signaling server
 * 5. set remote sdp (set_remote_description)
 * 6. wait for state to change to Connected
 * 7. if client, create data channel (DataChannel.create) else await data channel (DataChannel.await)
 * 8. connection established, begin communicating
 *)

module PeerConnection : sig

    type t

    val create : rtc_configuration -> (t, string) Result.result Lwt.t

    val close : t -> unit

    val states : t -> rtc_state Tube.t

    val local_descriptions : t -> (string * string) Tube.t
    val local_candidates : t -> (string * string) Tube.t

    val set_local_description : t -> string -> unit Lwt.t
    val set_remote_description : t -> string -> unit Lwt.t

    val get_local_description : t -> string option
    val get_remote_description : t -> string option

    val create_offer : t -> string option Lwt.t
    val create_answer : t -> string option Lwt.t

end

module DataChannel : sig
    
    type t

    val create : PeerConnection.t -> string -> t Lwt.t
    val await : PeerConnection.t -> t Tube.t

    val source : t -> (string, string) Result.result Tube.t

    val send : t -> string -> unit Lwt.t

    val close : t -> unit

    val await_close : t -> unit Lwt.t
    val await_open : t -> unit Lwt.t

end

module WebSocket : sig

    type t

    val create : string -> (t, string) Result.result Lwt.t

    val send : t -> string -> unit Lwt.t

    val source : t -> (string, string) Result.result Tube.t

    val close : t -> unit

    val await_close : t -> unit Lwt.t
    val await_open : t -> unit Lwt.t


end
