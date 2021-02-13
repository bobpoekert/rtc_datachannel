
type peer_connection_t
type data_channel_t

module type PeerConnection = sig

    type t = peer_connection_t

    (* https://trac.webkit.org/browser/trunk/Source/WebCore/Modules/mediastream/RTCConfiguration.idl 
     * https://developer.mozilla.org/en-US/docs/Web/API/RTCPeerConnection/RTCPeerConnection
     *)
    type rtc_ice_transport_policy = All | Relay
    type rtc_mux_policy = Invalid | Require
    type rtc_bundle_policy = Balanced | Max_compat | Max_bundle
    type rtc_state = New | Connecting | Connected | Disconnected | Failed | Closed
    type rtc_certificate = string
    type ice_server = string


    type rtc_configuration = {
        bundle_policy: rtc_bundle_policy option;
        certificates: rtc_certificate list option;
        ice_candidate_pool_size: int option;
        ice_servers: ice_server list option;
        ice_transport_policy: rtc_ice_transport_policy option;
        peer_identity: string option;
        rtcp_mux_policy: rtc_mux_policy option;

    }

    val create : rtc_configuration -> t Lwt.t

    val close : t -> unit

    val on_close : t -> (t -> unit) -> unit

    val get_state : t -> rtc_state

    val states : t -> rtc_state Lwt_stream.t

end

module type DataChannel = sig
    
    type t = data_channel_t

    val create : peer_connection_t -> t Lwt.t
    val await : peer_connection_t -> t Lwt.t

    val sink : t -> bytes Lwt_stream.t -> unit

    val source : t -> (bytes, string) Result.result Lwt_stream.t

    val close : t -> unit

    val on_close : t -> (t -> unit) -> unit

end

module type WebSocket = sig

    type t

    val create : string -> (string, t) Result.result Lwt.t

    val sink : t -> bytes Lwt_stream.t -> unit

    val source : t -> (bytes, string) Result.result Lwt_stream.t

    val close : t -> unit

    val on_close : t -> (t -> unit) -> unit

end
