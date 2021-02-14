open Js_of_ocaml
module Lwt_js = Js_of_ocaml_lwt.Lwt_js
open Rtc_datachannel_generic

let string_option_to_js v = 
    match v with 
    | None -> Js.null
    | Some v -> Js.some (Js.string v)

let rtc_configuration_to_js (cfg : rtc_configuration) = 
    object%js (_self)
        val bundlePolicy = cfg.bundle_policy
        val iceCandidatePoolSize = cfg.ice_candidate_pool_size
        val iceServers = cfg.ice_servers
        val iceTransportPolicy = cfg.ice_transport_policy
        val peerIdentity = (string_option_to_js cfg.peer_identity)
    end

module PeerConnection = struct

    type t = Webrtc._RTCPeerConnection Js.t

    let create cfg = 
		let pc = Js.Unsafe.global##.RTCPeerConnection in
        let res = new%js pc (rtc_configuration_to_js cfg) in 
        Lwt.return res


    let close (v : t) = 
        v##close

    let connection_state_of_js v : rtc_state =
        match (Js.to_string v) with
        | "new" -> New
        | "connecting" -> Connecting
        | "connected" -> Connected
        | "disconnected" -> Disconnected
        | "failed" -> Failed
        | "closed" -> Closed
        | _ -> Failed (* TODO: maybe crash here? *)

    let get_state (v : t) = connection_state_of_js v##.connectionState

    let states (v : t) = 
        let stream, put = Lwt_stream.create () in 
		v##.onconnectionstatechange := Dom.handler
            (fun _e -> 
                put (Some (get_state v));
				Js._false);
        stream

end


module DataChannel = struct

    type t = Webrtc._RTCDataChannel Js.t

    let create (pc : PeerConnection.t) (name : string) : t Lwt.t = 
        pc##createDataChannel_ (Js.string name) |> Lwt.return

    let await (pc : PeerConnection.t) = 
        let o, i = Lwt.task () in 
		pc##.ondatachannel := Dom.handler
            (fun e -> Lwt.wakeup_later i e##.channel; Js._false);
        o

    let source (dc : t) = 
        let stream, put = Lwt_stream.create () in 
		dc##.onmessage := Dom.handler
        	(fun (e : Webrtc._RTCDataChannel Webrtc.messageEvent Js.t) -> 
				put (Some (Result.ok (Bytes.of_string (Js.to_string e##.data)))); 
				Js._false);
        stream


    let send (dc : t) buf = 
        dc##send_string (Js.string (Bytes.to_string buf))

    let close (dc : t) = 
        dc##close

    let on_close (dc : t) cb = 
		dc##.onclose := Dom.handler (fun _ -> cb dc; Js._false)

end

module WebSocket = struct 

	type t = WebSockets.webSocket Js.t

    let create url =
		new%js WebSockets.webSocket (Js.string url) |> Result.ok |> Lwt.return

    let send (ws : t) buf = 
        ws##send (Js.string (Bytes.to_string buf))

    let source (ws : t) = 
        let stream, put = Lwt_stream.create () in 
        ws##.onmessage := Dom.handler
            (fun (e : 'a WebSockets.messageEvent Js.t) -> put (Some (Result.ok (Bytes.of_string (Js.to_string e##.data)))); Js._false);
        stream

    let close (ws : t) = 
        ws##close 

    let on_close ws cb = 
        ws##.onclose := Dom.handler (fun _e -> cb ws; Js._false)

end
