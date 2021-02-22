open Js_of_ocaml
module Lwt_js = Js_of_ocaml_lwt.Lwt_js
open Rtc_datachannel_generic

let rtc_configuration_to_js (cfg : rtc_configuration) = 
    object%js (_self)
        val iceServers = cfg.ice_servers
    end

module PeerConnection = struct

    type t = {
        conn: Webrtc._RTCPeerConnection Js.t;
        states: rtc_state Tube.register;
        datachannels: Webrtc._RTCDataChannel Js.t Tube.register;
        local_descriptions: (string * string) Tube.register;
        local_candidates: (string * string) Tube.register;
    }

    let datachannels v = Tube.deref v.datachannels

    let close (v : t) = 
        (v.conn)##close

    let connection_state_of_js v : rtc_state =
        match (Js.to_string v) with
        | "new" -> New
        | "connecting" -> Connecting
        | "connected" -> Connected
        | "disconnected" -> Disconnected
        | "failed" -> Failed
        | "closed" -> Closed
        | _ -> Failed (* TODO: maybe crash here? *)

    let get_state (v : t) = connection_state_of_js (v.conn)##.connectionState

    let states (v : t) = Tube.deref v.states
    
    let unpack_offer res =
        let res = Promise.to_lwt res in
        Lwt.map (fun v -> 
            match Js.Optdef.to_option v##.sdp with
            | None -> None
            | Some v -> Some (Js.to_string v)) res

    let create_offer (v : t) =
        (v.conn)##createOffer_ |> unpack_offer

    let create_answer (v : t) = 
        (v.conn)##createAnswer_ |> unpack_offer

    let create cfg = 
		let pc = Js.Unsafe.global##.RTCPeerConnection in
        let dc = new%js pc (rtc_configuration_to_js cfg) in
        let states = Tube.create () in 
        let datachannels = Tube.create () in
        let local_descriptions = Tube.create () in
        let local_candidates = Tube.create () in
        let res = {
            conn = dc;
            states = states;
            datachannels = datachannels;
            local_descriptions = local_descriptions;
            local_candidates = local_candidates;
        } in
		pc##.onconnectionstatechange := Dom.handler
            (fun _e -> 
                Tube.add states (Some (get_state res));
				Js._false);
        pc##.ondatachannel := Dom.handler
            (fun e -> 
                Tube.add datachannels e##.channel;
                Js._false);
        Tube.await
            (fun state ->
                match state with 
                | New -> None
                | Connecting -> None
                | Connected -> Some (Result.ok res)
                | Failed -> Some (Result.error "failed")
                | Disconnected -> Some (Result.error "disconnected")
                | Closed -> Some (Result.error "closed"))
            (Tube.deref states)

    let local_candidates v = Tube.deref v.local_candidates
    let local_descriptions v = Tube.deref v.local_descriptions

    let description_init sdp = 
        let cls = Js.Unsafe.global##.RTCSessionDescriptionInit in
        let init = new%js cls in 
        init##.sdp := sdp;
        init

    let set_local_description pc sdp = 
        let init = description_init sdp in
        let res = (pc.conn)##setLocalDescription init in 
        Promise.to_lwt res

    let set_remote_description pc sdp = 
        let init = description_init sdp in 
        let res = (pc.conn)##setRemoteDescription init in
        Promise.to_lwt res


    let get_local_description pc = 
        let desc = (pc.conn)##.localDescription in 
        match Js.Opt.to_option desc with
        | None -> None
        | Some v -> Some (Js.to_string v##.sdp)

    let get_remote_description pc = 
        let desc = (pc.conn)##.remoteDescription in
        match Js.Opt.to_option desc with
        | None -> None
        | Some v -> Some (Js.to_string v##.sdp)

end

module DataChannelWriter = Writer_make(struct
    type t = Webrtc._RTCDataChannel Js.t

    let send (dc : t) v = 
        dc##send_string (Js.string v)

    let set_buffered_low_callback (dc : t) cb = 
        dc##.onbufferedamountlow := Dom.handler
            (fun _ -> cb (); Js._false)

    let cleanup _ = ()

end)

module DataChannelReader = Reader_make(struct
    type t = Webrtc._RTCDataChannel Js.t

    let set_message_callback (h : t) cb = 
        h##.onmessage := Dom.handler
            (fun (e : Webrtc._RTCDataChannel Webrtc.messageEvent Js.t) -> 
                cb (Js.to_string e##.data);
                Js._false)

    let cleanup _ = ()

end)

type datachannel_t = Webrtc._RTCDataChannel Js.t

module DataChannel = struct

    type t = {
        channel: datachannel_t;
        reader: DataChannelReader.t;
        writer: DataChannelWriter.t;
        closed: unit Lwt.t;
        opened: unit Lwt.t;
    }

    let inflate (dc : datachannel_t) =
        let close_in, close_out = Lwt.task () in 
        let open_in, open_out = Lwt.task () in 
        dc##.onclose := Dom.handler
            (fun _ -> Lwt.wakeup_later close_out (); Js._false);
        dc##.onopen := Dom.handler
            (fun _ -> Lwt.wakeup_later open_out (); Js._false);
        let reader = DataChannelReader.create dc in
        let writer = DataChannelWriter.create dc in
        Lwt.on_success close_in (fun _ -> 
            DataChannelReader.cleanup reader;
            DataChannelWriter.cleanup writer);
        Lwt.map (fun _ -> {
            channel = dc;
            closed = close_in;
            opened = open_in;
            reader = reader;
            writer = writer;
        }) open_in

    let create (pc : PeerConnection.t) (name : string) : t Lwt.t = 
        let dc = (pc.conn)##createDataChannel_ (Js.string name) in 
        inflate dc

    let await (pc : PeerConnection.t) = 
        Tube.map_lwt
            inflate 
            (PeerConnection.datachannels pc)

    let source (dc : t) = 
        Tube.map
            Result.ok
            (DataChannelReader.source dc.reader)

    let send (dc : t) buf = 
        DataChannelWriter.send dc.writer buf


    let close (dc : t) = 
        (dc.channel)##close

    let await_open (dc : t) = dc.opened
    let await_close (dc : t) = dc.closed

end

module WebSocketWriterInner = struct
    type t = {
        handle: WebSockets.webSocket Js.t;
        running: bool ref;
    }

    let send (dc : t) v = 
        (dc.handle)##send (Js.string v)

    let set_buffered_low_callback (dc : t) cb = 
        (* TODO turn off poller if queue is empty *)
        let rec it _ = 
            if (dc.handle)##.bufferedAmount < 1024 then
                cb ();
            if !(dc.running) then
                (Dom_html.window##requestAnimationFrame 
                    (Js.wrap_callback it) |> ignore)
        in
        it 0.

    let cleanup h = h.running := false

end

module WebSocketWriter = Writer_make(WebSocketWriterInner)
module WebSocketReader = Reader_make(struct
    type t = WebSockets.webSocket Js.t

    let set_message_callback (h : t) cb = 
        h##.onmessage := Dom.handler
            (fun e -> 
                cb (Js.to_string e##.data);
                Js._false)

    let cleanup _ = ()

end)

module WebSocket = struct 

	type t = {
        handle: WebSockets.webSocket Js.t;
        writer: WebSocketWriter.t;
        reader: WebSocketReader.t;
        closed: unit Lwt.t;
        opened: unit Lwt.t;
    }

    let create (url : string) : (t, string) Result.result Lwt.t  =
        let ws = new%js WebSockets.webSocket (Js.string url) in
        let close_in, close_out = Lwt.task () in 
        let open_in, open_out = Lwt.task () in 
        ws##.onclose := Dom.handler 
            (fun _e -> Lwt.wakeup_later close_out (); Js._false);
        ws##.onopen := Dom.handler 
            (fun _e -> Lwt.wakeup_later open_out (); Js._false);
        Lwt.map (fun () -> 
            {
                handle = ws;
                writer = WebSocketWriter.create {
                    handle = ws;
                    running = ref false;
                };
                reader = WebSocketReader.create ws;
                closed = close_in;
                opened = open_in;
            } |> Result.ok
        ) open_in

    let send (ws : t) buf = 
        WebSocketWriter.send ws.writer buf

    let source (ws : t) = 
        Tube.map
            Result.ok
            (WebSocketReader.source ws.reader)

    let close (ws : t) = 
        (ws.handle)##close;
        WebSocketWriter.cleanup ws.writer;
        WebSocketReader.cleanup ws.reader

    let await_open (ws : t) = ws.opened
    let await_close (ws : t) = ws.closed


end
