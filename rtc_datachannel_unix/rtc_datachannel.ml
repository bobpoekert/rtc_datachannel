open Rtc_datachannel_generic

(* TODO: find all occurrences of ignore and replace with proper error checking *)

module C = Datachannel_c

module PeerConnection = struct


    type t = {
        handle: int;
        closed: (unit Lwt.t * unit Lwt.u);
        opened: (unit Lwt.t * unit Lwt.u);
        states: rtc_state Tube.register;
        datachannels: int Tube.register;
        local_descriptions : (string * string) Tube.register;
        local_candidates : (string * string) Tube.register;
    }

    let cleanup v = 
        let fd = v.handle in
        Hashtbl.remove C.peer_connection_open_callbacks fd;
        Hashtbl.remove C.peer_connection_close_callbacks fd;
        Hashtbl.remove C.peer_connection_state_callbacks fd;
        Hashtbl.remove C.peer_connection_data_channel_callbacks fd;
        Hashtbl.remove C.peer_connection_local_description_callbacks fd;
        Hashtbl.remove C.peer_connection_local_candidate_callbacks fd;
        Tube.add v.states None;
        Tube.add v.datachannels None

    let create (cfg : rtc_configuration) = 
        let socket = C.ortc_peer_connection_create
            (match cfg.ice_servers with
            | Some v -> Array.of_list v
            | None -> [||])
            true
            0 0 in

        let res = {
            handle = socket;
            closed = Lwt.task ();
            opened = Lwt.task ();
            states = Tube.create ();
            datachannels = Tube.create ();
            local_descriptions = Tube.create ();
            local_candidates = Tube.create ();
        } in 

        Hashtbl.add C.peer_connection_open_callbacks
            socket
            (fun () -> Lwt.wakeup_later (snd res.opened) ());
        Hashtbl.add C.peer_connection_close_callbacks
            socket
            (fun () -> 
                Lwt.wakeup_later (snd res.closed) ();
                cleanup res);
        Hashtbl.add C.peer_connection_state_callbacks socket
            (fun state_id ->
                let state = C.unpack_connection_state state_id in 
                Tube.add res.states (Some state));
        Hashtbl.add C.peer_connection_data_channel_callbacks socket
            (fun dc_handle -> Tube.add res.datachannels (Some dc_handle));
        Hashtbl.add C.peer_connection_local_description_callbacks socket
            (fun v -> (Tube.add res.local_descriptions (Some v)));
        Hashtbl.add C.peer_connection_local_candidate_callbacks socket
            (fun desc -> Tube.add res.local_candidates (Some desc));
        Lwt.bind (fst res.opened) (fun () -> Lwt.return (Result.ok res))

    let close socket = 
        C.ortc_peer_connection_close socket.handle |> ignore

    let states socket = Tube.deref socket.states
    let datachannels socket = Tube.deref socket.datachannels
    let local_descriptions socket = Tube.deref socket.local_descriptions
    let local_candidates socket = Tube.deref socket.local_candidates

    let set_local_description sock desc = 
        C.ortc_peer_connection_set_local_description
            sock.handle
            desc
        |> ignore

    let set_remote_description sock desc = 
        C.ortc_peer_connection_set_remote_description
            sock.handle
            desc
        |> ignore

    let get_local_description sock = 
        C.ortc_peer_connection_get_local_description sock.handle
    let get_remote_description sock = 
        C.ortc_peer_connection_get_remote_description sock.handle
    let get_local_address sock = 
        C.ortc_peer_connection_get_local_description sock.handle
    let get_remote_address sock = 
        C.ortc_peer_connection_get_remote_description sock.handle

    let add_remote_candidate sock candidate mid =
        C.ortc_peer_connection_add_remote_candidate sock.handle
            candidate mid
        |> ignore

end

module DataChannelWriter = Writer_make(struct
    type t = int

    let send h v = 
        C.ortc_send_message h v

    let set_buffered_low_callback h cb = 
        Hashtbl.add C.data_channel_buffered_amount h cb

    let cleanup h =
        Hashtbl.remove C.data_channel_buffered_amount h
end)

module DataChannelReader = Reader_make(struct

    type t = int

    let set_message_callbach h cb = 
        Hashtbl.add C.data_channel_message_callbacks h cb

    let cleanup h = 
        Hashtbl.remove C.data_channel_message_callbacks h

end)


module DataChannel = struct 

    type t = {
        handle: int;
        closed: (unit Lwt.t * unit Lwt.u);
        opened: (unit Lwt.t * unit Lwt.u);
        states: rtc_state Tube.register;
        writer: DataChannelWriter.t;
        reader: DataChannelReader.t;
    }

    
    let cleanup dc = 
        DataChannelWriter.cleanup dc.writer;
        DataChannelReader.cleanup dc.reader;
        let handle = dc.handle in
        Hashtbl.remove C.data_channel_close_callbacks handle;
        Hashtbl.remove C.data_channel_open_callbacks handle;
        Tube.add dc.states None

    let inflate handle = 
        let res = {
            handle = handle;
            closed = Lwt.task ();
            opened = Lwt.task ();
            states = Tube.create ();
            reader = Reader.create handle;
            writer = Writer.create handle;
        } in
        Hashtbl.add C.data_channel_close_callbacks handle
            (fun () -> Lwt.wakeup_later (snd res.closed) (); cleanup res);
        Hashtbl.add C.data_channel_open_callbacks handle
            (fun () -> Lwt.wakeup_later (snd res.opened) ());
        res

    let create (pc : PeerConnection.t) name = 
        let handle = C.ortc_data_channel_create pc.handle name in 
        inflate handle |> Lwt.return

    let await pc =
        Tube.map inflate (PeerConnection.datachannels pc)

    let send dc message =
        DataChannelWriter.send dc.writer message

    let close dc = 
        C.ortc_data_channel_close dc.handle |> ignore;
        cleanup dc

    let source dc = 
        Tube.map
            (fun v -> Result.ok (Bytes.of_string v))
            (DataChannelReader.source dc.reader)

    let await_open dc = fst dc.opened
    let await_close dc = fst dc.closed

end

module WebSocket = struct 


    type t = {
        handle: int;
        closed: (unit Lwt.t * unit Lwt.u);
        opened: (unit Lwt.t * unit Lwt.u);
        writer: DataChannelWriter.t;
        reader: DataChannelReader.t;
    }

    let cleanup ws = 
        let handle = ws.handle in 
        Reader.cleanup ws.reader;
        Writer.cleanup ws.writer;
        Hashtbl.remove C.websocket_close_callbacks handle;
        Hashtbl.remove C.websocket_open_callbacks handle

    let create uri = 
        let handle = C.ortc_websocket_create uri in 
        let res = {
            handle = handle;
            reader = Reader.create handle;
            writer = Writer.create handle;
            closed = Lwt.task ();
            opened = Lwt.task ();
        } in
        Hashtbl.add C.websocket_close_callbacks handle
            (fun () -> Lwt.wakeup_later (snd res.closed) (); cleanup res);
        Hashtbl.add C.websocket_open_callbacks handle
            (fun () -> Lwt.wakeup_later (snd res.opened) ());
        Lwt.return (Result.ok res)


    let send ws message = 
        DataChannelWriter.send ws.writer message

    let source ws = 
        Tube.map
            (fun v -> Result.ok (Bytes.of_string v ))
            (DataChannelReader.source ws.reader)

    let close ws = 
        C.ortc_websocket_close ws.handle |> ignore

    let await_open ws = fst ws.opened
    let await_close ws = fst ws.closed

end
