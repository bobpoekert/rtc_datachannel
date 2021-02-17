open Rtc_datachannel_generic

(* TODO: find all occurrences of ignore and replace with proper error checking *)

module C = Datachannel_c

type 'a conduit = ('a Lwt_stream.t * ('a option -> unit))

module PeerConnection = struct


    type t = {
        handle: int;
        closed: (unit Lwt.t * unit Lwt.u);
        opened: (unit Lwt.t * unit Lwt.u);
        states: rtc_state conduit;
        datachannels: int conduit;
        local_descriptions : (string * string) conduit;
        local_candidates : (string * string) conduit;
    }

    let cleanup v = 
        let fd = v.handle in
        Hashtbl.remove C.peer_connection_open_callbacks fd;
        Hashtbl.remove C.peer_connection_close_callbacks fd;
        Hashtbl.remove C.peer_connection_state_callbacks fd;
        Hashtbl.remove C.peer_connection_data_channel_callbacks fd;
        Hashtbl.remove C.peer_connection_local_description_callbacks fd;
        Hashtbl.remove C.peer_connection_local_candidate_callbacks fd;
        (snd v.states) None;
        (snd v.datachannels) None

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
            states = Lwt_stream.create ();
            datachannels = Lwt_stream.create ();
            local_descriptions = Lwt_stream.create ();
            local_candidates = Lwt_stream.create ();
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
                (snd res.states) (Some state));
        Hashtbl.add C.peer_connection_data_channel_callbacks socket
            (fun dc_handle -> (snd res.datachannels) (Some dc_handle));
        Hashtbl.add C.peer_connection_local_description_callbacks socket
            (fun v -> ((snd res.local_descriptions) (Some v)));
        Hashtbl.add C.peer_connection_local_candidate_callbacks socket
            (fun desc -> (snd res.local_candidates) (Some desc));
        Lwt.bind (fst res.opened) (fun () -> Lwt.return (Result.ok res))

    let close socket = 
        C.ortc_peer_connection_close socket.handle |> ignore

    let states socket = fst socket.states
    let datachannels socket = fst socket.datachannels
    let local_descriptions socket = fst socket.local_descriptions
    let local_candidates socket = fst socket.local_candidates

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

module Writer = struct

    type t = {
        write_queue: (string * unit Lwt.u) Queue.t;
        write_in_flight: unit Lwt.u option ref;
        handle: int
    }
    
    let queue_pull chan () = 
        match !(chan.write_in_flight) with
        | None -> ()
        | Some v -> Lwt.wakeup_later v (); chan.write_in_flight := None;
        match Queue.take_opt chan.write_queue with
        | None -> ()
        | Some (buf, f) ->
                C.ortc_send_message chan.handle buf |> ignore;
                chan.write_in_flight := Some f

    let create handle = 
        let res = {
            handle = handle;
            write_queue = Queue.create ();
            write_in_flight = ref None;
        } in
        Hashtbl.add C.data_channel_buffered_amount_low_callbacks handle
            (queue_pull res);
        res

    let cleanup v = 
        let handle = v.handle in
        Hashtbl.remove C.data_channel_buffered_amount_low_callbacks handle
    
    let send dc message =
        let message = Bytes.to_string message in 
        match !(dc.write_in_flight) with
        | None -> 
                let o, i = Lwt.task () in 
                dc.write_in_flight := (Some i);
                C.ortc_send_message dc.handle message |> ignore;
                o
        | Some _ -> 
                let o, i = Lwt.task () in 
                Queue.add (message, i) dc.write_queue;
                o

end

module Reader = struct

    type t = {
        handle: int;
        data: string conduit;
    }

    let create handle = 
        let res = {
            handle = handle;
            data = Lwt_stream.create ();
        } in 
        Hashtbl.add C.data_channel_message_callbacks handle
            (fun s -> (snd res.data) (Some s));
        res

    let cleanup v = 
        Hashtbl.remove C.data_channel_message_callbacks v.handle

    let source v = fst v.data


end

module DataChannel = struct 

    type t = {
        handle: int;
        closed: (unit Lwt.t * unit Lwt.u);
        opened: (unit Lwt.t * unit Lwt.u);
        states: rtc_state conduit;
        writer: Writer.t;
        reader: Reader.t;
    }

    
    let cleanup dc = 
        Writer.cleanup dc.writer;
        Reader.cleanup dc.reader;
        let handle = dc.handle in
        Hashtbl.remove C.data_channel_close_callbacks handle;
        Hashtbl.remove C.data_channel_open_callbacks handle;
        (snd dc.states) None

    let inflate handle = 
        let res = {
            handle = handle;
            closed = Lwt.task ();
            opened = Lwt.task ();
            states = Lwt_stream.create ();
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
        Lwt_stream.map inflate (PeerConnection.datachannels pc)

    let send dc message =
        Writer.send dc.writer message

    let close dc = 
        C.ortc_data_channel_close dc.handle |> ignore;
        cleanup dc

    let source dc = 
        Lwt_stream.map
            (fun v -> Result.ok (Bytes.of_string v))
            (Reader.source dc.reader)

    let await_open dc = fst dc.opened
    let await_close dc = fst dc.closed

end

module WebSocket = struct 


    type t = {
        handle: int;
        closed: (unit Lwt.t * unit Lwt.u);
        opened: (unit Lwt.t * unit Lwt.u);
        writer: Writer.t;
        reader: Reader.t;
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
        Writer.send ws.writer message

    let source ws = 
        Lwt_stream.map
            (fun v -> Result.ok (Bytes.of_string v ))
            (Reader.source ws.reader)

    let close ws = 
        C.ortc_websocket_close ws.handle |> ignore

    let await_open ws = fst ws.opened
    let await_close ws = fst ws.closed

end
