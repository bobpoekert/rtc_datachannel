open Rtc_datachannel_generic

module C = Datachannel_c

module PeerConnection = struct

    type t = {
        handle: int;
        closed: (unit Lwt.u * unit Lwt.t);
        opened: (unit Lwt.u * unit Lwt.t);
        states: (rtc_state Lwt_stream.t * (rtc_state option -> unit));
        datachannels: (int Lwt_stream.t * (int option -> unit));

    }

    let cleanup v = 
        let fd = v.handle in
        Hashtbl.remove C.peer_connection_open_callbacks fd;
        Hashtbl.remove C.peer_connection_close_callbacks fd;
        Hashtbl.remove C.peer_connection_state_callbacks fd;
        Hashtbl.remove C.peer_connection_data_channel_callbacks fd;
        Hashtbl.remove C.peer_connection_local_description_callbacks fd;
        Hashtbl.remove C.peer_connection_data_local_candidate_callbacks fd
        (snd v.states) None;
        (snd v.datachannels) None

    let create (cfg : rtc_configuration) = 
        let socket = C.ortc_peer_connection_create
            cfg.ice_servers
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
            (fun () -> Lwt.wakeup_later (fst res.opened) ());
        Hashtbl.add C.peer_connection_close_callbacks
            socket
            (fun () -> 
                Lwt.wakeup_later (fst res.closed) ();
                cleanup res);
        Hashtbl.add C.peer_connection_state_callbacks socket
            (fun state_id ->
                let state = C.unpack_connection_state state_id in 
                (snd res.states) (Some state));
        Hashtbl.add C.peer_connection_data_channel_callbacks socket
            (fun dc_handle -> (snd res.datachannels) dc_handle);
        Hashtbl.add C.peer_connection_local_description_callbacks socket
            (fun desc -> (snd res.local_descriptions) (Some desc))
        Hashtbl.add C.peer_connection_local_candidate_callbacks socket
            (fun desc -> (snd res.local_candidates) (Some desc))
        Lwt.bind (snd res.opened) (fun () -> Lwt.return res)

    let close socket = 
        C.ortc_peer_connection_close socket.handle

    let states socket = fst socket.states
    let datachannels socket = fst socket.datachannels
    let local_descriptions socket = fst socket.local_descriptions
    let local_candidates socket = fst socket.local_candidates

    let set_local_description sock desc = 
        C.ortc_peer_connection_set_local_description
            sock.handle
            desc

    let set_remote_description sock desc = 
        C.ortc_peer_connection_set_remote_description
            sock.handle
            desc

    let add_remote_candidiate sock a b =
        C.ortc_peer_connection_add_remote_candidate
            sock.handle a b

    let get_local_description sock = 
        C.ortc_peer_connection_get_local_description sock.handle
    let get_remote_description sock = 
        C.ortc_peer_connection_get_remote_description sock.handle
    let get_local_address sock = 
        C.ortc_peer_connection_get_local_description sock.handle
    let get_remote_address sock = 
        C.ortc_peer_connection_get_remote_description sock.handle


end

module DataChannel = struct 

    type t = {
        handle: int;
        closed: (unit Lwt.u * unit Lwt.t);
        opened: (unit Lwt.u * unit Lwt.t);
        states: (rtc_state Lwt_stream.t * (rtc_state option -> unit));
        data: (string Lwt_stream.t * (string option -> unit));
        write_queue: (string * unit Lwt.u) Queue.t
        write_in_flight: unit Lwt.u option ref;
    }

    let queue_pull chan () = 
        match !(chan.write_in_flight) with
        | None -> ()
        | Some v -> Lwt.wakeup_later v (); chan.write_in_flight := None
        match Queue.take_opt chan.write_queue with
        | None -> ()
        | Some (buf, f) ->
                C.ortc_send_message chan.handle buf |> ignore;
                chan.write_in_flight := Some f
    
    let cleanup dc = 
        Hashtbl.remove C.data_channel_message_callbacks handle;
        Hashtbl.remove C.data_channel_close_callbacks handle;
        Hashtbl.remove C.data_channel_buffered_amount_low_callbacks handle;
        (snd dc.states) None;

    let inflate handle = 
        let res = {
            handle = handle;
            closed = Lwt.task ();
            opened = Lwt.task ();
            states = Lwt_stream.create ();
            data = Lwt_stream.create ();
            write_queue = Queue.create ();
            write_in_flight = ref None
        } in
        Hashtbl.add C.data_channel_message_callbacks handle
            (fun s -> (snd res.data) (Some s));
        Hashtbl.add C.data_channel_close_callbacks handle
            (fun () -> Lwt.wakeup_later res.closed (); cleanup res);
        Hashtbl.add C.data_channel_buffered_amount_low_callbacks handle
            (queue_pull res);

    let create pc name = 
        let handle = C.ortc_data_channel_create pc name in 
        inflate handle

    let await pc =
        Lwt_stream.map inflate (PeerConnection.datachannels pc)

    let send dc message =
        let message = Bytes.to_string message in 
        match !(dc.write_in_flight) with
        | None -> 
                let o, i = Lwt.task () in 
                dc.write_in_flight := Some i;
                C.ortc_send_message dc.handle message;
                o
        | Some f -> 
                let o, i = Lwt.task () in 
                Queue.add dc.write_queue (message, i);
                o

    let close dc = 
        C.ortc_data_channel_close dc.handle;
        cleanup dc

end
