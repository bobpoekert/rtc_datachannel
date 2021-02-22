
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
    ice_servers: ice_server list option;
}

module Writer_make(S : sig
    type t
    val send : t -> string -> unit
    val set_buffered_low_callback : t -> (unit -> unit) -> unit
    val cleanup : t -> unit
end) = struct

    type t = {
        write_queue: (string * unit Lwt.u) Queue.t;
        write_in_flight: unit Lwt.u option ref;
        handle: S.t
    }
    
    let queue_pull chan () = 
        match !(chan.write_in_flight) with
        | None -> ()
        | Some v -> Lwt.wakeup_later v (); chan.write_in_flight := None;
        match Queue.take_opt chan.write_queue with
        | None -> ()
        | Some (buf, f) ->
                S.send chan.handle buf;
                chan.write_in_flight := Some f

    let create (handle : S.t) = 
        let res = {
            handle = handle;
            write_queue = Queue.create ();
            write_in_flight = ref None;
        } in
        S.set_buffered_low_callback handle (queue_pull res);
        res

    let cleanup v = S.cleanup v.handle
    
    let send dc message =
        match !(dc.write_in_flight) with
        | None -> 
                let o, i = Lwt.task () in 
                dc.write_in_flight := (Some i);
                S.send dc.handle message;
                o
        | Some _ -> 
                let o, i = Lwt.task () in 
                Queue.add (message, i) dc.write_queue;
                o

end

module Reader_make(S : sig
    type t

    val set_message_callback : t -> (string -> unit) -> unit
    val cleanup : t -> unit
end) = struct

    type t = {
        handle: S.t;
        data: string Tube.register;
    }

    let create (handle : S.t) = 
        let res = {
            handle = handle;
            data = Tube.create ();
        } in 
        S.set_message_callback handle
            (fun s -> Tube.add res.data (Some s));
        res

    let cleanup v = S.cleanup v.handle

    let source v = Tube.deref v.data


end
