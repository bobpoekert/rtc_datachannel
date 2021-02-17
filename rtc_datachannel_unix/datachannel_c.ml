open Rtc_datachannel_generic

type 'a callbacks_t = (int, 'a -> unit) Hashtbl.t

let register_callback name = 
    let table = Hashtbl.create 256 in 
    let wrapper (fd : int) =
        match Hashtbl.find_opt table fd with 
        | None -> ()
        | Some cb -> cb () in
    Callback.register name wrapper;
    table

let register_callback1 name = 
    let table = Hashtbl.create 256 in 
    let wrapper (fd : int) arg = 
        match Hashtbl.find_opt table fd with
        | None -> ()
        | Some cb -> cb arg in
    Callback.register name wrapper;
    table

let register_callback2 name = 
    let table = Hashtbl.create 256 in 
    let wrapper (fd : int) arg arg2 = 
        match Hashtbl.find_opt table fd with
        | None -> ()
        | Some cb -> cb (arg, arg2) in
    Callback.register name wrapper;
    table

let peer_connection_state_callbacks : int callbacks_t = register_callback1 "ortc_peer_connection_state_callback" 

let unpack_connection_state state_id : rtc_state = 
    match state_id with 
    | 0 -> New
    | 1 -> Connecting
    | 2 -> Connected
    | 3 -> Disconnected
    | 4 -> Failed 
    | 5 -> Closed
    | _ -> raise (Failure "invalid rtc state !")

let peer_connection_gathering_state_callbacks : int callbacks_t =
    register_callback1 "ortc_peer_connection_gathering_state_callback"


    (*
let unpack_gathering_state state_id = 
    match state_id with 
    | 0 -> New
    | 1 -> In_progress
    | 2 -> Complete
    | _ -> raise (Faulure "invalid gathering state !")
    *)

let peer_connection_signaling_state_callbacks : int callbacks_t = 
    register_callback1 "ortc_peer_connection_signaling_state_callback"

    (*
let unpack_signaling_state state_id = 
    match state_id with 
    | 0 -> Stable 
    | 1 -> Have_local_offer
    | 2 -> Have_remote_offer
    | 3 -> Have_local_pranswer
    | 4 -> Have_remote_pranswer 
    | _ -> raise (Failure "invalid signaling state !")
    *)

let peer_connection_data_channel_callbacks : int callbacks_t = 
    register_callback1 "ortc_peer_connection_data_channel_callback"

let peer_connection_open_callbacks : unit callbacks_t = 
    register_callback "ortc_peer_connection_open_callback"
let peer_connection_close_callbacks : unit callbacks_t = 
    register_callback "ortc_peer_connection_close_callback"

let peer_connection_local_description_callbacks : (string * string) callbacks_t = 
    register_callback2 "ortc_peer_connection_local_description_callback"
let peer_connection_local_candidate_callbacks : (string * string) callbacks_t = 
    register_callback2 "ortc_peer_connection_local_candidate_callback"

external ortc_peer_connection_create : 
    string array (* ice servers *)
    -> bool (* enable ice tcp *) 
    -> int (* port range begin *)
    -> int (* port range end *)
    -> int (* fd *) = "ortc_peer_connection_create"
external ortc_peer_connection_close : int -> int = "ortc_peer_connection_close"

external ortc_peer_connection_set_local_description : int -> string -> int = "ortc_peer_connection_set_local_description"
external ortc_peer_connection_set_remote_description :
    int -> string -> string -> int = "ortc_peer_connection_set_remote_description"
external ortc_peer_connection_add_remote_candidate : 
    int -> string -> string -> int = "ortc_peer_connection_add_remote_candidate"

external ortc_peer_connection_get_local_description : int -> string = "ortc_peer_connection_get_local_description"
external ortc_peer_connection_get_local_address : int -> string = "ortc_peer_connection_get_local_address"
external ortc_peer_connection_get_remote_description : int -> string = "ortc_peer_connection_get_remote_description"
external ortc_peer_connection_get_remote_address : int -> string = "ortc_peer_connection_get_remote_address"


external ortc_data_channel_create : int -> string -> int  = "ortc_data_channel_create"
external ortc_data_channel_close : int -> int = "ortc_data_channel_close"

let data_channel_message_callbacks : string callbacks_t = 
    register_callback1 "ortc_data_channel_message_callback"

let data_channel_close_callbacks = 
    register_callback "ortc_data_channel_close_callback"
let data_channel_open_callbacks = 
    register_callback "ortc_data_channel_open_callback"

let data_channel_buffered_amount_low_callbacks = 
    register_callback "ortc_data_channel_buffered_amount_low_callback"

external ortc_send_message : int -> string -> int = "ortc_send_message"
external ortc_get_buffered_amount : int -> int  = "ortc_get_buffered_amount"
external ortc_set_buffered_amount_low_threshold : int -> int = "ortc_set_buffered_amount_low_threshold"

external ortc_websocket_create : string -> int = "ortc_websocket_create"
external ortc_websocket_close : int -> int = "ortc_websocket_close"

let websocket_message_callbacks : string callbacks_t = 
    register_callback1 "ortc_websocket_message_callback"

let websocket_close_callbacks = 
    register_callback "ortc_websocket_close_callback"
let websocket_open_callbacks = 
    register_callback "ortc_websocket_open_callback"

let websocket_buffered_amount_low_callbacks = 
    register_callback "ortc_websocket_buffered_amount_low_callback"
