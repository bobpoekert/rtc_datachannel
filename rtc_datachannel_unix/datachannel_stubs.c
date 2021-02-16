#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/bigarray.h>
#include <caml/threads.h>
#include <caml/version.h>
#include <caml/callback.h>

#include <string.h>
#include <stdlib.h>
#include <stdint.h>

#include <rtc/rtc.h>

/* PeerConnection */

void ortc_peer_connection_state_callback(int pc, rtcState state, void *ptr) {

    caml_callback(*caml_named_value("ortc_peer_connection_state_callback"), Val_int(pc));

}

void ortc_peer_connection_gathering_state_callback(int pc, rtcState state, void *ptr) {

    caml_callback(*caml_named_value("ortc_peer_connection_gathering_state_callback"), 
            Val_int(pc));

}

void ortc_peer_connection_signaling_state_callback(int pc, rtcState state, void *ptr) {

    caml_callback(*caml_named_value("ortc_peer_connection_signaling_state_callback"), 
            Val_int(pc));

}

void ortc_peer_connection_data_channel_callback(int pc, int dc, void *ptr) {

    caml_callback(*caml_named_value("ortc_peer_connection_data_channel_callback"), 
            Val_int(dc));

}

void ortc_peer_connection_open_callback(int pc, void *ptr) {

    caml_callback(*caml_named_value("ortc_peer_connection_open_callback"), Val_unit);

}

void ortc_peer_connection_close_callback(int pc, void *ptr) {

    caml_callback(*caml_named_value("ortc_peer_connection_close_callback"), Val_int(pc));

}

void ortc_peer_connection_local_description_callback(
        int pc, const char *sdp, const char *type, void *pc) {

    caml_callback2(*caml_named_value("ortc_peer_connection_local_description_callback"),
            Val_int(pc), caml_copy_string(sdp), caml_copy_string(type));
}

void ortc_peer_connection_local_candidate_callback(
        int pc, const char *cand, const char *mid, void *pc) {

    caml_callback2(*caml_named_value("ortc_peer_connection_local_candidate_callback"),
            Val_int(pc), caml_copy_string(cand), caml_copy_string(mid));
}

CAMLprim value ortc_peer_connection_create(
        value ice_servers,
        value enable_ice_tcp,
        value port_range_begin
        value port_range_end) {

    size_t ice_server_count = Wosize_val(ice_servers) / sizeof(void *);
    size_t total_strings_size = 0;
    char **ice_strings = malloc(ice_server_count * sizeof(char *));

    if (!ice_strings) caml_failwith("out of memory");

    for (size_t i=0; i < ice_server_count; i++) {
        value v = Field(ice_servers, i);
        size_t size = caml_string_length(v);

        total_string_size += size;

        ice_strings[i] = String_val(v);
    }

    bool _enable_ice_tcp = Bool_val(enable_ice_tcp);

    uint16_t _port_range_begin = Int_val(port_range_begin);
    uint16_t _port_range_end = Int_val(port_range_end);

    rtcConfiguration cfg;
    cfg.iceServers = ice_strings;
    cfg.iceServersCount = ice_server_count;
    cfg.enableIceTcp = _enable_ice_tcp;
    cfg.portRangeBegin = _port_range_begin;
    cfg.portRangeEnd = _port_range_end;


    int fd = rtcCreatePeerConnection(&cfg);

    rtcSetStateChangeCallback(fd, 
            ortc_peer_connection_state_callback);
    rtcSetGatheringStateChangeCallback(fd, 
            ortc_peer_connection_gathering_state_callback);
    rtcSetSignalingStateChangeCallback(fd, 
            ortc_peer_connection_signaling_state_callback);
    rtcSetSignalingOpenCallback(fd, 
            ortc_peer_connection_open_callback);
    rtcSetSignalingCloseCallback(fd, 
            ortc_peer_connection_close_callback);
    rtcSetLocalDescriptionCallback(fd,
            ortc_local_description_callback);
    rtcSetLocalCandidateCallback(fd,
            ortc_local_candidate_callback);

    free(ice_strings);

    CAMLreturn(Val_int(fd));

}

CAMLprim value ortc_peer_connection_set_local_description(
        value pc, value type) {

    int _pc = Int_val(pc);
    const char *_type = String_val(type);

    int res = rtcSetLocalDescription(_pc, _type);

    CAMLreturn(Val_int(res));

}

#define STRING_GETTER(src, target) \
    CAMLprim value target(value pc) {\
        int _pc = Int_val(pc);\
        size_t size = 40960;\
        char *buf = NULL; \
        int res_size;\
        value v_res;\
        while(1) {\
            buf = malloc(size);\
            if (!buf) {\
                caml_failwith("out of memory");\
                break;\
            }\
            res_size = src(_pc, size);\
            if (res_size == RTC_ERR_TOO_SMALL) {\
                free(buf);\
                size *= 2;\
                continue;\
            } else {\
                v_res = caml_alloc_string(res);\
                char *c_res = String_val(v_res);\
                memcpy(c_res, v_res, res_size);\
                break;\
            }\
        }\
        if (buf) free(buf);\
        CAMLreturn(v_res);\
    }

STRING_GETTER(rtcGetLocalDescription,
        ortc_peer_connection_get_local_description)

STRING_GETTER(rtcGetRemoteDescription,
        ortc_peer_connection_get_remote_description)


STRING_GETTER(rtcGetLocalAddress,
        ortc_peer_connection_get_local_address)

STRING_GETTER(rtcGetRemoteAddress,
        ortc_peer_connection_get_remote_address)


CAMLprim value ortc_peer_connection_set_remote_description(
        value pc, value sdp, value type) {

    int _pc = Int_val(pc);
    char *_sdp = String_val(sdp);
    char *_type = String_val(type);

    int res = rtcSetRemoteDescription(_pc, _sdp, _type);

    CAMLreturn(Val_int(res));

}

CAMLprim value ortc_peer_connection_add_remote_candidate(
        value pc, value cand, value mid) {

    int _pc = Int_val(pc);
    char *_cand = String_val(cand);
    char *_mid = String_val(mid);

    int res = rtcAddRemoteCandidate(_pc, _cand, _mid);

    CAMLreturn(Val_int(res));

}

CAMLprim value ortc_peer_connection_close(value fd) {
    int _fd = Int_val(fd);

    int res = rtcDeletePeerConnection(_fd);

    CAMLreturn(Val_int(res));

}

/* DataChannel */

void ortc_data_channel_message_callback(int fd, const char *message, int size, void *p) {

    value v_message = caml_alloc_initialized_string(size, message);

    caml_callback2(*caml_named_value("ortc_data_channel_message_callback"),
            Val_int(fd), v_message);

}

void ortc_data_channel_error_callback(int fd, const char *error, void *p) {

    value v_error = caml_copy_string(error);

    caml_callback2(*caml_named_value("ortc_data_channel_error_callback"),
            Val_int(fd), v_error);

}

void ortc_data_channel_open_callback(int fd, void *p) {
    caml_callback(*caml_named_value("ortc_data_channel_open_callback"),
            Val_int(fd));
}

void ortc_data_channel_close_callback(int fd, void *p) {
    caml_callback(*caml_named_value("ortc_data_channel_close_callback"),
            Val_int(fd));

}

void ortc_data_channel_buffered_amount_low_callback(int fd) {
    caml_callback(*caml_named_value("ortc_data_channel_buffered_amount_callback"), 
            Val_int(fd));
}


CAMLprim value ortc_data_channel_create(
        value pc, value name) {

    int _pc = Int_val(pc);
    char *_name = String_val(name);

    int res = rtcCreateDataChannel(_pc, _name);

    rtcSetMessageCallback(res, ortc_data_channel_message_callback);
    rtcSetErrorCallback(res, ortc_data_channel_error_callback);
    rtcSetOpenCallback(res, ortc_data_channel_open_callback);
    rtcSetClosedCallback(res, ortc_data_channel_closed_callback);
    rtcSetBufferedAmountLowCallback(res, ortc_data_channel_buffered_amount_low_callback);


    CAMLreturn(Val_int(res));

}

CAMLprim value ortc_data_channel_close(value fd) {
    
    int _fd = Int_val(fd);

    int res = rtcDeleteDataChannel(_fd);

    CAMLreturn(Val_int(res));

}

CAMLprim value ortc_send_message(value fd, value message) {
    int _fd = Int_val(fd);
    char *_message = String_val(message);
    int message_size = caml_string_length(message);

    int res = rtcSendMessage(_fd, _message, message_size);

    CAMLreturn(Val_int(res));

}

CAMLprim value ortc_get_buffered_amount(value fd) {
    int _fd = Int_val(fd);

    int res = rtcGetBufferedAmount(_fd);

    CAMLreturn(Val_int(res));

}

CAMLprim value ortc_set_buffered_amount_low_threshold(
        value fd, value thresh) {

    int _fd = Int_val(fd);
    int _thresh = Int_val(thresh);

    int res = rtcSetBufferedAmountLowThreshold(_fd, _thresh);

    CAMLreturn(Val_int(res));

}

/* WebSocket */

void ortc_websocket_message_callback(int fd, const char *message, int size, void *p) {

    value v_message = caml_alloc_initialized_string(size, message);

    caml_callback2(*caml_named_value("ortc_websocketl_message_callback"),
            Int_val(fd), v_message);

}

void ortc_websocket_error_callback(int fd, const char *error, void *p) {

    value v_error = caml_copy_string(error);

    caml_callback2(*caml_named_value("ortc_websocket_error_callback"),
            Int_val(fd), v_error);

}

void ortc_websocket_open_callback(int fd, void *p) {
    caml_callback(*caml_named_value("ortc_websocket_open_callback"),
            Int_val(fd));
}

void ortc_websocket_close_callback(int fd, void *p) {
    caml_callback(*caml_named_value("ortc_websocket_close_callback"),
            Int_val(fd));

}

void ortc_websocket_buffered_amount_low_callback(int fd) {
    caml_callback(*caml_named_value("ortc_websocket_buffered_amount_callback"), 
            Int_val(fd));
}

CAMLprim value ortc_websocket_create(value url) {
    char *_url = String_val(url);

    int res = rtcCreateWebSocket(_url);
    
    rtcSetMessageCallback(res, ortc_websocket_message_callback);
    rtcSetErrorCallback(res, ortc_websocket_error_callback);
    rtcSetOpenCallback(res, ortc_websocket_open_callback);
    rtcSetClosedCallback(res, ortc_websocket_closed_callback);
    rtcSetBufferedAmountLowCallback(res, ortc_websocket_buffered_amount_low_callback);

    CAMLreturn(Val_int(res));

}

CAMLprim value ortc_websocket_close(value handle) {
    int _handle = Int_val(handle);

    int res = rtcDeleteWebSocket(_handle);

    CAMLreturn(Val_int(res));

}
