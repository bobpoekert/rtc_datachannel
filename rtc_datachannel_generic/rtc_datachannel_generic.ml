
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
    ice_candidate_pool_size: int option;
    ice_servers: ice_server list option;
    ice_transport_policy: rtc_ice_transport_policy option;
    peer_identity: string option;
    rtcp_mux_policy: rtc_mux_policy option;

}