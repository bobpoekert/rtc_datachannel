(*
MIT License

Copyright (c) 2019 Alexander Yanin

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

open Js_of_ocaml

class type longRange =
  object
    method max : int Js.prop
    method min : int Js.prop
  end

class type doubleRange =
  object
    method max : float Js.prop
    method min : float Js.prop
  end

class type point =
  object
    method x : float Js.prop
    method y : float Js.prop
  end

type 'a or_array

let wrap_single (x : 'a) : 'a or_array Js.t =
  (Obj.magic x)

let wrap_array (x : 'a Js.js_array Js.t) : 'a or_array Js.t =
  Js.Unsafe.coerce x

let cast_single (x : 'a or_array Js.t) : 'a Js.opt =
  if not @@ Js.instanceof x (Js.Unsafe.global##._Array)
  then Js.some (Obj.magic x)
  else Js.null

let cast_array (x : 'a or_array Js.t) : 'a Js.js_array Js.t Js.opt =
  if Js.instanceof x (Js.Unsafe.global##._Array)
  then Js.some (Js.Unsafe.coerce x)
  else Js.null

module Constrain = struct

  type ('a, 'b) v =
    [ `V of 'a
    | `O of 'b
    ]

  type ('a, 'b) mv =
    [ ('a, 'b) v
    | `A of 'a list
    ]

  module Long = struct

    type t

    class type obj =
      object
        inherit longRange
        method exact : int Js.optdef_prop
        method ideal : int Js.optdef_prop
      end

    type value = (int, obj Js.t) v

    let wrap : value -> t Js.t = function
      | `V x -> Js.Unsafe.coerce @@ Js.number_of_float @@ float_of_int x
      | `O x -> Js.Unsafe.coerce x

    let unwrap (x : t Js.t) : value =
      if Js.typeof x == Js.string "number"
      then `V (int_of_float @@ Js.float_of_number @@ Js.Unsafe.coerce x)
      else if Js.typeof x == Js.string "object"
      then `O (Js.Unsafe.coerce x)
      else failwith "Bad long value"

  end

  module Double = struct

    type t

    class type obj =
      object
        inherit doubleRange
        method exact : float Js.optdef_prop
        method ideal : float Js.optdef_prop
      end

    type value = (float, obj Js.t) v

    let wrap : value -> t Js.t = function
      | `V x -> Js.Unsafe.coerce @@ Js.number_of_float x
      | `O x -> Js.Unsafe.coerce x

    let unwrap (x : t Js.t) : value =
      if Js.typeof x == Js.string "number"
      then `V (Js.float_of_number @@ Js.Unsafe.coerce x)
      else if Js.typeof x == Js.string "object"
      then `O (Js.Unsafe.coerce x)
      else failwith "Bad double value"

  end

  module Bool = struct

    type t

    class type obj =
      object
        method exact : bool Js.t Js.optdef_prop
        method ideal : bool Js.t Js.optdef_prop
      end

    type value = (bool, obj Js.t) v

    let wrap : value -> t Js.t = function
      | `V x -> Js.Unsafe.coerce @@ Js.bool x
      | `O x -> Js.Unsafe.coerce x

    let unwrap (x : t Js.t) : value =
      if Js.typeof x == Js.string "boolean"
      then `V (Js.to_bool @@ Js.Unsafe.coerce x)
      else if Js.typeof x == Js.string "object"
      then `O (Js.Unsafe.coerce x)
      else failwith "Bad boolean value"

  end

  module String = struct
    type t

    class type obj =
      object
        method exact : Js.js_string Js.t Js.optdef_prop
        method ideal : Js.js_string Js.t or_array Js.optdef_prop
      end

    type value = (string, obj Js.t) mv

    let wrap : value -> t Js.t = function
      | `V x -> Js.Unsafe.coerce @@ Js.string x
      | `O x -> Js.Unsafe.coerce x
      | `A x -> Js.Unsafe.coerce
                @@ Js.array
                @@ Array.of_list
                @@ List.map Js.string x

    let unwrap (x : t Js.t) : value =
      if Js.instanceof x (Js.Unsafe.global##._String)
      then `V (Js.to_string @@ Js.Unsafe.coerce x)
      else if Js.typeof x == Js.string "object"
      then `O (Js.Unsafe.coerce x)
      else if Js.instanceof x (Js.Unsafe.global##._Array)
      then `A (List.map Js.to_string
               @@ Array.to_list
               @@ Js.to_array
               @@ Js.Unsafe.coerce x)
      else failwith "Bad dom string value"
  end

end

module KV : sig
  type 'a t
  val key : 'a t -> Js.js_string Js.t
  val value : 'a t -> 'a
end = struct

  type 'a t = 'a Js.js_array Js.t

  let key (t : 'a t) : Js.js_string Js.t =
    Obj.magic
    @@ Js.Optdef.get (Js.array_get t 0) (fun () -> assert false)

  let value (t : 'a t) : 'a =
    Js.Optdef.get (Js.array_get t 1) (fun () -> assert false)

end

class type ['a] iterator =
  object
    method next : 'a next Js.t Js.meth
  end
  and ['a] next =
    object
    method _done : bool Js.t Js.prop
    method value : 'a Js.optdef Js.prop
    end

let iterator_to_js_array (i : 'a iterator) : 'a Js.js_array Js.t =
  Js.Unsafe.global##._Array##from i

class type _RTCPeerConnection =
  object


    (** Returns a Boolean which indicates whether or not the remote peer
        can accept trickled ICE candidates. *)
    method canTrickleIceCandidates : bool Js.t Js.readonly_prop

    (** Indicates the current state of the peer connection by returning one of
        the string values specified by the enum RTCPeerConnectionState. *)
    method connectionState : Js.js_string Js.t Js.readonly_prop

    (** Returns an RTCSessionDescription object describing the local end
        of the connection as it was most recently successfully negotiated
        since the last time the  RTCPeerConnection finished negotiating and
        connecting to a remote peer. Also included is a list of any ICE
        candidates that may already have been generated by the ICE agent since
        the offer or answer represented by the description was first
        instantiated. *)
    method currentLocalDescription : _RTCSessionDescription Js.t Js.opt Js.readonly_prop

    (** Returns an RTCSessionDescription object describing the remote end
        of the connection as it was most recently successfully negotiated
        since the last time the RTCPeerConnection finished negotiating and
        connecting to a remote peer. Also included is a list of any ICE
        candidates that may already have been generated by the ICE agent since
        the offer or answer represented by the description was first
        instantiated. *)
    method currentRemoteDescription : _RTCSessionDescription Js.t Js.opt Js.readonly_prop

    (** Returns an array of objects based on the RTCIceServer dictionary, which
        indicates what, if any, ICE servers the browser will use by default
        if none are provided to the RTCPeerConnection in its RTCConfiguration.
        However, browsers are not required to provide any default ICE servers
        at all. *)
    method defaultIceServers : _RTCIceServer Js.t Js.js_array Js.t Js.readonly_prop

    (** Returns an enum of type RTCIceConnectionState which state of the
        ICE agent associated with the RTCPeerConnection. *)
    method iceConnectionState : Js.js_string Js.t Js.readonly_prop

    (** Returns an enum of type RTCIceGatheringState that describes connection's
        ICE gathering state. This lets you detect, for example, when collection
        of ICE candidates has finished. *)
    method iceGatheringState : Js.js_string Js.t Js.readonly_prop

    (** Returns an RTCSessionDescription describing the session for the local
        end of the connection. If it has not yet been set, this is null. *)
    method localDescription : _RTCSessionDescription Js.t Js.opt Js.readonly_prop

    (* TODO add type *)
    (** Returns a RTCIdentityAssertion, containing a DOMString once set and
        verified. If no peer has yet been set and verified, this property will
        return null. Once set, via the appropriate method,
        it can't be changed. *)
    method peerIdentity : 'a Js.t Js.opt Js.readonly_prop

    (** Returns an RTCSessionDescription object describing a pending
        configuration change for the local end of the connection.
        This does not describe the connection as it currently stands, but as
        it may exist in the near future.
        Use RTCPeerConnection.currentLocalDescription or
        RTCPeerConnection.localDescription to get the current state of
        the endpoint. For details on the difference, see Pending and current
        descriptions in WebRTC connectivity. *)
    method pendingLocalDescription : _RTCSessionDescription Js.t Js.opt Js.readonly_prop

    (** Returns an RTCSessionDescription object describing a pending
        configuration change for the remote end of the connection.
        This does not describe the connection as it currently stands, but as
        it may exist in the near future.
        Use RTCPeerConnection.currentRemoteDescription or
        RTCPeerConnection.remoteDescription to get the current session
        description for the remote endpoint. For details on the difference, see
        Pending and current descriptions in WebRTC connectivity. *)
    method pendingRemoteDescription : _RTCSessionDescription Js.t Js.opt Js.readonly_prop

    (** Returns a RTCSessionDescription describing the session (which includes
        configuration and media information) for the remote end of the
        connection. If this hasn't been set yet, this is null. *)
    method remoteDescription : _RTCSessionDescription Js.t Js.opt Js.readonly_prop

    (* TODO add type *)
    (** Returns a RTCSctpTransport describing the SCTP transport over which SCTP
        data is being sent and received. If SCTP hasn't been negotiated, this
        value is null. *)
    method sctp : 'a Js.t Js.opt Js.readonly_prop

    (** Returns one of the string values specified by the RTCSignalingState
        enum; these values describe the state of the signaling process on the
        local end of the connection while connecting or reconnecting to another
        peer. See Signaling in Lifetime of a WebRTC session for more details
        about the signaling process. *)
    method signalingState : Js.js_string Js.t Js.readonly_prop

    (* Event handlers *)

    (** Called when the addstream event, of type MediaStreamEvent, is received
        by this RTCPeerConnection. Such an event is sent when a MediaStream is
        added to this connection by the remote peer.
        The event is sent immediately after the call setRemoteDescription() and
        doesn't wait for the result of the SDP negotiation. *)
    method onaddstream
           : ('b Js.t, mediaStreamEvent Js.t) Dom.event_listener
               Js.writeonly_prop

    (** Called to handle the connectionstatechange event when it occurs on an
        instance of RTCPeerConnection. This happens whenever the aggregate
        state of the connection changes. *)
    method onconnectionstatechange
           : ('b Js.t, _RTCPeerConnection Js.t Dom.event Js.t) Dom.event_listener
               Js.writeonly_prop

    (** Called when the datachannel event occurs on an RTCPeerConnection.
        This event, of type RTCDataChannelEvent, is sent when an RTCDataChannel
        is added to the connection by the remote peer calling
        createDataChannel(). *)
    method ondatachannel
           : ('b Js.t, _RTCDataChannelEvent Js.t) Dom.event_listener
               Js.writeonly_prop

    (** Called when the icecandidate event occurs on an RTCPeerConnection
        instance. This happens whenever the local ICE agent needs to deliver
        a message to the other peer through the signaling server. *)
    method onicecandidate
           : ('b Js.t, _RTCPeerConnectionIceEvent Js.t) Dom.event_listener
               Js.writeonly_prop

    (** Called when the iceconnectionstatechange event is fired on an
        RTCPeerConnection instance. This happens when the state of the
        connection's ICE agent, as represented by the iceConnectionState
        property, changes. *)
    method oniceconnectionstatechange
           : ('b Js.t, _RTCPeerConnection Dom.event Js.t) Dom.event_listener
               Js.writeonly_prop

    (** Called when the icegatheringstatechange event is sent to an
        RTCPeerConnection instance. This happens when the ICE gathering
        state—that is, whether or not the ICE agent is actively gathering
        candidates—changes. *)
    method onicegatheringstatechange
           : ('b Js.t, _RTCPeerConnection Dom.event Js.t) Dom.event_listener
               Js.writeonly_prop

    (** Called when the identityresult event, of type RTCIdentityEvent, is
        received by this RTCPeerConnection. Such an event is sent when an
        identity assertion is generated, via getIdentityAssertion() or during
        the creation of an offer or an answer. *)
    method onidentityresult
           : ('b Js.t, _RTCIdentityEvent Js.t) Dom.event_listener
               Js.writeonly_prop

    (** Called to handle the negotiationneeded event when it occurs on an
        RTCPeerConnection instance. This event is fired when a change has
        occurred which requires session negotiation. This negotiation should
        be carried out as the offerer, because some session changes cannot be
        negotiated as the answerer. *)
    method onnegotiationneeded
           : ('b Js.t, _RTCPeerConnection Dom.event Js.t) Dom.event_listener
               Js.writeonly_prop

    (** Called when the removestream event, of type mediaStreamEvent, is
        received by this RTCPeerConnection. Such an event is sent when
        a MediaStream is removed from this connection. *)
    method onremovestream
           : ('b Js.t, mediaStreamEvent Js.t) Dom.event_listener
               Js.writeonly_prop

    (** Called when the signalingstatechange event occurs on an
        RTCPeerConnection interface. *)
    method onsignalingstatechange
           : ('b Js.t, _RTCPeerConnection Dom.event Js.t) Dom.event_listener
               Js.writeonly_prop

    (** Called when the track event occurs, indicating that a track has been
        added to the RTCPeerConnection. *)
    method ontrack
           : ('b Js.t, _RTCTrackEvent Js.t) Dom.event_listener
               Js.writeonly_prop

    (* Methods *)

    (** When a web site or app using RTCPeerConnection receives a new ICE
        candidate from the remote peer over its signaling channel, it delivers
        the newly-received candidate to the browser's ICE agent by calling
        RTCPeerConnection.addIceCandidate(). *)
    method addIceCandidate : _RTCIceCandidateInit Js.t -> (unit, exn) Promise.t Js.meth

    (** Adds a new media track to the set of tracks which will be transmitted to
        the other peer. *)
    method addTrack :
             mediaStreamTrack Js.t ->
             mediaStream Js.t ->
             _RTCRtpSender Js.t Js.meth
    method addTrack_2 :
             mediaStreamTrack Js.t ->
             mediaStream Js.t ->
             mediaStream Js.t ->
             _RTCRtpSender Js.t Js.meth
    method addTrack_3 :
             mediaStreamTrack Js.t ->
             mediaStream Js.t ->
             mediaStream Js.t ->
             mediaStream Js.t ->
             _RTCRtpSender Js.t Js.meth

    (** Closes the current peer connection. *)
    method close : unit Js.meth

    (** Initiates the creation of an SDP offer for the purpose of starting
        a new WebRTC connection to a remote peer. *)
    method createOffer
           : _RTCOfferOptions Js.t ->
             (_RTCSessionDescriptionInit Js.t, exn) Promise.t Js.meth
    method createOffer_
           : (_RTCSessionDescriptionInit Js.t, exn) Promise.t Js.meth

    (** Creates an SDP answer to an offer received from a remote peer during the
        offer/answer negotiation of a WebRTC connection. The answer contains
        information about any media already attached to the session, codecs
        and options supported by the browser, and any ICE candidates already
        gathered. The answer is delivered to the returned Promise, and should
        then be sent to the source of the offer to continue the negotiation
        process. *)
    method createAnswer
           : _RTCAnswerOptions Js.t ->
             (_RTCSessionDescriptionInit Js.t, exn) Promise.t Js.meth
    method createAnswer_
           : (_RTCSessionDescriptionInit Js.t, exn) Promise.t Js.meth

    (** Creates a new channel over which any kind of data may be transmitted.
        This can be useful for back-channel content such as images, file
        transfer, text chat, game update packets, and so forth. *)
    method createDataChannel
           : Js.js_string Js.t ->
             _RTCDataChannelInit Js.t ->
             _RTCDataChannel Js.t Js.meth
    method createDataChannel_
           : Js.js_string Js.t ->
             _RTCDataChannel Js.t Js.meth

    method generateCertificate : 'a Js.meth

    (** Returns a RTCConfiguration object which indicates the current
        configuration of the RTCPeerConnection on which the method is
        called. *)
    method getConfiguration : _RTCConfiguration Js.t Js.meth

    (** Initiates the gathering of an identity assertion. This has an
        effect only if the signalingState is not "closed". *)
    method getIdentityAssertion : unit Js.meth

    (** Returns an array of MediaStream associated with the local end of
        the connection. The array may be empty. *)
    method getLocalStreams : mediaStream Js.t Js.js_array Js.t Js.meth

    (** Returns an array of RTCRtpReceiver objects, each of which represents
        one RTP receiver. Each RTP receiver manages the reception and decoding
        of data for a MediaStreamTrack on an RTCPeerConnection *)
    method getReceivers : _RTCRtpReceiver Js.t Js.js_array Js.t Js.meth

    (** Returns an array of MediaStream associated with the remote end of the
        connection. The array may be empty. *)
    method getRemoteStreams : mediaStream Js.t Js.js_array Js.t Js.meth

    (** Returns an array of RTCRtpSender objects, each of which represents the
        RTP sender responsible for transmitting one track's data. *)
    method getSenders : _RTCRtpSender Js.t Js.js_array Js.t Js.meth

    (** Returns an array of RTCRtpTransceiver objects representing the RTP
        transceivers that are currently attached to the RTCPeerConnection. *)
    method getTransceivers : _RTCRtpTransceiver Js.t Js.js_array Js.t Js.meth

    (** Creates a new RTCRtpTransceiver and adds it to the set of transceivers
        associated with the RTCPeerConnection. Each transceiver represents a
        bidirectional stream, with both an RTCRtpSender and an RTCRtpReceiver
        associated with it. *)
    method addTransceiver
           : Js.js_string Js.t ->
             _RTCRtpTransceiverInit Js.t ->
             _RTCRtpTransceiver Js.t Js.meth
    method addTransceiver_void
           : Js.js_string Js.t ->
             _RTCRtpTransceiver Js.t Js.meth
    method addTransceiver_track
           : mediaStreamTrack Js.t ->
             _RTCRtpTransceiverInit Js.t ->
             _RTCRtpTransceiver Js.t Js.meth
    method addTransceiver_trackVoid
           : mediaStreamTrack Js.t ->
             _RTCRtpTransceiver Js.t Js.meth

    (** Returns a promise which resolves with data providing statistics about
        either the overall connection or about the specified MediaStreamTrack. *)
    method getStats : mediaStreamTrack Js.t Js.opt ->
                      (_RTCStatsReport Js.t, exn) Promise.t Js.meth

    (** Returns the MediaStream with the given id that is associated with local
        or remote end of the connection. If no stream matches, it returns null. *)
    method getStreamById : Js.js_string Js.t -> mediaStream Js.t Js.opt Js.meth

    (** Removes a MediaStream as a local source of audio or video.
        If the negotiation already happened, a new one will be needed for the
        remote peer to be able to use it. Because this method has been
        deprecated, you should instead use removeTrack() if your target browser
        versions have implemented it. *)
    method removeStream : mediaStream Js.t -> unit Js.meth

    (** Tells the local end of the connection to stop sending media from the
        specified track, without actually removing the corresponding
        RTCRtpSender from the list of senders as reported by
        RTCPeerConnection.getSenders(). *)
    method removeTrack : _RTCRtpSender Js.t -> unit Js.meth

    (** Sets the current configuration of the RTCPeerConnection based on the
        values included in the specified RTCConfiguration object.
        This lets you change the ICE servers used by the connection and which
        transport policies to use. *)
    method setConfiguration : _RTCConfiguration Js.t -> unit Js.meth Js.meth

    (** Sets the Identity Provider (IdP) to the triplet given in parameter:
        its name, the protocol used to communicate with it (optional) and an
        optional username. The IdP will be used only when an assertion is
        needed. *)
    method setIdentityProvider : Js.js_string Js.t -> unit Js.meth
    method setIdentityProvider_protocol : Js.js_string Js.t ->
                                          Js.js_string Js.t ->
                                          unit Js.meth
    method setIdentityProvider_username : Js.js_string Js.t ->
                                          Js.js_string Js.t ->
                                          Js.js_string Js.t ->
                                          unit Js.meth

    (** Changes the local description associated with the connection. This
        description specifies the properties of the local end of the connection,
        including the media format. *)
    method setLocalDescription : _RTCSessionDescriptionInit Js.t ->
                                 (unit, exn) Promise.t Js.meth


    (** Canges the remote description associated with the connection. This
        description specifies the properties of the remote end of the connection,
        including the media format. *)
    method setRemoteDescription : _RTCSessionDescriptionInit Js.t ->
                                  (unit, exn) Promise.t Js.meth

  end

  and _RTCStatsReport =
    object
      (* Properties *)

      method size : int Js.readonly_prop

      (* Methods *)

      method entries : _RTCStats KV.t iterator Js.t Js.meth

      method forEach : (_RTCStats Js.t -> Js.js_string Js.t ->
                        _RTCStatsReport Js.t -> unit) Js.callback ->
                       unit Js.meth

      method get : Js.js_string Js.t -> _RTCStats Js.t Js.optdef Js.meth

      method has : Js.js_string Js.t -> bool Js.t Js.meth

      method keys : Js.js_string Js.t iterator Js.t Js.meth

      method values : _RTCStats Js.t iterator Js.t Js.meth

    end

  and _RTCStats =
    object
      (** The timestamp in milliseconds associated with this object.
          The time is relative to the UNIX epoch (Jan 1, 1970, UTC). *)
      method timestamp : float Js.prop

      (** The type of this object. *)
      method _type : Js.js_string Js.t Js.prop

      (** A unique id that is associated with the object that was inspected to
          produce this RTCStats object. Two RTCStats objects, extracted from two
          different RTCStatsReport objects, MUST have the same id if they were
          produced by inspecting the same underlying object. User agents are free
          to pick any format for the id as long as it meets the requirements
          above. *)
      method id : Js.js_string Js.t Js.prop

    end

  and _RTCRtpStreamStats =
    object
      inherit _RTCStats

      (** The 32-bit unsigned integer value used to identify the source of the
          stream of RTP packets that this stats object concerns. *)
      method ssrc : int Js.prop

      (** Either "audio" or "video". This must match the media type part of the
          information in the corresponding codec member of RTCCodecStats, and
          must match the "kind" attribute of the related MediaStreamTrack. *)
      method kind : Js.js_string Js.t Js.prop

      (** It is a unique identifier that is associated to the object that was
          inspected to produce the RTCTransportStats associated with this RTP
          stream. *)
      method transportId : Js.js_string Js.t Js.prop

      (** It is a unique identifier that is associated to the object that was
          inspected to produce the RTCCodecStats associated with this RTP
          stream. *)
      method codecId : Js.js_string Js.t Js.prop

      (** Count the total number of Full Intra Request (FIR) packets received
          by the sender. This metric is only valid for video and is sent by
          receiver. *)
      method firCount : int Js.prop

      (** Count the total number of Picture Loss Indication (PLI) packets
          received by the sender. This metric is only valid for video and
          is sent by receiver. *)
      method pliCount : int Js.optdef_prop

      (** Count the total number of Negative ACKnowledgement (NACK) packets
          received by the sender and is sent by receiver. *)
      method nackCount : int Js.prop

      (** Count the total number of Slice Loss Indication (SLI) packets
          received by the sender. This metric is only valid for video and
          is sent by receiver. *)
      method sliCount : int Js.optdef_prop

      (** The sum of the QP values of frames passed. The count of frames is in
          framesDecoded for inbound stream stats, and in framesEncoded for
          outbound stream stats.
          The definition of QP value depends on the codec; for VP8, the QP value
          is the value carried in the frame header as the syntax element
          "y_ac_qi", and defined in [RFC6386] section 19.2. Its range is 0..127.
          Note that the QP value is only an indication of quantizer values used;
          many formats have ways to vary the quantizer value within the frame.
          Only valid for video. *)
      method qpSum : int Js.optdef_prop

    end

  and _RTCReceivedRtpStreamStats =
    object
      inherit _RTCRtpStreamStats

      (** Total number of RTP packets received for this SSRC. *)
      method packetsReceived : int Js.prop

      (** Total number of RTP packets lost for this SSRC. *)
      method packetsLost : int Js.prop

      (** Packet Jitter measured in seconds for this SSRC. *)
      method jitter : float Js.prop

    end

  and _RTCInboundRtpStreamStats =
    object
      inherit _RTCReceivedRtpStreamStats

      (** The identifier of the stats object representing the receiving track. *)
      method trackId : Js.js_string Js.t Js.prop

      (** Only valid for video. It represents the total number of frames
          correctly decoded for this SSRC, i.e., frames that would be
          displayed if no frames are dropped. *)
      method framesDecoded : int Js.optdef_prop

      (** Total number of bytes received for this SSRC. *)
      method bytesReceived : int Js.prop

    end

  and _RTCSentRtpStreamStats =
    object
      inherit _RTCRtpStreamStats

      (** Total number of RTP packets sent for this SSRC. *)
      method packetsSent : int Js.prop

      (** Total number of RTP packets for this SSRC that have been discarded
          due to socket errors, i.e. a socket error occured when handing the
          packets to the socket. This might happen due to various reasons,
          including full buffer or no available memory. *)
      method packetsDiscardedOnSend : int Js.prop

      (** Total number of RTP FEC packets sent for this SSRC. This counter can
          also be incremented when sending FEC packets in-band with media
          packets (e.g., with Opus). *)
      method fecPacketsSent : int Js.prop

      (** Total number of bytes sent for this SSRC. *)
      method bytesSent : int Js.prop

      (** Total number of bytes for this SSRC that have been discarded due to
          socket errors, i.e. a socket error occured when handing the packets
          containing the bytes to the socket. This might happen due to various
          reasons, including full buffer or no available memory. *)
      method bytesDiscardedOnSend : int Js.prop
    end

  and _RTCOutboundRtpStreamStats =
    object
      inherit _RTCSentRtpStreamStats

      (** The identifier of the stats object representing the current track. *)
      method trackId : Js.js_string Js.t Js.prop
    end

  and _RTCOfferAnswerOptions =
    object

      (** Many codecs and systems are capable of detecting "silence" and
          changing their behavior in this case by doing things such as not
          transmitting any media. In many cases, such as when dealing with
          emergency calling or sounds other than spoken voice, it is desirable
          to be able to turn off this behavior. This option allows the
          application to provide information about whether it wishes this type
          of processing enabled or disabled. *)
      method voiceActivityDetection : bool Js.t Js.optdef_prop

    end

  and _RTCAnswerOptions =
    object

      inherit _RTCOfferAnswerOptions

    end

  and _RTCOfferOptions =
    object

      inherit _RTCOfferAnswerOptions

      (** To restart ICE on an active connection, set this to [true].
          This will cause the returned offer to have different credentials
          than those already in place. If you then apply the returned offer,
          ICE will restart. Specify false to keep the same credentials and
          therefore not restart ICE. The default is [false]. *)
      method iceRestart : bool Js.t Js.optdef_prop

      (** A legacy option which used to control whether or not to offer
          to the remote peer the opportunity to try to send audio.
          If this value is [false], the remote peer will not be offered to send
          audio data, even if the local side will be sending audio data.
          If this value is [true], the remote peer will be offered to send audio
          data, even if the local side will not be sending audio data.
          The default behavior is to offer to receive audio only if the local
          side is sending audio, not otherwise. *)
      method offerToReceiveAudio : bool Js.t Js.optdef_prop

      (** A legacy option which used to control whether or not to offer
          to the remote peer the opportunity to try to send video.
          If this value is [false], the remote peer will not be offered to send
          video data, even if the local side will be sending video data.
          If this value is [true], the remote peer will be offered to send video
          data, even if the local side will not be sending video data.
          The default behavior is to offer to receive video only if the local
          side is sending video, not otherwise. *)
      method offerToReceiveVideo : bool Js.t Js.optdef_prop

    end

  (** Used when calling the WebRTC function RTCPeerConnection.addTransceiver()
      to provide configuration options for the new transceiver. *)
  and _RTCRtpTransceiverInit =
    object

      (** The new transceiver's preferred directionality. *)
      method direction : Js.js_string Js.t Js.optdef_prop

      (** A list of encodings to allow when sending RTP media from
          the RTCRtpSender. Each entry is of type RTCRtpEncodingParameters. *)
      method sendEncodings
             : _RTCRtpEncodingParameters Js.t Js.js_array Js.t Js.optdef_prop

      (** A list of MediaStream objects to add to the transceiver's
          RTCRtpReceiver; when the remote peer's RTCPeerConnection's track event
          occurs, these are the streams that will be specified by that event. *)
      method streams : mediaStream Js.t Js.js_array Js.t Js.optdef_prop

    end

  (** An instance of the WebRTC API's RTCRtpEncodingParameters dictionary
      describes a single configuration of a codec for an RTCRtpSender.
      It's used in the RTCRtpSendParameters describing the configuration of
      an RTP sender's encodings; RTCRtpDecodingParameters is used to describe
      the configuration of an RTP receiver's encodings. *)
  and _RTCRtpEncodingParameters =
    object

      (** If [true], the described encoding is currently actively being used.
          That is, for RTP senders, the encoding is currently being used to
          send data, while for receivers, the encoding is being used to decode
          received data. The default value is [true]. *)
      method active : bool Js.t Js.prop

      (** Indicates the priority of this encoding. *)
      method priority : Js.js_string Js.t Js.prop

      (** When describing a codec for an RTCRtpSender, codecPayloadType is a
          single 8-bit byte (or octet) specifying the codec to use for sending
          the stream; the value matches one from the owning RTCRtpParameters
          object's codecs parameter. This value can only be set when creating
          the transceiver; after that, this value is read only. *)
      method codecPayloadType : int Js.readonly_prop

      (** Only used for an RTCRtpSender whose kind is audio, this property
          indicates whether or not to use discontinuous transmission (a feature
          by which a phone is turned off or the microphone muted automatically
          in the absence of voice activity).
          The value is taken from the enumerated string type RTCDtxStatus. *)
      method dtx : bool Js.t Js.prop

      (** Indicates the maximum number of bits per second to allow for this
          encoding. Other parameters may further constrain the bit rate, such
          as the value of maxFramerate or transport or physical network
          limitations. *)
      method maxBitrate : int Js.prop

      (** Specifies the maximum number of frames per second to allow for this
          encoding. *)
      method maxFramerate : float Js.prop

      (** Indicates the preferred duration of a media packet in milliseconds.
          This is typically only relevant for audio encodings. The user agent
          will try to match this as well as it can, but there is no guarantee. *)
      method ptime : int Js.prop

      (** Specifies an RTP stream ID (RID) to be sent using the RID header
          extension. This parameter cannot be modified using setParameters().
          Its value can only be set when the transceiver is first created. *)
      method rid : Js.js_string Js.t Js.readonly_prop

      (** Only used for senders whose track's kind is video. Specifies a factor
          by which to scale down the video during encoding. The default value,
          1.0, means that the sent video's size will be the same as the
          original. A value of 2.0 scales the video frames down by a factor of
          2 in each dimension, resulting in a video 1/4 the size of the original.
          The value must not be less than 1.0 (you can't use this to scale the
          video up). *)
      method scaleResolutionDownBy : float Js.prop

    end

  (** The WebRTC interface RTCRtpTransceiver describes a permanent pairing of
      an RTCRtpSender and an RTCRtpReceiver, along with some shared state. *)
  and _RTCRtpTransceiver =
    object
      (* Properties *)

      (** Indicates the transceiver's current directionality, or null if the
          transceiver is stopped or has never participated in an exchange of
          offers and answers. *)
      method currentDirection : Js.js_string Js.t Js.opt Js.readonly_prop

      (** Used to set the transceiver's desired direction. *)
      method direction : Js.js_string Js.t Js.prop

      (** The media ID of the m-line associated with this transceiver.
          This association is established, when possible, whenever either
          a local or remote description is applied. This field is null if
          neither a local or remote description has been applied, or if its
          associated m-line is rejected by either a remote offer
          or any answer. *)
      method mid : Js.js_string Js.t Js.readonly_prop

      (** The RTCRtpReceiver object that handles receiving and decoding incoming
          media. *)
      method receiver : _RTCRtpReceiver Js.t Js.readonly_prop

      (** The RTCRtpSender object responsible for encoding and sending data to
          the remote peer. *)
      method sender : _RTCRtpSender Js.t Js.readonly_prop

      (** Indicates whether or not sending and receiving using the paired
          RTCRtpSender and RTCRtpReceiver has been permanently disabled, either
          due to SDP offer/answer, or due to a call to stop(). *)
      method stopped : bool Js.t Js.prop

      (* Methods *)

      (** A list of RTCRtpCodecParameters objects which override the default
          preferences used by the user agent for the transceiver's codecs. *)
      method setCodecPreferences
             : _RTCRtpCodecParameters Js.t Js.js_array Js.t ->
               unit Js.meth

      (** Permanently stops the RTCRtpTransceiver. The associated sender stops
          sending data, and the associated receiver likewise stops receiving
          and decoding incoming data. *)
      method stop : unit Js.meth

    end

  (** The RTCRtpCodecParameters dictionary, part of the WebRTC API, is used to
      describe the configuration parameters for a single media codec.
      In addition to being the type of the RTCRtpParameters.codecs property,
      it's used when calling RTCRtpTransceiver.setCodecPreferences()
      to configure a transceiver's codecs before beginning the offer/answer
      process to establish a WebRTC peer connection. *)
  and _RTCRtpCodecParameters =
    object

      (** The RTP payload type used to identify this codec. *)
      method payloadType : int Js.optdef Js.readonly_prop

      (** The codec's MIME media type and subtype specified as a DOMString
          of the form "type/subtype".
          IANA maintains a registry of valid MIME types. *)
      method mimeType : Js.js_string Js.t Js.optdef Js.readonly_prop

      (** Specifies the codec's clock rate in hertz (Hz). The clock rate is
          the rate at which the codec's RTP timestamp advances. Most codecs
          have specific values or ranges of values they permit; see the IANA
          payload format media type registry for details. *)
      method clockRate : int Js.optdef Js.readonly_prop

      (** Indicates the number of channels the codec should support.
          For example, for audio codecs, a value of 1 specifies
          monaural sound while 2 indicates stereo. *)
      method channels : int Js.optdef Js.readonly_prop

      (** Contains the format-specific parameters field from the "a=fmtp"
          line in the codec's SDP, if one is present; see section 5.8 of the
          IETF specification for JSEP. *)
      method sdpFmtpLine : Js.js_string Js.t Js.optdef Js.readonly_prop

    end

  (** The RTCRtpReceiver interface of the WebRTC API manages the reception and
      decoding of data for a MediaStreamTrack on an RTCPeerConnection. *)
  and _RTCRtpReceiver =
    object
      (* TODO implement *)

      (** The MediaStreamTrack which is being handled by the RTCRtpSender.
          If track is null, the RTCRtpSender doesn't transmit anything. *)
      method track : mediaStreamTrack Js.t Js.opt Js.readonly_prop

    end

  (** The RTCRtpSender interface provides the ability to control and obtain
      details about how a particular MediaStreamTrack is encoded and sent to
      a remote peer. With it, you can configure the encoding used for the
      corresponding track, get information about the device's media
      capabilities, and so forth. You can also obtain access to an RTCDTMFSender
      which can be used to send DTMF codes to the remote peer. *)
  and _RTCRtpSender =
    object
      (* Properties *)

      (** An RTCDTMFSender which can be used to send DTMF tones using
          "telephone-event" payloads on the RTP session represented by
          the RTCRtpSender object. If null, the track and/or the connection
          doesn't support DTMF. Only audio tracks can support DTMF. *)
      method dtmf : _RTCDTMFSender Js.t Js.opt Js.readonly_prop

      (* FIXME add type *)
      (** The transport over which Real-time Transport Control Protocol (RTCP)
          information is exchanged. This value is null before the
          RTCDtlsTransport object is created. When bundling is in use, more than
          one RTCRtpSender can share the same transport, sending all RTP and
          RTCP information over that one transport. *)
      method rtcpTransport : 'a Js.t Js.opt Js.readonly_prop

      (** The MediaStreamTrack which is being handled by the RTCRtpSender.
          If track is [null], the RTCRtpSender doesn't transmit anything. *)
      method track : mediaStreamTrack Js.t Js.opt Js.readonly_prop

      (* FIXME add type *)
      (** The RTCDtlsTransport over which media data for the track is being
          transmitted. The data is transmitted using RTP packets. Before the
          transport is established, this value is null. *)
      method transport : 'a Js.t Js.opt Js.readonly_prop

      (* Methods *)

      (** Returns a RTCRtpParameters object describing the current configuration
          for the encoding and transmission of media on the track. *)
      method getParameters : _RTCRtpParameters Js.t Js.meth

      (** Returns a Promise which is fulfilled with a RTCStatsReport which
          provides statistics data for all outbound streams being sent using
          this RTCRtpSender. *)
      method getStats : (_RTCStatsReport Js.t, exn) Promise.t Js.meth

      (** Applies changes to parameters which configure how the track is encoded
          and transmitted to the remote peer. *)
      method setParameters : _RTCRtpParameters Js.t -> unit Js.meth
      method setParameters_void : unit Js.meth

      (** Attempts to replace the track currently being sent by the RTCRtpSender
          with another track, without performing renegotiation. This method can
          be used, for example, to toggle between the front- and rear-facing
          cameras on a device. *)
      method replaceTrack : mediaStreamTrack Js.t Js.opt ->
                            (unit, exn) Promise.t Js.meth
      method replaceTrack_void : (unit, exn) Promise.t Js.meth

    end

  and _RTCRtpParameters =
    object

      (** A sequence containing the codecs that an RTCRtpSender will choose
          from in order to send media. *)
      method codecs : _RTCRtpCodecParameters Js.t Js.js_array Js.t Js.prop

      (** When bandwidth is constrained and the RtpSender needs to choose
          between degrading resolution or degrading framerate,
          degradationPreference indicates which is preferred. *)
      method degradationPreference : Js.js_string Js.t Js.prop

      (** A sequence containing parameters for RTP encodings of media. *)
      method encodings : _RTCRtpEncodingParameters Js.t Js.js_array Js.t Js.prop

      (** A sequence containing parameters for RTP header extensions. *)
      method headerExtensions
             : _RTCRtpHeaderExtensionParameters Js.t Js.js_array Js.t Js.prop

      (** Parameters used for RTCP. *)
      method rtcp : _RTCRtcpParameters Js.t Js.prop

    end

  and _RTCRtpHeaderExtensionParameters =
    object

      (** Whether the header extension is encryted or not. *)
      method encrypted : bool Js.t Js.readonly_prop

      (** The value put in the RTP packet to identify the header extension. *)
      method id : int Js.readonly_prop

      (** The URI of the RTP header extension, as defined in RFC5285. *)
      method uri : Js.js_string Js.t Js.readonly_prop

    end

  and _RTCRtcpParameters =
    object

      (** The Canonical Name (CNAME) used by RTCP (e.g. in SDES messages) *)
      method cname : Js.js_string Js.t Js.readonly_prop

      (** Whether reduced size RTCP [RFC5506] is configured (if true)
          or compound RTCP as specified in RFC3550 *)
      method reducedSize : bool Js.t Js.readonly_prop

    end

  and _RTCDTMFSender =
    object
      (* Properties *)

      (** Contains the list of DTMF tones currently in the queue to be
          transmitted (tones which have already been played are no longer
          included in the string). See toneBuffer for details on the format
          of the tone buffer. *)
      method toneBuffer : Js.js_string Js.t Js.readonly_prop

      (* Methods *)

      (** Given a string describing a set of DTMF codes and, optionally,
          the duration of and inter-tone gap between the tones, insertDTMF()
          starts sending the specified tones. Calling insertDTMF() replaces
          any already-pending tones from the toneBuffer. You can abort sending
          queued tones by specifying an empty string ("") as the set of tones
          to play. *)
      method insertDTMF : Js.js_string Js.t -> unit Js.meth
      method insertDTMF_duration : Js.js_string Js.t -> int -> unit Js.meth
      method insertDTMF_gap : Js.js_string Js.t -> int -> int -> unit Js.meth

      (* Event handlers *)

      (** The tonechange event is sent to the RTCDTMFSender instance's
          ontonechange event handler to indicate that a tone has either started
          or stopped playing. *)
      method ontonechange
             : ('b Js.t, _RTCTrackEvent Js.t) Dom.event_listener
                 Js.writeonly_prop
    end

  (** The WebRTC API's RTCIceCandidateInit dictionary, which contains the
      information needed to fundamentally describe an RTCIceCandidate.
      RTCIceCandidateInit is used when using new RTCIceCandidate() to create
      a new ICE candidate object. It's also used as the return value from the
      RTCIceCandidate.toJSON() method, and can be passed directly into
      RTCPeerConnection.addIceCandidate() to add a candidate to the peer
      connection. *)
  and _RTCIceCandidateInit =
    object

      (** The ICE candidate-attribute. If the candidate is an indicator that
          there are no further candidates (rather than representing a new
          candidate), this is the empty string (""). The default is the empty
          string. *)
      method candidate : Js.js_string Js.t Js.optdef_prop

      (** The identification tag of the media stream with which the candidate is
          associated, or null if there is no associated media stream.
          The default is null. *)
      method sdpMid : Js.js_string Js.t Js.opt Js.optdef_prop

      (** The zero-based index of the m-line within the SDP of the media
          description with which the candidate is associated, or null if
          no such associated exists. The default is null. *)
      method sdpMLineIndex : int Js.opt Js.optdef_prop

      (** A string which uniquely identifies the remote peer. This string is
          generated by WebRTC at the beginning of the session, and at least 24
          bits worth of the string contain random data. The string may be up to
          256 characters long. This property has no default value and is not
          present unless set explicitly. *)
      method usernameFragment : Js.js_string Js.t Js.optdef_prop
    end

  and _RTCIceCandidate =
    object
      (** The transport address for the candidate that can be used for
          connectivity checks. The format of this address is a
          candidate-attribute as defined in RFC 5245. This string is empty ("")
          if the RTCIceCandidate is an "end of candidates" indicator. *)
      method candidate : Js.js_string Js.t Js.readonly_prop

      (** Indicates whether the candidate is an RTP or an RTCP candidate; its
          value is either "rtp" or "rtcp", and is derived from the "component-id"
          field in the candidate a-line string. The permitted values are listed
          in the RTCIceComponent enumerated type. *)
      method component : Js.js_string Js.t Js.readonly_prop

      (** Unique identifier that is the same for any candidates of the same type,
          share the same base (the address from which the ICE agent sent the
          candidate), and come from the same STUN server. This is used to help
          optimize ICE performance while prioritizing and correlating candidates
          that appear on multiple RTCIceTransport objects. *)
      method foundation : Js.js_string Js.t Js.readonly_prop

      (** IP address of the candidate. *)
      method ip : Js.js_string Js.t Js.readonly_prop

      (** Candidate's port number. *)
      method port : int Js.readonly_prop

      (** Candidate's priority. *)
      method priority : int Js.readonly_prop

      (** Indicates whether the candidate's protocol is "tcp" or "udp".
          The string is one of those in the enumerated type RTCIceProtocol. *)
      method protocol : Js.js_string Js.t Js.readonly_prop

      (** If the candidate is derived from another candidate, relatedAddress
          is a DOMString containing that host candidate's IP address.
          For host candidates, this value is null. *)
      method relatedAddress : Js.js_string Js.t Js.opt Js.readonly_prop

      (** For a candidate that is derived from another, such as a relay or
          reflexive candidate, the relatedPort is a number indicating the port
          number of the candidate from which this candidate is derived.
          For host candidates, the relatedPort property is null. *)
      method relatedPort : int Js.opt Js.readonly_prop

      (** Candidate's media stream identification tag which uniquely identifies
          the media stream within the component with which the candidate is
          associated, or null if no such association exists. *)
      method sdpMid : Js.js_string Js.t Js.readonly_prop

      (** If not [null], sdpMLineIndex indicates the zero-based index number of
          the media description (as defined in RFC 4566) in the SDP with which
          the candidate is associated. *)
      method sdpMLineIndex : int Js.opt Js.readonly_prop

      (** If protocol is "tcp", tcpType represents the type of TCP candidate.
          Otherwise, tcpType is [null]. *)
      method tcpType : Js.js_string Js.t Js.opt Js.readonly_prop

      (** Indicates the type of candidate as one of the strings from the
          RTCIceCandidateType enumerated type. *)
      method _type : Js.js_string Js.t Js.readonly_prop

      (** Contains a randomly-generated username fragment ("ice-ufrag") which
          ICE uses for message integrity along with a randomly-generated password
          ("ice-pwd"). You can use this string to verify generations of ICE
          generation; each generation of the same ICE process will use the same
          usernameFragment, even across ICE restarts. *)
      method usernameFragment : Js.js_string Js.t Js.readonly_prop

      (** Given the RTCIceCandidate's current configuration, toJSON() returns
          a DOMString containing a JSON representation of that configuration in
          the form of a RTCIceCandidateInit object. *)
      method toJSON : Js.js_string Js.t Js.meth

    end

  and _RTCSessionDescriptionInit =
    object

      (** Session description's type. *)
      method _type : Js.js_string Js.t Js.prop

      (** The string representation of the SDP;
          if type is "rollback", this member is unused. *)
      method sdp : Js.js_string Js.t Js.optdef_prop

    end

  and _RTCSessionDescription =
    object
      (** Session description's type. *)
      method _type : Js.js_string Js.t Js.readonly_prop

      (** SDP describing the session.*)
      method sdp : Js.js_string Js.t Js.readonly_prop
    end

  (** The RTCDataChannel interface represents a network channel which can be
      used for bidirectional peer-to-peer transfers of arbitrary data.
      Every data channel is associated with an RTCPeerConnection, and each peer
      connection can have up to a theoretical maximum of 65,534 data channels
      (the actual limit may vary from browser to browser). *)
  and _RTCDataChannel =
    object

      (* Properties *)

      (** Specifies the type of JavaScript object which should be used to
          represent binary data received on the RTCDataChannel. Values allowed
          by the WebSocket.binaryType property are also permitted here: "blob"
          if Blob objects are being used or "arraybuffer" if ArrayBuffer objects
          are being used. The default is "blob". *)
      method binaryType : Js.js_string Js.t Js.prop

      (** The number of bytes of data currently queued to be sent over the data
          channel. *)
      method bufferedAmount : int Js.readonly_prop

      (** The number of bytes of buffered outgoing data that is considered "low".
          The default value is 0. *)
      method bufferedAmountLowtThreshold : int Js.prop

      (** ID number (between 0 and 65,534) which uniquely identifies
          the RTCDataChannel. *)
      method id : int Js.readonly_prop

      (** Name describing the data channel.
          These labels are not required to be unique. *)
      method label : Js.js_string Js.t Js.readonly_prop

      (** The amount of time, in milliseconds, the browser is allowed to take
          to attempt to transmit a message, as set when the data channel was
          created, or null. *)
      method maxPacketLifeTime : int Js.opt Js.readonly_prop

      (** The maximum number of times the browser should try to retransmit a
          message before giving up, as set when the data channel was created,
          or null, which indicates that there is no maximum. *)
      method maxRetransmits : int Js.opt Js.readonly_prop

      (** Indicates whether the RTCDataChannel's connection was negotiated by
          the Web app (true) or by the WebRTC layer (false). The default is
          false. *)
      method negotiated : bool Js.t Js.readonly_prop

      (** Indicates whether or not the data channel guarantees in-order delivery
          of messages; the default is true, which indicates that the data channel
          is indeed ordered. *)
      method ordered : bool Js.t Js.readonly_prop

      (** Contains the name of the subprotocol in use. If no protocol was
          specified when the data channel was created, then this property's
          value is "" *)
      method protocol : Js.js_string Js.t Js.readonly_prop

      (** Indicates the state of the data channel's underlying data connection. *)
      method readyState : Js.js_string Js.t Js.readonly_prop

      (* Event handlers *)

      (** Called when the bufferedamountlow event is sent to the RTCDataChannel.
          This event, which is represented by a simple Event object, is sent
          when the amount of data buffered to be sent falls to or below the
          threshold specified by the channel's bufferedAmountLowThreshold. *)
      method onbufferedamountlow
             : ('b Js.t, _RTCDataChannel Dom.event Js.t) Dom.event_listener
                 Js.writeonly_prop

      (** Called when the close event is received by the RTCDataChannel.
          This is a simple Event which indicates that the data channel has
          closed down. *)
      method onclose
             : ('b Js.t, _RTCDataChannel Dom.event Js.t) Dom.event_listener
                 Js.writeonly_prop

      (** Called when the error event is received. When an error occurs on the
          data channel, the function receives as input an ErrorEvent object
          describing the error which occurred. *)
      method onerror
             : ('b Js.t, _RTCDataChannel _RTCErrorEvent Js.t) Dom.event_listener
                 Js.writeonly_prop

      (** Called when the message event is fired on the channel. This event is
          represented by the MessageEvent interface. This event is sent to the
          channel when a message is received from the other peer. *)
      method onmessage
             : ('b Js.t, _RTCDataChannel messageEvent Js.t) Dom.event_listener
                 Js.writeonly_prop

      (** Called when the open event is fired; this is a simple Event which is
          sent when the data channel's underlying data transport — the link over
          which the RTCDataChannel's messages flow — is established or
          re-established. *)
      method onopen
             : ('b Js.t, _RTCDataChannel Dom.event Js.t) Dom.event_listener
                 Js.writeonly_prop

      (* Methods *)

      (** Closes the RTCDataChannel. Either peer is permitted to call this method
          to initiate closure of the channel. *)
      method close : unit Js.meth

      (** Sends data across the data channel to the remote peer. *)
      method send_blob : #File.blob Js.t -> unit Js.meth
      method send_string : Js.js_string Js.t -> unit Js.meth
      method send_arrayBuffer : #Typed_array.arrayBuffer Js.t -> unit Js.meth
      method send_arrayBufferView : #Typed_array.arrayBufferView Js.t -> unit Js.meth

    end

  and _RTCError =
    object
      method errorDetail : Js.js_string Js.t Js.readonly_prop

      method sdpLineNumber : int Js.opt Js.readonly_prop

      method httpRequestStatusCode : int Js.opt Js.readonly_prop

      method sctpCauseCode : int Js.opt Js.readonly_prop

      method receivedAlert : int Js.opt Js.readonly_prop

      method sentAlert : int Js.opt Js.readonly_prop

    end

  and _RTCDataChannelInit =
    object
      method ordered : bool Js.t Js.optdef_prop

      method maxPacketLifeTime : int Js.opt Js.optdef_prop

      method maxRetransmits : int Js.opt Js.optdef_prop

      method protocol : Js.js_string Js.t Js.optdef_prop

      method negotiated : bool Js.t Js.optdef_prop

      method id : int Js.optdef_prop

    end

  and mediaStream =
    object
      (* Properties *)

      (** Contains 36 characters denoting a universally unique identifier (UUID)
          for the object. *)
      method id : Js.js_string Js.t Js.prop

      (** [true] if the MediaStream is active, or [false] otherwise. *)
      method active : bool Js.t Js.prop

      (* Event handlers *)

      (** Called when an addtrack event is fired when a new MediaStreamTrack
          object is added. *)
      method onaddtrack
             : ('b Js.t, mediaStreamTrackEvent Js.t) Dom.event_listener
                 Js.writeonly_prop

      (** Called when a MediaStreamTrack object is removed from a stream. *)
      method onremovetrack
             : ('b Js.t, mediaStreamTrackEvent Js.t) Dom.event_listener
                 Js.writeonly_prop

      (* Methods *)

      (** Stores a copy of the MediaStreamTrack given as argument.
          If the track has already been added to the MediaStream object,
          nothing happens. *)
      method addTrack : mediaStreamTrack Js.t -> unit Js.meth

      (** Returns a clone of the MediaStream object.
          The clone will, however, have a unique value for id. *)
      method clone : mediaStream Js.t Js.meth

      (** Returns a list of the MediaStreamTrack objects stored in the
          MediaStream object that have their kind attribute set to "audio".
          The order is not defined, and may not only vary from one browser
          to another, but also from one call to another. *)
      method getAudioTracks : mediaStreamTrack Js.t Js.js_array Js.t Js.meth

      (** Returns the track whose ID corresponds to the one given in parameters,
          trackid. If no parameter is given, or if no track with that ID does
          exist, it returns null. If several tracks have the same ID,
          it returns the first one. *)
      method getTrackById : Js.js_string Js.t ->
                            mediaStreamTrack Js.t Js.opt Js.meth

      (** Returns a list of all MediaStreamTrack objects stored in the
          MediaStream object, regardless of the value of the kind attribute.
          The order is not defined, and may not only vary from one browser
          to another, but also from one call to another. *)
      method getTracks : mediaStreamTrack Js.t Js.js_array Js.t Js.meth

      (** Returns a list of the MediaStreamTrack objects stored in the
          MediaStream object that have their kind attribute set to "video".
          The order is not defined, and may not only vary from one browser
          to another, but also from one call to another. *)
      method getVideoTracks : mediaStreamTrack Js.t Js.js_array Js.t Js.meth

      (** Removes the MediaStreamTrack given as argument.
          If the track is not part of the MediaStream object,
          nothing happens. *)
      method removeTrack : mediaStreamTrack Js.t -> unit Js.meth

    end

  and mediaStreamTrack =
    object
      (* Properties *)

      (** Unique identifier (GUID) for the track *)
      method id : Js.js_string Js.t Js.readonly_prop

      (** [true] if the track is enabled, that is allowed to render the media
          source stream; [false] if it is disabled, that is not rendering the
          media source stream but silence and blackness. If the track has been
          disconnected, this value can be changed but has no more effect. *)
      method enabled : bool Js.t Js.prop

      (** May be used by the web application to provide a hint as to what type
          of content the track contains to guide how it should be treated by
          API consumers. *)
      method contentHint : Js.js_string Js.t Js.prop

      (** [true] if the track is isolated; that is, the track cannot be accessed
          by the document that owns the MediaStreamTrack. This happens when the
          peerIdentity property is set, or if the track comes from a cross-origin
          source. *)
      method isolated : bool Js.t Js.readonly_prop

      (** ["audio"] if the track is an audio track and ["video"], if it is
          a video track. It doesn't change if the track is deassociated from
          its source. *)
      method kind : Js.js_string Js.t Js.readonly_prop

      (** User agent-assigned label that identifies the track source, as in
          "internal microphone". The string may be left empty and is empty
          as long as no source has been connected. When the track is deassociated
          from its source, the label is not changed. *)
      method label : Js.js_string Js.t Js.readonly_prop

      (** Indicates whether the track is unable to provide media data due to
          a technical issue. *)
      method muted : bool Js.t Js.readonly_prop

      (** [true] if the track is readonly (such a video file source or a camera
          that settings can't be modified), [false] otherwise. *)
      method readonly : bool Js.t Js.readonly_prop

      (** Status of the track. This will be one of the following values:
          ["live"] - indicates that an input is connected and does its best-effor
          t in providing real-time data. In that case, the output of data can be
          switched on or off using the enabled attribute.
          ["ended"] - indicates that the input is not giving any more data and
          will never provide new data. *)
      method readyState : Js.js_string Js.t Js.readonly_prop

      (* Event handlers *)

      (** Called when an started event is fired on the object, that is when a
          new MediaStreamTrack object is added. *)
      method onstarted
             : ('b Js.t, mediaStreamTrack Dom.event Js.t) Dom.event_listener
                 Js.writeonly_prop

      (** Called when an ended event is fired on the object, that is when a
          MediaStreamTrack object is removed from it *)
      method onended
             : ('b Js.t, mediaStreamTrack Dom.event Js.t) Dom.event_listener
                 Js.writeonly_prop

      (** Called when the isolationchange event is fired at the track object.
          This occurs whenever the value of the isolated property changes due
          to the document gaining or losing permission to access the track. *)
      method onisolationchange
             : ('b Js.t, mediaStreamTrack Dom.event Js.t) Dom.event_listener
                 Js.writeonly_prop

      (** Called when an mute event is fired on the object, that is when
          the streaming is terminating. *)
      method onmute
             : ('b Js.t, mediaStreamTrack Dom.event Js.t) Dom.event_listener
                 Js.writeonly_prop

      (** Called when an unmute event is fired on the object, that is when a
          MediaStreamTrack object is removed from it. *)
      method onunmute
             : ('b Js.t, mediaStreamTrack Dom.event Js.t) Dom.event_listener
                 Js.writeonly_prop

      (** Called when an overconstrained event is fired on the object, that is
          when a MediaStreamTrack object is removed from it. *)
      method onoverconstrained
             : ('b Js.t, overconstrainedErrorEvent Js.t) Dom.event_listener
                 Js.writeonly_prop

      (* Methods *)

      (** Lets the application specify the ideal and/or ranges of acceptable
          values for any number of the available constrainable properties of
          the MediaStreamTrack. *)
      method applyConstraints : mediaTrackConstraints Js.t -> unit Js.meth

      (** Returns a duplicate of the MediaStreamTrack. *)
      method clone : mediaStreamTrack Js.t Js.meth

      (** Returns the a list of constrainable properties available for
          the MediaStreamTrack. *)
      method getCapabilities : mediaTrackCapabilities Js.t Js.meth

      (** Returns a MediaTrackConstraints object containing the currently
          set constraints for the track; the returned value matches the
          constraints last set using applyConstraints(). *)
      method getConstraints : mediaTrackConstraints Js.t Js.meth

      (** Returns a MediaTrackSettings object containing the current values
          of each of the MediaStreamTrack's constrainable properties. *)
      method getSettings : mediaTrackSettings Js.t Js.meth

      (** Stops playing the source associated to the track, both the source
          and the track are deassociated. The track state is set to ended. *)
      method stop : unit Js.meth

    end

  and overconstrainedError =
    object
      method constraint_ : Js.js_string Js.t Js.readonly_prop

      method message : Js.js_string Js.t Js.readonly_prop

      method name : Js.js_string Js.t Js.readonly_prop
    end

  and mediaTrackSettings =
    object

      (** Current value of the deviceId property. The device ID is a origin-unique
          string identifying the source of the track; this is usually a GUID.
          This value is specific to the source of the track's data and is not
          usable for setting constraints; it can, however, be used for initially
          selecting media when calling MediaDevices.getUserMedia(). *)
      method deviceId : Js.js_string Js.t Js.optdef_prop

      (** The group ID is a browsing session-unique string identifying the source
          group of the track. Two devices (as identified by the deviceId) are
          considered part of the same group if they are from the same physical
          device. For instance, the audio input and output devices for the speaker
          and microphone built into a phone would share the same group ID, since
          they're part of the same physical device. The microphone on a headset
          would have a different ID, though. This value is specific to the source
          of the track's data and is not usable for setting constraints; it can,
          however, be used for initially selecting media when calling
          MediaDevices.getUserMedia(). *)
      method groupId : Js.js_string Js.t Js.optdef_prop

      (* AUDIO *)

      (** [true] if automatic gain control is enabled and is [false] otherwise. *)
      method audioGainControl : bool Js.t Js.optdef_prop

      (** A long integer value indicating the current value of the channelCount
          property, specifying the number of audio channels present on the track
          (therefore indicating how many audio samples exist in each audio
          frame). This is 1 for mono, 2 for stereo, and so forth. *)
      method channelCount : int Js.t Js.optdef_prop

      (** A Boolean indicating the current value of the echoCancellation
          property, specifying true if echo cancellation is enabled,
          otherwise false. *)
      method echoCancellation : bool Js.t Js.optdef_prop

      (** Audio latency, in seconds. Latency is the amount of time which elapses
          between the start of processing the audio and the data being available
          to the next stop in the audio utilization process. This value is a
          target value; actual latency may vary to some extent for various
          reasons. *)
      method latency : float Js.t Js.optdef_prop

      (** [true] if noise suppression is enabled and is [false] otherwise. *)
      method noiseSuppression : bool Js.t Js.optdef_prop

      (** Sample rate in samples per second of the audio data. Standard CD-quality
          audio, for example, has a sample rate of 41,000 samples per second. *)
      method sampleRate : int Js.t Js.optdef_prop

      (** Linear size, in bits, of each audio sample. CD-quality audio, for
          example, is 16-bit, so this value would be 16 in that case. *)
      method sampleSize : int Js.t Js.optdef_prop

      (** Volume level of the track. This value will be between 0.0 (silent)
          to 1.0 (maximum supported volume). *)
      method volume : float Js.t Js.optdef_prop

      (* VIDEO *)

      (** The width of the image in pixels divided by its height in pixels.
          Common values include 1.3333333333 (for the classic televison 4:3
          "standard" aspect ratio, also used on tablets such as Apple's iPad),
          1.7777777778 (for the 16:9 high-definition widescreen aspect ratio),
          and 1.6 (for the 16:10 aspect ratio common among widescreen computers
          and tablets). *)
      method aspectRatio : float Js.t Js.optdef_prop

      (** The direction the camera is facing. The value will be one of:
          ["user"] - a camera facing the user (commonly known as a "selfie cam"),
          used for self-portraiture and video calling.
          ["environment"] - a camera facing away from the user (when the user is
          looking at the screen). This is typically the highest quality camera on
          the device, used for general photography.
          ["left"] - a camera facing toward the environment to the user's left.
          ["right"] - a camera facing toward the environment to the user's
          right. *)
      method facingMode : Js.js_string Js.t Js.optdef_prop

      (** Specifies how many frames of video per second the track includes.
          If the value can't be determined for any reason, the value will match
          the vertical sync rate of the device the user agent is running on. *)
      method frameRate : float Js.t Js.optdef_prop

      (** Height of the track's video data in pixels. *)
      method height : int Js.t Js.optdef_prop

      (** Width of the track's video data in pixels. *)
      method width : int Js.t Js.optdef_prop

      (** The mode used by the user agent to derive the resolution of the track.
          The value will be one of:
          ["none"] - the track has the resolution offered by the camera, its
          driver or the OS.
          ["crop-and-scale"] - the track's resolution might be the result of the
          user agent using cropping or downscaling from a higher camera
          resolution. *)
      method resizeMode : Js.js_string Js.t Js.optdef_prop

      (* SHARED SCREEN TRACKS *)

      (** Indicates whether or not the mouse cursor is being included in the
          generated stream and under what conditions. Possible values are:
          ["always"] - the mouse is always visible in the video content of the
          {domxref("MediaStream"), unless the mouse has moved outside the area
          of the content.
          ["motion"] - the mouse cursor is always included in the video if it's
          moving, and for a short time after it stops moving.
          ["never"] - the mouse cursor is never included in the shared video. *)
      method cursor : Js.js_string Js.t Js.optdef_prop

      (** The type of source the track contains; one of:
          ["application"] - the stream contains all of the windows of the
          application chosen by the user rendered into the one video track.
          ["browser"] - the stream contains the contents of a single browser
          tab selected by the user.
          ["monitor"] - the stream's video track contains the entire contents
          of one or more of the user's screens.
          ["window"] - the stream contains a single window selected by the user
          for sharing. *)
      method displaySurface : Js.js_string Js.t Js.optdef_prop

      (** If [true], indicates that the video contained in the stream's video
          track contains a background rendering context, rather than a
          user-visible one. This is false if the video being captured is
          coming from a foreground (user-visible) source. *)
      method logicalSurface : bool Js.t Js.optdef_prop

    end

  and mediaTrackConstraints =
    object

      (* Properties of all media tracks *)

      (** A device ID or an array of device IDs which are acceptable and/or
          required. *)
      method deviceId : Constrain.String.t Js.t Js.optdef_prop

      (** A group ID or an array of group IDs which are acceptable and/or
          required. *)
      method groupId : Constrain.String.t Js.t Js.optdef_prop

      (* Properties of audio tracks *)

      (** Whether automatic gain control is preferred and/or required. *)
      method autoGainControl : Constrain.Bool.t Js.t Js.optdef_prop

      (** The channel count or range of channel counts which are acceptable
          and/or required. *)
      method channelCount : Constrain.Long.t Js.t Js.optdef_prop

      (** Whether or not echo cancellation is preferred and/or required. *)
      method echoCancellation : Constrain.Bool.t Js.t Js.optdef_prop

      (** The latency or range of latencies which are acceptable
          and/or required. *)
      method latency : Constrain.Double.t Js.t Js.optdef_prop

      (** Whether noise suppression is preferred and/or required. *)
      method noiseSuppression : Constrain.Bool.t Js.t Js.optdef_prop

      (** The sample rate or range of sample rates which are acceptable
          and/or required. *)
      method sampleRate : Constrain.Long.t Js.t Js.optdef_prop

      (** The sample size or range of sample sizes which are acceptable
          and/or required. *)
      method sampleSize : Constrain.Long.t Js.t Js.optdef_prop

      (** The volume or range of volumes which are acceptable and/or required. *)
      method volume : Constrain.Double.t Js.t Js.optdef_prop

      (* Properties of image tracks *)

      (** A String specifying one of "none", "manual", "single-shot",
          or "continuous". *)
      method whiteBalanceMode : Js.js_string Js.t Js.optdef_prop

      (** A String specifying one of "none", "manual", "single-shot",
          or "continuous". *)
      method exposureMode : Js.js_string Js.t Js.optdef_prop

      (** A String specifying one of "none", "manual", "single-shot",
          or "continuous".*)
      method focusMode: Js.js_string Js.t Js.optdef_prop

      (** The pixel coordinates on the sensor of one or more points of interest.
          This is either an object in the form { x:value, y:value } or an array
          of such objects, where value  is a double-precision integer. *)
      method pointsOfInterest : point Js.t or_array Js.t Js.optdef_prop

      (** f-stop adjustment by up to ±3. *)
      method exposureCompensation : Constrain.Double.t Js.t Js.optdef_prop

      (** Desired color temperature in degrees kelvin. *)
      method colorTemperature : Constrain.Double.t Js.t Js.optdef_prop

      (** Desired iso setting. *)
      method iso : Constrain.Double.t Js.t Js.optdef_prop

      (** Desired brightness setting. *)
      method brightness : Constrain.Double.t Js.t Js.optdef_prop

      (** Degree of difference between light and dark. *)
      method contrast : Constrain.Double.t Js.t Js.optdef_prop

      (** Degree of color intensity. *)
      method saturation : Constrain.Double.t Js.t Js.optdef_prop

      (** Intensity of edges. *)
      method sharpness : Constrain.Double.t Js.t Js.optdef_prop

      (** Distance to a focused object. *)
      method focusDistance : Constrain.Double.t Js.t Js.optdef_prop

      (** Desired focal length. *)
      method zoom : Constrain.Double.t Js.t Js.optdef_prop

      (** Whether the fill light is continuously connected, meaning it stays on
          as long as the track is active. *)
      method torch : bool Js.t Js.optdef_prop

      (* Properties of video tracks *)

      (** Video aspect ratio or range of aspect ratios which are acceptable
          and/or required. *)
      method aspectRatio : Constrain.Double.t Js.t Js.optdef_prop

      (** Facing or an array of facings which are acceptable and/or required. *)
      method facingMode : Constrain.String.t Js.t Js.optdef_prop

      (** Frame rate or range of frame rates which are acceptable
          and/or required. *)
      method frameRate : Constrain.Double.t Js.t Js.optdef_prop

      (** Video height or range of heights which are acceptable and/or required. *)
      method height : Constrain.Long.t Js.t Js.optdef_prop

      (** Video width or range of widths which are acceptable and/or required. *)
      method width : Constrain.Long.t Js.t Js.optdef_prop

      (** Mode or an array of modes the UA can use to derive the resolution of a
          video track. Allowed values are none and crop-and-scale. none means that
          the user agent uses the resolution provided by the camera, its driver or
          the OS. crop-and-scale means that the user agent can use cropping and
          downscaling on the camera output in order to satisfy other constraints
          that affect the resolution. *)
      method resizeMode : Constrain.String.t Js.t Js.optdef_prop

      (* Properties of shared screen tracks *)

      (** Whether or not to include the mouse cursor in the generated track,
          and if so, whether or not to hide it while not moving. The value may
          be a single one of the following strings, or an array of them to
          allow the browser flexibility in deciding what to do about the cursor.
          ["always"] - the mouse is always visible in the video content of the
          {domxref("MediaStream"), unless the mouse has moved outside the area
          of the content.
          ["motion"] - the mouse cursor is always included in the video if it's
          moving, and for a short time after it stops moving.
          ["never"] - the mouse cursor is never included in the shared video. *)
      method cursor : Constrain.String.t Js.t Js.optdef_prop

      (** Types of display surface that may be selected by the user.
          This may be a single one of the following strings, or a list of them
          to allow multiple source surfaces:
          ["application"] - the stream contains all of the windows of the
          application chosen by the user rendered into the one video track.
          ["browser"] - the stream contains the contents of a single browser tab
          selected by the user.
          ["monitor"] - the stream's video track contains the entire contents of
          one or more of the user's screens.
          ["window"] - the stream contains a single window selected by the user
          for sharing. *)
      method displaySurface : Constrain.String.t Js.t Js.optdef_prop

      (** Whether or not to allow the user to choose source surfaces which do not
          directly correspond to display areas. These may include backing buffers
          for windows to allow capture of window contents that are hidden by other
          windows in front of them, or buffers containing larger documents that
          need to be scrolled through to see the entire contents in their
          windows. *)
      method logicalSurface : Constrain.Bool.t Js.t Js.optdef_prop

    end

  and mediaTrackCapabilities =
    object

      method width : longRange Js.t Js.optdef_prop

      method height : longRange Js.t Js.optdef_prop

      method aspectRatio : doubleRange Js.t Js.optdef_prop

      method frameRate : doubleRange Js.t Js.optdef_prop

      method facingMode : Js.js_string Js.t or_array Js.t Js.optdef_prop

      method volume : doubleRange Js.t Js.optdef_prop

      method sampleRate : longRange Js.t Js.optdef_prop

      method sampleSize : longRange Js.t Js.optdef_prop

      method echoCancellation : bool Js.t or_array Js.t Js.optdef_prop

      method autoGainControl : bool Js.t or_array Js.t Js.optdef_prop

      method noiseSupression : bool Js.t or_array Js.t Js.optdef_prop

      method latency : doubleRange Js.t Js.optdef_prop

      method channelCount : longRange Js.t Js.optdef_prop

      method deviceId : Js.js_string Js.t Js.optdef_prop

      method groupId : Js.js_string Js.t Js.optdef_prop

    end

  (** The RTCIceServer dictionary defines how to connect to a single ICE server
      (such as a STUN or TURN server). It includes both the URL and
      the necessary credentials, if any, to connect to the server. *)
  and _RTCIceServer =
    object

      method credential : Js.js_string Js.t Js.optdef_prop

      method credentialType : Js.js_string Js.t Js.optdef_prop

      method urls : Js.js_string Js.t Js.optdef_prop

      method urls_ : Js.js_string Js.t Js.js_array Js.t Js.optdef_prop

      method username : Js.js_string Js.t Js.optdef_prop
    end

  (** An RTCConfiguration dictionary providing options
      to configure the new connection. *)
  and _RTCConfiguration =
    object

      (** Specifies how to handle negotiation of candidates when the remote
          peer is not compatible with the SDP BUNDLE standard. This must be one
          of the values from the enum RTCBundlePolicy. If this value isn't
          included in the dictionary, "balanced" is assumed. *)
      method bundlePolicy : Js.js_string Js.t Js.optdef_prop

      (** An Array of objects of type RTCCertificate which are used by the
          connection for authentication. If this property isn't specified,
          a set of certificates is generated automatically for each
          RTCPeerConnection instance. Although only one certificate is used by
          a given connection, providing certificates for multiple algorithms may
          improve the odds of successfully connecting in some circumstances.
          See Using certificates below for additional information. *)
      method certificates : 'a Js.t Js.js_array Js.t Js.optdef_prop

      (** An unsigned 16-bit integer value which specifies the size of the
          prefetched ICE candidate pool. The default value is 0 (meaning no
          candidate prefetching will occur). You may find in some cases that
          connections can be established more quickly by allowing the ICE agent
          to start fetching ICE candidates before you start trying to connect,
          so that they're already available for inspection when
          RTCPeerConnection.setLocalDescription() is called. *)
      method iceCandidatePoolSize : int Js.optdef_prop

      (** An array of RTCIceServer objects, each describing one server which
          may be used by the ICE agent; these are typically STUN and/or TURN
          servers. If this isn't specified, the ICE agent may choose to use
          its own ICE servers; otherwise, the connection attempt will be made
          with no STUN or TURN server available, which limits the connection
          to local peers. *)
      method iceServers : _RTCIceServer Js.t Js.js_array Js.t Js.optdef_prop

      (** The current ICE transport policy; this must be one of the values
          from the RTCIceTransportPolicy enum. If this isn't specified,
          "all" is assumed. *)
      method iceTransportPolicy : Js.js_string Js.t Js.optdef_prop

      (** The target peer identity for the RTCPeerConnection. If this value
          is set (it defaults to null), the RTCPeerConnection will not connect
          to a remote peer unless it can successfully authenticate with the
          given name. *)
      method peerIdentity : Js.js_string Js.t Js.optdef_prop

      (** The RTCP mux policy to use when gathering ICE candidates, in order
          to support non-multiplexed RTCP. The value must be one of those from
          the RTCRtcpMuxPolicy enum. The default is "require". *)
      method rtcpMuxPolicy : Js.js_string Js.t Js.optdef_prop

    end

  and mediaStreamEvent =
    object
      inherit [_RTCPeerConnection] Dom.event

      (** Stream associated with the event. *)
      method stream : mediaStream Js.t Js.readonly_prop
    end

  and mediaStreamTrackEvent =
    object
      inherit [mediaStream] Dom.event

      method track : mediaStreamTrack Js.t Js.readonly_prop
    end

  and _RTCIdentityEvent =
    object
      inherit [_RTCPeerConnection] Dom.event

      (** A blob being the assertion generated. *)
      method assertion : Js.js_string Js.t Js.readonly_prop
    end

  and _RTCDataChannelEvent =
    object
      inherit [_RTCPeerConnection] Dom.event

      (** RTCDataChannel associated with the event. *)
      method channel : _RTCDataChannel Js.t Js.readonly_prop
    end

  and _RTCDTMFToneChangeEvent =
    object
      inherit [_RTCDTMFSender] Dom.event

      (** Specifies the tone which has begun playing, or an empty
          string ("") if the previous tone has finished playing. *)
      method tone : Js.js_string Js.t Js.readonly_prop
    end

  and _RTCPeerConnectionIceEvent =
    object
      inherit [_RTCPeerConnection] Dom.event

      (** The candidate associated with the event *)
      method candidate : _RTCIceCandidate Js.t Js.opt Js.readonly_prop
    end

  and _RTCTrackEvent =
    object
      inherit [_RTCPeerConnection] Dom.event

      (** The RTCRtpReceiver used by the track that's been added
          to the RTCPeerConnection. *)
      method receiver : _RTCRtpReceiver Js.t Js.readonly_prop

      (** An array of MediaStream objects, each representing one of the media
          streams which comprise the track that was added to the connection.
          By default, the array is empty. *)
      method streams : mediaStream Js.t Js.js_array Js.t Js.readonly_prop

      (** The MediaStreamTrack which has been added to the connection. *)
      method track : mediaStreamTrack Js.t Js.readonly_prop

      (** The RTCRtpTransceiver being used by the new track. *)
      method transceiver : _RTCRtpTransceiver Js.t Js.readonly_prop
    end

  and ['a] messageEvent =
    object

      (* TODO add 'source' and 'ports' properties *)

      inherit ['a] Dom.event

      method data : 'b Js.t Js.readonly_prop

      method origin : Js.js_string Js.t Js.readonly_prop

      method lastEventId : Js.js_string Js.t Js.readonly_prop

    end

  and ['a] _RTCErrorEvent =
    object
      inherit ['a] Dom.event

      (** The RTCError describing the error that triggered the event (if any). *)
      method error : _RTCError Js.t Js.opt Js.readonly_prop
    end

  and overconstrainedErrorEvent =
    object
      inherit [mediaStreamTrack] Dom.event

      method error : overconstrainedError Js.t Js.opt Js.readonly_prop
    end

type bool_or_constraints

let wrap_bool (x : bool) : bool_or_constraints Js.t =
  Js.Unsafe.coerce (Js.bool x)

let wrap_constraints (x : mediaTrackConstraints Js.t) :
      bool_or_constraints Js.t =
  Js.Unsafe.coerce x

let cast_bool (x : bool_or_constraints Js.t) : bool Js.t Js.opt =
  if Js.typeof x == Js.typeof Js._true
  then Js.some (Js.Unsafe.coerce x)
  else Js.null

let cast_constraints (x : bool_or_constraints Js.t) :
      mediaTrackConstraints Js.t Js.opt =
  if String.equal (Js.to_string @@ Js.typeof x) "object"
  then Js.some (Js.Unsafe.coerce x)
  else Js.null

class type mediaDevices =
  object

    (* Properties *)

    (** The event handler for the devicechange event.
        This event is delivered to the MediaDevices object when a media input
        or output device is attached to or removed from the user's computer. *)
    method ondevicechange :
             ('b Js.t, mediaDevices Dom.event Js.t) Dom.event_listener
               Js.writeonly_prop

    (* Methods *)

    (** Obtains an array of information about the media input and output devices
        available on the system. *)
    method enumerateDevices :
             (mediaDeviceInfo Js.t Js.js_array Js.t, exn) Promise.t Js.meth

    (** Returns an object conforming to MediaTrackSupportedConstraints
        indicating which constrainable properties are supported on the
        MediaStreamTrack interface. See Capabilities and constraints in
        Media Capture and Streams API (Media Stream) to learn more about
        constraints and how to use them. *)
    method getSupportedConstraints : mediaTrackSupportedConstraints Js.t Js.meth

    (** Prompts the user to select a display or portion of a display
        (such as a window) to capture as a MediaStream for sharing or recording
        purposes.  Returns a promise that resolves to a MediaStream. *)
    method getDisplayMedia :
             mediaStreamConstraints Js.t ->
             (mediaStream Js.t, exn) Promise.t Js.meth
    method getDisplayMedia_ : (mediaStream Js.t, exn) Promise.t Js.t Js.meth

    (** With the user's permission through a prompt, turns on a camera and/or
        a microphone on the system and provides a MediaStream containing a video
        track and/or an audio track with the input. *)
    method getUserMedia :
             mediaStreamConstraints Js.t ->
             (mediaStream Js.t, exn) Promise.t Js.meth
    method getUserMedia_ : (mediaStream Js.t, exn) Promise.t Js.meth

  end

  and mediaStreamConstraints =
    object

      (* Properties *)

      (** Constraints which must be met by the audio track included in the
          returned MediaStream. If constraints are specified, an audio track
          is inherently requested. *)
      method audio : bool_or_constraints Js.t Js.optdef_prop

      (** Constraints which must be met by the video track included in the
          returned MediaStream. If constraints are specified, a video track
          is inherently requested. *)
      method video : bool_or_constraints Js.t Js.optdef_prop

      (** The peer who has sole access to the stream. If this property is
          specified, only the indicated peer can receive and use the stream.
          Streams isolated in this way can only be displayed in a media element
          (<audio> or <video>) where the content is protected just as if CORS
          cross-origin rules were in effect. When a peer identity is set,
          MediaStreamTracks from that peer have their isolated flag set to true. *)
      method peerIdentity : Js.js_string Js.t Js.optdef_prop

    end

  and mediaDeviceInfo =
    object

      (* Properties *)

      (** Identifier for the represented device that is persisted across sessions.
          It is un-guessable by other applications and unique to the origin of the
          calling application. It is reset when the user clears cookies (for
          Private Browsing, a different identifier is used that is not persisted
          across sessions). *)
      method deviceId : Js.js_string Js.t Js.readonly_prop

      (** Group identifier. Two devices have the same group identifier if they
          belong to the same physical device — for example a monitor with both
          a built-in camera and a microphone. *)
      method groupId : Js.js_string Js.t Js.readonly_prop

      (** "videoinput", "audioinput" or "audiooutput". *)
      method kind : Js.js_string Js.t Js.readonly_prop

      (** Label describing this device (for example "External USB Webcam"). *)
      method label : Js.js_string Js.t Js.readonly_prop

    end

  and mediaTrackSupportedConstraints =
    object

      method autoGainControl : bool Js.t Js.optdef_prop

      method width : bool Js.t Js.optdef_prop

      method height : bool Js.t Js.optdef_prop

      method aspectRatio : bool Js.t Js.optdef_prop

      method frameRate : bool Js.t Js.optdef_prop

      method facingMode : bool Js.t Js.optdef_prop

      method resizeMode : bool Js.t Js.optdef_prop

      method volume : bool Js.t Js.optdef_prop

      method sampleRate : bool Js.t Js.optdef_prop

      method sampleSize : bool Js.t Js.optdef_prop

      method echoCancellation : bool Js.t Js.optdef_prop

      method latency : bool Js.t Js.optdef_prop

      method noiseSuppression : bool Js.t Js.optdef_prop

      method channelCount : bool Js.t Js.optdef_prop

      method deviceId : bool Js.t Js.optdef_prop

      method groupId : bool Js.t Js.optdef_prop

      (* Properties specific to shared screen tracks *)

      method cursor : bool Js.t Js.optdef_prop

      method displaySurface : bool Js.t Js.optdef_prop

      method logicalSurface : bool Js.t Js.optdef_prop

    end
