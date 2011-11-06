%% @author Bruno Rijsman <brunorijsman@hotmail.com>
%% @copyright 2011 Bruno Rijsman
%%

-module(of_decoder).

-export([decode_header/1,
         decode_hello/1,
         decode_error/1,
         decode_echo_request/1,
         decode_echo_reply/1,
         decode_experimenter/1,
         decode_features_request/1,
         decode_features_reply/1]).

%% TODO: Specify the search path
%% TODO: Emacs indentation for single percent (%) comments is broken

-include_lib("../include/of.hrl").

%%
%% Exported functions.
%%

%% TODO: Validate length?
%% TODO: Validate xid?
-spec decode_header(binary()) -> #of_header{}.
decode_header(?OF_HEADER_PATTERN) ->
    Header = #of_header{
      version = Version,
      type    = Type,
      length  = Length,
      xid     = Xid
     },
    if
        (Version < ?OF_VERSION_MIN) or (Version > ?OF_VERSION_MAX) ->
            throw({error, ?OF_ERROR_TYPE_BAD_REQUEST, ?OF_ERROR_CODE_BAD_REQUEST_BAD_VERSION});
        (Type < ?OF_MESSAGE_TYPE_MIN) or (Type > ?OF_MESSAGE_TYPE_MAX) ->
            throw({error, ?OF_ERROR_TYPE_BAD_REQUEST, ?OF_ERROR_CODE_BAD_REQUEST_BAD_TYPE});
        true ->
            Header
    end.

-spec decode_hello(binary()) -> #of_hello{}.
decode_hello(?OF_HELLO_PATTERN) ->
    _Hello = #of_hello{}.

-spec decode_error(binary()) -> #of_error{}.
decode_error(?OF_ERROR_PATTERN) ->
    %% No validation, accept unrecognized types and codes.
    _Error = #of_error{
      type = Type,
      code = Code,
      data = Data
     }.

-spec decode_echo_request(binary()) -> #of_echo_request{}.
decode_echo_request(?OF_ECHO_REQUEST_PATTERN) ->
    _EchoRequest = #of_echo_request{
      data = Data
     }.

-spec decode_echo_reply(binary()) -> #of_echo_reply{}.
decode_echo_reply(?OF_ECHO_REPLY_PATTERN) ->
    _EchoReply = #of_echo_reply{
      data = Data
     }.

-spec decode_experimenter(binary()) -> #of_experimenter{}.
decode_experimenter(?OF_EXPERIMENTER_PATTERN) ->
    %% No validation, higher layer to determine if the extension is supported.
    _Experimenter = #of_experimenter{
      experimenter_id = ExperimenterId,
      data = Data
     }.

-spec decode_features_request(binary()) -> #of_features_request{}.
decode_features_request(?OF_FEATURES_REQUEST_PATTERN) ->
    _ReaturesRequest = #of_features_request{}.

-spec decode_features_reply(binary()) -> #of_features_reply{}.
decode_features_reply(?OF_FEATURES_REPLY_PATTERN) ->
    _ReaturesReply = #of_features_reply{
      data_path_id = DataPathId,
      n_buffers    = NBuffers,
      n_tables     = NTables,
      capabilities = decode_capabilities(Capabilities),
      ports        = decode_ports(Ports)
     }.

%%
%% Internal functions.
%%

-spec decode_capabilities(binary()) -> #of_capabilities{}.
decode_capabilities(?OF_CAPABILITIES_PATTERN) ->
    _Capabilities = #of_capabilities{
      flow_stats   = FlowStats,
      table_stats  = TableStats,
      port_stats   = PortStats,
      group_stats  = GroupStats,
      ip_reasm     = IpReasm,
      queue_stats  = QueueStats,
      arp_match_ip = ArpMatchIp
     }.

-spec decode_port_config(binary()) -> #of_port_config{}.
decode_port_config(?OF_PORT_CONFIG_PATTERN) ->
    _PortConfig = #of_port_config{
      port_down    = PortDown,
      no_recv      = NoRecv,
      no_fwd       = NoFwd,
      no_packet_in = NoPacketIn
     }.

-spec decode_port_state(binary()) -> #of_port_state{}.
decode_port_state(?OF_PORT_STATE_PATTERN) ->
    _PortState = #of_port_state{
      link_down = LinkDown,
      blocked   = Blocked,
      live      = Live
     }.

-spec decode_port_features(binary()) -> #of_port_features{}.
decode_port_features(?OF_PORT_FEATURES_PATTERN) ->
    _PortFeatures = #of_port_features{
      half_duplex_10_mbps  = HalfDuplex10Mbps,
      full_duplex_10_mbps  = FullDuplex10Mbps,
      half_duplex_100_mbps = HalfDuplex100Mbps,
      full_duplex_100_mbps = FullDuplex100Mbps,
      half_duplex_1_gbps   = HalfDuplex1Gbps,
      full_duplex_1_gbps   = FullDuplex1Gbps,
      full_duplex_10_gbps  = FullDuplex10Gbps,
      full_duplex_40_gbps  = FullDuplex40Gbps,
      full_duplex_100_gbps = FullDuplex100Gbps,
      full_duplex_1_tbps   = FullDuplex1Tbps,
      other_rate           = OtherRate,
      copper_medium        = CopperMedium,
      fiber_medium         = FiberMedium,
      auto_negotiation     = AutoNegotiation,
      pause                = Pause,
      pause_asymetric      = PauseAsymetric
     }.

-spec decode_ports(binary()) -> [#of_port{}].
decode_ports(Binary) ->
    decode_ports(Binary, []).
    
-spec decode_ports(binary(), [#of_port{}]) -> [#of_port{}].
decode_ports(<<>>, ParsedPorts) ->
    ParsedPorts;
decode_ports(?OF_PORTS_PATTERN, ParsedPorts) ->
    Port = #of_port {
      port_no             = PortNo,
      hw_addr             = HwAddr,       %% TODO: Convert binary to ...
      name                = Name,         %% TODO: Convert binary to string
      config              = decode_port_config(Config),
      state               = decode_port_state(State),
      current_features    = decode_port_features(CurrentFeatures),
      advertised_features = decode_port_features(AdvertisedFeatures),
      supported_features  = decode_port_features(SupportedFeatures),
      current_speed_kbps  = CurrentSpeedKbps,
      max_speed_kbps      = MaxSpeedKbps
     },
    decode_ports(MorePorts, [Port | ParsedPorts]).
