%% @author Bruno Rijsman <brunorijsman@hotmail.com>
%% @copyright 2011 Bruno Rijsman
%%

%% TODO: Check for correct length of binary in all decode functions

-module(of_decoder_v11).

-export([decode_header/1,
         decode_hello/1,
         decode_error/1,
         decode_echo_request/1,
         decode_echo_reply/1,
         decode_experimenter/1,
         decode_features_request/1,
         decode_features_reply/1,
         decode_get_config_request/1,
         decode_get_config_reply/1]).

-include_lib("eunit/include/eunit.hrl").

%% TODO: Specify the search path
%% TODO: Emacs indentation for single percent (%) comments is broken

-include_lib("../include/of_protocol_v11.hrl").

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
            throw({malformed, ?OF_ERROR_TYPE_BAD_REQUEST, ?OF_ERROR_CODE_BAD_REQUEST_BAD_VERSION});
        (Type < ?OF_MESSAGE_TYPE_MIN) or (Type > ?OF_MESSAGE_TYPE_MAX) ->
            throw({malformed, ?OF_ERROR_TYPE_BAD_REQUEST, ?OF_ERROR_CODE_BAD_REQUEST_BAD_TYPE});
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

-spec decode_get_config_request(binary()) -> #of_get_config_request{}.
decode_get_config_request(?OF_GET_CONFIG_REQUEST_PATTERN) ->
    _GetConfigRequest = #of_get_config_request{}.

-spec decode_get_config_reply(binary()) -> #of_get_config_reply{}.
decode_get_config_reply(?OF_GET_CONFIG_REPLY_PATTERN) ->
    _GetConfigReply = #of_get_config_reply{
      switch_config = decode_switch_config(SwitchConfig)
     }.

%%
%% Internal functions.
%%

-spec decode_capabilities(binary()) -> #of_capabilities{}.
decode_capabilities(?OF_CAPABILITIES_PATTERN) ->
    _Capabilities = #of_capabilities{
      flow_stats   = (FlowStats == 1),
      table_stats  = (TableStats == 1),
      port_stats   = (PortStats == 1),
      group_stats  = (GroupStats == 1),
      ip_reasm     = (IpReasm == 1),
      queue_stats  = (QueueStats == 1),
      arp_match_ip = (ArpMatchIp == 1)
     }.

-spec decode_port_config(binary()) -> #of_port_config{}.
decode_port_config(?OF_PORT_CONFIG_PATTERN) ->
    _PortConfig = #of_port_config{
      port_down    = (PortDown == 1),
      no_recv      = (NoRecv == 1),
      no_fwd       = (NoFwd == 1),
      no_packet_in = (NoPacketIn == 1)
     }.

-spec decode_port_state(binary()) -> #of_port_state{}.
decode_port_state(?OF_PORT_STATE_PATTERN) ->
    _PortState = #of_port_state{
      link_down = (LinkDown == 1),
      blocked   = (Blocked == 1),
      live      = (Live == 1)
     }.

-spec decode_port_features(binary()) -> #of_port_features{}.
decode_port_features(?OF_PORT_FEATURES_PATTERN) ->
    _PortFeatures = #of_port_features{
      half_duplex_10_mbps  = (HalfDuplex10Mbps == 1),
      full_duplex_10_mbps  = (FullDuplex10Mbps == 1),
      half_duplex_100_mbps = (HalfDuplex100Mbps == 1),
      full_duplex_100_mbps = (FullDuplex100Mbps == 1),
      half_duplex_1_gbps   = (HalfDuplex1Gbps == 1),
      full_duplex_1_gbps   = (FullDuplex1Gbps == 1),
      full_duplex_10_gbps  = (FullDuplex10Gbps == 1),
      full_duplex_40_gbps  = (FullDuplex40Gbps == 1),
      full_duplex_100_gbps = (FullDuplex100Gbps == 1),
      full_duplex_1_tbps   = (FullDuplex1Tbps == 1),
      other_rate           = (OtherRate == 1),
      copper_medium        = (CopperMedium == 1),
      fiber_medium         = (FiberMedium == 1),
      auto_negotiation     = (AutoNegotiation == 1),
      pause                = (Pause == 1),
      pause_asymetric      = (PauseAsymetric == 1)
     }.

-spec decode_string(binary()) -> string().
decode_string(Binary) ->
    decode_string(Binary, []).

-spec decode_string(binary(), string()) -> string().
decode_string(<<>>, Accum) ->
    lists:reverse(Accum);
decode_string(<< 0, _/binary>>, Accum) ->
    lists:reverse(Accum);
decode_string(<< Char, Rest/binary>>, Accum) ->
    decode_string(Rest, [Char | Accum]).

-spec decode_ports(binary()) -> [#of_port{}].
decode_ports(Binary) ->
    decode_ports(Binary, []).
    
-spec decode_ports(binary(), [#of_port{}]) -> [#of_port{}].
decode_ports(<<>>, ParsedPorts) ->
    ParsedPorts;
decode_ports(?OF_PORTS_PATTERN, ParsedPorts) ->
    Port = #of_port {
      port_no             = PortNo,
      hw_addr             = HwAddr,
      name                = decode_string(Name),
      config              = decode_port_config(Config),
      state               = decode_port_state(State),
      current_features    = decode_port_features(CurrentFeatures),
      advertised_features = decode_port_features(AdvertisedFeatures),
      supported_features  = decode_port_features(SupportedFeatures),
      current_speed_kbps  = CurrentSpeedKbps,
      max_speed_kbps      = MaxSpeedKbps
     },
    decode_ports(MorePorts, [Port | ParsedPorts]).

-spec decode_switch_config(binary()) -> #of_switch_config{}.
decode_switch_config(?OF_SWITCH_CONFIG_PATTERN) ->
    _SwitchConfig = #of_switch_config{
      frag_drop                 = FragDrop,
      frag_reasm                = FragReasm,
      invalid_ttl_to_controller = InvalidTtlToController,
      miss_send_len             = MissSendLen
     }.

%%
%% Unit tests.
%%

decode_string_no_trailing_zero_test() ->
    Bin = << "hello" >>,
    ActualStr = decode_string(Bin),
    ExpectedStr = "hello",
    ?assertEqual(ActualStr, ExpectedStr).

decode_string_trailing_zero_test() ->
    Bin = << "hello", 0, 1, 2, 3 >>,
    ActualStr = decode_string(Bin),
    ExpectedStr = "hello",
    ?assertEqual(ActualStr, ExpectedStr).

decode_string_only_zero_test() ->
    Bin = << 0 >>,
    ActualStr = decode_string(Bin),
    ExpectedStr = "",
    ?assertEqual(ActualStr, ExpectedStr).

decode_string_empty_test() ->
    Bin = << >>,
    ActualStr = decode_string(Bin),
    ExpectedStr = "",
    ?assertEqual(ActualStr, ExpectedStr).

decode_header_test() ->
    Bin = of_test_msgs_v11:header_bin(),
    ActualRec = decode_header(Bin),
    ExpectedRec = of_test_msgs_v11:header_rec(),
    ?assertEqual(ActualRec, ExpectedRec).

decode_header_bad_version_test() ->
    Bin = of_test_msgs_v11:header_bad_version_bin(),
    ?assertThrow({malformed, 
                  ?OF_ERROR_TYPE_BAD_REQUEST, 
                  ?OF_ERROR_CODE_BAD_REQUEST_BAD_VERSION},
                 decode_header(Bin)).

decode_header_bad_message_type_test() ->
    Bin = of_test_msgs_v11:header_bad_message_type_bin(),
    ?assertThrow({malformed, 
                  ?OF_ERROR_TYPE_BAD_REQUEST, 
                  ?OF_ERROR_CODE_BAD_REQUEST_BAD_TYPE},
                 decode_header(Bin)).

decode_hello_test() ->
    Bin = of_test_msgs_v11:hello_bin(),
    ActualRec = decode_hello(Bin),
    ExpectedRec = of_test_msgs_v11:hello_rec(),
    ?assertEqual(ActualRec, ExpectedRec).

decode_hello_with_extension_test() ->
    Bin = of_test_msgs_v11:hello_with_extension_bin(),
    ActualRec = decode_hello(Bin),
    ExpectedRec = of_test_msgs_v11:hello_with_extension_rec(),
    ?assertEqual(ActualRec, ExpectedRec).

decode_error_test() ->
    Bin = of_test_msgs_v11:error_bin(),
    ActualRec = decode_error(Bin),
    ExpectedRec = of_test_msgs_v11:error_rec(),
    ?assertEqual(ActualRec, ExpectedRec).

decode_error_with_data_test() ->
    Bin = of_test_msgs_v11:error_with_data_bin(),
    ActualRec = decode_error(Bin),
    ExpectedRec = of_test_msgs_v11:error_with_data_rec(),
    ?assertEqual(ActualRec, ExpectedRec).

decode_echo_request_test() ->
    Bin = of_test_msgs_v11:echo_request_bin(),
    ActualRec = decode_echo_request(Bin),
    ExpectedRec = of_test_msgs_v11:echo_request_rec(),
    ?assertEqual(ActualRec, ExpectedRec).

decode_echo_request_with_data_test() ->
    Bin = of_test_msgs_v11:echo_request_with_data_bin(),
    ActualRec = decode_echo_request(Bin),
    ExpectedRec = of_test_msgs_v11:echo_request_with_data_rec(),
    ?assertEqual(ActualRec, ExpectedRec).

decode_echo_reply_test() ->
    Bin = of_test_msgs_v11:echo_reply_bin(),
    ActualRec = decode_echo_reply(Bin),
    ExpectedRec = of_test_msgs_v11:echo_reply_rec(),
    ?assertEqual(ActualRec, ExpectedRec).

decode_echo_reply_with_data_test() ->
    Bin = of_test_msgs_v11:echo_reply_with_data_bin(),
    ActualRec = decode_echo_reply(Bin),
    ExpectedRec = of_test_msgs_v11:echo_reply_with_data_rec(),
    ?assertEqual(ActualRec, ExpectedRec).

decode_experimenter_test() ->
    Bin = of_test_msgs_v11:experimenter_bin(),
    ActualRec = decode_experimenter(Bin),
    ExpectedRec = of_test_msgs_v11:experimenter_rec(),
    ?assertEqual(ActualRec, ExpectedRec).

decode_experimenter_with_data_test() ->
    Bin = of_test_msgs_v11:experimenter_with_data_bin(),
    ActualRec = decode_experimenter(Bin),
    ExpectedRec = of_test_msgs_v11:experimenter_with_data_rec(),
    ?assertEqual(ActualRec, ExpectedRec).

decode_features_request_test() ->
    Bin = of_test_msgs_v11:features_request_bin(),
    ActualRec = decode_features_request(Bin),
    ExpectedRec = of_test_msgs_v11:features_request_rec(),
    ?assertEqual(ActualRec, ExpectedRec).

decode_features_reply_test() ->
    Bin = of_test_msgs_v11:features_reply_bin(),
    ActualRec = decode_features_reply(Bin),
    ExpectedRec = of_test_msgs_v11:features_reply_rec(),
    ?assertEqual(ActualRec, ExpectedRec).

decode_get_config_request_test() ->
    Bin = of_test_msgs_v11:get_config_request_bin(),
    ActualRec = decode_get_config_request(Bin),
    ExpectedRec = of_test_msgs_v11:get_config_request_rec(),
    ?assertEqual(ActualRec, ExpectedRec).
