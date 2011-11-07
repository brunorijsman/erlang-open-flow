%% @author Bruno Rijsman <brunorijsman@hotmail.com>
%% @copyright 2011 Bruno Rijsman
%%

%% TODO: Check for correct length of binary in all decode functions

-module(of_decoder).

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

-include_lib("../include/of.hrl").

-include_lib("../include/of_test_msgs.hrl").   %% ifdef TEST

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

decode_header_ok_test() ->
    Bin = ?HEADER_OK_BIN,
    ActualRec = decode_header(Bin),
    ExpectedRec = ?HEADER_OK_REC,
    ?assertEqual(ActualRec, ExpectedRec).

decode_header_bad_version_test() ->
    Bin = ?HEADER_BAD_VERSION_BIN,
    ?assertThrow({malformed, 
                  ?OF_ERROR_TYPE_BAD_REQUEST, 
                  ?OF_ERROR_CODE_BAD_REQUEST_BAD_VERSION},
                 decode_header(Bin)).

decode_header_bad_message_type_test() ->
    Bin = ?HEADER_BAD_MESSAGE_TYPE_BIN,
    ?assertThrow({malformed, 
                  ?OF_ERROR_TYPE_BAD_REQUEST, 
                  ?OF_ERROR_CODE_BAD_REQUEST_BAD_TYPE},
                 decode_header(Bin)).

decode_hello_ok_test() ->
    Bin = ?HELLO_OK_BIN,
    ActualRec = decode_hello(Bin),
    ExpectedRec = ?HELLO_OK_REC,
    ?assertEqual(ActualRec, ExpectedRec).

%% Openflow spec v1.1.0 section A.5.1: Implementations must be prepared to
%% receive a hello message that includes a body, ignoring its contents, to
%% allow for later extensions.
%%
decode_hello_long_test() ->
    Binary = << 1, 2, 3, 4, 5 >>,
    Record = #of_hello{},
    ?assert(decode_hello(Binary) =:= Record).

decode_error_test() ->
    Data = << 5, 4, 3, 2, 1 >>,
    Binary = << ?OF_ERROR_TYPE_BAD_ACTION : 16,               % Type
                ?OF_ERROR_CODE_BAD_ACTION_BAD_OUT_PORT : 16,  % Code
                Data/binary >>,                               % Data
    Record = #of_error{type = ?OF_ERROR_TYPE_BAD_ACTION,
                       code = ?OF_ERROR_CODE_BAD_ACTION_BAD_OUT_PORT,
                       data = Data},
    ?assert(decode_error(Binary) =:= Record).

decode_echo_request_ok_test() ->
    Binary = << 1, 1, 1, 2, 2, 2 >>,
    Record = #of_echo_request{data = Binary},
    ?assert(decode_echo_request(Binary) =:= Record).

decode_echo_reply_ok_test() ->
    Binary = << 3, 3, 3, 4, 4, 4 >>,
    Record = #of_echo_reply{data = Binary},
    ?assert(decode_echo_reply(Binary) =:= Record).

decode_experimenter_ok_test() ->
    Data = << 99, 99, 99, 88, 88, 88 >>,
    Binary = << 1 : 32,           % Experimenter ID
                0 : 32,           % Pad
                Data/binary >>,   % Data
    Record = #of_experimenter{experimenter_id = 1,
                              data = Data},
    ?assert(decode_experimenter(Binary) =:= Record).

decode_features_request_ok_test() ->
    Binary = <<>>,
    Record = #of_features_request{},
    ?assert(decode_features_request(Binary) =:= Record).

decode_features_reply_ok_test() ->
    DataPathId       = 123456789,
    Capabilities     = << 0 : 24,                     %% Reserved
                          1 : 1,                      %% ARP match IP
                          0 : 1,                      %% Queue stats
                          0 : 1,                      %% IP reassembly
                          0 : 1,                      %% Reserved
                          1 : 1,                      %% Group stats
                          0 : 1,                      %% Port stats
                          0 : 1,                      %% Table stats
                          0 : 1 >>,                   %% Flow stats
    Port1HwAddrBin   = << 1, 2, 3, 4, 5, 6 >>,
    Port1NameBin     = << "port1", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 >>,
    Port1ConfigBin   = << 0 : 25,                     %% Reserved
                          1 : 1,                      %% No packet in
                          1 : 1,                      %% No forward
                          0 : 2,                      %% Reserved
                          0 : 1,                      %% No receive
                          0 : 1,                      %% Reserved
                          1 : 1 >>,                   %% Port down
    Port1StateBin    = << 0 : 29,                     %% Reserved,
                          1 : 1,                      %% Live
                          0 : 1,                      %% Blocked
                          0 : 1 >>,                   %% Link down
    Port1FeaturesBin = << 0 : 16,                     %% Reserved
                          0 : 1,                      %% Pause asymetric
                          0 : 1,                      %% Pause
                          1 : 1,                      %% Auto negotiation
                          0 : 1,                      %% Fiber medium
                          1 : 1,                      %% Copper medium
                          0 : 1,                      %% Other rate
                          0 : 1,                      %% Full duplex 1 Tbps
                          0 : 1,                      %% Full duplex 100 Gbps
                          0 : 1,                      %% Full duplex 40 Gbps
                          0 : 1,                      %% Full duplex 10 Gbps
                          0 : 1,                      %% Full duplex 1 Gbps
                          0 : 1,                      %% Half duplex 1 Gbps
                          1 : 1,                      %% Full duplex 100 Mbps
                          1 : 1,                      %% Half duplex 100 Mbps
                          1 : 1,                      %% Full duplex 10 Mbps
                          1 : 1 >>,                   %% Half duplex 10 Mbps
    Port1Bin         = << 1            : 32,          %% Port no
                          0            : 32,          %% Padding
                          Port1HwAddrBin/binary,      %% Hardware address
                          0            : 16,          %% Padding
                          Port1NameBin/binary,        %% Name
                          Port1ConfigBin/binary,      %% Config
                          Port1StateBin/binary,       %% State
                          Port1FeaturesBin/binary,    %% Current features
                          Port1FeaturesBin/binary,    %% Advertised features
                          Port1FeaturesBin/binary,    %% Supported features
                          50000        : 32,          %% Current speed in kbps
                          100000       : 32 >>,       %% Max speed in kbps
    Port2Bin         = Port1Bin,
    Bin              = << DataPathId   : 64,          %% Data path ID
                          5000         : 32,          %% Number of buffers
                          50           : 8,           %% Number of tables
                          0            : 24,          %% Padding
                          Capabilities/binary,        %% Capabilities
                          0            : 32,          %% Reserved
                          Port1Bin/binary,            %% Port 1 configuration
                          Port2Bin/binary >>,         %% Port 2 configuration
    CapabilitiesRec = #of_capabilities{flow_stats      = false,
                                          table_stats  = false,
                                          port_stats   = false,
                                          group_stats  = true,
                                          ip_reasm     = false,
                                          queue_stats  = false,
                                          arp_match_ip = true},
    Port1ConfigRec = #of_port_config{port_down    = true,
                                     no_recv      = false,
                                     no_fwd       = true,
                                     no_packet_in = true},
    Port1StateRec = #of_port_state{link_down = false,
                                   blocked   = false,
                                   live      = true},
    Port1FeaturesRec = #of_port_features{half_duplex_10_mbps  = true,
                                         full_duplex_10_mbps  = true,
                                         half_duplex_100_mbps = true,
                                         full_duplex_100_mbps = true,
                                         half_duplex_1_gbps   = false,
                                         full_duplex_1_gbps   = false,
                                         full_duplex_10_gbps  = false,
                                         full_duplex_40_gbps  = false,
                                         full_duplex_100_gbps = false,
                                         full_duplex_1_tbps   = false,
                                         other_rate           = false,
                                         copper_medium        = true,
                                         fiber_medium         = false,
                                         auto_negotiation     = true,
                                         pause                = false,
                                         pause_asymetric      = false},
    Port1Rec = #of_port{port_no             = 1,
                        hw_addr             = Port1HwAddrBin,
                        name                = "port1",
                        config              = Port1ConfigRec,
                        state               = Port1StateRec,
                        current_features    = Port1FeaturesRec,
                        advertised_features = Port1FeaturesRec,
                        supported_features  = Port1FeaturesRec,
                        current_speed_kbps  = 50000,
                        max_speed_kbps      = 100000},
    Port2Rec = Port1Rec,
    Rec = #of_features_reply{data_path_id = 123456789,
                             n_buffers    = 5000,
                             n_tables     = 50,
                             capabilities = CapabilitiesRec,
                             ports        = [Port1Rec, Port2Rec]},
    ActualRec = decode_features_reply(Bin),
    io:format("A = ~P~n", [ActualRec, 100]),
    io:format("R = ~P~n", [Rec, 100]),
    ?assert(ActualRec =:= Rec).

decode_get_config_request_ok_test() ->
    Binary = <<>>,
    Record = #of_get_config_request{},
    ?assert(decode_get_config_request(Binary) =:= Record).

