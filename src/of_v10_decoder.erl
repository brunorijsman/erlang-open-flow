%% @author Bruno Rijsman <brunorijsman@hotmail.com>
%% @copyright 2011 Bruno Rijsman
%%

%% @@@ ### CONTINUE FROM HERE: ADD TEST CASE FOR FLOW REMOVED

%% TODO: Check for correct length of binary in all decode functions

-module(of_v10_decoder).

-export([decode_header/1,
         decode_hello/1,
         decode_error/1,
         decode_echo_request/1,
         decode_echo_reply/1,
         decode_vendor/1,
         decode_features_request/1,
         decode_features_reply/1,
         decode_get_config_request/1,
         decode_get_config_reply/1,
         decode_set_config/1,
         decode_packet_in/1,
         decode_flow_removed/1]).

-include_lib("eunit/include/eunit.hrl").

-include_lib("../include/of.hrl").
-include_lib("../include/of_v10.hrl").

%%
%% Exported functions.
%%

%% TODO: Validate length?
%% TODO: Validate xid?
-spec decode_header(binary()) -> #of_v10_header{}.
decode_header(?OF_V10_HEADER_PATTERN) ->
    Header = #of_v10_header{
      version = Version,
      type    = Type,
      length  = Length,
      xid     = Xid
     },
    if
        Version /= ?OF_V10_VERSION ->
            throw({malformed, ?OF_V10_ERROR_TYPE_BAD_REQUEST, ?OF_V10_ERROR_CODE_BAD_REQUEST_BAD_VERSION});
        (Type < ?OF_V10_MESSAGE_TYPE_MIN) or (Type > ?OF_V10_MESSAGE_TYPE_MAX) ->
            throw({malformed, ?OF_V10_ERROR_TYPE_BAD_REQUEST, ?OF_V10_ERROR_CODE_BAD_REQUEST_BAD_TYPE});
        true ->
            Header
    end.

-spec decode_hello(binary()) -> #of_v10_hello{}.
decode_hello(?OF_V10_HELLO_PATTERN) ->
    _Hello = #of_v10_hello{}.

-spec decode_error(binary()) -> #of_v10_error{}.
decode_error(?OF_V10_ERROR_PATTERN) ->
    %% No validation, accept unrecognized types and codes.
    _Error = #of_v10_error{
      type = Type,
      code = Code,
      data = Data
     }.

-spec decode_echo_request(binary()) -> #of_v10_echo_request{}.
decode_echo_request(?OF_V10_ECHO_REQUEST_PATTERN) ->
    _EchoRequest = #of_v10_echo_request{
      data = Data
     }.

-spec decode_echo_reply(binary()) -> #of_v10_echo_reply{}.
decode_echo_reply(?OF_V10_ECHO_REPLY_PATTERN) ->
    _EchoReply = #of_v10_echo_reply{
      data = Data
     }.

-spec decode_vendor(binary()) -> #of_v10_vendor{}.
decode_vendor(?OF_V10_VENDOR_PATTERN) ->
    %% No validation, higher layer to determine if the extension is supported.
    _Vendor = #of_v10_vendor{
      vendor_id = VendorId,
      data = Data
     }.

-spec decode_features_request(binary()) -> #of_v10_features_request{}.
decode_features_request(?OF_V10_FEATURES_REQUEST_PATTERN) ->
    _ReaturesRequest = #of_v10_features_request{}.

-spec decode_features_reply(binary()) -> #of_v10_features_reply{}.
decode_features_reply(?OF_V10_FEATURES_REPLY_PATTERN) ->
    _FeaturesReply = #of_v10_features_reply{
      data_path_id = DataPathId,
      n_buffers    = NBuffers,
      n_tables     = NTables,
      capabilities = decode_capabilities(Capabilities),
      actions      = decode_actions(Actions),
      ports        = decode_phy_ports(Ports)
     }.

-spec decode_get_config_request(binary()) -> #of_v10_get_config_request{}.
decode_get_config_request(?OF_V10_GET_CONFIG_REQUEST_PATTERN) ->
    _GetConfigRequest = #of_v10_get_config_request{}.

-spec decode_get_config_reply(binary()) -> #of_v10_get_config_reply{}.
decode_get_config_reply(?OF_V10_GET_CONFIG_REPLY_PATTERN) ->
    _GetConfigReply = #of_v10_get_config_reply{
      switch_config = decode_switch_config(SwitchConfig)
     }.

-spec decode_set_config(binary()) -> #of_v10_set_config{}.
decode_set_config(?OF_V10_SET_CONFIG_PATTERN) ->
    _SetConfig = #of_v10_set_config{
      switch_config = decode_switch_config(SwitchConfig)
     }.

-spec decode_packet_in(binary()) -> #of_v10_packet_in{}.
decode_packet_in(?OF_V10_PACKET_IN_PATTERN) ->
    _PacketIn = #of_v10_packet_in{
      buffer_id = BufferId,
      total_len = TotalLen,
      in_port   = InPort,
      reason    = Reason,
      data      = Data
     }.

%% TODO: validate Reason (similar in all other messages)
-spec decode_flow_removed(binary()) -> #of_v10_flow_removed{}.
decode_flow_removed(?OF_V10_FLOW_REMOVED_PATTERN) ->
    _FlowRemoved = #of_v10_flow_removed{
      match         = decode_flow_match(Match),
      cookie        = Cookie,
      priority      = Priority,
      reason        = Reason,
      duration_sec  = DurationSec,
      duration_nsec = DurationNsec,
      idle_timeout  = IdleTimeout,
      packet_count  = PacketCount,
      byte_count    = ByteCount
     }.

%%
%% Internal functions.
%%

-spec decode_capabilities(binary()) -> #of_v10_capabilities{}.
decode_capabilities(?OF_V10_CAPABILITIES_PATTERN) ->
    _Capabilities = #of_v10_capabilities{
      flow_stats   = (FlowStats == 1),
      table_stats  = (TableStats == 1),
      port_stats   = (PortStats == 1),
      stp          = (Stp == 1),
      ip_reasm     = (IpReasm == 1),
      queue_stats  = (QueueStats == 1),
      arp_match_ip = (ArpMatchIp == 1)
     }.

-spec decode_actions(binary()) -> #of_v10_actions{}.
decode_actions(?OF_V10_ACTIONS_BITMAP_PATTERN) ->
    _Actions = #of_v10_actions{
      output       = (Output == 1),
      set_vlan_id  = (SetVlanId == 1),
      set_vlan_pcp = (SetVlanPcp == 1),
      strip_vlan   = (StripVlan == 1),
      set_dl_src   = (SetDlSrc == 1),
      set_dl_dst   = (SetDlDst == 1),
      set_nw_src   = (SetNwSrc == 1),
      set_nw_dst   = (SetNwDst == 1),
      set_nw_tos   = (SetNwTos == 1),
      set_tp_src   = (SetTpSrc == 1),
      set_tp_dst   = (SetTpDst == 1),
      enqueue      = (Enqueue == 1)
     }.

-spec decode_phy_port_config(binary()) -> #of_v10_phy_port_config{}.
decode_phy_port_config(?OF_V10_PHY_PORT_CONFIG_PATTERN) ->
    _PhyPortConfig = #of_v10_phy_port_config{
      port_down    = (PortDown == 1),
      no_stp       = (NoStp == 1),
      no_recv      = (NoRecv == 1),
      no_recv_stp  = (NoRecvStp == 1),
      no_flood     = (NoFlood == 1),
      no_fwd       = (NoFwd == 1),
      no_packet_in = (NoPacketIn == 1)
     }.

-spec decode_phy_port_state(binary()) -> #of_v10_phy_port_state{}.
decode_phy_port_state(?OF_V10_PHY_PORT_STATE_PATTERN) ->
    _PhyPortState = #of_v10_phy_port_state{
      link_down      = (LinkDown == 1),
      stp_port_state = StpPortState
     }.

-spec decode_phy_port_features(binary()) -> #of_v10_phy_port_features{}.
decode_phy_port_features(?OF_V10_PHY_PORT_FEATURES_PATTERN) ->
    _PhyPortFeatures = #of_v10_phy_port_features{
      half_duplex_10_mbps  = (HalfDuplex10Mbps == 1),
      full_duplex_10_mbps  = (FullDuplex10Mbps == 1),
      half_duplex_100_mbps = (HalfDuplex100Mbps == 1),
      full_duplex_100_mbps = (FullDuplex100Mbps == 1),
      half_duplex_1_gbps   = (HalfDuplex1Gbps == 1),
      full_duplex_1_gbps   = (FullDuplex1Gbps == 1),
      full_duplex_10_gbps  = (FullDuplex10Gbps == 1),
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

-spec decode_phy_ports(binary()) -> [#of_v10_phy_port{}].
decode_phy_ports(Binary) ->
    decode_phy_ports(Binary, []).
    
-spec decode_phy_ports(binary(), [#of_v10_phy_port{}]) -> [#of_v10_phy_port{}].
decode_phy_ports(<<>>, ParsedPhyPorts) ->
    ParsedPhyPorts;
decode_phy_ports(<<PhyPortBin: 48/binary, RestBin/binary>>, ParsedPhyPorts) ->
    PhyPortRec = decode_phy_port(PhyPortBin),
    decode_phy_ports(RestBin, [PhyPortRec | ParsedPhyPorts]).

-spec decode_phy_port(binary()) -> #of_v10_phy_port{}.
decode_phy_port(?OF_V10_PHY_PORT_PATTERN) ->
    _PhyPort = #of_v10_phy_port {
      port_no             = PortNo,
      hw_addr             = HwAddr,
      name                = decode_string(Name),
      config              = decode_phy_port_config(Config),
      state               = decode_phy_port_state(State),
      current_features    = decode_phy_port_features(CurrentFeatures),
      advertised_features = decode_phy_port_features(AdvertisedFeatures),
      supported_features  = decode_phy_port_features(SupportedFeatures),
      peer_features       = decode_phy_port_features(PeerFeatures)
     }.

-spec decode_switch_config_flags(binary()) -> of_v10_frag_handling().
decode_switch_config_flags(?OF_V10_SWITCH_CONFIG_FLAGS_PATTERN) ->
    FragHandling.

-spec decode_switch_config(binary()) -> #of_v10_switch_config{}.
decode_switch_config(?OF_V10_SWITCH_CONFIG_PATTERN) ->
    _SwitchConfig = #of_v10_switch_config{
      frag_handling = decode_switch_config_flags(Flags),
      miss_send_len = MissSendLen
     }.

-spec decode_flow_match_wildcards(binary()) -> #of_v10_flow_match_wildcards{}.
decode_flow_match_wildcards(?OF_V10_FLOW_MATCH_WILDCARDS_PATTERN) ->
    _FlowMatchWildcards = #of_v10_flow_match_wildcards{
      in_port     = (InPort == 1),
      dl_vlan     = (DlVlan == 1),
      dl_src      = (DlSrc == 1),
      dl_dst      = (DlDst == 1),
      dl_type     = (DlType == 1),
      nw_proto    = (NwProto == 1),
      tp_src      = (TpSrc == 1),
      tp_dst      = (TpDst == 1),
      nw_src_bits = NwSrcBits,
      nw_dst_bits = NwDstBits,
      dl_vlan_pcp = (DlVlanPcp == 1),
      nw_tos      = (NwTos == 1)
     }.

-spec decode_flow_match(binary()) -> #of_v10_flow_match{}.
decode_flow_match(?OF_V10_FLOW_MATCH_PATTERN) ->
    _FlowMatch = #of_v10_flow_match{
      wildcards   = decode_flow_match_wildcards(Wildcards),
      in_port     = InPort,
      dl_src      = DlSrc,
      dl_dst      = DlDst,
      dl_vlan     = DlVlan,
      dl_vlan_pcp = DlVlanPcp,
      dl_type     = DlType,
      nw_tos      = NwTos,
      nw_proto    = NwProto,
      nw_src      = NwSrc,
      nw_dst      = NwDst,
      tp_src      = TpSrc,
      tp_dst      = TpDst
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
    Bin = of_v10_test_msgs:header_bin(),
    ActualRec = decode_header(Bin),
    ExpectedRec = of_v10_test_msgs:header_rec(),
    ?assertEqual(ActualRec, ExpectedRec).

decode_header_bad_version_test() ->
    Bin = of_v10_test_msgs:header_bad_version_bin(),
    ?assertThrow({malformed, 
                  ?OF_V10_ERROR_TYPE_BAD_REQUEST, 
                  ?OF_V10_ERROR_CODE_BAD_REQUEST_BAD_VERSION},
                 decode_header(Bin)).

decode_header_bad_message_type_test() ->
    Bin = of_v10_test_msgs:header_bad_message_type_bin(),
    ?assertThrow({malformed, 
                  ?OF_V10_ERROR_TYPE_BAD_REQUEST, 
                  ?OF_V10_ERROR_CODE_BAD_REQUEST_BAD_TYPE},
                 decode_header(Bin)).

decode_hello_test() ->
    Bin = of_v10_test_msgs:hello_bin(),
    ActualRec = decode_hello(Bin),
    ExpectedRec = of_v10_test_msgs:hello_rec(),
    ?assertEqual(ActualRec, ExpectedRec).

decode_hello_with_extension_test() ->
    Bin = of_v10_test_msgs:hello_with_extension_bin(),
    ActualRec = decode_hello(Bin),
    ExpectedRec = of_v10_test_msgs:hello_with_extension_rec(),
    ?assertEqual(ActualRec, ExpectedRec).

decode_error_test() ->
    Bin = of_v10_test_msgs:error_bin(),
    ActualRec = decode_error(Bin),
    ExpectedRec = of_v10_test_msgs:error_rec(),
    ?assertEqual(ActualRec, ExpectedRec).

decode_error_with_data_test() ->
    Bin = of_v10_test_msgs:error_with_data_bin(),
    ActualRec = decode_error(Bin),
    ExpectedRec = of_v10_test_msgs:error_with_data_rec(),
    ?assertEqual(ActualRec, ExpectedRec).

decode_echo_request_test() ->
    Bin = of_v10_test_msgs:echo_request_bin(),
    ActualRec = decode_echo_request(Bin),
    ExpectedRec = of_v10_test_msgs:echo_request_rec(),
    ?assertEqual(ActualRec, ExpectedRec).

decode_echo_request_with_data_test() ->
    Bin = of_v10_test_msgs:echo_request_with_data_bin(),
    ActualRec = decode_echo_request(Bin),
    ExpectedRec = of_v10_test_msgs:echo_request_with_data_rec(),
    ?assertEqual(ActualRec, ExpectedRec).

decode_echo_reply_test() ->
    Bin = of_v10_test_msgs:echo_reply_bin(),
    ActualRec = decode_echo_reply(Bin),
    ExpectedRec = of_v10_test_msgs:echo_reply_rec(),
    ?assertEqual(ActualRec, ExpectedRec).

decode_echo_reply_with_data_test() ->
    Bin = of_v10_test_msgs:echo_reply_with_data_bin(),
    ActualRec = decode_echo_reply(Bin),
    ExpectedRec = of_v10_test_msgs:echo_reply_with_data_rec(),
    ?assertEqual(ActualRec, ExpectedRec).

decode_vendor_test() ->
    Bin = of_v10_test_msgs:vendor_bin(),
    ActualRec = decode_vendor(Bin),
    ExpectedRec = of_v10_test_msgs:vendor_rec(),
    ?assertEqual(ActualRec, ExpectedRec).

decode_vendor_with_data_test() ->
    Bin = of_v10_test_msgs:vendor_with_data_bin(),
    ActualRec = decode_vendor(Bin),
    ExpectedRec = of_v10_test_msgs:vendor_with_data_rec(),
    ?assertEqual(ActualRec, ExpectedRec).

decode_features_request_test() ->
    Bin = of_v10_test_msgs:features_request_bin(),
    ActualRec = decode_features_request(Bin),
    ExpectedRec = of_v10_test_msgs:features_request_rec(),
    ?assertEqual(ActualRec, ExpectedRec).

decode_features_reply_test() ->
    Bin = of_v10_test_msgs:features_reply_bin(),
    ActualRec = decode_features_reply(Bin),
    ExpectedRec = of_v10_test_msgs:features_reply_rec(),
    ?assertEqual(ActualRec, ExpectedRec).

decode_get_config_request_test() ->
    Bin = of_v10_test_msgs:get_config_request_bin(),
    ActualRec = decode_get_config_request(Bin),
    ExpectedRec = of_v10_test_msgs:get_config_request_rec(),
    ?assertEqual(ActualRec, ExpectedRec).

decode_get_config_reply_test() ->
    Bin = of_v10_test_msgs:get_config_reply_bin(),
    ActualRec = decode_get_config_reply(Bin),
    ExpectedRec = of_v10_test_msgs:get_config_reply_rec(),
    ?assertEqual(ActualRec, ExpectedRec).

decode_set_config_test() ->
    Bin = of_v10_test_msgs:set_config_bin(),
    ActualRec = decode_set_config(Bin),
    ExpectedRec = of_v10_test_msgs:set_config_rec(),
    ?assertEqual(ActualRec, ExpectedRec).

decode_packet_in_test() ->
    Bin = of_v10_test_msgs:packet_in_bin(),
    ActualRec = decode_packet_in(Bin),
    ExpectedRec = of_v10_test_msgs:packet_in_rec(),
    ?assertEqual(ActualRec, ExpectedRec).
