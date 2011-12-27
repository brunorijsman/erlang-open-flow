%% @author Bruno Rijsman <brunorijsman@hotmail.com>
%% @copyright 2011 Bruno Rijsman

-module(of_v10_encoder).

-export([encode/2]).

%% TODO: ifdef TEST everywhere?
-include_lib("eunit/include/eunit.hrl").

-include_lib("../include/of_v10.hrl").

%%
%% Exported functions.
%%

encode(MessageRec, Xid) ->
    {Type, BodyBin} = encode_body(MessageRec),
    HeaderRec = #of_v10_header{
      version = ?OF_V10_VERSION,
      type    = Type,
      length  = size(BodyBin) + ?OF_V10_HEADER_LEN,
      xid     = Xid},
    HeaderBin = encode_header(HeaderRec),
    [HeaderBin, BodyBin].

%%
%% Internal functions.
%%

encode_bool(false) -> 0;
encode_bool(true)  -> 1.

encode_string(String, Length) 
  when length(String) =< Length ->
    StringLength = length(String),
    PaddingLength = Length - StringLength,
    ZeroPadding = lists:duplicate(PaddingLength, 0),
    PaddedString = lists:concat([String, ZeroPadding]),
    list_to_binary(PaddedString).

encode_capabilities(CapabilitiesRec) ->
    FlowStats  = encode_bool(CapabilitiesRec#of_v10_capabilities.flow_stats),
    TableStats = encode_bool(CapabilitiesRec#of_v10_capabilities.table_stats),
    PortStats  = encode_bool(CapabilitiesRec#of_v10_capabilities.port_stats),
    Stp        = encode_bool(CapabilitiesRec#of_v10_capabilities.stp),
    IpReasm    = encode_bool(CapabilitiesRec#of_v10_capabilities.ip_reasm),
    QueueStats = encode_bool(CapabilitiesRec#of_v10_capabilities.queue_stats),
    ArpMatchIp = encode_bool(CapabilitiesRec#of_v10_capabilities.arp_match_ip),
    _Reserved1 = 0,
    _Reserved2 = 0,
    ?OF_V10_CAPABILITIES_PATTERN.

encode_actions_bitmap(ActionsRec) ->
    Output     = encode_bool(ActionsRec#of_v10_actions.output),
    SetVlanId  = encode_bool(ActionsRec#of_v10_actions.set_vlan_id),
    SetVlanPcp = encode_bool(ActionsRec#of_v10_actions.set_vlan_pcp),
    StripVlan  = encode_bool(ActionsRec#of_v10_actions.strip_vlan),
    SetDlSrc   = encode_bool(ActionsRec#of_v10_actions.set_dl_src),
    SetDlDst   = encode_bool(ActionsRec#of_v10_actions.set_dl_dst),
    SetNwSrc   = encode_bool(ActionsRec#of_v10_actions.set_nw_src),
    SetNwDst   = encode_bool(ActionsRec#of_v10_actions.set_nw_dst),
    SetNwTos   = encode_bool(ActionsRec#of_v10_actions.set_nw_tos),
    SetTpSrc   = encode_bool(ActionsRec#of_v10_actions.set_tp_src),
    SetTpDst   = encode_bool(ActionsRec#of_v10_actions.set_tp_dst),
    Enqueue    = encode_bool(ActionsRec#of_v10_actions.enqueue),
    _Reserved  = 0,
    ?OF_V10_ACTIONS_BITMAP_PATTERN.

encode_phy_ports(PortList) ->
    iolist_to_binary(lists:map(fun encode_phy_port/1, PortList)).

encode_phy_port(PortRec) ->
    #of_v10_phy_port{port_no             = PortNo,
                     hw_addr             = HwAddr,
                     name                = NameStr,
                     config              = ConfigRec,
                     state               = StateRec,
                     current_features    = CurrentFeaturesRec,
                     advertised_features = AdvertisedFeaturesRec,
                     supported_features  = SupportedFeaturesRec,
                     peer_features       = PeerFeaturesRec} = PortRec,
    Name = encode_string(NameStr, ?OF_V10_MAX_PORT_NAME_LEN),
    Config = encode_phy_port_config(ConfigRec),
    State = encode_phy_port_state(StateRec),
    CurrentFeatures = encode_phy_port_features(CurrentFeaturesRec),
    AdvertisedFeatures = encode_phy_port_features(AdvertisedFeaturesRec),
    SupportedFeatures = encode_phy_port_features(SupportedFeaturesRec),
    PeerFeatures = encode_phy_port_features(PeerFeaturesRec),
    ?OF_V10_PHY_PORT_PATTERN.

encode_phy_port_config(ConfigRec) ->
    PortDown   = encode_bool(ConfigRec#of_v10_phy_port_config.port_down),
    NoStp      = encode_bool(ConfigRec#of_v10_phy_port_config.no_stp),
    NoRecv     = encode_bool(ConfigRec#of_v10_phy_port_config.no_recv),
    NoRecvStp  = encode_bool(ConfigRec#of_v10_phy_port_config.no_recv_stp),
    NoFlood    = encode_bool(ConfigRec#of_v10_phy_port_config.no_flood),
    NoFwd      = encode_bool(ConfigRec#of_v10_phy_port_config.no_fwd),
    NoPacketIn = encode_bool(ConfigRec#of_v10_phy_port_config.no_packet_in),
    _Reserved  = 0,
    ?OF_V10_PHY_PORT_CONFIG_PATTERN.

encode_phy_port_state(StateRec) ->
    LinkDown     = encode_bool(StateRec#of_v10_phy_port_state.link_down),
    StpPortState = StateRec#of_v10_phy_port_state.stp_port_state,
    _Reserved1   = 0,
    _Reserved2   = 0,
    ?OF_V10_PHY_PORT_STATE_PATTERN.

encode_phy_port_features(FeaturesRec) ->
    HalfDuplex10Mbps  = encode_bool(FeaturesRec#of_v10_phy_port_features.half_duplex_10_mbps),
    FullDuplex10Mbps  = encode_bool(FeaturesRec#of_v10_phy_port_features.full_duplex_10_mbps),
    HalfDuplex100Mbps = encode_bool(FeaturesRec#of_v10_phy_port_features.half_duplex_100_mbps),
    FullDuplex100Mbps = encode_bool(FeaturesRec#of_v10_phy_port_features.full_duplex_100_mbps),
    HalfDuplex1Gbps   = encode_bool(FeaturesRec#of_v10_phy_port_features.half_duplex_1_gbps),
    FullDuplex1Gbps   = encode_bool(FeaturesRec#of_v10_phy_port_features.full_duplex_1_gbps),
    FullDuplex10Gbps  = encode_bool(FeaturesRec#of_v10_phy_port_features.full_duplex_10_gbps),
    CopperMedium      = encode_bool(FeaturesRec#of_v10_phy_port_features.copper_medium),
    FiberMedium       = encode_bool(FeaturesRec#of_v10_phy_port_features.fiber_medium),
    AutoNegotiation   = encode_bool(FeaturesRec#of_v10_phy_port_features.auto_negotiation),
    Pause             = encode_bool(FeaturesRec#of_v10_phy_port_features.pause),
    PauseAsymetric    = encode_bool(FeaturesRec#of_v10_phy_port_features.pause_asymetric),
    _Reserved         = 0,
    ?OF_V10_PHY_PORT_FEATURES_PATTERN.

encode_switch_config_flags(FragHandling) ->
    _Reserved = 0,
    ?OF_V10_SWITCH_CONFIG_FLAGS_PATTERN.

encode_switch_config(SwitchConfigRec) ->
    Flags       = encode_switch_config_flags(SwitchConfigRec#of_v10_switch_config.frag_handling),
    MissSendLen = SwitchConfigRec#of_v10_switch_config.miss_send_len,
    ?OF_V10_SWITCH_CONFIG_PATTERN.

encode_flow_match_wildcards(WildCardsRec) ->
    InPort    = encode_bool(WildCardsRec#of_v10_flow_match_wildcards.in_port),
    DlVlan    = encode_bool(WildCardsRec#of_v10_flow_match_wildcards.dl_vlan),
    DlSrc     = encode_bool(WildCardsRec#of_v10_flow_match_wildcards.dl_src),
    DlDst     = encode_bool(WildCardsRec#of_v10_flow_match_wildcards.dl_dst),
    DlType    = encode_bool(WildCardsRec#of_v10_flow_match_wildcards.dl_type),
    NwProto   = encode_bool(WildCardsRec#of_v10_flow_match_wildcards.nw_proto),
    TpSrc     = encode_bool(WildCardsRec#of_v10_flow_match_wildcards.tp_src),
    TpDst     = encode_bool(WildCardsRec#of_v10_flow_match_wildcards.tp_dst),
    NwSrcBits = WildCardsRec#of_v10_flow_match_wildcards.nw_src_bits,
    NwDstBits = WildCardsRec#of_v10_flow_match_wildcards.nw_dst_bits,
    DlVlanPcp = encode_bool(WildCardsRec#of_v10_flow_match_wildcards.dl_vlan_pcp),
    NwTos     = encode_bool(WildCardsRec#of_v10_flow_match_wildcards.nw_tos),
    _Reserved = 0,
    ?OF_V10_FLOW_MATCH_WILDCARDS_PATTERN.

encode_flow_match(FlowMatchRec) ->
    Wildcards = encode_flow_match_wildcards(FlowMatchRec#of_v10_flow_match.wildcards),
    #of_v10_flow_match{in_port     = InPort,
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
                       tp_dst      = TpDst} = FlowMatchRec,
    _Pad1 = 0,
    _Pad2 = 0,
    ?OF_V10_FLOW_MATCH_PATTERN.

encode_actions(ActionList) ->
    %% CONTINUTE FROM HERE.
    [].
    

-spec encode_header(#of_v10_header{}) -> binary().
encode_header(Header) ->
    #of_v10_header{version = Version, 
                   type    = Type, 
                   length  = Length,
                   xid     = Xid
                  } = Header,
    ?OF_V10_HEADER_PATTERN.

-spec encode_body(of_v10_message()) -> {of_v10_message_type(), binary()}.

encode_body(BodyRec) ->
    if
        is_record(BodyRec, of_v10_hello)                  -> encode_hello(BodyRec);
        is_record(BodyRec, of_v10_error)                  -> encode_error(BodyRec);
        is_record(BodyRec, of_v10_echo_request)           -> encode_echo_request(BodyRec);
        is_record(BodyRec, of_v10_echo_reply)             -> encode_echo_reply(BodyRec);
        is_record(BodyRec, of_v10_vendor)                 -> encode_vendor(BodyRec);
        is_record(BodyRec, of_v10_features_request)       -> encode_features_request(BodyRec);
        is_record(BodyRec, of_v10_features_reply)         -> encode_features_reply(BodyRec);
        is_record(BodyRec, of_v10_get_config_request)     -> encode_get_config_request(BodyRec);
        is_record(BodyRec, of_v10_get_config_reply)       -> encode_get_config_reply(BodyRec);
        is_record(BodyRec, of_v10_set_config)             -> encode_set_config(BodyRec);
        is_record(BodyRec, of_v10_packet_in)              -> encode_packet_in(BodyRec);
        is_record(BodyRec, of_v10_flow_removed)           -> encode_flow_removed(BodyRec);
        is_record(BodyRec, of_v10_port_status)            -> encode_port_status(BodyRec);
        is_record(BodyRec, of_v10_packet_out)             -> encode_packet_out(BodyRec)
    end.

encode_hello(_HelloRec) ->
    _FutureExtension = <<>>,
    {?OF_V10_MESSAGE_TYPE_HELLO, ?OF_V10_HELLO_PATTERN}.

encode_error(ErrorRec) ->
    #of_v10_error{type = Type, code = Code, data = Data} = ErrorRec,
    {?OF_V10_MESSAGE_TYPE_ERROR, ?OF_V10_ERROR_PATTERN}.

encode_echo_request(EchoRequestRec) ->
    #of_v10_echo_request{data = Data} = EchoRequestRec,
    {?OF_V10_MESSAGE_TYPE_ECHO_REQUEST, ?OF_V10_ECHO_REQUEST_PATTERN}.

encode_echo_reply(EchoReplyRec) ->
    #of_v10_echo_reply{data = Data} = EchoReplyRec,
    {?OF_V10_MESSAGE_TYPE_ECHO_REPLY, ?OF_V10_ECHO_REPLY_PATTERN}.

encode_vendor(VendorRec) ->
    #of_v10_vendor{vendor_id = VendorId, data = Data} = VendorRec,
    {?OF_V10_MESSAGE_TYPE_VENDOR, ?OF_V10_VENDOR_PATTERN}.

encode_features_request(_FeaturesRequestRec) ->
    {?OF_V10_MESSAGE_TYPE_FEATURES_REQUEST, ?OF_V10_FEATURES_REQUEST_PATTERN}.

encode_features_reply(FeaturesReplyRec) ->
    #of_v10_features_reply{data_path_id = DataPathId, 
                           n_buffers = NBuffers,
                           n_tables = NTables,
                           capabilities = CapabilitiesRec,
                           actions = ActionsRec,
                           ports = PortsList} = FeaturesReplyRec,
    Capabilities = encode_capabilities(CapabilitiesRec),
    Actions = encode_actions_bitmap(ActionsRec),
    Ports = encode_phy_ports(PortsList),
    _Pad1 = 0,
    {?OF_V10_MESSAGE_TYPE_FEATURES_REPLY, ?OF_V10_FEATURES_REPLY_PATTERN}.

encode_get_config_request(_GetConfigRequestRec) ->
    {?OF_V10_MESSAGE_TYPE_GET_CONFIG_REQUEST, ?OF_V10_GET_CONFIG_REQUEST_PATTERN}.

encode_get_config_reply(GetConfigReplyRec) ->
    SwitchConfig = encode_switch_config(GetConfigReplyRec#of_v10_get_config_reply.switch_config),
    {?OF_V10_MESSAGE_TYPE_GET_CONFIG_REPLY, ?OF_V10_GET_CONFIG_REPLY_PATTERN}.

encode_set_config(SetConfigRec) ->
    SwitchConfig = encode_switch_config(SetConfigRec#of_v10_set_config.switch_config),
    {?OF_V10_MESSAGE_TYPE_SET_CONFIG, ?OF_V10_SET_CONFIG_PATTERN}.

encode_packet_in(PacketInRec) ->
    #of_v10_packet_in{buffer_id = BufferId,
                      total_len = TotalLen,
                      in_port   = InPort,
                      reason    = Reason,
                      data      = Data} = PacketInRec,
    _Pad = 0,
    {?OF_V10_MESSAGE_TYPE_PACKET_IN, ?OF_V10_PACKET_IN_PATTERN}.

encode_flow_removed(FlowRemovedRec) ->
    Match = encode_flow_match(FlowRemovedRec#of_v10_flow_removed.match),
    #of_v10_flow_removed{cookie        = Cookie,
                         priority      = Priority,
                         reason        = Reason,
                         duration_sec  = DurationSec,
                         duration_nsec = DurationNsec,
                         idle_timeout  = IdleTimeout,
                         packet_count  = PacketCount,
                         byte_count    = ByteCount} = FlowRemovedRec,
    _Pad1 = 0,
    _Pad2 = 0,
    {?OF_V10_MESSAGE_TYPE_FLOW_REMOVED, ?OF_V10_FLOW_REMOVED_PATTERN}.

encode_port_status(PortStatusRec) ->
    Desc   = encode_phy_port(PortStatusRec#of_v10_port_status.desc),
    Reason = PortStatusRec#of_v10_port_status.reason,
    _Pad   = 0,
    {?OF_V10_MESSAGE_TYPE_PORT_STATUS, ?OF_V10_PORT_STATUS_PATTERN}.

encode_packet_out(PacketOutRec) ->
    Actions = encode_actions(PacketOutRec#of_v10_packet_out.actions),
    #of_v10_packet_out{buffer_id = BufferId,
                       in_port   = InPort,
                       actions   = ActionRecs,
                       data      = Data} = PacketOutRec,
    {?OF_V10_MESSAGE_TYPE_PACKET_OUT, ?OF_V10_PACKET_OUT_PATTERN}.

%%
%% Unit tests.
%%

encode_bool_true_test() ->
    ?assertEqual(1, encode_bool(true)).

encode_bool_false_test() ->
    ?assertEqual(0, encode_bool(false)).

encode_string_exact_length_test() ->
    String = "hello",
    EncodeLength = 5,
    ActualBin = encode_string(String, EncodeLength),
    ExpectedBin = << "hello" >>,
    ?assertEqual(ExpectedBin, ActualBin).

encode_empty_string_test() ->
    String = "",
    EncodeLength = 5,
    ActualBin = encode_string(String, EncodeLength),
    ExpectedBin = << 0, 0, 0, 0, 0 >>,
    ?assertEqual(ExpectedBin, ActualBin).

encode_string_longer_length_test() ->
    String = "hello",
    EncodeLength = 10,
    ActualBin = encode_string(String, EncodeLength),
    ExpectedBin = << "hello", 0, 0, 0, 0, 0 >>,
    ?assertEqual(ExpectedBin, ActualBin).

encode_string_shorter_length_test() ->
    String = "hello",
    EncodeLength = 3,
    ?assertError(function_clause, encode_string(String, EncodeLength)).

encode_test() ->
    MessageRec = of_v10_test_msgs:echo_request_rec(),
    Xid = 1234,
    ActualResult = encode(MessageRec, Xid),
    ExpectedBodyBin = of_v10_test_msgs:echo_request_bin(),
    Version = 1,
    Type = ?OF_V10_MESSAGE_TYPE_ECHO_REQUEST,
    Length = size(ExpectedBodyBin) + ?OF_V10_HEADER_LEN,
    ExpectedHeaderBin = ?OF_V10_HEADER_PATTERN,
    ExpectedResult = [ExpectedHeaderBin, ExpectedBodyBin],
    ?assertEqual(ExpectedResult, ActualResult).

encode_header_test() ->
    Rec = of_v10_test_msgs:header_rec(),
    ActualResult = encode_header(Rec),
    ExpectedResult = of_v10_test_msgs:header_bin(),
    ?assertEqual(ExpectedResult, ActualResult).

encode_body_hello_test() ->
    Rec = of_v10_test_msgs:hello_rec(),
    ActualResult = encode_body(Rec),
    ExpectedResult = {?OF_V10_MESSAGE_TYPE_HELLO, of_v10_test_msgs:hello_bin()},
    ?assertEqual(ExpectedResult, ActualResult).

encode_body_error_test() ->
    Rec = of_v10_test_msgs:error_rec(),
    ActualResult = encode_body(Rec),
    ExpectedResult = {?OF_V10_MESSAGE_TYPE_ERROR, of_v10_test_msgs:error_bin()},
    ?assertEqual(ExpectedResult, ActualResult).

encode_body_echo_request_test() ->
    Rec = of_v10_test_msgs:echo_request_rec(),
    ActualResult = encode_body(Rec),
    ExpectedResult = {?OF_V10_MESSAGE_TYPE_ECHO_REQUEST, of_v10_test_msgs:echo_request_bin()},
    ?assertEqual(ExpectedResult, ActualResult).

encode_body_echo_reply_test() ->
    Rec = of_v10_test_msgs:echo_reply_rec(),
    ActualResult = encode_body(Rec),
    ExpectedResult = {?OF_V10_MESSAGE_TYPE_ECHO_REPLY, of_v10_test_msgs:echo_reply_bin()},
    ?assertEqual(ExpectedResult, ActualResult).

encode_body_vendor_test() ->
    Rec = of_v10_test_msgs:vendor_rec(),
    ActualResult = encode_body(Rec),
    ExpectedResult = {?OF_V10_MESSAGE_TYPE_VENDOR, of_v10_test_msgs:vendor_bin()},
    ?assertEqual(ExpectedResult, ActualResult).

encode_body_features_request_test() ->
    Rec = of_v10_test_msgs:features_request_rec(),
    ActualResult = encode_body(Rec),
    ExpectedResult = {?OF_V10_MESSAGE_TYPE_FEATURES_REQUEST, of_v10_test_msgs:features_request_bin()},
    ?assertEqual(ExpectedResult, ActualResult).

encode_body_features_reply_test() ->
    Rec = of_v10_test_msgs:features_reply_rec(),
    ActualResult = encode_body(Rec),
    ExpectedResult = {?OF_V10_MESSAGE_TYPE_FEATURES_REPLY, of_v10_test_msgs:features_reply_bin()},
    ?assertEqual(ExpectedResult, ActualResult).

encode_body_get_config_request_test() ->
    Rec = of_v10_test_msgs:get_config_request_rec(),
    ActualResult = encode_body(Rec),
    ExpectedResult = {?OF_V10_MESSAGE_TYPE_GET_CONFIG_REQUEST, of_v10_test_msgs:get_config_request_bin()},
    ?assertEqual(ExpectedResult, ActualResult).

encode_body_get_config_reply_test() ->
    Rec = of_v10_test_msgs:get_config_reply_rec(),
    ActualResult = encode_body(Rec),
    ExpectedResult = {?OF_V10_MESSAGE_TYPE_GET_CONFIG_REPLY, of_v10_test_msgs:get_config_reply_bin()},
    ?assertEqual(ExpectedResult, ActualResult).

encode_body_set_config_test() ->
    Rec = of_v10_test_msgs:set_config_rec(),
    ActualResult = encode_body(Rec),
    ExpectedResult = {?OF_V10_MESSAGE_TYPE_SET_CONFIG, of_v10_test_msgs:set_config_bin()},
    ?assertEqual(ExpectedResult, ActualResult).

encode_body_packet_in_test() ->
    Rec = of_v10_test_msgs:packet_in_rec(),
    ActualResult = encode_body(Rec),
    ExpectedResult = {?OF_V10_MESSAGE_TYPE_PACKET_IN, of_v10_test_msgs:packet_in_bin()},
    ?assertEqual(ExpectedResult, ActualResult).

encode_body_flow_removed_test() ->
    Rec = of_v10_test_msgs:flow_removed_rec(),
    ActualResult = encode_body(Rec),
    ExpectedResult = {?OF_V10_MESSAGE_TYPE_FLOW_REMOVED, of_v10_test_msgs:flow_removed_bin()},
    ?assertEqual(ExpectedResult, ActualResult).

encode_body_port_status_test() ->
    Rec = of_v10_test_msgs:port_status_rec(),
    ActualResult = encode_body(Rec),
    ExpectedResult = {?OF_V10_MESSAGE_TYPE_PORT_STATUS, of_v10_test_msgs:port_status_bin()},
    ?assertEqual(ExpectedResult, ActualResult).
