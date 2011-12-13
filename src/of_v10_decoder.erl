%% @author Bruno Rijsman <brunorijsman@hotmail.com>
%% @copyright 2011 Bruno Rijsman
%%

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
         decode_flow_removed/1,
         decode_port_status/1,
         decode_packet_out/1,
         decode_flow_mod/1,
         decode_port_mod/1,
         decode_stats_request/1,
         decode_stats_reply/1,
         decode_barrier_request/1,
         decode_barrier_reply/1,
         decode_queue_get_config_request/1,
         decode_queue_get_config_reply/1]).

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
      actions      = decode_actions_bitmap(Actions),
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

-spec decode_port_status(binary()) -> #of_v10_port_status{}.
decode_port_status(?OF_V10_PORT_STATUS_PATTERN) ->
    DescRec = decode_phy_port(Desc),
    _PortStatus = #of_v10_port_status{
      reason = Reason,
      desc   = DescRec}.

-spec decode_packet_out(binary()) -> #of_v10_packet_out{}.
decode_packet_out(?OF_V10_PACKET_OUT_PATTERN) ->
    ActionRecs = decode_actions(Actions),
    _PacketOut = #of_v10_packet_out{
      buffer_id = BufferId,
      in_port   = InPort,
      actions   = ActionRecs,
      data      = Data
     }.

%% TODO: validate command
-spec decode_flow_mod(binary()) -> #of_v10_flow_mod{}.
decode_flow_mod(?OF_V10_FLOW_MOD_PATTERN) ->
    _FlowMod = #of_v10_flow_mod{
      match         = decode_flow_match(Match),
      cookie        = Cookie,
      command       = Command,
      idle_timeout  = IdleTimeout,
      hard_timeout  = HardTimeout,
      priority      = Priority,
      buffer_id     = BufferId,
      out_port      = OutPort,
      send_flow_rem = (SendFlowRem == 1),
      check_overlap = (CheckOverlap == 1),
      emerg         = (Emerg == 1),
      actions       = decode_actions(Actions)
     }.

-spec decode_port_mod(binary()) -> #of_v10_port_mod{}.
decode_port_mod(?OF_V10_PORT_MOD_PATTERN) ->
    _PortMod = #of_v10_port_mod{
      port_no   = PortNo,
      hw_addr   = HwAddr,
      config    = decode_phy_port_config(Config),
      mask      = decode_phy_port_config(Mask),
      advertise = decode_phy_port_features(Advertise)
     }.

-spec decode_stats_request(binary()) -> #of_v10_stats_request{}.
decode_stats_request(?OF_V10_STATS_REQUEST_PATTERN) ->
    _StatsRequest = #of_v10_stats_request{
      body = decode_stats_request_body(Type, Body)
     }.

-spec decode_stats_reply(binary()) -> #of_v10_stats_reply{}.
decode_stats_reply(?OF_V10_STATS_REPLY_PATTERN) ->
    _StatsReply = #of_v10_stats_reply{
      more = (More == 1),
      body = decode_stats_reply_body(Type, Body)
     }.

-spec decode_barrier_request(binary()) -> #of_v10_barrier_request{}.
decode_barrier_request(?OF_V10_BARRIER_REQUEST_PATTERN) ->
    _BarrierRequest = #of_v10_barrier_request{}.

-spec decode_barrier_reply(binary()) -> #of_v10_barrier_reply{}.
decode_barrier_reply(?OF_V10_BARRIER_REPLY_PATTERN) ->
    _BarrierReply = #of_v10_barrier_reply{}.

-spec decode_queue_get_config_request(binary()) -> #of_v10_queue_get_config_request{}.
decode_queue_get_config_request(?OF_V10_QUEUE_GET_CONFIG_REQUEST_PATTERN) ->
    _QueueGetConfigRequest = #of_v10_queue_get_config_request{port = Port}.

-spec decode_queue_get_config_reply(binary()) -> #of_v10_queue_get_config_reply{}.
decode_queue_get_config_reply(?OF_V10_QUEUE_GET_CONFIG_REPLY_PATTERN) ->
    _QueueGetConfigReply = #of_v10_queue_get_config_reply{
      port = Port,
      queues = decode_queues(Queues)
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

-spec decode_actions_bitmap(binary()) -> #of_v10_actions{}.
decode_actions_bitmap(?OF_V10_ACTIONS_BITMAP_PATTERN) ->
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

-spec decode_actions(binary()) -> [of_v10_action()].
decode_actions(Actions) ->
    decode_actions(Actions, []).
    
-spec decode_actions(binary(), [of_v10_action()]) -> [of_v10_action()].
decode_actions(<<>>, ParsedActions) ->
    lists:reverse(ParsedActions);
decode_actions(?OF_V10_ACTIONS_PATTERN, ParsedActions) ->
    BodyLen = Len-4,
    << Body : BodyLen/binary, NewRest/binary >> = Rest,
    Action = decode_action(Type, Body),
    decode_actions(NewRest, [Action | ParsedActions]).

-spec decode_action(uint16(), uint16()) -> of_v10_action().
decode_action(?OF_V10_ACTION_TYPE_OUTPUT, ?OF_V10_ACTION_OUTPUT_PATTERN) ->
    #of_v10_action_output{port = Port, max_len = MaxLen};
decode_action(?OF_V10_ACTION_TYPE_SET_VLAN_VID, ?OF_V10_ACTION_SET_VLAN_VID_PATTERN) ->
    #of_v10_action_set_vlan_vid{vlan_vid = VlanVid};
decode_action(?OF_V10_ACTION_TYPE_SET_VLAN_PCP, ?OF_V10_ACTION_SET_VLAN_PCP_PATTERN) ->
    #of_v10_action_set_vlan_pcp{vlan_pcp = VlanPcp};
decode_action(?OF_V10_ACTION_TYPE_STRIP_VLAN, ?OF_V10_ACTION_STRIP_VLAN_PATTERN) ->
    #of_v10_action_strip_vlan{};
decode_action(?OF_V10_ACTION_TYPE_SET_DL_SRC, ?OF_V10_ACTION_SET_DL_ADDR_PATTERN) ->
    #of_v10_action_set_dl_src{dl_src = DlAddr};
decode_action(?OF_V10_ACTION_TYPE_SET_DL_DST, ?OF_V10_ACTION_SET_DL_ADDR_PATTERN) ->
    #of_v10_action_set_dl_dst{dl_dst = DlAddr};
decode_action(?OF_V10_ACTION_TYPE_SET_NW_SRC, ?OF_V10_ACTION_SET_NW_ADDR_PATTERN) ->
    #of_v10_action_set_nw_src{nw_src = NwAddr};
decode_action(?OF_V10_ACTION_TYPE_SET_NW_DST, ?OF_V10_ACTION_SET_NW_ADDR_PATTERN) ->
    #of_v10_action_set_nw_dst{nw_dst = NwAddr};
decode_action(?OF_V10_ACTION_TYPE_SET_NW_TOS, ?OF_V10_ACTION_SET_NW_TOS_PATTERN) ->
    #of_v10_action_set_nw_tos{nw_tos = NwTos};
decode_action(?OF_V10_ACTION_TYPE_SET_TP_SRC, ?OF_V10_ACTION_SET_TP_PORT_PATTERN) ->
    #of_v10_action_set_tp_src{tp_src = TpPort};
decode_action(?OF_V10_ACTION_TYPE_SET_TP_DST, ?OF_V10_ACTION_SET_TP_PORT_PATTERN) ->
    #of_v10_action_set_tp_dst{tp_dst = TpPort};
decode_action(?OF_V10_ACTION_TYPE_ENQUEUE, ?OF_V10_ACTION_ENQUEUE_PATTERN) ->
    #of_v10_action_enqueue{port = Port, queue_id = QueueId};
decode_action(?OF_V10_ACTION_TYPE_VENDOR, ?OF_V10_ACTION_VENDOR_PATTERN) ->
    #of_v10_action_vendor{vendor = Vendor}.

-spec decode_stats_request_body(uint8(), binary()) -> of_v10_stats_request_body().
decode_stats_request_body(?OF_V10_STATS_TYPE_DESC, ?OF_V10_DESC_STATS_REQUEST_PATTERN) ->
    #of_v10_desc_stats_request{};
decode_stats_request_body(?OF_V10_STATS_TYPE_FLOW, ?OF_V10_FLOW_STATS_REQUEST_PATTERN) ->
    #of_v10_flow_stats_request{match = decode_flow_match(Match), table_id = TableId, out_port = OutPort};
decode_stats_request_body(?OF_V10_STATS_TYPE_AGGREGATE, ?OF_V10_AGGREGATE_STATS_REQUEST_PATTERN) ->
    #of_v10_aggregate_stats_request{match = decode_flow_match(Match), table_id = TableId, out_port = OutPort};
decode_stats_request_body(?OF_V10_STATS_TYPE_TABLE, ?OF_V10_TABLE_STATS_REQUEST_PATTERN) ->
    #of_v10_table_stats_request{};
decode_stats_request_body(?OF_V10_STATS_TYPE_PORT, ?OF_V10_PORT_STATS_REQUEST_PATTERN) ->
    #of_v10_port_stats_request{port_no = PortNo};
decode_stats_request_body(?OF_V10_STATS_TYPE_QUEUE, ?OF_V10_QUEUE_STATS_REQUEST_PATTERN) ->
    #of_v10_queue_stats_request{port_no = PortNo, queue_id = QueueId};
decode_stats_request_body(?OF_V10_STATS_TYPE_VENDOR, ?OF_V10_VENDOR_STATS_REQUEST_PATTERN) ->
    #of_v10_vendor_stats_request{vendor_id = VendorId, body = Body}.


-spec decode_stats_reply_body(uint8(), binary()) -> of_v10_stats_reply_body().
decode_stats_reply_body(?OF_V10_STATS_TYPE_DESC, ?OF_V10_DESC_STATS_REPLY_PATTERN) ->
    #of_v10_desc_stats_reply{mfr_desc   = decode_string(MfrDesc),
                             hw_desc    = decode_string(HwDesc),
                             sw_desc    = decode_string(SwDesc),
                             serial_num = decode_string(SerialNum),
                             dp_desc    = decode_string(DpDesc)};
decode_stats_reply_body(?OF_V10_STATS_TYPE_FLOW, ?OF_V10_FLOW_STATS_REPLY_PATTERN) ->
    #of_v10_flow_stats_reply{table_id      = TableId,
                             match         = decode_flow_match(Match),
                             duration_sec  = DurationSec,
                             duration_nsec = DurationNsec,
                             priority      = Priority,
                             idle_timeout  = IdleTimeout,
                             hard_timeout  = HardTimeout,
                             cookie        = Cookie,
                             packet_count  = PacketCount,
                             byte_count    = ByteCount,
                             actions       = decode_actions(Actions)};
decode_stats_reply_body(?OF_V10_STATS_TYPE_AGGREGATE, ?OF_V10_AGGREGATE_STATS_REPLY_PATTERN) ->
    #of_v10_aggregate_stats_reply{packet_count = PacketCount,
                                  byte_count   = ByteCount,
                                  flow_count   = FlowCount};
decode_stats_reply_body(?OF_V10_STATS_TYPE_TABLE, ?OF_V10_TABLE_STATS_REPLY_PATTERN) ->
    #of_v10_table_stats_reply{table_id      = TableId,
                              name          = decode_string(Name),
                              wildcards     = decode_flow_match_wildcards(Wildcards),
                              max_entries   = MaxEntries,
                              active_count  = ActiveCount,
                              lookup_count  = LookupCount,
                              matched_count = MatchedCount};
decode_stats_reply_body(?OF_V10_STATS_TYPE_PORT, ?OF_V10_PORT_STATS_REPLY_PATTERN) ->
    #of_v10_port_stats_reply{port_no      = PortNo,
                             rx_packets   = RxPackets,
                             tx_packets   = TxPackets,
                             rx_bytes     = RxBytes,
                             tx_bytes     = TxBytes,
                             rx_dropped   = RxDropped,
                             tx_dropped   = TxDropped,
                             rx_errors    = RxErrors,
                             tx_errors    = TxErrors,
                             rx_frame_err = RxFrameErr,
                             tx_over_err  = TxOverErr,
                             rx_crc_err   = RxCrcErr,
                             collisions   = Collisions};
decode_stats_reply_body(?OF_V10_STATS_TYPE_QUEUE, ?OF_V10_QUEUE_STATS_REPLY_PATTERN) ->
    #of_v10_queue_stats_reply{port_no    = PortNo,
                              queue_id   = QueueId,
                              tx_bytes   = TxBytes,
                              tx_packets = TxPackets,
                              tx_errors  = TxErrors};
decode_stats_reply_body(?OF_V10_STATS_TYPE_VENDOR, ?OF_V10_VENDOR_STATS_REPLY_PATTERN) ->
    #of_v10_vendor_stats_reply{vendor_id = VendorId, 
                               body      = Body}.

%% TODO: make naming conventions consistent with decode_actions
-spec decode_queues(binary()) -> [#of_v10_queue{}].
decode_queues(QueuesBin) ->
    decode_queues(QueuesBin, []).
    
%% TODO: validate Len >= 8
-spec decode_queues(binary(), [#of_v10_queue{}]) -> [#of_v10_queue{}].
decode_queues(<<>>, QueueRecs) ->
    lists:reverse(QueueRecs);
decode_queues(?OF_V10_QUEUES_PATTERN, QueueRecs) ->
    PropertiesLen = Len - 8,
    << Properties : PropertiesLen/binary, NewRest/binary >> = Rest,
    PropertyRecs = decode_queue_properties(Properties),
    QueueRec = #of_v10_queue{queue_id = QueueId, properties = PropertyRecs},
    decode_queues(NewRest, [QueueRec | QueueRecs]).

-spec decode_queue_properties(binary()) -> [of_v10_queue_property()].
decode_queue_properties(PropertiesBin) ->
    decode_queue_properties(PropertiesBin, []).

-spec decode_queue_properties(binary(), [of_v10_queue_property()]) -> [of_v10_queue_property()].
decode_queue_properties(<<>>, PropertyRecs) ->
    lists:reverse(PropertyRecs);
decode_queue_properties(?OF_V10_QUEUE_PROPERTIES_PATTERN, PropertyRecs) ->
    PropertyLen = Len - 8,
    << PropertyBin : PropertyLen/binary, NewRest/binary >> = Rest,
    PropertyRec = decode_queue_property(Type, PropertyBin),
    case PropertyRec of
        none ->
            decode_queue_properties(NewRest, PropertyRecs);
        _ ->
            decode_queue_properties(NewRest, [PropertyRec | PropertyRecs])
    end.

-spec decode_queue_property(of_v10_queue_property_type(), binary()) -> of_v10_queue_property() | none.
decode_queue_property(?OF_V10_QUEUE_PROPERTY_TYPE_NONE, ?OF_V10_QUEUE_PROPERTY_NONE_PATTERN) ->
    none;
decode_queue_property(?OF_V10_QUEUE_PROPERTY_TYPE_MIN_RATE, ?OF_V10_QUEUE_PROPERTY_MIN_RATE_PATTERN) ->
    #of_v10_queue_property_min_rate{rate = Rate}.

%%
%% Unit tests.
%%

decode_string_no_trailing_zero_test() ->
    Bin = << "hello" >>,
    ActualStr = decode_string(Bin),
    ExpectedStr = "hello",
    ?assertEqual(ExpectedStr, ActualStr).

decode_string_trailing_zero_test() ->
    Bin = << "hello", 0, 1, 2, 3 >>,
    ActualStr = decode_string(Bin),
    ExpectedStr = "hello",
    ?assertEqual(ExpectedStr, ActualStr).

decode_string_only_zero_test() ->
    Bin = << 0 >>,
    ActualStr = decode_string(Bin),
    ExpectedStr = "",
    ?assertEqual(ExpectedStr, ActualStr).

decode_string_empty_test() ->
    Bin = << >>,
    ActualStr = decode_string(Bin),
    ExpectedStr = "",
    ?assertEqual(ExpectedStr, ActualStr).

decode_header_test() ->
    Bin = of_v10_test_msgs:header_bin(),
    ActualRec = decode_header(Bin),
    ExpectedRec = of_v10_test_msgs:header_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

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
    ?assertEqual(ExpectedRec, ActualRec).

decode_hello_with_extension_test() ->
    Bin = of_v10_test_msgs:hello_with_extension_bin(),
    ActualRec = decode_hello(Bin),
    ExpectedRec = of_v10_test_msgs:hello_with_extension_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_error_test() ->
    Bin = of_v10_test_msgs:error_bin(),
    ActualRec = decode_error(Bin),
    ExpectedRec = of_v10_test_msgs:error_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_error_with_data_test() ->
    Bin = of_v10_test_msgs:error_with_data_bin(),
    ActualRec = decode_error(Bin),
    ExpectedRec = of_v10_test_msgs:error_with_data_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_echo_request_test() ->
    Bin = of_v10_test_msgs:echo_request_bin(),
    ActualRec = decode_echo_request(Bin),
    ExpectedRec = of_v10_test_msgs:echo_request_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_echo_request_with_data_test() ->
    Bin = of_v10_test_msgs:echo_request_with_data_bin(),
    ActualRec = decode_echo_request(Bin),
    ExpectedRec = of_v10_test_msgs:echo_request_with_data_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_echo_reply_test() ->
    Bin = of_v10_test_msgs:echo_reply_bin(),
    ActualRec = decode_echo_reply(Bin),
    ExpectedRec = of_v10_test_msgs:echo_reply_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_echo_reply_with_data_test() ->
    Bin = of_v10_test_msgs:echo_reply_with_data_bin(),
    ActualRec = decode_echo_reply(Bin),
    ExpectedRec = of_v10_test_msgs:echo_reply_with_data_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_vendor_test() ->
    Bin = of_v10_test_msgs:vendor_bin(),
    ActualRec = decode_vendor(Bin),
    ExpectedRec = of_v10_test_msgs:vendor_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_vendor_with_data_test() ->
    Bin = of_v10_test_msgs:vendor_with_data_bin(),
    ActualRec = decode_vendor(Bin),
    ExpectedRec = of_v10_test_msgs:vendor_with_data_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_features_request_test() ->
    Bin = of_v10_test_msgs:features_request_bin(),
    ActualRec = decode_features_request(Bin),
    ExpectedRec = of_v10_test_msgs:features_request_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_features_reply_test() ->
    Bin = of_v10_test_msgs:features_reply_bin(),
    ActualRec = decode_features_reply(Bin),
    ExpectedRec = of_v10_test_msgs:features_reply_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_get_config_request_test() ->
    Bin = of_v10_test_msgs:get_config_request_bin(),
    ActualRec = decode_get_config_request(Bin),
    ExpectedRec = of_v10_test_msgs:get_config_request_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_get_config_reply_test() ->
    Bin = of_v10_test_msgs:get_config_reply_bin(),
    ActualRec = decode_get_config_reply(Bin),
    ExpectedRec = of_v10_test_msgs:get_config_reply_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_set_config_test() ->
    Bin = of_v10_test_msgs:set_config_bin(),
    ActualRec = decode_set_config(Bin),
    ExpectedRec = of_v10_test_msgs:set_config_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_packet_in_test() ->
    Bin = of_v10_test_msgs:packet_in_bin(),
    ActualRec = decode_packet_in(Bin),
    ExpectedRec = of_v10_test_msgs:packet_in_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_flow_removed_test() ->
    Bin = of_v10_test_msgs:flow_removed_bin(),
    ActualRec = decode_flow_removed(Bin),
    ExpectedRec = of_v10_test_msgs:flow_removed_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_port_status_test() ->
    Bin = of_v10_test_msgs:port_status_bin(),
    ActualRec = decode_port_status(Bin),
    ExpectedRec = of_v10_test_msgs:port_status_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_packet_out_no_actions_no_data_test() ->
    Bin = of_v10_test_msgs:packet_out_no_actions_no_data_bin(),
    ActualRec = decode_packet_out(Bin),
    ExpectedRec = of_v10_test_msgs:packet_out_no_actions_no_data_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_packet_out_no_actions_data_test() ->
    Bin = of_v10_test_msgs:packet_out_no_actions_data_bin(),
    ActualRec = decode_packet_out(Bin),
    ExpectedRec = of_v10_test_msgs:packet_out_no_actions_data_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_packet_out_action_output_test() ->
    Bin = of_v10_test_msgs:packet_out_action_output_bin(),
    ActualRec = decode_packet_out(Bin),
    ExpectedRec = of_v10_test_msgs:packet_out_action_output_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_packet_out_action_set_vlan_vid_test() ->
    Bin = of_v10_test_msgs:packet_out_action_set_vlan_vid_bin(),
    ActualRec = decode_packet_out(Bin),
    ExpectedRec = of_v10_test_msgs:packet_out_action_set_vlan_vid_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_packet_out_action_set_vlan_pcp_test() ->
    Bin = of_v10_test_msgs:packet_out_action_set_vlan_pcp_bin(),
    ActualRec = decode_packet_out(Bin),
    ExpectedRec = of_v10_test_msgs:packet_out_action_set_vlan_pcp_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_packet_out_action_stip_vlan_test() ->
    Bin = of_v10_test_msgs:packet_out_action_strip_vlan_bin(),
    ActualRec = decode_packet_out(Bin),
    ExpectedRec = of_v10_test_msgs:packet_out_action_strip_vlan_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_packet_out_action_set_dl_src_test() ->
    Bin = of_v10_test_msgs:packet_out_action_set_dl_src_bin(),
    ActualRec = decode_packet_out(Bin),
    ExpectedRec = of_v10_test_msgs:packet_out_action_set_dl_src_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_packet_out_action_set_dl_dst_test() ->
    Bin = of_v10_test_msgs:packet_out_action_set_dl_dst_bin(),
    ActualRec = decode_packet_out(Bin),
    ExpectedRec = of_v10_test_msgs:packet_out_action_set_dl_dst_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_packet_out_action_set_nw_src_test() ->
    Bin = of_v10_test_msgs:packet_out_action_set_nw_src_bin(),
    ActualRec = decode_packet_out(Bin),
    ExpectedRec = of_v10_test_msgs:packet_out_action_set_nw_src_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_packet_out_action_set_nw_dst_test() ->
    Bin = of_v10_test_msgs:packet_out_action_set_nw_dst_bin(),
    ActualRec = decode_packet_out(Bin),
    ExpectedRec = of_v10_test_msgs:packet_out_action_set_nw_dst_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_packet_out_action_set_nw_tos_test() ->
    Bin = of_v10_test_msgs:packet_out_action_set_nw_tos_bin(),
    ActualRec = decode_packet_out(Bin),
    ExpectedRec = of_v10_test_msgs:packet_out_action_set_nw_tos_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_packet_out_action_set_tp_src_test() ->
    Bin = of_v10_test_msgs:packet_out_action_set_tp_src_bin(),
    ActualRec = decode_packet_out(Bin),
    ExpectedRec = of_v10_test_msgs:packet_out_action_set_tp_src_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_packet_out_action_set_tp_dst_test() ->
    Bin = of_v10_test_msgs:packet_out_action_set_tp_dst_bin(),
    ActualRec = decode_packet_out(Bin),
    ExpectedRec = of_v10_test_msgs:packet_out_action_set_tp_dst_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_packet_out_action_enqueue_test() ->
    Bin = of_v10_test_msgs:packet_out_action_enqueue_bin(),
    ActualRec = decode_packet_out(Bin),
    ExpectedRec = of_v10_test_msgs:packet_out_action_enqueue_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_packet_out_action_vendor_test() ->
    Bin = of_v10_test_msgs:packet_out_action_vendor_bin(),
    ActualRec = decode_packet_out(Bin),
    ExpectedRec = of_v10_test_msgs:packet_out_action_vendor_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_packet_out_multiple_actions_no_data_test() ->
    Bin = of_v10_test_msgs:packet_out_multiple_actions_no_data_bin(),
    ActualRec = decode_packet_out(Bin),
    ExpectedRec = of_v10_test_msgs:packet_out_multiple_actions_no_data_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_packet_out_multiple_actions_data_test() ->
    Bin = of_v10_test_msgs:packet_out_multiple_actions_data_bin(),
    ActualRec = decode_packet_out(Bin),
    ExpectedRec = of_v10_test_msgs:packet_out_multiple_actions_data_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_flow_mod_test() ->
    Bin = of_v10_test_msgs:flow_mod_bin(),
    ActualRec = decode_flow_mod(Bin),
    ExpectedRec = of_v10_test_msgs:flow_mod_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_port_mod_test() ->
    Bin = of_v10_test_msgs:port_mod_bin(),
    ActualRec = decode_port_mod(Bin),
    ExpectedRec = of_v10_test_msgs:port_mod_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_stats_request_desc_test() ->
    Bin = of_v10_test_msgs:stats_request_desc_bin(),
    ActualRec = decode_stats_request(Bin),
    ExpectedRec = of_v10_test_msgs:stats_request_desc_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_stats_request_flow_test() ->
    Bin = of_v10_test_msgs:stats_request_flow_bin(),
    ActualRec = decode_stats_request(Bin),
    ExpectedRec = of_v10_test_msgs:stats_request_flow_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_stats_request_aggregate_test() ->
    Bin = of_v10_test_msgs:stats_request_aggregate_bin(),
    ActualRec = decode_stats_request(Bin),
    ExpectedRec = of_v10_test_msgs:stats_request_aggregate_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_stats_request_table_test() ->
    Bin = of_v10_test_msgs:stats_request_table_bin(),
    ActualRec = decode_stats_request(Bin),
    ExpectedRec = of_v10_test_msgs:stats_request_table_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_stats_request_port_test() ->
    Bin = of_v10_test_msgs:stats_request_port_bin(),
    ActualRec = decode_stats_request(Bin),
    ExpectedRec = of_v10_test_msgs:stats_request_port_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_stats_request_queue_test() ->
    Bin = of_v10_test_msgs:stats_request_queue_bin(),
    ActualRec = decode_stats_request(Bin),
    ExpectedRec = of_v10_test_msgs:stats_request_queue_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_stats_request_vendor_test() ->
    Bin = of_v10_test_msgs:stats_request_vendor_bin(),
    ActualRec = decode_stats_request(Bin),
    ExpectedRec = of_v10_test_msgs:stats_request_vendor_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_stats_reply_desc_test() ->
    Bin = of_v10_test_msgs:stats_reply_desc_bin(),
    ActualRec = decode_stats_reply(Bin),
    ExpectedRec = of_v10_test_msgs:stats_reply_desc_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_stats_reply_flow_test() ->
    Bin = of_v10_test_msgs:stats_reply_flow_bin(),
    ActualRec = decode_stats_reply(Bin),
    ExpectedRec = of_v10_test_msgs:stats_reply_flow_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_stats_reply_aggregate_test() ->
    Bin = of_v10_test_msgs:stats_reply_aggregate_bin(),
    ActualRec = decode_stats_reply(Bin),
    ExpectedRec = of_v10_test_msgs:stats_reply_aggregate_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_stats_reply_table_test() ->
    Bin = of_v10_test_msgs:stats_reply_table_bin(),
    ActualRec = decode_stats_reply(Bin),
    ExpectedRec = of_v10_test_msgs:stats_reply_table_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

decode_stats_reply_port_test() ->
    Bin = of_v10_test_msgs:stats_reply_port_bin(),
    ActualRec = decode_stats_reply(Bin),
    ExpectedRec = of_v10_test_msgs:stats_reply_port_rec(),
    ?assertEqual(ExpectedRec, ActualRec).

%% decode_stats_reply_queue_test() ->
%%     Bin = of_v10_test_msgs:stats_reply_queue_bin(),
%%     ActualRec = decode_stats_reply(Bin),
%%     ExpectedRec = of_v10_test_msgs:stats_reply_queue_rec(),
%%     ?assertEqual(ExpectedRec, ActualRec).

%% decode_stats_reply_vendor_test() ->
%%     Bin = of_v10_test_msgs:stats_reply_vendor_bin(),
%%     ActualRec = decode_stats_reply(Bin),
%%     ExpectedRec = of_v10_test_msgs:stats_reply_vendor_rec(),
%%     ?assertEqual(ExpectedRec, ActualRec).
