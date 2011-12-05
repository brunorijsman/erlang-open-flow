-ifndef(OF_V10_HRL).
-define(OF_V10_HRL, true).

%% TODO: Fix the include paths
-include_lib("../include/of_types.hrl").

%% Protocol version
-define(OF_V10_VERSION, 1).

%% Message type
-define(OF_V10_MESSAGE_TYPE_MIN,                      0).
-define(OF_V10_MESSAGE_TYPE_MAX,                      21).
-define(OF_V10_MESSAGE_TYPE_HELLO,                    0).
-define(OF_V10_MESSAGE_TYPE_ERROR,                    1).
-define(OF_V10_MESSAGE_TYPE_ECHO_REQUEST,             2).
-define(OF_V10_MESSAGE_TYPE_ECHO_REPLY,               3).
-define(OF_V10_MESSAGE_TYPE_VENDOR,                   4).
-define(OF_V10_MESSAGE_TYPE_FEATURES_REQUEST,         5).
-define(OF_V10_MESSAGE_TYPE_FEATURES_REPLY,           6).
-define(OF_V10_MESSAGE_TYPE_GET_CONFIG_REQUEST,       7).
-define(OF_V10_MESSAGE_TYPE_GET_CONFIG_REPLY,         8).
-define(OF_V10_MESSAGE_TYPE_SET_CONFIG,               9).
-define(OF_V10_MESSAGE_TYPE_PACKET_IN,                10).
-define(OF_V10_MESSAGE_TYPE_FLOW_REMOVED,             11).
-define(OF_V10_MESSAGE_TYPE_PORT_STATUS,              12).
-define(OF_V10_MESSAGE_TYPE_PACKET_OUT,               13).
-define(OF_V10_MESSAGE_TYPE_FLOW_MOD,                 14).
-define(OF_V10_MESSAGE_TYPE_PORT_MOD,                 15).
-define(OF_V10_MESSAGE_TYPE_STATS_REQUEST,            16).
-define(OF_V10_MESSAGE_TYPE_STATS_REPLY,              17).
-define(OF_V10_MESSAGE_TYPE_BARRIER_REQUEST,          18).
-define(OF_V10_MESSAGE_TYPE_BARRIER_REPLY,            19).
-define(OF_V10_MESSAGE_TYPE_QUEUE_GET_CONFIG_REQUEST, 20).
-define(OF_V10_MESSAGE_TYPE_QUEUE_GET_CONFIG_REPLY,   21).

%% Error types
-define(OF_V10_ERROR_TYPE_MIN,                  0).
-define(OF_V10_ERROR_TYPE_MAX,                  5).
-define(OF_V10_ERROR_TYPE_HELLO_FAILED,         0).
-define(OF_V10_ERROR_TYPE_BAD_REQUEST,          1).
-define(OF_V10_ERROR_TYPE_BAD_ACTION,           2).
-define(OF_V10_ERROR_TYPE_FLOW_MOD_FAILED,      3).
-define(OF_V10_ERROR_TYPE_PORT_MOD_FAILED,      4).
-define(OF_V10_ERROR_TYPE_QUEUE_OP_FAILED,      5).

%% Error codes
-define(OF_V10_ERROR_CODE_MIN, 0).
-define(OF_V10_ERROR_CODE_MAX, 8).

-define(OF_V10_ERROR_CODE_HELLO_FAILED_INCOMPATIBLE, 0).
-define(OF_V10_ERROR_CODE_HELLO_FAILED_EPERM,        1).

-define(OF_V10_ERROR_CODE_BAD_REQUEST_BAD_VERSION,    0).
-define(OF_V10_ERROR_CODE_BAD_REQUEST_BAD_TYPE,       1).
-define(OF_V10_ERROR_CODE_BAD_REQUEST_BAD_STAT,       2).
-define(OF_V10_ERROR_CODE_BAD_REQUEST_BAD_VENDOR,     3).
-define(OF_V10_ERROR_CODE_BAD_REQUEST_BAD_SUBTYPE,    4).
-define(OF_V10_ERROR_CODE_BAD_REQUEST_EPERM,          5).
-define(OF_V10_ERROR_CODE_BAD_REQUEST_BAD_LEN,        6).
-define(OF_V10_ERROR_CODE_BAD_REQUEST_BUFFER_EMPTY,   7).
-define(OF_V10_ERROR_CODE_BAD_REQUEST_BUFFER_UNKNOWN, 8).

-define(OF_V10_ERROR_CODE_BAD_ACTION_BAD_TYPE,        0).
-define(OF_V10_ERROR_CODE_BAD_ACTION_BAD_LEN,         1).
-define(OF_V10_ERROR_CODE_BAD_ACTION_BAD_VENDOR,      2).
-define(OF_V10_ERROR_CODE_BAD_ACTION_BAD_VENDOR_TYPE, 3).
-define(OF_V10_ERROR_CODE_BAD_ACTION_BAD_OUT_PORT,    4).
-define(OF_V10_ERROR_CODE_BAD_ACTION_BAD_ARGUMENT,    5).
-define(OF_V10_ERROR_CODE_BAD_ACTION_EPERM,           6).
-define(OF_V10_ERROR_CODE_BAD_ACTION_TOO_MANY,        7).
-define(OF_V10_ERROR_CODE_BAD_ACTION_BAD_QUEUE,       8).

-define(OF_V10_ERROR_CODE_FLOW_MOD_FAILED_ALL_TABLES_FULL,   0).
-define(OF_V10_ERROR_CODE_FLOW_MOD_FAILED_OVERLAP,           1).
-define(OF_V10_ERROR_CODE_FLOW_MOD_FAILED_EPERM,             2).
-define(OF_V10_ERROR_CODE_FLOW_MOD_FAILED_BAD_EMERG_TIMEOUT, 3).
-define(OF_V10_ERROR_CODE_FLOW_MOD_FAILED_BAD_COMMAND,       4).
-define(OF_V10_ERROR_CODE_FLOW_MOD_FAILED_UNSUPPROTED,       5).

-define(OF_V10_ERROR_CODE_PORT_MOD_FAILED_BAD_PORT,      0).
-define(OF_V10_ERROR_CODE_PORT_MOD_FAILED_BAD_HW_ADDR,   1).

-define(OF_V10_ERROR_CODE_QUEUE_OP_FAILED_BAD_PORT,  0).
-define(OF_V10_ERROR_CODE_QUEUE_OP_FAILED_BAD_QUEUE, 1).
-define(OF_V10_ERROR_CODE_QUEUE_OP_FAILED_EPERM,     2).

%% Port numbers
-define(OF_V10_PORT_NO_MAX,        0xff00).
-define(OF_V10_PORT_NO_IN,         0xfff8).
-define(OF_V10_PORT_NO_TABLE,      0xfff9).
-define(OF_V10_PORT_NO_NORMAL,     0xfffa).
-define(OF_V10_PORT_NO_FLOOD,      0xfffb).
-define(OF_V10_PORT_NO_ALL,        0xfffc).
-define(OF_V10_PORT_NO_CONTROLLER, 0xfffd).
-define(OF_V10_PORT_NO_LOCAL,      0xfffe).
-define(OF_V10_PORT_NO_NONE,       0xffff).

%% Queue property types
-define(OF_V10_QUEUE_PROPERTY_TYPE_NONE,     0).
-define(OF_V10_QUEUE_PROPERTY_TYPE_MIN_RATE, 1).

%% STP port states
-define(OF_V10_STP_PORT_STATE_MIN,     0).
-define(OF_V10_STP_PORT_STATE_MAX,     3).
-define(OF_V10_STP_PORT_STATE_LISTEN,  0).
-define(OF_V10_STP_PORT_STATE_LEARN,   1).
-define(OF_V10_STP_PORT_STATE_FORWARD, 2).
-define(OF_V10_STP_PORT_STATE_BLOCK,   3).

%% Fragment handling
-define(OF_V10_FRAG_HANDLING_MIN,    0).
-define(OF_V10_FRAG_HANDLING_MAX,    2).
-define(OF_V10_FRAG_HANDLING_NORMAL, 0).
-define(OF_V10_FRAG_HANDLING_DROP,   1).
-define(OF_V10_FRAG_HANDLING_REASM,  2).

%% Packet in reasons
-define(OF_V10_PACKET_IN_REASON_MIN,      0).
-define(OF_V10_PACKET_IN_REASON_MAX,      1).
-define(OF_V10_PACKET_IN_REASON_NO_MATCH, 0).
-define(OF_V10_PACKET_IN_REASON_ACTION,   1).

%% Flow removed reasons
-define(OF_V10_FLOW_REMOVED_REASON_MIN,          0).
-define(OF_V10_FLOW_REMOVED_REASON_MAX,          2).
-define(OF_V10_FLOW_REMOVED_REASON_IDLE_TIMEOUT, 0).
-define(OF_V10_FLOW_REMOVED_REASON_HARD_TIMEOUT, 1).
-define(OF_V10_FLOW_REMOVED_REASON_DELETE,       2).

%% Port status reasons
-define(OF_V10_PORT_STATUS_REASON_MIN,    0).
-define(OF_V10_PORT_STATUS_REASON_MAX,    2).
-define(OF_V10_PORT_STATUS_REASON_ADD,    0).
-define(OF_V10_PORT_STATUS_REASON_DELETE, 1).
-define(OF_V10_PORT_STATUS_REASON_MODIFY, 2).

%% Maximum length of hardware address
-define(OF_V10_ETH_ALEN, 6).

%% Maximum length of a port name (inluding terminating null character)
-define(OF_V10_MAX_PORT_NAME_LEN, 16).

-define(OF_V10_HEADER_LEN, 8).

-define(OF_V10_HEADER_PATTERN,
        << Version : 8,
           Type    : 8,
           Length  : 16,
           Xid     : 32 >>).

-define(OF_V10_PHY_PORT_CONFIG_PATTERN,
        << _Reserved  : 25,
           NoPacketIn : 1,
           NoFwd      : 1,
           NoFlood    : 1,
           NoRecvStp  : 1,
           NoRecv     : 1,
           NoStp      : 1,
           PortDown   : 1 >>).

-define(OF_V10_PHY_PORT_STATE_PATTERN,
        << _Reserved1   : 22,
           StpPortState : 2,
           _Reserved2   : 7,
           LinkDown     : 1 >>).

-define(OF_V10_PHY_PORT_FEATURES_PATTERN,
        << _Reserved         : 20,
           PauseAsymetric    : 1,
           Pause             : 1,
           AutoNegotiation   : 1,
           FiberMedium       : 1,
           CopperMedium      : 1,
           FullDuplex10Gbps  : 1,
           FullDuplex1Gbps   : 1,
           HalfDuplex1Gbps   : 1,
           FullDuplex100Mbps : 1,
           HalfDuplex100Mbps : 1,
           FullDuplex10Mbps  : 1,
           HalfDuplex10Mbps  : 1 >>).

-define(OF_V10_PHY_PORT_PATTERN,
        << PortNo             : 16,
           HwAddr             : ?OF_V10_ETH_ALEN/binary-unit:8,
           Name               : ?OF_V10_MAX_PORT_NAME_LEN/binary-unit:8,
           Config             : 4/binary,
           State              : 4/binary,
           CurrentFeatures    : 4/binary,
           AdvertisedFeatures : 4/binary,
           SupportedFeatures  : 4/binary,
           PeerFeatures       : 4/binary >>.

-define(OF_V10_QUEUE_PATTERN,
        << QueueId : 32,
           Length  : 16,
           _Pad    : 16,
           QueueProperties/binary >>).

-define(OF_V10_QUEUE_PROPERTY_HEADER_PATTERN,
        << Type   : 16,
           Length : 16,
           _Pad   : 32 >>).

-define(OF_V10_QUEUE_PROPERTY_MIN_RATE_PATTERN,
        << RateInTenthPercent : 16,
           _Pad               : 48 >>).

-define(OF_V10_CAPABILITIES_PATTERN, 
        << _Reserved1 : 24,
           ArpMatchIp : 1,
           QueueStats : 1,
           IpReasm    : 1,
           _Reserved2 : 1,
           Stp        : 1,
           PortStats  : 1,
           TableStats : 1,
           FlowStats  : 1 >>).

-define(OF_V10_ACTIONS_BITMAP_PATTERN, 
        << _Reserved  : 20,
           Enqueue    : 1,
           SetTpDst   : 1,
           SetTpSrc   : 1,
           SetNwTos   : 1,
           SetNwDst   : 1,
           SetNwSrc   : 1,
           SetDlDst   : 1,
           SetDlSrc   : 1,
           StripVlan  : 1,
           SetVlanPcp : 1,
           SetVlanId  : 1,
           Output     : 1 >>).

-define(OF_V10_SWITCH_CONFIG_FLAGS_PATTERN,
        << _Reserved    : 14,
           FragHandling : 2 >>).

-define(OF_V10_SWITCH_CONFIG_PATTERN, 
        << Flags       : 2/binary,
           MissSendLen : 16 >>).

-define(OF_V10_HELLO_PATTERN,
        << _FutureExtension/binary >>).

-define(OF_V10_ERROR_PATTERN,
        << Type : 16,
           Code : 16,
           Data/binary >>).

-define(OF_V10_ECHO_REQUEST_PATTERN,
        << Data/binary >>).

-define(OF_V10_ECHO_REPLY_PATTERN,
        << Data/binary >>).

-define(OF_V10_VENDOR_PATTERN,
        << VendorId : 32,
           Data/binary >>).

-define(OF_V10_FEATURES_REQUEST_PATTERN, << >>).

-define(OF_V10_FEATURES_REPLY_PATTERN,
        << DataPathId   : 64,
           NBuffers     : 32,
           NTables      : 8,
           _Pad1        : 24,
           Capabilities : 4/binary,
           Actions      : 4/binary,
           Ports/binary >>).

-define(OF_V10_GET_CONFIG_REQUEST_PATTERN, << >>).

-define(OF_V10_GET_CONFIG_REPLY_PATTERN,
        << SwitchConfig/binary >>).

-define(OF_V10_SET_CONFIG_PATTERN,
        << SwitchConfig/binary >>).

-define(OF_V10_PACKET_IN_PATTERN,
        << BufferId : 32,
           TotalLen : 16,
           InPort   : 16,
           Reason   : 8,
           _Pad     : 8,
           Data/binary >>).

-define(OF_V10_FLOW_MATCH_WILDCARDS_PATTERN,
        << _Reserved : 10,
           NwTos     : 1,
           DlVlanPcp : 1,
           NwDstBits : 6,
           NwSrcBits : 6,
           TpDst     : 1,
           TpSrc     : 1,
           NwProto   : 1,
           DlType    : 1,
           DlDst     : 1,
           DlSrc     : 1,
           DlVlan    : 1,
           InPort    : 1 >>).

-define(OF_V10_FLOW_MATCH_PATTERN,
        << Wildcards    : 4/binary,
           InPort       : 32,
           DlSrc         : ?OF_V10_ETH_ALEN/binary-unit:8,
           DlDst        : ?OF_V10_ETH_ALEN/binary-unit:8,
           DlVlan       : 16,
           DlVlanPcp    : 8,
           _Pad1        : 8,
           DlType       : 16,
           NwTos        : 8,
           NwProto      : 8,
           _Pad2        : 16,
           NwSrc        : 32,
           NwDst        : 32,
           TpSrc        : 16,
           TpDst        : 16 >>).

-define(OF_V10_FLOW_REMOVED_PATTERN,
        << Match        : 10/binary,
           Cookie       : 64,
           Priority     : 16,
           Reason       : 8,
           _Pad1        : 8,
           DurationSec  : 32,
           DurationNsec : 32,
           IdleTimeout  : 16,
           _Pad2        : 16,
           PacketCount  : 64,
           ByteCount    : 64 >>.

-define(OF_V10_PORT_STATUS_PATTERN,
        << Reason : 8,
           _Pad   : 56,
           Desc   : 6/binary >>.

-type of_v10_version() :: ?OF_V10_VERSION.

-type of_v10_message_type() :: ?OF_V10_MESSAGE_TYPE_MIN..?OF_V10_MESSAGE_TYPE_MAX.

-type of_v10_error_type() :: ?OF_V10_ERROR_TYPE_MIN..?OF_V10_ERROR_TYPE_MAX.

-type of_v10_error_code() :: ?OF_V10_ERROR_CODE_MIN..?OF_V10_ERROR_CODE_MAX.

-type of_v10_error_data() :: binary().

-type of_v10_stp_port_state() :: ?OF_V10_STP_PORT_STATE_MIN..?OF_V10_STP_PORT_STATE_MAX.

-type of_v10_frag_handling() :: ?OF_V10_FRAG_HANDLING_MIN..?OF_V10_FRAG_HANDLING_MAX.

-type of_v10_packet_in_reason() :: ?OF_V10_PACKET_IN_REASON_MIN..?OF_V10_PACKET_IN_REASON_MAX.

-type of_v10_flow_removed_reason() :: ?OF_V10_FLOW_REMOVED_REASON_MIN..?OF_V10_FLOW_REMOVED_REASON_MAX.

-type of_v10_port_status_reason() :: ?OF_V10_PORT_STATUS_REASON_MIN..?OF_V10_PORT_STATUS_REASON_MAX.

-record(of_v10_header, {
          version :: of_v10_version(),
          type    :: of_v10_message_type(),
          length  :: uint16(),
          xid     :: of_xid() }). 

-record(of_v10_hello, {}).

-record(of_v10_error, {
          type :: uint16(),   %% Accept unrecognized types
          code :: uint16(),   %% Accept unrecognized codes
          data :: binary() }).

-record(of_v10_echo_request, {
          data :: binary() }).

-record(of_v10_echo_reply, {
          data :: binary() }).

-record(of_v10_vendor, {
          vendor_id :: uint32(),
          data      :: binary() }).

-record(of_v10_features_request, {}).

-record(of_v10_phy_port_config, {
          port_down    :: boolean(),
          no_stp       :: boolean(),
          no_recv      :: boolean(),
          no_recv_stp  :: boolean(),
          no_flood     :: boolean(),
          no_fwd       :: boolean(),
          no_packet_in :: boolean() }).

-record(of_v10_phy_port_state, {
          link_down      :: boolean(),
          stp_port_state :: of_v10_stp_port_state() }).

-record(of_v10_phy_port_features, {
          half_duplex_10_mbps  :: boolean(),
          full_duplex_10_mbps  :: boolean(),
          half_duplex_100_mbps :: boolean(),
          full_duplex_100_mbps :: boolean(),
          half_duplex_1_gbps   :: boolean(),
          full_duplex_1_gbps   :: boolean(),
          full_duplex_10_gbps  :: boolean(),
          copper_medium        :: boolean(),
          fiber_medium         :: boolean(),
          auto_negotiation     :: boolean(),
          pause                :: boolean(),
          pause_asymetric      :: boolean() }).

-record(of_v10_phy_port, {
          port_no             :: uint16(),
          hw_addr             :: of_hw_addr(),
          name                :: string(),
          config              :: #of_v10_phy_port_config{},
          state               :: #of_v10_phy_port_state{},
          current_features    :: #of_v10_phy_port_features{},
          advertised_features :: #of_v10_phy_port_features{},
          supported_features  :: #of_v10_phy_port_features{},
          peer_features       :: #of_v10_phy_port_features{} }).

-record(of_v10_capabilities, {
          flow_stats   :: boolean(),
          table_stats  :: boolean(),
          port_stats   :: boolean(),
          stp          :: boolean(),
          ip_reasm     :: boolean(),
          queue_stats  :: boolean(),
          arp_match_ip :: boolean() }).

-record(of_v10_actions, {
          output       :: boolean(),
          set_vlan_id  :: boolean(),
          set_vlan_pcp :: boolean(),
          strip_vlan   :: boolean(),
          set_dl_src   :: boolean(),
          set_dl_dst   :: boolean(),
          set_nw_src   :: boolean(),
          set_nw_dst   :: boolean(),
          set_nw_tos   :: boolean(),
          set_tp_src   :: boolean(),
          set_tp_dst   :: boolean(),
          enqueue      :: boolean() }).

-record(of_v10_features_reply, {
          data_path_id :: uint64(),
          n_buffers    :: uint32(),
          n_tables     :: uint8(),
          capabilities :: #of_v10_capabilities{},
          actions      :: #of_v10_actions{},
          ports        :: [#of_v10_phy_port{}] }).

-record(of_v10_get_config_request, {}).

-record(of_v10_switch_config, {
          frag_handling :: of_v10_frag_handling(),
          miss_send_len :: uint16() }).

-record(of_v10_get_config_reply, {
          switch_config :: #of_v10_switch_config{} }).

-record(of_v10_set_config, {
          switch_config :: #of_v10_switch_config{} }).

-record(of_v10_packet_in, {
          buffer_id :: uint32(),
          total_len :: uint16(),
          in_port   :: uint16(),
          reason    :: of_v10_packet_in_reason(),
          data      :: binary() }).


-record(of_v10_flow_match_wildcards, {
          in_port     :: boolean(),
          dl_vlan     :: boolean(),
          dl_src      :: boolean(),
          dl_dst      :: boolean(),
          dl_type     :: boolean(),
          nw_proto    :: boolean(),
          tp_src      :: boolean(),
          tp_dst      :: boolean(),
          nw_src_bits :: 0..63,
          nw_dst_bits :: 0..63,
          dl_vlan_pcp :: boolean(),
          nw_tos      :: boolean() }).

-record(of_v10_flow_match, {
          wildcards   :: #of_v10_flow_match_wildcards{},
          in_port     :: uint16(),
          dl_src      :: of_hw_addr(),
          dl_dst      :: of_hw_addr(),
          dl_vlan     :: uint16(),
          dl_vlan_pcp :: uint8(),
          dl_type     :: uint16(),
          nw_tos      :: uint8(),
          nw_proto    :: uint8(),
          nw_src      :: uint32(),
          nw_dst      :: uint32(),
          tp_src      :: uint16(),
          tp_dst      :: uint16() }).

-record(of_v10_flow_removed, {
          match         :: #of_v10_flow_match{},
          cookie        :: uint64(),
          priority      :: uint16(),
          reason        :: of_v10_flow_removed_reason(),
          duration_sec  :: uint32(),
          duration_nsec :: uint32(),
          idle_timeout  :: uint16(),
          packet_count  :: uint64(),
          byte_count    :: uint64() }).

-record(of_v10_port_status, {
          reason :: of_v10_port_status_reason(),
          desc   :: #of_v10_phy_port{} }).

-type of_v10_message() :: #of_v10_hello{} |
                          #of_v10_error{} |
                          #of_v10_echo_request{} |
                          #of_v10_echo_reply{} |
                          #of_v10_vendor{} |
                          #of_v10_features_request{} |
                          #of_v10_features_reply{} |
                          #of_v10_get_config_request{} |
                          #of_v10_get_config_reply{} |
                          #of_v10_set_config{} |
                          #of_v10_packet_in{} |
                          #of_v10_flow_removed{} |
                          #of_v10_port_status{}.

-endif.
