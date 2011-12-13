-ifndef(OF_V10_HRL).
-define(OF_V10_HRL, true).

%% TODO: Fix the include paths
-include_lib("../include/of_types.hrl").

-define(OF_V10_VERSION, 1).

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

-define(OF_V10_ERROR_TYPE_MIN,                  0).
-define(OF_V10_ERROR_TYPE_MAX,                  5).
-define(OF_V10_ERROR_TYPE_HELLO_FAILED,         0).
-define(OF_V10_ERROR_TYPE_BAD_REQUEST,          1).
-define(OF_V10_ERROR_TYPE_BAD_ACTION,           2).
-define(OF_V10_ERROR_TYPE_FLOW_MOD_FAILED,      3).
-define(OF_V10_ERROR_TYPE_PORT_MOD_FAILED,      4).
-define(OF_V10_ERROR_TYPE_QUEUE_OP_FAILED,      5).

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

-define(OF_V10_PORT_NO_MAX,        16#ff00).
-define(OF_V10_PORT_NO_IN,         16#fff8).
-define(OF_V10_PORT_NO_TABLE,      16#fff9).
-define(OF_V10_PORT_NO_NORMAL,     16#fffa).
-define(OF_V10_PORT_NO_FLOOD,      16#fffb).
-define(OF_V10_PORT_NO_ALL,        16#fffc).
-define(OF_V10_PORT_NO_CONTROLLER, 16#fffd).
-define(OF_V10_PORT_NO_LOCAL,      16#fffe).
-define(OF_V10_PORT_NO_NONE,       16#ffff).

-define(OF_V10_STP_PORT_STATE_MIN,     0).
-define(OF_V10_STP_PORT_STATE_MAX,     3).
-define(OF_V10_STP_PORT_STATE_LISTEN,  0).
-define(OF_V10_STP_PORT_STATE_LEARN,   1).
-define(OF_V10_STP_PORT_STATE_FORWARD, 2).
-define(OF_V10_STP_PORT_STATE_BLOCK,   3).

-define(OF_V10_FRAG_HANDLING_MIN,    0).
-define(OF_V10_FRAG_HANDLING_MAX,    2).
-define(OF_V10_FRAG_HANDLING_NORMAL, 0).
-define(OF_V10_FRAG_HANDLING_DROP,   1).
-define(OF_V10_FRAG_HANDLING_REASM,  2).

-define(OF_V10_PACKET_IN_REASON_MIN,      0).
-define(OF_V10_PACKET_IN_REASON_MAX,      1).
-define(OF_V10_PACKET_IN_REASON_NO_MATCH, 0).
-define(OF_V10_PACKET_IN_REASON_ACTION,   1).

-define(OF_V10_FLOW_REMOVED_REASON_MIN,          0).
-define(OF_V10_FLOW_REMOVED_REASON_MAX,          2).
-define(OF_V10_FLOW_REMOVED_REASON_IDLE_TIMEOUT, 0).
-define(OF_V10_FLOW_REMOVED_REASON_HARD_TIMEOUT, 1).
-define(OF_V10_FLOW_REMOVED_REASON_DELETE,       2).

-define(OF_V10_PORT_STATUS_REASON_MIN,    0).
-define(OF_V10_PORT_STATUS_REASON_MAX,    2).
-define(OF_V10_PORT_STATUS_REASON_ADD,    0).
-define(OF_V10_PORT_STATUS_REASON_DELETE, 1).
-define(OF_V10_PORT_STATUS_REASON_MODIFY, 2).

%% TODO: Is there some way to set the type 0..11 or 0xffff?
-define(OF_V10_ACTION_TYPE_MIN,          0).
-define(OF_V10_ACTION_TYPE_MAX,          16#ffff).
-define(OF_V10_ACTION_TYPE_OUTPUT,       0).
-define(OF_V10_ACTION_TYPE_SET_VLAN_VID, 1).
-define(OF_V10_ACTION_TYPE_SET_VLAN_PCP, 2).
-define(OF_V10_ACTION_TYPE_STRIP_VLAN,   3).
-define(OF_V10_ACTION_TYPE_SET_DL_SRC,   4).
-define(OF_V10_ACTION_TYPE_SET_DL_DST,   5).
-define(OF_V10_ACTION_TYPE_SET_NW_SRC,   6).
-define(OF_V10_ACTION_TYPE_SET_NW_DST,   7).
-define(OF_V10_ACTION_TYPE_SET_NW_TOS,   8).
-define(OF_V10_ACTION_TYPE_SET_TP_SRC,   9).
-define(OF_V10_ACTION_TYPE_SET_TP_DST,   10).
-define(OF_V10_ACTION_TYPE_ENQUEUE,      11).
-define(OF_V10_ACTION_TYPE_VENDOR,       16#ffff).

-define(OF_V10_FLOW_MOD_COMMAND_MIN,           0).
-define(OF_V10_FLOW_MOD_COMMAND_MAX,           4).
-define(OF_V10_FLOW_MOD_COMMAND_ADD,           0).
-define(OF_V10_FLOW_MOD_COMMAND_MODIFY,        1).
-define(OF_V10_FLOW_MOD_COMMAND_MODIFY_STRICT, 2).
-define(OF_V10_FLOW_MOD_COMMAND_DELETE,        3).
-define(OF_V10_FLOW_MOD_COMMAND_DELETE_STRICT, 4).

-define(OF_V10_STATS_TYPE_MIN,       0).
-define(OF_V10_STATS_TYPE_MAX,       6).
-define(OF_V10_STATS_TYPE_DESC,      0).
-define(OF_V10_STATS_TYPE_FLOW,      1).
-define(OF_V10_STATS_TYPE_AGGREGATE, 2).
-define(OF_V10_STATS_TYPE_TABLE,     3).
-define(OF_V10_STATS_TYPE_PORT,      4).
-define(OF_V10_STATS_TYPE_QUEUE,     5).
-define(OF_V10_STATS_TYPE_VENDOR,    6).

-define(OF_V10_QUEUE_PROPERTY_TYPE_MIN,      0).
-define(OF_V10_QUEUE_PROPERTY_TYPE_MAX,      1).
-define(OF_V10_QUEUE_PROPERTY_TYPE_NONE,     0).
-define(OF_V10_QUEUE_PROPERTY_TYPE_MIN_RATE, 1).

-define(OF_V10_ETH_ALEN, 6).

-define(OF_V10_MAX_TABLE_NAME_LEN, 32).

-define(OF_V10_MAX_PORT_NAME_LEN, 16).

-define(OF_V10_DESC_STR_LEN, 256).

-define(OF_V10_SERIAL_NUM_LEN, 32).

-define(OF_V10_HEADER_LEN, 8).

-define(OF_V10_QUEUE_ID_ALL, 0xffffffff).   %% TODO: Spec not clear on this

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
        << Wildcards    : 4/binary,   %% TODO: define for magic number 4 
           InPort       : 16,
           DlSrc        : ?OF_V10_ETH_ALEN/binary-unit:8,
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
        << Match        : 40/binary,
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
           Desc   : 48/binary >>.

-define(OF_V10_ACTIONS_PATTERN,
        << Type : 16,
           Len  : 16,
           Rest/binary >>.

-define(OF_V10_ACTION_OUTPUT_PATTERN,
        << Port   : 16,
           MaxLen : 16 >>.

-define(OF_V10_ACTION_SET_VLAN_VID_PATTERN,
        << VlanVid : 16,
           _Pad    : 16 >>.

-define(OF_V10_ACTION_SET_VLAN_PCP_PATTERN,
        << VlanPcp : 8,
           _Pad2   : 24 >>.

-define(OF_V10_ACTION_STRIP_VLAN_PATTERN,
        << _Pad : 32 >>.

-define(OF_V10_ACTION_SET_DL_ADDR_PATTERN,
        << DlAddr : ?OF_V10_ETH_ALEN/binary-unit:8,
           _Pad   : 48 >>.

-define(OF_V10_ACTION_SET_NW_ADDR_PATTERN,
        << NwAddr : 32 >>.

-define(OF_V10_ACTION_SET_NW_TOS_PATTERN,
        << NwTos : 8,
           _Pad  : 24 >>.

-define(OF_V10_ACTION_SET_TP_PORT_PATTERN,
        << TpPort : 16,
           _Pad   : 16 >>.

-define(OF_V10_ACTION_ENQUEUE_PATTERN,
        << Port    : 16,
           _Pad    : 48,
           QueueId : 32 >>.

-define(OF_V10_ACTION_VENDOR_PATTERN,
        << Vendor : 32 >>.

-define(OF_V10_PACKET_OUT_PATTERN,
        << BufferId   : 32,
           InPort     : 16,
           ActionsLen : 16,
           Actions    : ActionsLen/binary-unit:8,
           Data/binary >>.

-define(OF_V10_FLOW_MOD_PATTERN,
        << Match        : 40/binary,
           Cookie       : 64,
           Command      : 16,
           IdleTimeout  : 16,
           HardTimeout  : 16,
           Priority     : 16,
           BufferId     : 32, 
           OutPort      : 16,
           _Reserved    : 13,
           Emerg        : 1,
           CheckOverlap : 1, 
           SendFlowRem  : 1,
           Actions/binary >>.

-define(OF_V10_PORT_MOD_PATTERN,
        << PortNo    : 16,
           HwAddr    : ?OF_V10_ETH_ALEN/binary-unit:8,
           Config    : 4/binary,
           Mask      : 4/binary,
           Advertise : 4/binary,
           _Pad      : 32 >>.

-define(OF_V10_STATS_REQUEST_PATTERN,
        << Type   : 16,
           _Flags : 16, 
           Body/binary >>.

-define(OF_V10_DESC_STATS_REQUEST_PATTERN, << >>).

-define(OF_V10_FLOW_STATS_REQUEST_PATTERN,
        << Match   : 40/binary,
           TableId : 8,
           _Pad    : 8,
           OutPort : 16 >>.

-define(OF_V10_AGGREGATE_STATS_REQUEST_PATTERN,
        << Match   : 40/binary,
           TableId : 8,
           _Pad    : 8,
           OutPort : 16 >>.

-define(OF_V10_TABLE_STATS_REQUEST_PATTERN, << >>).

-define(OF_V10_PORT_STATS_REQUEST_PATTERN,
        << PortNo : 16,
           _Pad   : 48 >>.

-define(OF_V10_QUEUE_STATS_REQUEST_PATTERN,
        << PortNo  : 16,
           _Pad    : 16,
           QueueId : 32 >>.

-define(OF_V10_VENDOR_STATS_REQUEST_PATTERN,
        << VendorId : 32,
           Body/binary >>.

-define(OF_V10_STATS_REPLY_PATTERN,
        << Type      : 16,
           _Reserved : 15,
           More      : 1,
           Body/binary >>.

-define(OF_V10_DESC_STATS_REPLY_PATTERN,
        << MfrDesc   : ?OF_V10_DESC_STR_LEN/binary-unit:8,
           HwDesc    : ?OF_V10_DESC_STR_LEN/binary-unit:8,
           SwDesc    : ?OF_V10_DESC_STR_LEN/binary-unit:8,
           SerialNum : ?OF_V10_SERIAL_NUM_LEN/binary-unit:8,
           DpDesc    : ?OF_V10_DESC_STR_LEN/binary-unit:8 >>.

-define(OF_V10_FLOW_STATS_REPLY_PATTERN,
        << _Length      : 16,     %% TODO: Why is this here? Am I missing something?
           TableId      : 8,
           _Pad1        : 8,
           Match        : 40/binary,
           DurationSec  : 32,
           DurationNsec : 32,
           Priority     : 16,
           IdleTimeout  : 16,
           HardTimeout  : 16,
           _Pad2        : 48,
           Cookie       : 64,
           PacketCount  : 64,
           ByteCount    : 64,
           Actions/binary >>.           

-define(OF_V10_AGGREGATE_STATS_REPLY_PATTERN,
        << PacketCount : 64,
           ByteCount   : 64,
           FlowCount   : 32,
           _Pad        : 32 >>.

-define(OF_V10_TABLE_STATS_REPLY_PATTERN,
        << TableId      : 8,
           _Pad         : 24,
           Name         : ?OF_V10_MAX_TABLE_NAME_LEN/binary-unit:8,
           Wildcards    : 4/binary,
           MaxEntries   : 32,
           ActiveCount  : 32,
           LookupCount  : 64,
           MatchedCount : 64 >>.

-define(OF_V10_PORT_STATS_REPLY_PATTERN,
        << PortNo     : 16,
           _Pad       : 48,
           RxPackets  : 64,
           TxPackets  : 64,
           RxBytes    : 64,
           TxBytes    : 64,
           RxDropped  : 64,
           TxDropped  : 64,
           RxErrors   : 64,
           TxErrors   : 64,
           RxFrameErr : 64,
           TxOverErr  : 64,
           RxCrcErr   : 64,
           Collisions : 64 >>.

-define(OF_V10_QUEUE_STATS_REPLY_PATTERN,
        << PortNo    : 16,
           _Pad      : 16,
           QueueId   : 32,
           TxBytes   : 64,
           TxPackets : 64,
           TxErrors  : 64 >>.

-define(OF_V10_VENDOR_STATS_REPLY_PATTERN,
        << VendorId : 32,
           Body/binary >>.

-define(OF_V10_BARRIER_REQUEST_PATTERN, << >>).

-define(OF_V10_BARRIER_REPLY_PATTERN, << >>).

-define(OF_V10_QUEUE_GET_CONFIG_REQUEST_PATTERN,
        << Port : 16,
           _Pad : 16 >>.

-define(OF_V10_QUEUE_GET_CONFIG_REPLY_PATTERN,
        << Port : 16,
           _Pad : 16,
           Queues/binary >>.

-define(OF_V10_QUEUES_PATTERN,
        << QueueId    : 32,
           Len        : 16,
           _Pad       : 16,
           Rest/binary >>.

-define(OF_V10_QUEUE_PROPERTIES_PATTERN,
        << Type : 16,
           Len  : 16,
           _Pad : 32,
           Rest/binary >>.

-define(OF_V10_QUEUE_PROPERTY_NONE_PATTERN, << >>).

-define(OF_V10_QUEUE_PROPERTY_MIN_RATE_PATTERN,
        << Rate : 16,
           _Pad : 48 >>.

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

-type of_v10_action_type() :: ?OF_V10_ACTION_TYPE_MIN..?OF_V10_ACTION_TYPE_MAX.

-type of_v10_flow_mod_command() :: ?OF_V10_FLOW_MOD_COMMAND_MIN..?OF_V10_FLOW_MOD_COMMAND_MAX.

-type of_v10_stats_type() :: ?OF_V10_STATS_TYPE_MIN..?OF_V10_STATS_TYPE_MAX.

-type of_v10_queue_property_type() :: ?OF_V10_QUEUE_PROPERTY_TYPE_MIN..?OF_V10_QUEUE_PROPERTY_TYPE_MAX.

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

-record(of_v10_action_header, {
          type :: of_v10_action_type(),
          len  :: uint16() }).

-record(of_v10_action_output, {
          port    :: uint16(),
          max_len :: uint16() }).

-record(of_v10_action_set_vlan_vid, {
          vlan_vid :: uint16() }).

-record(of_v10_action_set_vlan_pcp, {
          vlan_pcp :: uint8() }).

-record(of_v10_action_strip_vlan, {}).

-record(of_v10_action_set_dl_src, {
          dl_src :: of_hw_addr() }).

-record(of_v10_action_set_dl_dst, {
          dl_dst :: of_hw_addr() }).

-record(of_v10_action_set_nw_src, {
          nw_src :: uint32() }).

-record(of_v10_action_set_nw_dst, {
          nw_dst :: uint32() }).

-record(of_v10_action_set_nw_tos, {
          nw_tos :: uint8() }).

-record(of_v10_action_set_tp_src, {
          tp_src :: uint16() }).

-record(of_v10_action_set_tp_dst, {
          tp_dst :: uint16() }).

-record(of_v10_action_enqueue, {
          port     :: uint16(),
          queue_id :: uint32() }).

-record(of_v10_action_vendor, {
          vendor :: uint32() }).

-type of_v10_action() :: #of_v10_action_output{} |
                         #of_v10_action_set_vlan_vid{} |
                         #of_v10_action_set_vlan_pcp{} |
                         #of_v10_action_strip_vlan{} |
                         #of_v10_action_set_dl_src{} |
                         #of_v10_action_set_dl_dst{} |
                         #of_v10_action_set_nw_src{} |
                         #of_v10_action_set_nw_dst{} |
                         #of_v10_action_set_nw_tos{} |
                         #of_v10_action_set_tp_src{} |
                         #of_v10_action_set_tp_dst{} |
                         #of_v10_action_enqueue{} |
                         #of_v10_action_vendor{}.

-record(of_v10_packet_out, {
          buffer_id :: uint16(),
          in_port   :: uint16(),
          actions   :: [of_v10_action()],
          data      :: binary() }).

-record(of_v10_flow_mod, {
          match         :: #of_v10_flow_match{},
          cookie        :: uint64(),
          command       :: uint16(),
          idle_timeout  :: uint16(),
          hard_timeout  :: uint16(),
          priority      :: uint16(),
          buffer_id     :: uint32(),
          out_port      :: uint16(),
          send_flow_rem :: boolean(),
          check_overlap :: boolean(), 
          emerg         :: boolean(),
          actions       :: [of_v10_action()] }).

-record(of_v10_port_mod, {
          port_no   :: uint16(),
          hw_addr   :: of_hw_addr(),
          config    :: #of_v10_phy_port_config{},
          mask      :: #of_v10_phy_port_config{},
          advertise :: #of_v10_phy_port_features{} }).

-record(of_v10_desc_stats_request, {}).

-record(of_v10_flow_stats_request, {
          match    :: #of_v10_flow_match{},
          table_id :: uint8(),
          out_port :: uint16() }).

-record(of_v10_aggregate_stats_request, {
          match    :: #of_v10_flow_match{},
          table_id :: uint8(),
          out_port :: uint16() }).

-record(of_v10_table_stats_request, {}).

-record(of_v10_port_stats_request, {
          port_no :: uint16() }).

-record(of_v10_queue_stats_request, {
          port_no  :: uint16(),
          queue_id :: uint32() }).

-record(of_v10_vendor_stats_request, {
          vendor_id  :: uint32(),
          body       :: binary() }).

-type of_v10_stats_request_body() :: #of_v10_desc_stats_request{} |
                                     #of_v10_flow_stats_request{} |
                                     #of_v10_aggregate_stats_request{} |
                                     #of_v10_table_stats_request{} |
                                     #of_v10_port_stats_request{} |
                                     #of_v10_queue_stats_request{} |
                                     #of_v10_vendor_stats_request{}.

%% TODO: need this?
-record(of_v10_stats_request, {
          body :: of_v10_stats_request_body() }).

%% TODO: limit length of string in type
-record(of_v10_desc_stats_reply, {
        mfr_desc   :: string(),
        hw_desc    :: string(),
        sw_desc    :: string(),
        serial_num :: string(),
        dp_desc    :: string() }).

-record(of_v10_flow_stats_reply, {
        table_id      :: uint8(),
        match         :: #of_v10_flow_match{},
        duration_sec  :: uint32(),
        duration_nsec :: uint32(),
        priority      :: uint16(),
        idle_timeout  :: uint16(),
        hard_timeout  :: uint16(),
        cookie        :: uint64(),
        packet_count  :: uint64(),
        byte_count    :: uint64(),
        actions       :: [of_v10_action()] }).

-record(of_v10_aggregate_stats_reply, {
        packet_count :: uint64(),
        byte_count   :: uint64(),
        flow_count   :: uint32() }).

%% TODO: limit length of string
-record(of_v10_table_stats_reply, {
        table_id      :: uint8(),
        name          :: string(),
        wildcards     :: #of_v10_flow_match_wildcards{},
        max_entries   :: uint32(),
        active_count  :: uint32(),
        lookup_count  :: uint64(),
        matched_count :: uint64() }).

-record(of_v10_port_stats_reply, {
        port_no      :: uint16(),
        rx_packets   :: uint64(),
        tx_packets   :: uint64(),
        rx_bytes     :: uint64(),
        tx_bytes     :: uint64(),
        rx_dropped   :: uint64(),
        tx_dropped   :: uint64(),
        rx_errors    :: uint64(),
        tx_errors    :: uint64(),
        rx_frame_err :: uint64(),
        tx_over_err  :: uint64(),
        rx_crc_err   :: uint64(),
        collisions   :: uint64() }).

%% TODO: separate types for port_no and queue_id
-record(of_v10_queue_stats_reply, {
        port_no :: uint16(),
        queue_id :: uint32(),
        tx_bytes :: uint64(),
        tx_packets :: uint64(),
        tx_errors :: uint64() }).

-record(of_v10_vendor_stats_reply, {
        vendor_id :: uint32(),
        body      :: binary() }).

-type of_v10_stats_reply_body() :: #of_v10_flow_stats_reply{} |
                                   #of_v10_aggregate_stats_reply{} |
                                   #of_v10_table_stats_reply{} |
                                   #of_v10_port_stats_reply{} |
                                   #of_v10_queue_stats_reply{} |
                                   #of_v10_vendor_stats_reply{}.

-record(of_v10_stats_reply, {
          more :: boolean(),
          body :: of_v10_stats_reply_body() }).

-record(of_v10_barrier_request, {}).

-record(of_v10_barrier_reply, {}).

-record(of_v10_queue_get_config_request, {
          port :: uint16() }).

-record(of_v10_queue_property_min_rate, {
          rate :: uint16() }).

-type of_v10_queue_property() :: #of_v10_queue_property_min_rate{}.

-record(of_v10_queue, {
          queue_id   :: uint32(),
          properties :: [of_v10_queue_property()] }).

-record(of_v10_queue_get_config_reply, {
          port   :: uint16(),
          queues :: [#of_v10_queue{}] }).

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
                          #of_v10_port_status{} |
                          #of_v10_packet_out{} |
                          #of_v10_flow_mod{} |
                          #of_v10_port_mod{} |
                          #of_v10_stats_request{} |
                          #of_v10_stats_reply{} |
                          #of_v10_barrier_request{} |
                          #of_v10_barrier_reply{} |
                          #of_v10_queue_get_config_request{} |
                          #of_v10_queue_get_config_reply{}.

-endif.
