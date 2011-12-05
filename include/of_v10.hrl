-ifndef(OF_V10_HRL).
-define(OF_V10_HRL, true).

%% TODO: Fix the include paths
-include_lib("../include/of_types.hrl").

%% Protocol version
-define(OF_V10_VERSION, 1).

%% TODO: done
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

%% TODO: done
%% Error types
-define(OF_V10_ERROR_TYPE_MIN,                  0).
-define(OF_V10_ERROR_TYPE_MAX,                  5).
-define(OF_V10_ERROR_TYPE_HELLO_FAILED,         0).
-define(OF_V10_ERROR_TYPE_BAD_REQUEST,          1).
-define(OF_V10_ERROR_TYPE_BAD_ACTION,           2).
-define(OF_V10_ERROR_TYPE_FLOW_MOD_FAILED,      3).
-define(OF_V10_ERROR_TYPE_PORT_MOD_FAILED,      4).
-define(OF_V10_ERROR_TYPE_QUEUE_OP_FAILED,      5).

%% TODO: done
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

%% TODO: done
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

%% TODO: done
%% Queue property types
-define(OF_V10_QUEUE_PROPERTY_TYPE_NONE,     0).
-define(OF_V10_QUEUE_PROPERTY_TYPE_MIN_RATE, 1).

%% TODO: done
%% STP port states
-define(OF_V10_STP_PORT_STATE_MIN,     0).
-define(OF_V10_STP_PORT_STATE_MAX,     3).
-define(OF_V10_STP_PORT_STATE_LISTEN,  0).
-define(OF_V10_STP_PORT_STATE_LEARN,   1).
-define(OF_V10_STP_PORT_STATE_FORWARD, 2).
-define(OF_V10_STP_PORT_STATE_BLOCK,   3).

%% Match types
%% TODO: Not used in v1.0?
%% -define(OF_V10_MATCH_TYPE_STANDARD, 0).

%% TODO: done
%% Maximum length of hardware address
-define(OF_V10_ETH_ALEN, 6).

%% TODO: done
%% Maximum length of a port name (inluding terminating null character)
-define(OF_V10_MAX_PORT_NAME_LEN, 16).

%% TODO: done
-define(OF_V10_HEADER_LEN, 8).

%% TODO: done
-define(OF_V10_HEADER_PATTERN,
        << Version : 8,
           Type    : 8,
           Length  : 16,
           Xid     : 32 >>).

%% TODO: done
-define(OF_V10_PORTS_PATTERN,
        << PortNo             : 16,
           HwAddr             : ?OF_V10_ETH_ALEN/binary-unit:8,
           Name               : ?OF_V10_MAX_PORT_NAME_LEN/binary-unit:8,
           Config             : 4/binary,
           State              : 4/binary,
           CurrentFeatures    : 4/binary,
           AdvertisedFeatures : 4/binary,
           SupportedFeatures  : 4/binary,
           PeerFeatures       : 4/binary,
           MorePorts/binary >>).

%% TODO: done
-define(OF_V10_PORT_CONFIG_PATTERN,
        << _Reserved  : 25,
           NoPacketIn : 1,
           NoFwd      : 1,
           NoFlood    : 1,
           NoRecvStp  : 1,
           NoRecv     : 1,
           NoStp      : 1,
           PortDown   : 1 >>).

%% TODO: done
-define(OF_V10_PORT_STATE_PATTERN,
        << _Reserved1   : 22,
           StpPortState : 2,
           _Reserved2   : 7,
           LinkDown     : 1 >>).

%% TODO: done
-define(OF_V10_PORT_FEATURES_PATTERN,
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

%% TODO: done
-define(OF_V10_QUEUE_PATTERN,
        << QueueId : 32,
           Length  : 16,
           _Pad    : 16,
           QueueProperties/binary >>).

%% TODO: done
-define(OF_V10_QUEUE_PROPERTY_HEADER_PATTERN,
        << Type   : 16,
           Length : 16,
           _Pad   : 32 >>).

%% TODO: done
-define(OF_V10_QUEUE_PROPERTY_MIN_RATE_PATTERN,
        << RateInTenthPercent : 16,
           _Pad               : 48 >>).

%% TODO: done
-define(OF_V10_MATCH_PATTERN,
        << Wildcards    : 4/binary,
           InPort       : 32,
           DlSource     : ?OF_V10_ETH_ALEN/binary-unit:8,
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

%% TODO: done
-define(OF_V10_MATCH_WILDCARDS,
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

%% TODO: done
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

%% TODO: done
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

-define(OF_V10_SWITCH_CONFIG_PATTERN, 
        << _Reserved              : 13,
           InvalidTtlToController : 1,
           FragReasm              : 1,
           FragDrop               : 1,
           MissSendLen            : 16 >>).   

%% TODO: done
-define(OF_V10_HELLO_PATTERN,
        << _FutureExtension/binary >>).

%% TODO: done
-define(OF_V10_ERROR_PATTERN,
        << Type : 16,
           Code : 16,
           Data/binary >>).

%% TODO: done
-define(OF_V10_ECHO_REQUEST_PATTERN,
        << Data/binary >>).

%% TODO: done
-define(OF_V10_ECHO_REPLY_PATTERN,
        << Data/binary >>).

%% TODO: done
-define(OF_V10_VENDOR_PATTERN,
        << VendorId : 32,
           Data/binary >>).

%% TODO: done
-define(OF_V10_FEATURES_REQUEST_PATTERN, << >>).

%% TODO: done
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

-type of_v10_version() :: ?OF_V10_VERSION.

-type of_v10_message_type() :: ?OF_V10_MESSAGE_TYPE_MIN..?OF_V10_MESSAGE_TYPE_MAX.
-type of_v10_error_type() :: ?OF_V10_ERROR_TYPE_MIN..?OF_V10_ERROR_TYPE_MAX.

-type of_v10_error_code() :: ?OF_V10_ERROR_CODE_MIN..?OF_V10_ERROR_CODE_MAX.

-type of_v10_error_data() :: binary().

-type of_v10_stp_port_state() :: ?OF_V10_STP_PORT_STATE_MIN..?OF_V10_STP_PORT_STATE_MAX.

-record(of_v10_header, {
          version :: of_v10_version(),
          type    :: of_v10_message_type(),
          length  :: uint16(),
          xid     :: of_xid() }). 

%% TODO: done
-record(of_v10_hello, {}).

%% TODO: done
-record(of_v10_error, {
          type :: uint16(),   %% Accept unrecognized types
          code :: uint16(),   %% Accept unrecognized codes
          data :: binary() }).

%% TODO: done
-record(of_v10_echo_request, {
          data :: binary() }).

%% TODO: done
-record(of_v10_echo_reply, {
          data :: binary() }).

%% TODO: done
-record(of_v10_vendor, {
          vendor_id :: uint32(),
          data      :: binary() }).

-record(of_v10_features_request, {}).

%% TODO: done
-record(of_v10_port_config, {
          port_down    :: boolean(),
          no_stp       :: boolean(),
          no_recv      :: boolean(),
          no_recv_stp  :: boolean(),
          no_flood     :: boolean(),
          no_fwd       :: boolean(),
          no_packet_in :: boolean() }).

%% TODO: done
-record(of_v10_port_state, {
          link_down      :: boolean(),
          stp_port_state :: of_v10_stp_port_state() }).

%% TODO: done
-record(of_v10_port_features, {
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

%% TODO: done
-record(of_v10_port, {
          port_no             :: uint16(),
          hw_addr             :: of_hw_addr(),
          name                :: string(),
          config              :: #of_v10_port_config{},
          state               :: #of_v10_port_state{},
          current_features    :: #of_v10_port_features{},
          advertised_features :: #of_v10_port_features{},
          supported_features  :: #of_v10_port_features{},
          peer_features       :: #of_v10_port_features{} }).

%% TODO: done
-record(of_v10_capabilities, {
          flow_stats   :: boolean(),
          table_stats  :: boolean(),
          port_stats   :: boolean(),
          stp          :: boolean(),
          ip_reasm     :: boolean(),
          queue_stats  :: boolean(),
          arp_match_ip :: boolean() }).

%% TODO: done
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

%% TODO: done
-record(of_v10_features_reply, {
          data_path_id :: uint64(),
          n_buffers    :: uint32(),
          n_tables     :: uint8(),
          capabilities :: #of_v10_capabilities{},
          actions      :: #of_v10_actions{},
          ports        :: [#of_v10_port{}] }).

-record(of_v10_get_config_request, {}).

-record(of_v10_switch_config, {
          frag_drop                 :: boolean(),
          frag_reasm                :: boolean(),
          invalid_ttl_to_controller :: boolean(),
          miss_send_len             :: uint16() }).

-record(of_v10_get_config_reply, {
          switch_config :: #of_v10_switch_config{} }).

%% TODO: Add missing messages
-type of_v10_message() :: #of_v10_hello{} |
                          #of_v10_error{} |
                          #of_v10_echo_request{} |
                          #of_v10_echo_reply{} |
                          #of_v10_vendor{} |
                          #of_v10_features_request{} |
                          #of_v10_features_reply{}.

-endif.
