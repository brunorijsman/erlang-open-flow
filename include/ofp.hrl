-ifndef(OFP_HRL).
-define(OFP_HRL, true).

% Protocol version
-define(OFP_VERSION, 2).

% Message type
-define(OFP_TYPE_HELLO,                    0).
-define(OFP_TYPE_ERROR,                    1).
-define(OFP_TYPE_ECHO_REQUEST,             2).
-define(OFP_TYPE_ECHO_REPLY,               3).
-define(OFP_TYPE_EXPERIMENTER,             4).
-define(OFP_TYPE_FEATURES_REQUEST,         5).
-define(OFP_TYPE_FEATURES_REPLY,           6).
-define(OFP_TYPE_GET_CONFIG_REQUEST,       7).
-define(OFP_TYPE_GET_CONFIG_REPLY,         8).
-define(OFP_TYPE_SET_CONFIG,               9).
-define(OFP_TYPE_PACKET_IN,                10).
-define(OFP_TYPE_FLOW_REMOVED,             11).
-define(OFP_TYPE_PORT_STATUS,              12).
-define(OFP_TYPE_PACKET_OUT,               13).
-define(OFP_TYPE_FLOW_MOD,                 14).
-define(OFP_TYPE_GROUP_MOD,                15).
-define(OFP_TYPE_PORT_MOD,                 16).
-define(OFP_TYPE_TABLE_MOD,                17).
-define(OFP_TYPE_STATS_REQUEST,            18).
-define(OFP_TYPE_STATS_REPLY,              19).
-define(OFP_TYPE_BARRIER_REQUEST,          20).
-define(OFP_TYPE_BARRIER_REPLY,            21).
-define(OFP_TYPE_QUEUE_GET_CONFIG_REQUEST, 22).
-define(OFP_TYPE_QUEUE_GET_CONFIG_REPLY,   23).

% Port numbers
-define(OFP_PORT_NO_MAX,        0xffffff00).
-define(OFP_PORT_NO_IN,         0xfffffff8).
-define(OFP_PORT_NO_TABLE,      0xfffffff9).
-define(OFP_PORT_NO_NORMAL,     0xfffffffa).
-define(OFP_PORT_NO_FLOOD,      0xfffffffb).
-define(OFP_PORT_NO_ALL,        0xfffffffc).
-define(OFP_PORT_NO_CONTROLLER, 0xfffffffd).
-define(OFP_PORT_NO_LOCAL,      0xfffffffe).
-define(OFP_PORT_NO_ANY,        0xffffffff).

% Queue property types
-define(OFP_QUEUE_PROPERTY_TYPE_NONE,     0).
-define(OFP_QUEUE_PROPERTY_TYPE_MIN_RATE, 1).

% Match types
-define(OFP_MATCH_TYPE_STANDARD, 0).

% Maximum length of hardware address
-define(OFP_ETH_ALEN, 6).

% Maximum length of a port name (inluding terminating null character)
-define(OFP_MAX_PORT_NAME_LEN, 16).

-define(OFP_HEADER_PATTERN,
        << Version : 8,
           Type    : 8,
           Length  : 16,
           Xid     : 32 >>).

-define(OFP_PORT_PATTERN,
        << PortNo             : 32,
           _                  : 32,
           HwAddr             : OFP_ETH_ALEN/binary-unit:8,
           _                  : 16,
           Name               : OFP_MAX_PORT_NAME_LEN/binary-unit:8,
           Config             : 32/binary,
           State              : 32/binary,
           CurrentFeatures    : 32/binary,
           AdvertisedFeatures : 32/binary,
           SupportedFeatures  : 32/binary,
           CurrentSpeedKbps   : 32,
           MaxSpeedKbps       : 32 >>).

-define(OFP_PORT_CONFIG_PATTERN,
        << _          : 25,
           NoPacketIn : 1,
           NoFwd      : 1,
           _          : 2,
           NoRecv     : 1,
           _          : 1,
           PortDown   : 1 >>).

-define(OFP_PORT_STATE_PATTERN,
        << _        : 29,
           Live     : 1,
           Blocked  : 1,
           LinkDown : 1 >>).

-define(OFP_PORT_FEATURES_PATTERN,
        << _                 : 16
           PauseAsymetric    : 1,
           Pause             : 1,
           AutoNegotiation   : 1,
           FiberMedium       : 1,
           CopperMedium      : 1,
           OtherRate         : 1,
           FullDuplex1Tbps   : 1,
           FullDuplex100Gbps : 1,
           FullDuplex40Gbps  : 1,
           FullDuplex10Gbps  : 1,
           FullDuplex1Gbps   : 1,
           HalfDuplex1Gbps   : 1,
           FullDuplex100Mbps : 1,
           HalfDuplex100Mbps : 1,
           FullDuplex10Mbps  : 1,
           HalfDuplex10Mbps  : 1 >>).

-define(OFP_QUEUE_PATTERN,
        << QueueId : 32,
           Length  : 16,
           _       : 16,
           QueueProperties/binary >>).

-define(OFP_QUEUE_PROPERTY_HEADER_PATTERN,
        << Type   : 16,
           Length : 16,
           _      : 32 >>).

-define(OFP_QUEUE_PROPERTY_MIN_RATE_PATTERN,
        << RateInTenthPercent : 16,
           _                  : 48 >>).

-define(OFP_MATCH_PATTERN,
        << Type         : 16,
           Length       : 16,
           InPort       : 32,
           Wildcards    : 32,
           DlSource     : OFP_ETH_ALEN/binary-unit:8,
           DlSourceMask : OFP_ETH_ALEN/binary-unit:8,
           DlDst        : OFP_ETH_ALEN/binary-unit:8,
           DlDstMask    : OFP_ETH_ALEN/binary-unit:8,
           DlVlan       : 16,
           DlVlanPcp    : 8,
           _            : 8,
           DlType       : 16,
           NwTos        : 8,
           NwProto      : 8,
           NwSrc        : 32,
           NwSrcMask    : 32,
           NwDst        : 32,
           NwDstMask    : 32,
           TpSrc        : 16,
           TpDst        : 16,
           MplsLabel    : 32,
           MplsTc       : 8,
           _            : 24,
           MetaData     : 64,
           MetaDataMask : 64 >>).

%% TODO: Continue with wildcards on page 29 of the spec

-endif.
