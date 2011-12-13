%% @author Bruno Rijsman <brunorijsman@hotmail.com>
%% @copyright 2011 Bruno Rijsman
%%

-module(of_v10_test_msgs).

%% TODO: Have more succint way:
%% generate(request_with_data, bin)
%% that allows the testing to be done much simpler (generator)

-export([header_bin/0,
         header_rec/0,
         header_bad_version_bin/0,
         header_bad_message_type_bin/0,
         hello_bin/0,
         hello_rec/0,
         hello_with_extension_bin/0,
         hello_with_extension_rec/0,
         error_bin/0,
         error_rec/0,
         error_with_data_bin/0,
         error_with_data_rec/0,
         echo_request_bin/0,
         echo_request_rec/0,
         echo_request_with_data_bin/0,
         echo_request_with_data_rec/0,
         echo_reply_bin/0,
         echo_reply_rec/0,
         echo_reply_with_data_bin/0,
         echo_reply_with_data_rec/0,
         vendor_bin/0,
         vendor_rec/0,
         vendor_with_data_bin/0,
         vendor_with_data_rec/0,
         features_request_bin/0,
         features_request_rec/0,
         features_reply_bin/0,
         features_reply_rec/0,
         get_config_request_bin/0,
         get_config_request_rec/0,
         get_config_reply_bin/0,
         get_config_reply_rec/0,
         set_config_bin/0,
         set_config_rec/0,
         packet_in_bin/0,
         packet_in_rec/0,
         flow_removed_bin/0,
         flow_removed_rec/0,
         port_status_bin/0,
         port_status_rec/0,
         packet_out_no_actions_no_data_bin/0,
         packet_out_no_actions_no_data_rec/0,
         packet_out_no_actions_data_bin/0,
         packet_out_no_actions_data_rec/0,
         packet_out_action_output_bin/0,
         packet_out_action_output_rec/0,
         packet_out_action_set_vlan_vid_bin/0,
         packet_out_action_set_vlan_vid_rec/0,
         packet_out_action_set_vlan_pcp_bin/0,
         packet_out_action_set_vlan_pcp_rec/0,
         packet_out_action_strip_vlan_bin/0,
         packet_out_action_strip_vlan_rec/0,
         packet_out_action_set_dl_src_bin/0,
         packet_out_action_set_dl_src_rec/0,
         packet_out_action_set_dl_dst_bin/0,
         packet_out_action_set_dl_dst_rec/0,
         packet_out_action_set_nw_src_bin/0,
         packet_out_action_set_nw_src_rec/0,
         packet_out_action_set_nw_dst_bin/0,
         packet_out_action_set_nw_dst_rec/0,
         packet_out_action_set_nw_tos_bin/0,
         packet_out_action_set_nw_tos_rec/0,
         packet_out_action_set_tp_src_bin/0,
         packet_out_action_set_tp_src_rec/0,
         packet_out_action_set_tp_dst_bin/0,
         packet_out_action_set_tp_dst_rec/0,
         packet_out_action_enqueue_bin/0,
         packet_out_action_enqueue_rec/0,
         packet_out_action_vendor_bin/0,
         packet_out_action_vendor_rec/0,
         packet_out_multiple_actions_no_data_bin/0,
         packet_out_multiple_actions_no_data_rec/0,
         packet_out_multiple_actions_data_bin/0,
         packet_out_multiple_actions_data_rec/0,
         flow_mod_bin/0,
         flow_mod_rec/0,
         port_mod_bin/0,
         port_mod_rec/0,
         stats_request_desc_bin/0,
         stats_request_desc_rec/0,
         stats_request_flow_bin/0,
         stats_request_flow_rec/0,
         stats_request_aggregate_bin/0,
         stats_request_aggregate_rec/0,
         stats_request_table_bin/0,
         stats_request_table_rec/0,
         stats_request_port_bin/0,
         stats_request_port_rec/0,
         stats_request_queue_bin/0,
         stats_request_queue_rec/0,
         stats_request_vendor_bin/0,
         stats_request_vendor_rec/0,
         stats_reply_desc_bin/0,
         stats_reply_desc_rec/0,
         stats_reply_flow_bin/0,
         stats_reply_flow_rec/0,
         stats_reply_aggregate_bin/0,
         stats_reply_aggregate_rec/0,
         stats_reply_table_bin/0,
         stats_reply_table_rec/0,
         stats_reply_port_bin/0,
         stats_reply_port_rec/0,
         stats_reply_queue_bin/0,
         stats_reply_queue_rec/0,
         stats_reply_vendor_bin/0,
         stats_reply_vendor_rec/0,
         barrier_request_bin/0,
         barrier_request_rec/0,
         barrier_reply_bin/0,
         barrier_reply_rec/0,
         queue_get_config_request_bin/0,
         queue_get_config_request_rec/0,
         queue_get_config_reply_bin/0,
         queue_get_config_reply_rec/0]).
 
-include_lib("../include/of_v10.hrl").

%%
%% Exported functions.
%%

header_bin() ->
    << ?OF_V10_VERSION            : 8,      % Version
       ?OF_V10_MESSAGE_TYPE_HELLO : 8,      % Type
       0                          : 16,     % Length
       0                          : 32 >>.  % Xid

header_rec() ->
    #of_v10_header{version = ?OF_V10_VERSION,
                   type    = ?OF_V10_MESSAGE_TYPE_HELLO,
                   length  = 0,
                   xid     = 0}.

header_bad_version_bin() ->
    << 99                         : 8,      % Version
       ?OF_V10_MESSAGE_TYPE_HELLO : 8,      % Type
       0                          : 16,     % Length
       0                          : 32 >>.  % Xid

header_bad_message_type_bin() ->
    << ?OF_V10_VERSION   : 8,      % Version
       99                : 8,      % Type
       0                 : 16,     % Length
       0                 : 32 >>.  % Xid

hello_bin() ->
    << >>.

hello_rec() ->
    #of_v10_hello{}.

hello_with_extension_bin() ->
    << 1, 2, 3, 4, 5 >>.

hello_with_extension_rec() ->
    #of_v10_hello{}.

error_bin() ->
    <<?OF_V10_ERROR_TYPE_BAD_REQUEST         : 16,
      ?OF_V10_ERROR_CODE_BAD_REQUEST_BAD_LEN : 16 >>.

error_rec() ->
    #of_v10_error{type = ?OF_V10_ERROR_TYPE_BAD_REQUEST,
                  code = ?OF_V10_ERROR_CODE_BAD_REQUEST_BAD_LEN,
                  data = << >>}.

error_with_data_bin() ->
    <<?OF_V10_ERROR_TYPE_BAD_ACTION              : 16,
      ?OF_V10_ERROR_CODE_BAD_ACTION_BAD_OUT_PORT : 16,
      5, 4, 3, 2, 1 >>.

error_with_data_rec() ->
    #of_v10_error{type = ?OF_V10_ERROR_TYPE_BAD_ACTION,
                  code = ?OF_V10_ERROR_CODE_BAD_ACTION_BAD_OUT_PORT,
                  data = <<5, 4, 3, 2, 1>>}.

echo_request_bin() ->
    << >>.

echo_request_rec() ->
    #of_v10_echo_request{data = << >>}.

echo_request_with_data_bin() ->
    << 3, 3, 3, 4, 4, 4 >>.

echo_request_with_data_rec() ->
    #of_v10_echo_request{data = << 3, 3, 3, 4, 4, 4 >>}.

echo_reply_bin() ->
    << >>.

echo_reply_rec() ->
    #of_v10_echo_reply{data = << >>}.

echo_reply_with_data_bin() ->
    << 3, 3, 3, 4, 4, 4 >>.

echo_reply_with_data_rec() ->
    #of_v10_echo_reply{data = << 3, 3, 3, 4, 4, 4 >>}.

vendor_bin() ->
    << 1 : 32 >>.   % Vendor ID

vendor_rec() ->
    #of_v10_vendor{vendor_id = 1,
                   data      = << >>}.

vendor_with_data_bin() ->
    << 1234 : 32,            % Vendor ID
       99, 99, 88, 88 >>.    % Data

vendor_with_data_rec() ->
    #of_v10_vendor{vendor_id = 1234,
                   data      = << 99, 99, 88, 88 >>}.

features_request_bin() ->
    << >>.

features_request_rec() ->
    #of_v10_features_request{}.

features_reply_bin() ->
    CapabilitiesBin = capabilities_bin(),
    ActionsBin = actions_bitmap_bin(),
    Port1Bin = phy_port_bin(),
    Port2Bin = phy_port_bin(),
    << 123456789 : 64,             %% Data path ID
       5000      : 32,             %% Number of buffers
       50        : 8,              %% Number of tables
       0         : 24,             %% Padding
       CapabilitiesBin/binary,     %% Capabilities
       ActionsBin/binary,          %% Actions
       Port1Bin/binary,            %% Port 1 configuration
       Port2Bin/binary >>.         %% Port 2 configuration

features_reply_rec() ->
    #of_v10_features_reply{data_path_id = 123456789,
                           n_buffers    = 5000,
                           n_tables     = 50,
                           capabilities = capabilities_rec(),
                           actions      = actions_bitmap_rec(),
                           ports        = [phy_port_rec(), phy_port_rec()]}.

get_config_request_bin() ->
    << >>.

get_config_request_rec() ->
    #of_v10_get_config_request{}.

switch_config_flags_bin() ->
    << 0                          : 14,    %% Reserved
       ?OF_V10_FRAG_HANDLING_DROP : 2 >>.  %% Fragment handling

switch_config_bin() ->
    SwitchConfigFlagsBin = switch_config_flags_bin(),
    << SwitchConfigFlagsBin/binary,        %% Flags
       123 : 16 >>.                        %% Miss send length

get_config_reply_bin() ->
    _SwitchConfig = switch_config_bin().

switch_config_rec() ->
    #of_v10_switch_config{frag_handling = ?OF_V10_FRAG_HANDLING_DROP,
                          miss_send_len = 123}.

get_config_reply_rec() ->
    #of_v10_get_config_reply{switch_config = switch_config_rec()}.

set_config_bin() ->
    _SwitchConfig = switch_config_bin().

set_config_rec() ->
    #of_v10_set_config{switch_config = switch_config_rec()}.

packet_in_bin() ->
    Data = << 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 >>,
    << 12345                             : 32,  %% Buffer ID
       10                                : 16,  %% Total length
       222                               : 16,  %% In port
       ?OF_V10_PACKET_IN_REASON_NO_MATCH : 8,   %% Reason
       0                                 : 8,   %% Pad
       Data/binary >>.                          %% Data

packet_in_rec() ->
    #of_v10_packet_in{buffer_id = 12345,
                      total_len = 10,
                      in_port   = 222,
                      reason    = ?OF_V10_PACKET_IN_REASON_NO_MATCH,
                      data      = << 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 >>}.

flow_removed_bin() ->
    Match = flow_match_bin(),
    << Match/binary,
       11111                                    : 64,     %% Cookie
       222                                      : 16,     %% Priority
       ?OF_V10_FLOW_REMOVED_REASON_HARD_TIMEOUT : 8,      %% Reason
       0                                        : 8,      %% Padding
       3333                                     : 32,     %% Duration (sec)
       4444                                     : 32,     %% Duration (nsec)
       555                                      : 16,     %% Idle timeout
       0                                        : 16,     %% Padding
       66666                                    : 64,     %% Packet count
       77777                                    : 64 >>.  %% Byte count

flow_removed_rec() ->
    #of_v10_flow_removed{match         = flow_match_rec(),
                         cookie        = 11111,
                         priority      = 222,
                         reason        = ?OF_V10_FLOW_REMOVED_REASON_HARD_TIMEOUT,
                         duration_sec  = 3333,
                         duration_nsec = 4444,
                         idle_timeout  = 555,
                         packet_count  = 66666,
                         byte_count    = 77777}.

port_status_bin() ->
    Desc = phy_port_bin(),
    << ?OF_V10_PORT_STATUS_REASON_DELETE : 8,   %% Reason
       0                                 : 56,  %% Padding
       Desc/binary >>.

port_status_rec() ->
    #of_v10_port_status{reason = ?OF_V10_PORT_STATUS_REASON_DELETE,
                        desc   = phy_port_rec()}.

packet_out_no_actions_no_data_bin() ->
    ActionsBin = << >>, 
    DataBin    = << >>,
    packet_out_bin(ActionsBin, DataBin).
       
packet_out_no_actions_no_data_rec() ->
    Actions = [],
    Data    = << >>,
    packet_out_rec(Actions, Data).

packet_out_no_actions_data_bin() ->
    ActionsBin = << >>, 
    DataBin    = << 1, 2, 3, 4, 5 >>,
    packet_out_bin(ActionsBin, DataBin).
       
packet_out_no_actions_data_rec() ->
    Actions = [],
    Data    = << 1, 2, 3, 4, 5 >>,
    packet_out_rec(Actions, Data).

packet_out_action_output_bin() ->
    ActionsBin = action_output_bin(),
    DataBin = << 1, 2, 3, 4, 5 >>,
    packet_out_bin(ActionsBin, DataBin).

packet_out_action_output_rec() ->
    Actions = [action_output_rec()],
    Data    = << 1, 2, 3, 4, 5 >>,
    packet_out_rec(Actions, Data).

packet_out_action_set_vlan_vid_bin() ->
    ActionsBin = action_set_vlan_vid_bin(),
    DataBin = << 1, 2, 3, 4, 5 >>,
    packet_out_bin(ActionsBin, DataBin).

packet_out_action_set_vlan_vid_rec() ->
    Actions = [action_set_vlan_vid_rec()],
    Data    = << 1, 2, 3, 4, 5 >>,
    packet_out_rec(Actions, Data).

packet_out_action_set_vlan_pcp_bin() ->
    ActionsBin = action_set_vlan_pcp_bin(),
    DataBin = << 1, 2, 3, 4, 5 >>,
    packet_out_bin(ActionsBin, DataBin).

packet_out_action_set_vlan_pcp_rec() ->
    Actions = [action_set_vlan_pcp_rec()],
    Data    = << 1, 2, 3, 4, 5 >>,
    packet_out_rec(Actions, Data).

packet_out_action_strip_vlan_bin() ->
    ActionsBin = action_strip_vlan_bin(),
    DataBin = << 1, 2, 3, 4, 5 >>,
    packet_out_bin(ActionsBin, DataBin).

packet_out_action_strip_vlan_rec() ->
    Actions = [action_strip_vlan_rec()],
    Data    = << 1, 2, 3, 4, 5 >>,
    packet_out_rec(Actions, Data).

packet_out_action_set_dl_src_bin() ->
    ActionsBin = action_set_dl_src_bin(),
    DataBin = << 1, 2, 3, 4, 5 >>,
    packet_out_bin(ActionsBin, DataBin).

packet_out_action_set_dl_src_rec() ->
    Actions = [action_set_dl_src_rec()],
    Data    = << 1, 2, 3, 4, 5 >>,
    packet_out_rec(Actions, Data).

packet_out_action_set_dl_dst_bin() ->
    ActionsBin = action_set_dl_dst_bin(),
    DataBin = << 1, 2, 3, 4, 5 >>,
    packet_out_bin(ActionsBin, DataBin).

packet_out_action_set_dl_dst_rec() ->
    Actions = [action_set_dl_dst_rec()],
    Data    = << 1, 2, 3, 4, 5 >>,
    packet_out_rec(Actions, Data).

packet_out_action_set_nw_src_bin() ->
    ActionsBin = action_set_nw_src_bin(),
    DataBin = << 1, 2, 3, 4, 5 >>,
    packet_out_bin(ActionsBin, DataBin).

packet_out_action_set_nw_src_rec() ->
    Actions = [action_set_nw_src_rec()],
    Data    = << 1, 2, 3, 4, 5 >>,
    packet_out_rec(Actions, Data).

packet_out_action_set_nw_dst_bin() ->
    ActionsBin = action_set_nw_dst_bin(),
    DataBin = << 1, 2, 3, 4, 5 >>,
    packet_out_bin(ActionsBin, DataBin).

packet_out_action_set_nw_dst_rec() ->
    Actions = [action_set_nw_dst_rec()],
    Data    = << 1, 2, 3, 4, 5 >>,
    packet_out_rec(Actions, Data).

packet_out_action_set_nw_tos_bin() ->
    ActionsBin = action_set_nw_tos_bin(),
    DataBin = << 1, 2, 3, 4, 5 >>,
    packet_out_bin(ActionsBin, DataBin).

packet_out_action_set_nw_tos_rec() ->
    Actions = [action_set_nw_tos_rec()],
    Data    = << 1, 2, 3, 4, 5 >>,
    packet_out_rec(Actions, Data).

packet_out_action_set_tp_src_bin() ->
    ActionsBin = action_set_tp_src_bin(),
    DataBin = << 1, 2, 3, 4, 5 >>,
    packet_out_bin(ActionsBin, DataBin).

packet_out_action_set_tp_src_rec() ->
    Actions = [action_set_tp_src_rec()],
    Data    = << 1, 2, 3, 4, 5 >>,
    packet_out_rec(Actions, Data).

packet_out_action_set_tp_dst_bin() ->
    ActionsBin = action_set_tp_dst_bin(),
    DataBin = << 1, 2, 3, 4, 5 >>,
    packet_out_bin(ActionsBin, DataBin).

packet_out_action_set_tp_dst_rec() ->
    Actions = [action_set_tp_dst_rec()],
    Data    = << 1, 2, 3, 4, 5 >>,
    packet_out_rec(Actions, Data).

packet_out_action_enqueue_bin() ->
    ActionsBin = action_enqueue_bin(),
    DataBin = << 1, 2, 3, 4, 5 >>,
    packet_out_bin(ActionsBin, DataBin).

packet_out_action_enqueue_rec() ->
    Actions = [action_enqueue_rec()],
    Data    = << 1, 2, 3, 4, 5 >>,
    packet_out_rec(Actions, Data).

packet_out_action_vendor_bin() ->
    ActionsBin = action_vendor_bin(),
    DataBin = << 1, 2, 3, 4, 5 >>,
    packet_out_bin(ActionsBin, DataBin).

packet_out_action_vendor_rec() ->
    Actions = [action_vendor_rec()],
    Data    = << 1, 2, 3, 4, 5 >>,
    packet_out_rec(Actions, Data).

packet_out_multiple_actions_no_data_bin() ->
    Action1Bin = action_output_bin(),
    Action2Bin = action_set_dl_src_bin(),
    Action3Bin = action_set_nw_src_bin(),
    ActionsBin = << Action1Bin/binary, Action2Bin/binary, Action3Bin/binary >>,
    DataBin = << >>,
    packet_out_bin(ActionsBin, DataBin).

packet_out_multiple_actions_no_data_rec() ->
    Action1 = action_output_rec(),
    Action2 = action_set_dl_src_rec(),
    Action3 = action_set_nw_src_rec(),
    Actions = [Action1, Action2, Action3],
    Data    = << >>,
    packet_out_rec(Actions, Data).

packet_out_multiple_actions_data_bin() ->
    Action1Bin = action_output_bin(),
    Action2Bin = action_set_dl_src_bin(),
    Action3Bin = action_set_nw_src_bin(),
    ActionsBin = << Action1Bin/binary, Action2Bin/binary, Action3Bin/binary >>,
    DataBin = << 1, 2, 3, 4, 5 >>,
    packet_out_bin(ActionsBin, DataBin).

packet_out_multiple_actions_data_rec() ->
    Action1 = action_output_rec(),
    Action2 = action_set_dl_src_rec(),
    Action3 = action_set_nw_src_rec(),
    Actions = [Action1, Action2, Action3],
    Data    = << 1, 2, 3, 4, 5 >>,
    packet_out_rec(Actions, Data).

flow_mod_bin() ->
    MatchBin = flow_match_bin(),
    Action1Bin = action_output_bin(),
    Action2Bin = action_set_dl_src_bin(),
    Action3Bin = action_set_nw_src_bin(),
    ActionsBin = << Action1Bin/binary, Action2Bin/binary, Action3Bin/binary >>,
    << MatchBin/binary,
       11111                        : 64,   %% Cookie
       ?OF_V10_FLOW_MOD_COMMAND_ADD : 16,   %% Command
       222                          : 16,   %% Idle timeout
       333                          : 16,   %% Hard timeout
       444                          : 16,   %% Priority
       5555                         : 32,   %% Buffer ID
       666                          : 16,   %% Out port
       0                            : 13,   %% Reserved
       1                            : 1,    %% Emerg
       0                            : 1,    %% Check overlap
       1                            : 1,    %% Send flow rem
       ActionsBin/binary >>.

flow_mod_rec() ->
    Action1 = action_output_rec(),
    Action2 = action_set_dl_src_rec(),
    Action3 = action_set_nw_src_rec(),
    Actions = [Action1, Action2, Action3],
    #of_v10_flow_mod{match         = flow_match_rec(),
                     cookie        = 11111,
                     command       = ?OF_V10_FLOW_MOD_COMMAND_ADD,
                     idle_timeout  = 222,
                     hard_timeout  = 333,
                     priority      = 444,
                     buffer_id     = 5555,
                     out_port      = 666,
                     send_flow_rem = true,
                     check_overlap = false,
                     emerg         = true,
                     actions       = Actions}.

port_mod_bin() ->
    HwAddr    = hw_addr_bin(),
    Config    = phy_port_config_bin(),
    Mask      = phy_port_config_bin(),
    Advertise = phy_port_features_bin(),
    << 111 : 16,          %% Port no
       HwAddr/binary, 
       Config/binary,
       Mask/binary,
       Advertise/binary,
       0   : 32 >>.       %% Padding

port_mod_rec() ->
    #of_v10_port_mod{port_no   = 111,
                     hw_addr   = hw_addr_bin(),
                     config    = phy_port_config_rec(),
                     mask      = phy_port_config_rec(),
                     advertise = phy_port_features_rec()}.


%%
%% Internal functions.
%%

pad_list_with_zeroes(List, Length) ->
    if
        Length > length(List) ->
            pad_list_with_zeroes(List ++ [0], Length);
        Length == length(List) ->
            List
    end.
    
string_bin(String, BinLength) ->
    PaddedString = pad_list_with_zeroes(String, BinLength),
    list_to_binary(PaddedString).
    
capabilities_bin() ->
    << 0 : 24,                     %% Reserved
       1 : 1,                      %% ARP match IP
       0 : 1,                      %% Queue stats
       0 : 1,                      %% IP reassembly
       0 : 1,                      %% Reserved
       1 : 1,                      %% STP
       0 : 1,                      %% Port stats
       0 : 1,                      %% Table stats
       0 : 1 >>.                   %% Flow stats

capabilities_rec() ->
    #of_v10_capabilities{flow_stats   = false,
                         table_stats  = false,
                         port_stats   = false,
                         stp          = true,
                         ip_reasm     = false,
                         queue_stats  = false,
                         arp_match_ip = true}.

actions_bitmap_bin() ->
    << 0 : 20,                     %% Reserved
       1 : 1,                      %% Enqueue
       1 : 1,                      %% Set transport destination
       1 : 1,                      %% Set transport source
       1 : 1,                      %% Set network TOS
       1 : 1,                      %% Set network destination
       1 : 1,                      %% Set network source
       1 : 1,                      %% Set datalink destination
       1 : 1,                      %% Set datalink source
       0 : 1,                      %% Strip VLAN
       1 : 1,                      %% Set VLAN PCP
       1 : 1,                      %% Set VLAN ID
       1 : 1 >>.                   %% Output

actions_bitmap_rec() ->
    #of_v10_actions{output       = true,
                    set_vlan_id  = true,
                    set_vlan_pcp = true,
                    strip_vlan   = false,
                    set_dl_src   = true,
                    set_dl_dst   = true,
                    set_nw_src   = true,
                    set_nw_dst   = true,
                    set_nw_tos   = true,
                    set_tp_src   = true,
                    set_tp_dst   = true,
                    enqueue      = true}.

hw_addr_bin() ->
    << 1, 2, 3, 4, 5, 6 >>.

phy_port_name_bin() ->
    << "port1", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 >>.

phy_port_config_bin() ->
    << 0 : 25,                     %% Reserved
       1 : 1,                      %% No packet in
       1 : 1,                      %% No forward
       0 : 1,                      %% No flood
       0 : 1,                      %% No receive STP
       0 : 1,                      %% No receive
       0 : 1,                      %% No STP
       1 : 1 >>.                   %% Port down

phy_port_config_rec() ->
    #of_v10_phy_port_config{port_down    = true,
                            no_stp       = false,
                            no_recv      = false,
                            no_recv_stp  = false,
                            no_flood     = false,
                            no_fwd       = true,
                            no_packet_in = true}.

phy_port_state_bin() ->
    StpPortState = ?OF_V10_STP_PORT_STATE_LEARN,
    << 0            : 22,          %% Reserved,
       StpPortState : 2,           %% STP port state
       0            : 7,           %% Reserved
       0            : 1 >>.        %% Link down

phy_port_state_rec() ->
    #of_v10_phy_port_state{link_down      = false,
                           stp_port_state = ?OF_V10_STP_PORT_STATE_LEARN}.

phy_port_features_bin() ->
    << 0 : 20,                     %% Reserved
       0 : 1,                      %% Pause asymetric
       0 : 1,                      %% Pause
       1 : 1,                      %% Auto negotiation
       0 : 1,                      %% Fiber medium
       1 : 1,                      %% Copper medium
       0 : 1,                      %% Full duplex 10 Gbps
       0 : 1,                      %% Full duplex 1 Gbps
       0 : 1,                      %% Half duplex 1 Gbps
       1 : 1,                      %% Full duplex 100 Mbps
       1 : 1,                      %% Half duplex 100 Mbps
       1 : 1,                      %% Full duplex 10 Mbps
       1 : 1 >>.                   %% Half duplex 10 Mbps

phy_port_features_rec() ->
    #of_v10_phy_port_features{half_duplex_10_mbps  = true,
                              full_duplex_10_mbps  = true,
                              half_duplex_100_mbps = true,
                              full_duplex_100_mbps = true,
                              half_duplex_1_gbps   = false,
                              full_duplex_1_gbps   = false,
                              full_duplex_10_gbps  = false,
                              copper_medium        = true,
                              fiber_medium         = false,
                              auto_negotiation     = true,
                              pause                = false,
                              pause_asymetric      = false}.

phy_port_bin() ->
    HwAddrBin = hw_addr_bin(),
    PhyPortNameBin = phy_port_name_bin(),
    PhyPortConfigBin = phy_port_config_bin(),
    PhyPortStateBin = phy_port_state_bin(),
    PhyPortFeaturesBin = phy_port_features_bin(),
    << 1 : 16,                        %% Port no
       HwAddrBin/binary,              %% Hardware address
       PhyPortNameBin/binary,         %% Name
       PhyPortConfigBin/binary,       %% Config
       PhyPortStateBin/binary,        %% State
       PhyPortFeaturesBin/binary,     %% Current features
       PhyPortFeaturesBin/binary,     %% Advertised features
       PhyPortFeaturesBin/binary,     %% Supported features
       PhyPortFeaturesBin/binary >>.  %% Peer features

phy_port_rec() ->
    #of_v10_phy_port{port_no             = 1,
                     hw_addr             = hw_addr_bin(),
                     name                = "port1",
                     config              = phy_port_config_rec(),
                     state               = phy_port_state_rec(),
                     current_features    = phy_port_features_rec(),
                     advertised_features = phy_port_features_rec(),
                     supported_features  = phy_port_features_rec(),
                     peer_features       = phy_port_features_rec()}.

flow_match_wildcards_bin() ->
    << 0  : 10,    %% Reserved
       1  : 1,     %% Network TOS 
       0  : 1,     %% Datalink VLAN PCP
       11 : 6,     %% Network destination address wildcard bit count
       22 : 6,     %% Network source address wildcard bit count
       1  : 1,     %% Transport destination port
       0  : 1,     %% Transport source port
       1  : 1,     %% Network protocol
       0  : 1,     %% Datalink type
       1  : 1,     %% Datalink destination address
       0  : 1,     %% Datalink source address
       1  : 1,     %% Datalink VLAN
       0  : 1 >>.  %% In port

flow_match_wildcards_rec() ->
    #of_v10_flow_match_wildcards{in_port     = false,
                                 dl_vlan     = true,
                                 dl_src      = false,
                                 dl_dst      = true,
                                 dl_type     = false,
                                 nw_proto    = true,
                                 tp_src      = false,
                                 tp_dst      = true,
                                 nw_src_bits = 22,
                                 nw_dst_bits = 11,
                                 dl_vlan_pcp = false,
                                 nw_tos      = true}.

flow_match_bin() ->
    WildcardsBin = flow_match_wildcards_bin(),
    HwAddrBin = hw_addr_bin(),
    << WildcardsBin/binary,
       12345 : 16,         %% In port
       HwAddrBin/binary,   %% Datalink source address
       HwAddrBin/binary,   %% Datalink destination address
       111   : 16,         %% Datalink VLAN
       22    : 8,          %% Datalink VLAN PCP
       0     : 8,          %% Padding
       333   : 16,         %% Datalink type
       44    : 8,          %% Network TOS
       55    : 8,          %% Network protocol
       0     : 16,         %% Padding
       6666  : 32,         %% Network source address,
       7777  : 32,         %% Network destination address,
       88    : 16,         %% Transport source port
       99    : 16 >>.      %% Transport destination port
       
flow_match_rec() ->
    #of_v10_flow_match{wildcards   = flow_match_wildcards_rec(),
                       in_port     = 12345,
                       dl_src      = hw_addr_bin(),
                       dl_dst      = hw_addr_bin(),
                       dl_vlan     = 111,
                       dl_vlan_pcp = 22,
                       dl_type     = 333,
                       nw_tos      = 44,
                       nw_proto    = 55,
                       nw_src      = 6666,
                       nw_dst      = 7777,
                       tp_src      = 88,
                       tp_dst      = 99}.

packet_out_bin(ActionsBin, DataBin) ->
    ActionsLen = size(ActionsBin),
    << 11111      : 32,  %% Buffer ID,
       2222       : 16,  %% In port
       ActionsLen : 16, 
       ActionsBin/binary,
       DataBin/binary >>.

packet_out_rec(Actions, Data) ->
    #of_v10_packet_out{buffer_id = 11111,
                       in_port   = 2222,
                       actions   = Actions,
                       data      = Data}.

action_output_bin() ->
    << ?OF_V10_ACTION_TYPE_OUTPUT       : 16,
       8                                : 16,     %% Action length
       3333                             : 16,     %% Port
       4444                             : 16 >>.  %% Max length

action_output_rec() ->
    #of_v10_action_output{port = 3333, max_len = 4444}.

action_set_vlan_vid_bin() ->
    << ?OF_V10_ACTION_TYPE_SET_VLAN_VID : 16,
       8                                : 16,     %% Action length
       3333                             : 16,     %% VLAN VID
       0                                : 16 >>.  %% Padding

action_set_vlan_vid_rec() ->
    #of_v10_action_set_vlan_vid{vlan_vid = 3333}.

action_set_vlan_pcp_bin() ->
    << ?OF_V10_ACTION_TYPE_SET_VLAN_PCP : 16,
       8                                : 16,     %% Action length
       33                               : 8,      %% VLAN PCP
       0                                : 24 >>.  %% Padding

action_set_vlan_pcp_rec() ->
    #of_v10_action_set_vlan_pcp{vlan_pcp = 33}.

action_strip_vlan_bin() ->
    << ?OF_V10_ACTION_TYPE_STRIP_VLAN   : 16,
       8                                : 16,     %% Action length
       0                                : 32 >>.  %% Padding

action_strip_vlan_rec() ->
    #of_v10_action_strip_vlan{}.

action_set_dl_src_bin() ->
    HwAddrBin = hw_addr_bin(),
    << ?OF_V10_ACTION_TYPE_SET_DL_SRC   : 16,
       16                               : 16,     %% Action length
       HwAddrBin/binary,                          %% Datalink src addr
       0                                : 48 >>.  %% Padding

action_set_dl_src_rec() ->
    HwAddrBin = hw_addr_bin(),
    #of_v10_action_set_dl_src{dl_src = HwAddrBin}.

action_set_dl_dst_bin() ->
    HwAddrBin = hw_addr_bin(),
    << ?OF_V10_ACTION_TYPE_SET_DL_DST   : 16,
       16                               : 16,     %% Action length
       HwAddrBin/binary,                          %% Datalink dst addr
       0                                : 48 >>.  %% Padding

action_set_dl_dst_rec() ->
    HwAddrBin = hw_addr_bin(),
    #of_v10_action_set_dl_dst{dl_dst = HwAddrBin}.

action_set_nw_src_bin() ->
    << ?OF_V10_ACTION_TYPE_SET_NW_SRC   : 16,
       8                                : 16,     %% Action length
       3333                             : 32 >>.  %% Network src addr

action_set_nw_src_rec() ->
    #of_v10_action_set_nw_src{nw_src = 3333}.

action_set_nw_dst_bin() ->
    << ?OF_V10_ACTION_TYPE_SET_NW_DST   : 16,
       8                                : 16,     %% Action length
       3333                             : 32 >>.  %% Network dst addr

action_set_nw_dst_rec() ->
    #of_v10_action_set_nw_dst{nw_dst = 3333}.

action_set_nw_tos_bin() ->
    << ?OF_V10_ACTION_TYPE_SET_NW_TOS   : 16,
       8                                : 16,     %% Action length
       33                               : 8,      %% Network TOS
       0                                : 24 >>.  %% Padding

action_set_nw_tos_rec() ->
    #of_v10_action_set_nw_tos{nw_tos = 33}.

action_set_tp_src_bin() ->
    << ?OF_V10_ACTION_TYPE_SET_TP_SRC   : 16,
       8                                : 16,     %% Action length
       333                              : 16,     %% Transport src port
       0                                : 16 >>.  %% Padding

action_set_tp_src_rec() ->
    #of_v10_action_set_tp_src{tp_src = 333}.

action_set_tp_dst_bin() ->
    << ?OF_V10_ACTION_TYPE_SET_TP_DST   : 16,
       8                                : 16,     %% Action length
       333                              : 16,     %% Transport dst port
       0                                : 16 >>.  %% Padding

action_set_tp_dst_rec() ->
    #of_v10_action_set_tp_dst{tp_dst = 333}.

action_enqueue_bin() ->
    << ?OF_V10_ACTION_TYPE_ENQUEUE      : 16,
       16                               : 16,     %% Action length
       333                              : 16,     %% Port
       0                                : 48,     %% Padding
       4444                             : 32 >>.  %% Queue ID

action_enqueue_rec() ->
    #of_v10_action_enqueue{port = 333, queue_id = 4444}.

action_vendor_bin() ->
    << ?OF_V10_ACTION_TYPE_VENDOR       : 16,
       8                                : 16,     %% Action length
       3333                             : 32 >>.  %% Vendor

action_vendor_rec() ->
    #of_v10_action_vendor{vendor = 3333}.

stats_request_desc_bin() ->
    << ?OF_V10_STATS_TYPE_DESC : 16,      %% Type
       0                       : 16 >>.   %% Flags

stats_request_desc_rec() ->
    Body = #of_v10_desc_stats_request{},
    #of_v10_stats_request{body = Body}.

stats_request_flow_bin() ->
    Match = flow_match_bin(),
    << ?OF_V10_STATS_TYPE_FLOW : 16,      %% Type
       0                       : 16,      %% Flags
       Match/binary, 
       1                       : 8,       %% Table ID
       0                       : 8,       %% Padding
       22                      : 16 >>.   %% Out port

stats_request_flow_rec() ->
    Body = #of_v10_flow_stats_request{match    = flow_match_rec(),
                                      table_id = 1,
                                      out_port = 22},
    #of_v10_stats_request{body = Body}.

stats_request_aggregate_bin() ->
    Match = flow_match_bin(),
    << ?OF_V10_STATS_TYPE_AGGREGATE : 16,      %% Type
       0                            : 16,      %% Flags
       Match/binary, 
       1                            : 8,       %% Table ID
       0                            : 8,       %% Padding
       22                           : 16 >>.   %% Out port

stats_request_aggregate_rec() ->
    Body = #of_v10_aggregate_stats_request{match    = flow_match_rec(),
                                           table_id = 1,
                                           out_port = 22},
    #of_v10_stats_request{body = Body}.

stats_request_table_bin() ->
    << ?OF_V10_STATS_TYPE_TABLE : 16,      %% Type
       0                        : 16 >>.   %% Flags

stats_request_table_rec() ->
    Body = #of_v10_table_stats_request{},
    #of_v10_stats_request{body = Body}.

stats_request_port_bin() ->
    << ?OF_V10_STATS_TYPE_PORT : 16,      %% Type
       0                       : 16,      %% Flags
       11                      : 16,      %% Port no
       0                       : 48 >>.   %% Padding
       
stats_request_port_rec() ->
    Body = #of_v10_port_stats_request{port_no = 11},
    #of_v10_stats_request{body = Body}.

stats_request_queue_bin() ->
    << ?OF_V10_STATS_TYPE_QUEUE : 16,      %% Type
       0                        : 16,      %% Flags
       11                       : 16,      %% Port no
       0                        : 16,      %% Padding
       222                      : 32 >>.   %% Queue ID

stats_request_queue_rec() ->
    Body = #of_v10_queue_stats_request{port_no  = 11, 
                                       queue_id = 222},
    #of_v10_stats_request{body = Body}.

stats_request_vendor_bin() ->
    Body = << 1, 2, 3, 4, 5 >>,
    << ?OF_V10_STATS_TYPE_VENDOR : 16,      %% Type
       0                         : 16,      %% Flags
       111                       : 32,      %% Vendor ID
       Body/binary >>.                      %% Body

stats_request_vendor_rec() ->
    Body = #of_v10_vendor_stats_request{vendor_id = 111,
                                        body      = << 1, 2, 3, 4, 5 >>},
    #of_v10_stats_request{body = Body}.

stats_reply_desc_bin() ->
    MfrDesc   = string_bin("manufacturer", ?OF_V10_DESC_STR_LEN),
    HwDesc    = string_bin("hardware", ?OF_V10_DESC_STR_LEN),
    SwDesc    = string_bin("software", ?OF_V10_DESC_STR_LEN),
    SerialNum = string_bin("serial num", ?OF_V10_SERIAL_NUM_LEN),
    DpDesc    = string_bin("datapath", ?OF_V10_DESC_STR_LEN),
    << ?OF_V10_STATS_TYPE_DESC : 16,      %% Type
       0                       : 15,      %% Reserved
       0                       : 1,       %% More flag
       MfrDesc/binary,
       HwDesc/binary,
       SwDesc/binary,
       SerialNum/binary,
       DpDesc/binary >>.
         
stats_reply_desc_rec() ->
    Body = #of_v10_desc_stats_reply{mfr_desc   = "manufacturer",
                                    hw_desc    = "hardware",
                                    sw_desc    = "software",
                                    serial_num = "serial num",
                                    dp_desc    = "datapath"},
    #of_v10_stats_reply{more = false, body = Body}.

stats_reply_flow_bin() ->
    MatchBin = flow_match_bin(),
    Action1Bin = action_output_bin(),
    Action2Bin = action_set_dl_src_bin(),
    Action3Bin = action_set_nw_src_bin(),
    ActionsBin = << Action1Bin/binary, Action2Bin/binary, Action3Bin/binary >>,
    << ?OF_V10_STATS_TYPE_FLOW : 16,      %% Type
       0                       : 15,      %% Reserved
       0                       : 1,       %% More flag
       88                      : 16,      %% Length of this entry
       1                       : 8,       %% Table ID
       0                       : 8,       %% Padding
       MatchBin/binary,
       2222                    : 32,      %% Duration sec
       3333                    : 32,      %% Duration nsec
       44                      : 16,      %% Priority
       55                      : 16,      %% Idle timeout
       66                      : 16,      %% Hard timeout
       0                       : 48,      %% Padding
       77777777                : 64,      %% Cookie
       88888888                : 64,      %% Packet count
       99999999                : 64,      %% Byte count
       ActionsBin/binary >>.
         
stats_reply_flow_rec() ->
    FlowMatchRec = flow_match_rec(),
    ActionRec1 = action_output_rec(),
    ActionRec2 = action_set_dl_src_rec(),
    ActionRec3 = action_set_nw_src_rec(),
    ActionRecs = [ActionRec1, ActionRec2, ActionRec3],
    Body = #of_v10_flow_stats_reply{table_id = 1,
                                    match         = FlowMatchRec,
                                    duration_sec  = 2222,
                                    duration_nsec = 3333,
                                    priority      = 44,
                                    idle_timeout  = 55,
                                    hard_timeout  = 66,
                                    cookie        = 77777777,
                                    packet_count  = 88888888,
                                    byte_count    = 99999999,
                                    actions       = ActionRecs},
    #of_v10_stats_reply{more = false, body = Body}.

stats_reply_aggregate_bin() ->
    << ?OF_V10_STATS_TYPE_AGGREGATE : 16,      %% Type
       0                            : 15,      %% Reserved
       0                            : 1,       %% More flag
       11111111                     : 64,      %% Packet count
       22222222                     : 64,      %% Byte count
       3333                         : 32,      %% Padding
       0                            : 32 >>.   %% Flow count

stats_reply_aggregate_rec() ->
    Body = #of_v10_aggregate_stats_reply{packet_count = 11111111,
                                         byte_count   = 22222222,
                                         flow_count   = 3333},
    #of_v10_stats_reply{more = false, body = Body}.

stats_reply_table_bin() ->
    NameBin = string_bin("table", ?OF_V10_MAX_TABLE_NAME_LEN),
    WildcardsBin = flow_match_wildcards_bin(),
    << ?OF_V10_STATS_TYPE_TABLE : 16,      %% Type
       0                        : 15,      %% Reserved
       0                        : 1,       %% More flag
       1                        : 8,       %% Table ID
       0                        : 24,      %% Padding
       NameBin/binary,
       WildcardsBin/binary,
       2222                     : 32,      %% Max entries
       3333                     : 32,      %% Active count
       44444444                 : 64,      %% Lookup count
       55555555                 : 64 >>.   %% Matched count

stats_reply_table_rec() ->
    Body = #of_v10_table_stats_reply{table_id      = 1,
                                     name          = "table",
                                     wildcards     = flow_match_wildcards_rec(),
                                     max_entries   = 2222,
                                     active_count  = 3333,
                                     lookup_count  = 44444444,
                                     matched_count = 55555555},
    #of_v10_stats_reply{more = false, body = Body}.

stats_reply_port_bin() ->
    << ?OF_V10_STATS_TYPE_PORT : 16,      %% Type
       0                       : 15,      %% Reserved
       0                       : 1,       %% More flag
       11                      : 16,      %% Port no
       0                       : 48,      %% Padding
       22222222                : 64,      %% RX packets
       33333333                : 64,      %% TX packets
       44444444                : 64,      %% RX bytes
       55555555                : 64,      %% TX bytes
       66666666                : 64,      %% RX dropped
       77777777                : 64,      %% TX dropped
       88888888                : 64,      %% RX errors
       99999999                : 64,      %% TX errors
       21111111                : 64,      %% RX frame errors
       22222222                : 64,      %% TX overrun errors
       23333333                : 64,      %% RX CRC errors
       24444444                : 64 >>.   %% Collisions

stats_reply_port_rec() ->
    Body = #of_v10_port_stats_reply{port_no      = 11,
                                    rx_packets   = 22222222,
                                    tx_packets   = 33333333,
                                    rx_bytes     = 44444444,
                                    tx_bytes     = 55555555,
                                    rx_dropped   = 66666666,
                                    tx_dropped   = 77777777,
                                    rx_errors    = 88888888,
                                    tx_errors    = 99999999,
                                    rx_frame_err = 21111111,
                                    tx_over_err  = 22222222,
                                    rx_crc_err   = 23333333,
                                    collisions   = 24444444},
    #of_v10_stats_reply{more = false, body = Body}.

stats_reply_queue_bin() ->
    << ?OF_V10_STATS_TYPE_QUEUE : 16,      %% Type
       0                        : 15,      %% Reserved
       0                        : 1,       %% More flag
       11                       : 16,      %% Port no
       0                        : 16,      %% Padding
       2222                     : 32,      %% Queue ID
       33333333                 : 64,      %% TX bytes
       44444444                 : 64,      %% TX packets
       55555555                 : 64 >>.   %% TX errors

stats_reply_queue_rec() ->
    Body = #of_v10_queue_stats_reply{port_no    = 11,
                                     queue_id   = 2222,
                                     tx_bytes   = 33333333,
                                     tx_packets = 44444444,
                                     tx_errors  = 55555555},
    #of_v10_stats_reply{more = false, body = Body}.

stats_reply_vendor_bin() ->
    Vendor = << 1, 2, 3 >>,
    << ?OF_V10_STATS_TYPE_VENDOR : 16,      %% Type
       0                         : 15,      %% Reserved
       0                         : 1,       %% More flag
       1111                      : 32,      %% Vendor ID
       Vendor/binary >>.

stats_reply_vendor_rec() ->
    Body = #of_v10_vendor_stats_reply{vendor_id = 1111,
                                      body      = << 1, 2, 3 >>},
    #of_v10_stats_reply{more = false, body = Body}.

barrier_request_bin() ->
    << >>.

barrier_request_rec() ->
    #of_v10_barrier_request{}.

barrier_reply_bin() ->
    << >>.

barrier_reply_rec() ->
    #of_v10_barrier_reply{}.

queue_get_config_request_bin() ->
    << 11 : 16,      %% Port
       0  : 16 >>.   %% Padding

queue_get_config_request_rec() ->
    #of_v10_queue_get_config_request{port = 11}.

queue_property_none_bin() ->
    << ?OF_V10_QUEUE_PROPERTY_TYPE_NONE : 16,      %% Type 
        8                               : 16,      %% Length
        0                               : 32 >>.   %% Padding

queue_property_min_rate_bin() ->
    << ?OF_V10_QUEUE_PROPERTY_TYPE_MIN_RATE : 16,      %% Type 
        16                                  : 16,      %% Length
        0                                   : 32,      %% Padding
        11                                  : 16,      %% Min rate
        0                                   : 48 >>.   %% Padding

queue_properties_bin() ->
    QueuePropertyMinRate = queue_property_min_rate_bin(),
    QueuePropertyNone    = queue_property_none_bin(),
    << QueuePropertyMinRate/binary,
       QueuePropertyNone/binary >>.

queue_bin() ->
    Properties = queue_properties_bin(),
    Length = 8 + size(Properties),
    << 1111   : 32,            %% Queue ID
       Length : 16,            %% Length,
       0      : 16,            %% Padding
       Properties/binary >>.

queues_bin() ->
    Queue1 = queue_bin(),
    Queue2 = queue_bin(),
    << Queue1/binary,
       Queue2/binary >>.
    
queue_get_config_reply_bin() ->
    Queues = queues_bin(),
    << 11 : 16,      %% Port
       0  : 16,      %% Padding
       Queues/binary >>.

queue_property_min_rate_rec() ->
    #of_v10_queue_property_min_rate{rate = 11}.

queue_properties_rec() ->
    [queue_property_min_rate_rec()].

queue_rec() ->
    #of_v10_queue{queue_id = 1111,
                  properties = queue_properties_rec()}.

queues_rec() ->
    Queue1 = queue_rec(),
    Queue2 = queue_rec(),
    [Queue1, Queue2].
    
queue_get_config_reply_rec() ->
    #of_v10_queue_get_config_reply{port   = 11, 
                                   queues = queues_rec()}.

%% TODO: negative test cases
%% TODO: unrecognized action
%% TODO: wrong action length
%% TODO: split public and internal functions
