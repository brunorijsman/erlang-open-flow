%% @author Bruno Rijsman <brunorijsman@hotmail.com>
%% @copyright 2011 Bruno Rijsman
%%

%% TODO: Consider throwing an exception instead of returning an error?

-module(of_decoder).

-export([decode_header/1,
         decode_hello/1,
         decode_error/1,
         decode_echo_request/1,
         decode_echo_reply/1,
         decode_experimenter/1,
         decode_features_request/1,
         decode_features_reply/1]).

%% TODO: Specify the search path

-include_lib("../include/of.hrl").

%% TODO: Validate length?
%% TODO: Validate xid?
-spec decode_header(binary()) -> {ok, #of_header{}} | {error, of_error_type(), of_error_code()}.
decode_header(?OF_HEADER_PATTERN) ->
    Header = #of_header{
      version = Version,
      type    = Type,
      length  = Length,
      xid     = Xid
     },
    if
        (Version < ?OF_VERSION_MIN) or (Version > ?OF_VERSION_MAX) ->
            {error, ?OF_ERROR_TYPE_BAD_REQUEST, ?OF_ERROR_CODE_BAD_REQUEST_BAD_VERSION};
        (Type < ?OF_MESSAGE_TYPE_MIN) or (Type > ?OF_MESSAGE_TYPE_MAX) ->
            {error, ?OF_ERROR_TYPE_BAD_REQUEST, ?OF_ERROR_CODE_BAD_REQUEST_BAD_TYPE};
        true ->
            {ok, Header}
    end.

-spec decode_hello(binary()) -> {ok, #of_hello{}} | {error, of_error_type(), of_error_code()}.
decode_hello(?OF_HELLO_PATTERN) ->
    Hello = #of_hello{},
    {ok, Hello}.

-spec decode_error(binary()) -> {ok, #of_error{}} | {error, of_error_type(), of_error_code()}.
decode_error(?OF_ERROR_PATTERN) ->
    Error = #of_error{
      type = Type,
      code = Code,
      data = Data
     },
    %% No validation, accept unregnized types and codes.
    {ok, Error}.

-spec decode_echo_request(binary()) -> {ok, #of_echo_request{}} |  {error, of_error_type(), of_error_code()}.
decode_echo_request(?OF_ECHO_REQUEST_PATTERN) ->
    EchoRequest = #of_echo_request{
      data = Data
     },
    {ok, EchoRequest}.

-spec decode_echo_reply(binary()) -> {ok, #of_echo_reply{}} |  {error, of_error_type(), of_error_code()}.
decode_echo_reply(?OF_ECHO_REPLY_PATTERN) ->
    EchoReply = #of_echo_reply{
      data = Data
     },
    {ok, EchoReply}.

-spec decode_experimenter(binary()) -> {ok, #of_experimenter{}} | {error, of_error_type(), of_error_code()}.
decode_experimenter(?OF_EXPERIMENTER_PATTERN) ->
    Experimenter = #of_experimenter{
      experimenter_id = ExperimenterId,
      data = Data
     },
    %% No validation, reply on higher layer to determine if the extension is supported.
    {ok, Experimenter}.

-spec decode_features_request(binary()) -> {ok, #of_features_request{}} | {error, of_error_type(), of_error_code()}.
decode_features_request(?OF_FEATURES_REQUEST_PATTERN) ->
    FeaturesRequest = #of_echo_request{},
    {ok, FeaturesRequest}.

-spec decode_features_reply(binary()) -> {ok, #of_features_reply{}} |  {error, of_error_type(), of_error_code()}.
decode_features_reply(?OF_FEATURES_REQUEST_PATTERN) ->
    ReaturesReply = #of_features_reply{
      data_path_id = DataPathId,
      n_buffers = NBuffers,
      n_tables = NTables,
      capabilities = decode_capabilities
     },
    {ok, FeaturesReply}.


                          
