%% @author Bruno Rijsman <brunorijsman@hotmail.com>
%% @copyright 2011 Bruno Rijsman

-module(of_encoder).

-export([encode/2]).

%% TODO: ifdef TEST everywhere?
-include_lib("eunit/include/eunit.hrl").

-include_lib("../include/of.hrl").   

%%
%% Exported functions.
%%

encode(Message, Xid) ->
    {Type, Body} = encode_body(Message),
    Header = #of_header{
      version = ?OF_VERSION_2,
      type    = Type,
      length  = size(Body),
      xid     = Xid},
    [Header, Body].

%%
%% Internal functions.
%%

-spec encode_header(#of_header{}) -> binary().

encode_header(Header) ->
    #of_header{version = Version,
               type    = Type,
               length  = Length,
               xid     = Xid
              } = Header,
    ?OF_HEADER_PATTERN.

-spec encode_body(of_message()) -> {of_message_type(), binary()}.

encode_body(Hello) 
  when is_record(Hello, of_hello) ->
    _FutureExtension = <<>>,
    {?OF_MESSAGE_TYPE_HELLO, ?OF_HELLO_PATTERN};

encode_body(Error) 
  when is_record(Error, of_error) ->
    #of_error{type = Type,
              code = Code,
              data = Data} = Error,
    {?OF_MESSAGE_TYPE_ERROR, ?OF_ERROR_PATTERN};

encode_body(EchoRequest) 
  when is_record(EchoRequest, of_echo_request) ->
    #of_echo_request{data = Data} = EchoRequest,
    {?OF_MESSAGE_TYPE_ECHO_REQUEST, ?OF_ECHO_REQUEST_PATTERN};

encode_body(EchoReply) 
  when is_record(EchoReply, of_echo_reply) ->
    #of_echo_reply{data = Data} = EchoReply,
    {?OF_MESSAGE_TYPE_ECHO_REPLY, ?OF_ECHO_REPLY_PATTERN}.

%%
%% Unit tests.
%%
    
encode_header_test() ->
    Rec = of_test_msgs:header_rec(),
    ActualResult = encode_header(Rec),
    ExpectedResult = of_test_msgs:header_bin(),
    ?assert(ActualResult =:= ExpectedResult).

encode_hello_body_test() ->
    Rec = of_test_msgs:hello_rec(),
    ActualResult = encode_body(Rec),
    ExpectedResult = {?OF_MESSAGE_TYPE_HELLO, of_test_msgs:hello_bin()},
    ?assert(ActualResult =:= ExpectedResult).
    
encode_error_body_test() ->
    Rec = of_test_msgs:error_rec(),
    ActualResult = encode_body(Rec),
    ExpectedResult = {?OF_MESSAGE_TYPE_ERROR, of_test_msgs:error_bin()},
    ?assert(ActualResult =:= ExpectedResult).

encode_echo_request_body_test() ->
    Rec = of_test_msgs:echo_request_rec(),
    ActualResult = encode_body(Rec),
    ExpectedResult = {?OF_MESSAGE_TYPE_ECHO_REQUEST, of_test_msgs:echo_request_bin()},
    ?assert(ActualResult =:= ExpectedResult).

encode_echo_reply_body_test() ->
    Rec = of_test_msgs:echo_reply_rec(),
    ActualResult = encode_body(Rec),
    ExpectedResult = {?OF_MESSAGE_TYPE_ECHO_REPLY, of_test_msgs:echo_reply_bin()},
    ?assert(ActualResult =:= ExpectedResult).
