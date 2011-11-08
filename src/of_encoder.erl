%% @author Bruno Rijsman <brunorijsman@hotmail.com>
%% @copyright 2011 Bruno Rijsman
%%

-module(of_encoder).

-export([encode_header/1,
         encode_body/1]).

-include_lib("eunit/include/eunit.hrl").

-include_lib("../include/of.hrl").   


%%
%% Exported functions.
%%

-spec encode_header(#of_header{}) -> binary().
encode_header(Header) ->
    #of_header{version = Version,
               type    = Type,
               length  = Length,
               xid     = Xid
              } = Header,
    ?OF_HEADER_PATTERN.

%% TODO: Introduce an of_message datatype to avoid the long list

-spec encode_body(of_message()) -> binary().

encode_body(Hello) 
  when is_record(Hello, of_hello) ->
    _FutureExtension = <<>>,
    ?OF_HELLO_PATTERN;

encode_body(Error) 
  when is_record(Error, of_error) ->
    #of_error{type = Type,
              code = Code,
              data = Data} = Error,
    ?OF_ERROR_PATTERN;

encode_body(EchoRequest) 
  when is_record(EchoRequest, of_echo_request) ->
    #of_echo_request{data = Data} = EchoRequest,
    ?OF_ECHO_REQUEST_PATTERN;

encode_body(EchoReply) 
  when is_record(EchoReply, of_echo_reply) ->
    #of_echo_reply{data = Data} = EchoReply,
    ?OF_ECHO_REPLY_PATTERN.

%%
%% Internal functions.
%%

%%
%% Unit tests.
%%
    
encode_header_test() ->
    Rec = of_test_msgs:header_rec(),
    ActualBin = encode_header(Rec),
    ExpectedBin = of_test_msgs:header_bin(),
    ?assert(ActualBin =:= ExpectedBin).

encode_hello_body_test() ->
    Rec = of_test_msgs:hello_rec(),
    ActualBin = encode_body(Rec),
    ExpectedBin = of_test_msgs:hello_bin(),
    ?assert(ActualBin =:= ExpectedBin).
    
encode_error_body_test() ->
    Rec = of_test_msgs:error_rec(),
    ActualBin = encode_body(Rec),
    ExpectedBin = of_test_msgs:error_bin(),
    ?assert(ActualBin =:= ExpectedBin).

encode_echo_request_body_test() ->
    Rec = of_test_msgs:echo_request_rec(),
    ActualBin = encode_body(Rec),
    ExpectedBin = of_test_msgs:echo_request_bin(),
    ?assert(ActualBin =:= ExpectedBin).

encode_echo_reply_body_test() ->
    Rec = of_test_msgs:echo_reply_rec(),
    ActualBin = encode_body(Rec),
    ExpectedBin = of_test_msgs:echo_reply_bin(),
    ?assert(ActualBin =:= ExpectedBin).
