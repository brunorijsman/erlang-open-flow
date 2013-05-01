%%%=====================================================================================================================
%%% Copyright (c) 2012-2013, Bruno Rijsman
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without modification, are permitted provided that the 
%%% following conditions are met:
%%%
%%% * Redistributions of source code must retain the above copyright notice, this list of conditions and the following 
%%%   disclaimer.
%%%
%%% * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the 
%%%   following disclaimer in the documentation and/or other materials provided with the distribution.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
%%% USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%=====================================================================================================================

%%% @author Bruno Rijsman <brunorijsman@hotmail.com>
%%% @copyright 2012-2013 Bruno Rijsman

-module(of_v11_encoder).

-export([encode/2]).

%% TODO: ifdef TEST everywhere?
-include_lib("eunit/include/eunit.hrl").

-include_lib("../include/of_v11.hrl").   

%%
%% Exported functions.
%%

%% TODO: Fix this -- see v10 encoder
encode(Message, Xid) ->
    {Type, Body} = encode_body(Message),
    Header = #of_v11_header{
      version = ?OF_V11_VERSION,
      type    = Type,
      length  = size(Body),
      xid     = Xid},
    [Header, Body].

%%
%% Internal functions.
%%

-spec encode_header(#of_v11_header{}) -> binary().

encode_header(Header) ->
    #of_v11_header{version = Version,
               type    = Type,
               length  = Length,
               xid     = Xid
              } = Header,
    ?OF_V11_HEADER_PATTERN.

-spec encode_body(of_v11_message()) -> {of_v11_message_type(), binary()}.

encode_body(Hello) 
  when is_record(Hello, of_v11_hello) ->
    _FutureExtension = <<>>,
    {?OF_V11_MESSAGE_TYPE_HELLO, ?OF_V11_HELLO_PATTERN};

encode_body(Error) 
  when is_record(Error, of_v11_error) ->
    #of_v11_error{type = Type,
              code = Code,
              data = Data} = Error,
    {?OF_V11_MESSAGE_TYPE_ERROR, ?OF_V11_ERROR_PATTERN};

encode_body(EchoRequest) 
  when is_record(EchoRequest, of_v11_echo_request) ->
    #of_v11_echo_request{data = Data} = EchoRequest,
    {?OF_V11_MESSAGE_TYPE_ECHO_REQUEST, ?OF_V11_ECHO_REQUEST_PATTERN};

encode_body(EchoReply) 
  when is_record(EchoReply, of_v11_echo_reply) ->
    #of_v11_echo_reply{data = Data} = EchoReply,
    {?OF_V11_MESSAGE_TYPE_ECHO_REPLY, ?OF_V11_ECHO_REPLY_PATTERN}.

%%
%% Unit tests.
%%

encode_header_test() ->
    Rec = of_v11_test_msgs:header_rec(),
    ActualResult = encode_header(Rec),
    ExpectedResult = of_v11_test_msgs:header_bin(),
    ?assert(ActualResult =:= ExpectedResult).

encode_hello_body_test() ->
    Rec = of_v11_test_msgs:hello_rec(),
    ActualResult = encode_body(Rec),
    ExpectedResult = {?OF_V11_MESSAGE_TYPE_HELLO, of_v11_test_msgs:hello_bin()},
    ?assert(ActualResult =:= ExpectedResult).

encode_error_body_test() ->
    Rec = of_v11_test_msgs:error_rec(),
    ActualResult = encode_body(Rec),
    ExpectedResult = {?OF_V11_MESSAGE_TYPE_ERROR, of_v11_test_msgs:error_bin()},
    ?assert(ActualResult =:= ExpectedResult).

encode_echo_request_body_test() ->
    Rec = of_v11_test_msgs:echo_request_rec(),
    ActualResult = encode_body(Rec),
    ExpectedResult = {?OF_V11_MESSAGE_TYPE_ECHO_REQUEST, of_v11_test_msgs:echo_request_bin()},
    ?assert(ActualResult =:= ExpectedResult).

encode_echo_reply_body_test() ->
    Rec = of_v11_test_msgs:echo_reply_rec(),
    ActualResult = encode_body(Rec),
    ExpectedResult = {?OF_V11_MESSAGE_TYPE_ECHO_REPLY, of_v11_test_msgs:echo_reply_bin()},
    ?assert(ActualResult =:= ExpectedResult).
