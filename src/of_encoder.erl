%% @author Bruno Rijsman <brunorijsman@hotmail.com>
%% @copyright 2011 Bruno Rijsman
%%

-module(of_encoder).

-export([encode_header/1]).

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

%%
%% Unit tests.
%%
    
encode_header_test() ->
    Rec = of_test_msgs:header_rec(),
    ActualBin = encode_header(Rec),
    ExpectedBin = of_test_msgs:header_bin(),
    ?assert(ActualBin =:= ExpectedBin).

