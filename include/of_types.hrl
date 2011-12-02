-ifndef(OF_TYPES_HRL).
-define(OF_TYPES_HRL, true).

-type uint8() :: 0..255.

-type uint16() :: 0..65535.

-type uint32() :: 0..4294967295.

-type uint64() :: 0..18446744073709551615.

-type of_xid() :: uint32().

%% TODO: Is there some way to indicate that the length must be ?ETH_ALEN ?
-type of_hw_addr() :: binary().

-endif.
