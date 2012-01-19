-ifndef(OF_VXX_HRL).
-define(OF_VXX_HRL, true).

-include_lib("../include/of_types.hrl").

-define(OF_VXX_ERROR_TYPE_HELLO_FAILED, 0).

-define(OF_VXX_ERROR_CODE_HELLO_FAILED_INCOMPATIBLE, 0).

-record(of_vxx_hello, {
          version :: uint8() }).

-record(of_vxx_error, {
          version :: uint8(),
          type    :: uint16(),
          code    :: uint16(),
          data    :: binary() }).

-endif.
