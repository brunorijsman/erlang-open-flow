-ifndef(OF_LOG).
-define(OF_LOG, true).

-define(LOG(Level, Format, Args), of_log:log(?MODULE, Level, Format, Args)).
-define(DEBUG(Message), ?DEBUG(Message, [])).
-define(DEBUG(Format, Args), ?LOG(debug, Format, Args)).

-endif.
