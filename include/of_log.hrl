-ifndef(OF_LOG).
-define(OF_LOG, true).

-define(LOG(Level, Keys, Format, Args), of_logger:log(Level, ?MODULE, Keys, Format, Args)).
-define(DEBUG(Message), ?LOG(debug, [], Message, [])).
-define(DEBUG(Format, Args), ?LOG(debug, [], Format, Args)).
-define(DEBUG_KEY(Keys, Message), ?LOG(debug, Keys, Message, [])).
-define(DEBUG_KEY(Keys, Format, Args), ?LOG(debug, Keys, Format, Args)).

-endif.
