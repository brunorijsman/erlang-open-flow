-ifndef(OF_LOG).
-define(OF_LOG, true).

-define(LOG(Level, Keys, Format, Args), of_logger:log(Level, ?MODULE, Keys, Format, Args)).

-define(DEBUG(Message), ?LOG(debug, [], Message, [])).
-define(DEBUG(Format, Args), ?LOG(debug, [], Format, Args)).
-define(DEBUG_KEY(Keys, Message), ?LOG(debug, Keys, Message, [])).
-define(DEBUG_KEY(Keys, Format, Args), ?LOG(debug, Keys, Format, Args)).

-define(INFO(Message), ?LOG(info, [], Message, [])).
-define(INFO(Format, Args), ?LOG(info, [], Format, Args)).
-define(INFO_KEY(Keys, Message), ?LOG(info, Keys, Message, [])).
-define(INFO_KEY(Keys, Format, Args), ?LOG(info, Keys, Format, Args)).

-define(NOTICE(Message), ?LOG(notice, [], Message, [])).
-define(NOTICE(Format, Args), ?LOG(notice, [], Format, Args)).
-define(NOTICE_KEY(Keys, Message), ?LOG(notice, Keys, Message, [])).
-define(NOTICE_KEY(Keys, Format, Args), ?LOG(notice, Keys, Format, Args)).

-define(WARNING(Message), ?LOG(warning, [], Message, [])).
-define(WARNING(Format, Args), ?LOG(warning, [], Format, Args)).
-define(WARNING_KEY(Keys, Message), ?LOG(warning, Keys, Message, [])).
-define(WARNING_KEY(Keys, Format, Args), ?LOG(warning, Keys, Format, Args)).

-define(ERROR(Message), ?LOG(error, [], Message, [])).
-define(ERROR(Format, Args), ?LOG(error, [], Format, Args)).
-define(ERROR_KEY(Keys, Message), ?LOG(error, Keys, Message, [])).
-define(ERROR_KEY(Keys, Format, Args), ?LOG(error, Keys, Format, Args)).

-define(CRITICAL(Message), ?LOG(critical, [], Message, [])).
-define(CRITICAL(Format, Args), ?LOG(critical, [], Format, Args)).
-define(CRITICAL_KEY(Keys, Message), ?LOG(critical, Keys, Message, [])).
-define(CRITICAL_KEY(Keys, Format, Args), ?LOG(critical, Keys, Format, Args)).

-define(ALERT(Message), ?LOG(alert, [], Message, [])).
-define(ALERT(Format, Args), ?LOG(alert, [], Format, Args)).
-define(ALERT_KEY(Keys, Message), ?LOG(alert, Keys, Message, [])).
-define(ALERT_KEY(Keys, Format, Args), ?LOG(alert, Keys, Format, Args)).

-define(EMERGENCY(Message), ?LOG(emergency, [], Message, [])).
-define(EMERGENCY(Format, Args), ?LOG(emergency, [], Format, Args)).
-define(EMERGENCY_KEY(Keys, Message), ?LOG(emergency, Keys, Message, [])).
-define(EMERGENCY_KEY(Keys, Format, Args), ?LOG(emergency, Keys, Format, Args)).

-endif.
