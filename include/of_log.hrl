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

-ifndef(OF_LOG).
-define(OF_LOG, true).

-define(LOG(Level, Keys, Format, Args), of_logger:log(Level, ?MODULE, Keys, Format, Args)).

-define(DEBUG(Message), ?LOG(debug, [], Message, [])).
-define(DEBUG_FMT(Format, Args), ?LOG(debug, [], Format, Args)).
-define(DEBUG_KEY(Keys, Message), ?LOG(debug, Keys, Message, [])).
-define(DEBUG_KEY_FMT(Keys, Format, Args), ?LOG(debug, Keys, Format, Args)).

-define(INFO(Message), ?LOG(info, [], Message, [])).
-define(INFO_FMT(Format, Args), ?LOG(info, [], Format, Args)).
-define(INFO_KEY(Keys, Message), ?LOG(info, Keys, Message, [])).
-define(INFO_KEY_FMT(Keys, Format, Args), ?LOG(info, Keys, Format, Args)).

-define(NOTICE(Message), ?LOG(notice, [], Message, [])).
-define(NOTICE_FMT(Format, Args), ?LOG(notice, [], Format, Args)).
-define(NOTICE_KEY(Keys, Message), ?LOG(notice, Keys, Message, [])).
-define(NOTICE_KEY_FMT(Keys, Format, Args), ?LOG(notice, Keys, Format, Args)).

-define(WARNING(Message), ?LOG(warning, [], Message, [])).
-define(WARNING_FMT(Format, Args), ?LOG(warning, [], Format, Args)).
-define(WARNING_KEY(Keys, Message), ?LOG(warning, Keys, Message, [])).
-define(WARNING_KEY_FMT(Keys, Format, Args), ?LOG(warning, Keys, Format, Args)).

-define(ERROR(Message), ?LOG(error, [], Message, [])).
-define(ERROR_FMT(Format, Args), ?LOG(error, [], Format, Args)).
-define(ERROR_KEY(Keys, Message), ?LOG(error, Keys, Message, [])).
-define(ERROR_KEY_FMT(Keys, Format, Args), ?LOG(error, Keys, Format, Args)).

-define(CRITICAL(Message), ?LOG(critical, [], Message, [])).
-define(CRITICAL_FMT(Format, Args), ?LOG(critical, [], Format, Args)).
-define(CRITICAL_KEY(Keys, Message), ?LOG(critical, Keys, Message, [])).
-define(CRITICAL_KEY_FMT(Keys, Format, Args), ?LOG(critical, Keys, Format, Args)).

-define(ALERT(Message), ?LOG(alert, [], Message, [])).
-define(ALERT_FMT(Format, Args), ?LOG(alert, [], Format, Args)).
-define(ALERT_KEY(Keys, Message), ?LOG(alert, Keys, Message, [])).
-define(ALERT_KEY_FMT(Keys, Format, Args), ?LOG(alert, Keys, Format, Args)).

-define(EMERGENCY(Message), ?LOG(emergency, [], Message, [])).
-define(EMERGENCY_FMT(Format, Args), ?LOG(emergency, [], Format, Args)).
-define(EMERGENCY_KEY(Keys, Message), ?LOG(emergency, Keys, Message, [])).
-define(EMERGENCY_KEY_FMT(Keys, Format, Args), ?LOG(emergency, Keys, Format, Args)).


-ifdef(STATE_RECORD).

-define(DEBUG_STATE(State, Message), ?DEBUG_KEY(State#?STATE_RECORD.log_keys, Message)).
-define(DEBUG_STATE_FMT(State, Format, Args), ?DEBUG_KEY_FMT(State#?STATE_RECORD.log_keys, Format, Args)).

-define(INFO_STATE(State, Message), ?INFO_KEY(State#?STATE_RECORD.log_keys, Message)).
-define(INFO_STATE_FMT(State, Format, Args), ?INFO_KEY_FMT(State#?STATE_RECORD.log_keys, Format, Args)).

-define(NOTICE_STATE(State, Message), ?NOTICE_KEY(State#?STATE_RECORD.log_keys, Message)).
-define(NOTICE_STATE_FMT(State, Format, Args), ?NOTICE_KEY_FMT(State#?STATE_RECORD.log_keys, Format, Args)).

-define(WARNING_STATE(State, Message), ?WARNING_KEY(State#?STATE_RECORD.log_keys, Message)).
-define(WARNING_STATE_FMT(State, Format, Args), ?WARNING_KEY_FMT(State#?STATE_RECORD.log_keys, Format, Args)).

-define(ERROR_STATE(State, Message), ?ERROR_KEY(State#?STATE_RECORD.log_keys, Message)).
-define(ERROR_STATE_FMT(State, Format, Args), ?ERROR_KEY_FMT(State#?STATE_RECORD.log_keys, Format, Args)).

-define(CRITICAL_STATE(State, Message), ?CRITICAL_KEY(State#?STATE_RECORD.log_keys, Message)).
-define(CRITICAL_STATE_FMT(State, Format, Args), ?CRITICAL_KEY_FMT(State#?STATE_RECORD.log_keys, Format, Args)).

-define(ALERT_STATE(State, Message), ?ALERT_KEY(State#?STATE_RECORD.log_keys, Message)).
-define(ALERT_STATE_FMT(State, Format, Args), ?ALERT_KEY_FMT(State#?STATE_RECORD.log_keys, Format, Args)).

-define(EMERGENCY_STATE(State, Message), ?EMERGENCY_KEY(State#?STATE_RECORD.log_keys, Message)).
-define(EMERGENCY_STATE_FMT(State, Format, Args), ?EMERGENCY_KEY_FMT(State#?STATE_RECORD.log_keys, Format, Args)).

-endif.

-endif. 
