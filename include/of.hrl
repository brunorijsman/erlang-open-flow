-ifndef(OF_HRL).
-define(OF_HRL, true).

%% Implementation name
-define(OF_IMPLEMENTATION_NAME, "Anapurna (alpha)").

%% Protocol version
-define(OF_VERSION_MIN, 1).
-define(OF_VERSION_MAX, 1).   %% TODO: Support v1.1 (OF_V11_VERSION = 2) as well

%% Minimum header length, common across all versions
-define(OF_HEADER_LEN, 8).

-endif.
