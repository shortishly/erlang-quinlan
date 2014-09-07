%% Copyright (c) 2013-2014 Peter Morgan <peter.james.morgan@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(entropy_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).


all() ->
    common:all().

groups() ->
    common:groups(?MODULE).


n(N, X) ->
    lists:map(fun(_) -> X end, lists:seq(1, N)).

entropy_test(_) ->
    Yes = quinlan_id3:classify([], yes),
    No = quinlan_id3:classify([], no),
    0.9402859586706309 = quinlan_id3:entropy(n(9, Yes) ++ n(5, No)).

entropy_perfectly_classified_test(_) ->
    Yes = quinlan_id3:classify([], yes),
    0.0 = quinlan_id3:entropy([Yes]),
    0.0 = quinlan_id3:entropy([Yes, Yes]).

entropy_totally_random_test(_) ->
    Yes = quinlan_id3:classify([], yes),
    No = quinlan_id3:classify([], no),
    1.0 = quinlan_id3:entropy([Yes, No]).

