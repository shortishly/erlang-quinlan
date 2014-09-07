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

-module(golfing_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() ->
    common:all().

groups() ->
    common:groups(?MODULE).

init_per_suite(Config) ->
    common:init_per_suite(Config).

examples(Config) ->
    common:examples(Config).


golfing_entropy_test(Config) ->
    0.9402859586706309 = quinlan_id3:entropy(examples(Config)).

golfing_outlook_gain_test(Config) ->
    0.2467498197744391 = quinlan_id3:gain(examples(Config), outlook).

golfing_windy_gain_test(Config) ->
    0.04812703040826932 = quinlan_id3:gain(examples(Config), windy).

