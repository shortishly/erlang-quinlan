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

-module(sunburn_SUITE).
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


sunburn_highest_gain_attribute_test(Config) ->
    hair = quinlan_id3:highest_gain_attribute(examples(Config)).

sunburn_with_hair_blonde_test(Config) ->
    Blonde = quinlan_id3:subset(examples(Config), hair, blonde),
    1.0 = quinlan_id3:entropy(Blonde),
    lotion = quinlan_id3:highest_gain_attribute(quinlan_id3:subset(examples(Config), hair, blonde)).

sunburn_with_hair_red_test(Config) ->
    Red = quinlan_id3:subset(examples(Config), hair, red),
    0.0 = quinlan_id3:entropy(Red).

sunburn_with_hair_brown_test(Config) ->
    Brown = quinlan_id3:subset(examples(Config), hair, brown),
    0.0 = quinlan_id3:entropy(Brown).

sunburn_tree_test(Config) ->
    Tree = quinlan_id3:tree(examples(Config)),
    sunburned = quinlan_id3:walk([{hair, red}], Tree),
    none = quinlan_id3:walk([{hair, brown}], Tree),
    sunburned = quinlan_id3:walk([{hair, blonde}, {lotion, no}], Tree),
    none = quinlan_id3:walk([{hair, blonde}, {lotion, yes}], Tree).
