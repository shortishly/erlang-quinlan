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

-module(baseball_SUITE).
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

outlook_sunny_examples(Config) ->
    quinlan_id3:subset(examples(Config), outlook, sunny).


attributes_test(Config) ->
    [humidity, outlook, temperature, wind] = quinlan_id3:attributes(examples(Config)).

entropy_test(Config) ->
    0.9402859586706309 = quinlan_id3:entropy(examples(Config)).

gain_outlook_test(Config) ->
    0.2467498197744391 = quinlan_id3:gain(examples(Config), outlook).

gain_temperature_test(Config) ->
    0.029222565658954647 = quinlan_id3:gain(examples(Config), temperature).

gain_humidity_test(Config) ->
    0.15183550136234136 = quinlan_id3:gain(examples(Config), humidity).

gain_wind_test(Config) ->
    0.04812703040826927 = quinlan_id3:gain(examples(Config), wind).


outlook_sunny_gain_humidity_test(Config) ->
    0.9709505944546686 = quinlan_id3:gain(outlook_sunny_examples(Config), humidity).

outlook_sunny_gain_temperature_test(Config) ->
    0.5709505944546686 = quinlan_id3:gain(outlook_sunny_examples(Config), temperature).

outlook_wind_gain_temperature_test(Config) ->
    0.01997309402197489 = quinlan_id3:gain(outlook_sunny_examples(Config), wind).



highest_gain_attribute_test(Config) ->
    outlook = quinlan_id3:highest_gain_attribute(examples(Config)).

highest_gain_attribute_with_outlook_rain_test(Config) ->
    wind = quinlan_id3:highest_gain_attribute(quinlan_id3:subset(examples(Config), outlook, rain)).

highest_gain_attribute_with_outlook_sunny_test(Config) ->
    humidity = quinlan_id3:highest_gain_attribute(quinlan_id3:subset(examples(Config), outlook, sunny)).


tree_walk_test(Config) ->
    Tree = quinlan_id3:tree(examples(Config)),
    yes = quinlan_id3:walk([{outlook, overcast}], Tree),
    no = quinlan_id3:walk([{outlook, rain}, {wind, strong}], Tree),
    yes = quinlan_id3:walk([{outlook, rain}, {wind, weak}], Tree),
    no = quinlan_id3:walk([{humidity, high}, {outlook, sunny}], Tree),
    yes = quinlan_id3:walk([{humidity, normal}, {outlook, sunny}], Tree).
