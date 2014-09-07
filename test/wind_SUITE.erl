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

-module(wind_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).


all() ->
    common:all().

groups() ->
    common:groups(?MODULE).

%% From http://www.cise.ufl.edu/~ddd/cap6635/Fall-97/Short-papers/2.htm:
%%
%% Suppose S is a set of 14 examples in which one of the attributes is
%% wind speed. The values of Wind can be Weak or Strong. The
%% classification of these 14 examples are 9 YES and 5 NO. For
%% attribute Wind, suppose there are 8 occurrences of Wind = Weak and
%% 6 occurrences of Wind = Strong. For Wind = Weak, 6 of the examples
%% are YES and 2 are NO. For Wind = Strong, 3 are YES and 3 are NO.
%%
examples() ->
    n(6, quinlan_id3:classify([{wind, weak}], yes)) ++
	n(2, quinlan_id3:classify([{wind, weak}], no)) ++
	n(3, quinlan_id3:classify([{wind, strong}], yes)) ++
	n(3, quinlan_id3:classify([{wind, strong}], no)).

n(N, X) ->
    lists:map(fun(_) -> X end, lists:seq(1, N)).

wind_gain_test(_) ->
    0.04812703040826927 = quinlan_id3:gain(examples(), wind).

entropy_wind_weak_test(_) ->
    0.8112781244591328 = quinlan_id3:entropy(quinlan_id3:subset(examples(), wind, weak)).

entropy_wind_strong_test(_) ->
    1.0 = quinlan_id3:entropy(quinlan_id3:subset(examples(), wind, strong)).
    


