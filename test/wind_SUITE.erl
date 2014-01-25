-module(wind_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0]).

-export([wind_gain_test/1,
	 entropy_wind_weak_test/1,
	 entropy_wind_strong_test/1]).

-import(quinlan_id3, [gain/2, entropy/1, subset/3]).

all() ->
    [wind_gain_test, entropy_wind_weak_test, entropy_wind_strong_test].


%% Suppose S is a set of 14 examples in which one of the attributes is
%% wind speed. The values of Wind can be Weak or Strong. The
%% classification of these 14 examples are 9 YES and 5 NO. For
%% attribute Wind, suppose there are 8 occurrences of Wind = Weak and
%% 6 occurrences of Wind = Strong. For Wind = Weak, 6 of the examples
%% are YES and 2 are NO. For Wind = Strong, 3 are YES and 3 are NO.
%%
wind_examples() ->
    n(6, quinlan_id3:classify([{wind, weak}], yes)) ++
	n(2, quinlan_id3:classify([{wind, weak}], no)) ++
	n(3, quinlan_id3:classify([{wind, strong}], yes)) ++
	n(3, quinlan_id3:classify([{wind, strong}], no)).

n(N, X) ->
    lists:map(fun(_) -> X end, lists:seq(1, N)).

wind_gain_test(_) ->
    ?assertEqual(0.04812703040826927, gain(wind_examples(), wind)).

entropy_wind_weak_test(_) ->
    ?assertEqual(0.8112781244591328, entropy(subset(wind_examples(), wind, weak))).

entropy_wind_strong_test(_) ->
    ?assertEqual(1.0, entropy(subset(wind_examples(), wind, strong))).
    


