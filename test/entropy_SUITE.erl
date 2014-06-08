-module(entropy_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0,
	 groups/0]).

-export([entropy_test/1,
	 entropy_perfectly_classified_test/1, 
	 entropy_totally_random_test/1]).

-import(quinlan_id3, [classify/2,
		      entropy/1]).

all() ->
    [{group, entropy}].

groups() ->
    [{entropy, [parallel], [entropy_test, entropy_perfectly_classified_test, entropy_totally_random_test]}].


n(N, X) ->
    lists:map(fun(_) -> X end, lists:seq(1, N)).

entropy_test(_) ->
    Yes = classify([], yes),
    No = classify([], no),
    ?assertEqual(0.9402859586706309, entropy(n(9, Yes) ++ n(5, No))).

entropy_perfectly_classified_test(_) ->
    Yes = classify([], yes),
    ?assertEqual(0.0, entropy([Yes])),
    ?assertEqual(0.0, entropy([Yes, Yes])).

entropy_totally_random_test(_) ->
    Yes = classify([], yes),
    No = classify([], no),
    ?assertEqual(1.0, entropy([Yes, No])).

