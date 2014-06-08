-module(sunburn_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0]).

-export([sunburn_highest_gain_attribute_test/1,
	 sunburn_with_hair_blonde_test/1,
	 sunburn_with_hair_red_test/1,
	 sunburn_with_hair_brown_test/1]).

-import(quinlan_id3, [classify/2, entropy/1, subset/3, highest_gain_attribute/1]).

all() ->
    [sunburn_highest_gain_attribute_test,
     sunburn_with_hair_blonde_test,
     sunburn_with_hair_red_test,
     sunburn_with_hair_brown_test].

sunburn_examples() ->
    [classify([{hair, Hair},
	       {height, Height},
	       {weight, Weight},
	       {lotion, Lotion}],
	      Result) || {Hair,
			  Height,
			  Weight,
			  Lotion,
			  Result} <- [{blonde, average, light, no, sunburned},
				      {blonde, tall, average, yes, none},
				      {brown, short, average, yes, none},
				      {blonde, short, average, no, sunburned},
				      {red, average, heavy, no, sunburned},
				      {brown, tall, heavy, no, none},
				      {brown, average, heavy, no, none},
				      {blonde, short, light, yes, none}]].


sunburn_highest_gain_attribute_test(_) ->
    ?assertEqual(hair, highest_gain_attribute(sunburn_examples())).

sunburn_with_hair_blonde_test(_) ->
    Blonde = subset(sunburn_examples(), hair, blonde),
    ?assertEqual(1.0, entropy(Blonde)),
    ?assertEqual(lotion, highest_gain_attribute(subset(sunburn_examples(), hair, blonde))).

sunburn_with_hair_red_test(_) ->
    Red = subset(sunburn_examples(), hair, red),
    ?assertEqual(0.0, entropy(Red)).

sunburn_with_hair_brown_test(_) ->
    Brown = subset(sunburn_examples(), hair, brown),
    ?assertEqual(0.0, entropy(Brown)).

