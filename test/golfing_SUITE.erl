-module(golfing_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0,
	 groups/0]).

-export([golfing_entropy_test/1,
	 golfing_outlook_gain_test/1,
	 golfing_windy_gain_test/1]).

all() ->
    [{group, golf}].

groups() ->
    [{golf, [parallel], [golfing_entropy_test, golfing_outlook_gain_test, golfing_windy_gain_test]}].


golfing_examples() ->
    [quinlan_id3:classify([{outlook, Outlook},
			   {temperature, Temperature},
			   {humidity, Humidity},
			   {windy, Windy}],
			  Play) || {Outlook,
				    Temperature,
				    Humidity,
				    Windy,
				    Play} <- [{sunny, 85, 85, false, dont_play},
					      {sunny, 80, 90, true, dont_play},
					      {overcast, 83, 78, false, play},
					      {rain, 70, 96, false, play},
					      {rain, 68, 80, false, play},
					      {rain, 65, 70, true, dont_play},
					      {overcast, 64, 65, true, play},
					      {sunny, 72, 95, false, dont_play},
					      {sunny, 69, 70, false, play},
					      {rain, 75, 80, false, play},
					      {sunny, 75, 70, true, play},
					      {overcast, 72, 90, true, play},
					      {overcast, 81, 75, false, play},
					      {rain, 71, 80, true, dont_play}]].

golfing_entropy_test(_) ->
    ?assertEqual(0.9402859586706309, quinlan_id3:entropy(golfing_examples())).

golfing_outlook_gain_test(_) ->
    ?assertEqual(0.2467498197744391, quinlan_id3:gain(golfing_examples(), outlook)).

golfing_windy_gain_test(_) ->
    ?assertEqual(0.04812703040826932, quinlan_id3:gain(golfing_examples(), windy)).

