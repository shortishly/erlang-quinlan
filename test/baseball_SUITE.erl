-module(baseball_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0]).

-export([baseball_attributes_test/1,
	 baseball_entropy_test/1,
	 baseball_gain_outlook_test/1,
	 baseball_gain_temperature_test/1,
	 baseball_gain_humidity_test/1,
	 baseball_gain_wind_test/1,
	 baseball_outlook_sunny_gain_humidity_test/1,
	 baseball_outlook_sunny_gain_temperature_test/1,
	 baseball_outlook_wind_gain_temperature_test/1,
	 baseball_highest_gain_attribute_test/1,
	 baseball_highest_gain_attribute_with_outlook_rain_test/1,
	 baseball_highest_gain_attribute_with_outlook_sunny_test/1]).

-import(quinlan_id3, [classify/2, gain/2, entropy/1, subset/3, attributes/1, highest_gain_attribute/1]).

all() ->
    [baseball_attributes_test,
     baseball_entropy_test,
     baseball_gain_outlook_test,
     baseball_gain_temperature_test,
     baseball_gain_humidity_test,
     baseball_gain_wind_test,
     baseball_outlook_sunny_gain_humidity_test,
     baseball_outlook_sunny_gain_temperature_test,
     baseball_outlook_wind_gain_temperature_test,
     baseball_highest_gain_attribute_test,
     baseball_highest_gain_attribute_with_outlook_rain_test,
     baseball_highest_gain_attribute_with_outlook_sunny_test].


%%
%% Baseball examples
%%
baseball_examples() ->
    [classify([{outlook, Outlook}, 
	       {temperature, Temperature}, 
	       {humidity, Humidity}, 
	       {wind, Wind}],
	      PlayBall) || {Outlook, 
			    Temperature, 
			    Humidity, 
			    Wind, 
			    PlayBall} <- [{sunny, hot, high, weak, no},
					  {sunny, hot, high, strong, no},
					  {overcast, hot, high, weak, yes},
					  {rain, mild, high, weak, yes},
					  {rain, cool, normal, weak, yes},
					  {rain, cool, normal, strong, no},
					  {overcast, cool, normal, strong, yes},
					  {sunny, mild, high, weak, no},
					  {sunny, cool, normal, weak, yes},
					  {rain, mild, normal, weak, yes},
					  {sunny, mild, normal, strong, yes},
					  {overcast, mild, high, strong, yes},
					  {overcast, hot, normal, weak, yes},
					  {rain, mild, high, strong, no}]].

baseball_attributes_test(_) ->
    ?assertEqual([humidity, outlook, temperature, wind], attributes(baseball_examples())).

baseball_entropy_test(_) ->
    ?assertEqual(0.9402859586706309, entropy(baseball_examples())).

baseball_gain_outlook_test(_) ->
    ?assertEqual(0.2467498197744391, gain(baseball_examples(), outlook)).

baseball_gain_temperature_test(_) ->
    ?assertEqual(0.029222565658954647, gain(baseball_examples(), temperature)).

baseball_gain_humidity_test(_) ->
    ?assertEqual(0.15183550136234136, gain(baseball_examples(), humidity)).

baseball_gain_wind_test(_) ->
    ?assertEqual(0.04812703040826927, gain(baseball_examples(), wind)).

baseball_outlook_sunny_examples() ->
    subset(baseball_examples(), outlook, sunny).

baseball_outlook_sunny_gain_humidity_test(_) ->
    ?assertEqual(0.9709505944546686, gain(baseball_outlook_sunny_examples(), humidity)).

baseball_outlook_sunny_gain_temperature_test(_) ->
    ?assertEqual(0.5709505944546686, gain(baseball_outlook_sunny_examples(), temperature)).

baseball_outlook_wind_gain_temperature_test(_) ->
    ?assertEqual(0.01997309402197489, gain(baseball_outlook_sunny_examples(), wind)).



baseball_highest_gain_attribute_test(_) ->
    ?assertEqual(outlook, highest_gain_attribute(baseball_examples())).

baseball_highest_gain_attribute_with_outlook_rain_test(_) ->
    ?assertEqual(wind, highest_gain_attribute(subset(baseball_examples(), outlook, rain))).

baseball_highest_gain_attribute_with_outlook_sunny_test(_) ->
    ?assertEqual(humidity, highest_gain_attribute(subset(baseball_examples(), outlook, sunny))).


