-module(quinlan_id3).
-export([tree/1, classify/2]).

-record(example, {attributes = [], classification}).

tree(Examples) ->
    tree(Examples, orddict:new()).

tree([#example{classification = Classification} | _] = Examples, Tree) ->
    case {entropy(Examples), lists:sum([gain(Examples, Attribute) || Attribute <- attributes(Examples)])} of
	{0.0, _} ->
	    Classification;

	{_, 0.0} ->
	    [classification(Example) || Example <- Examples];
	    
	_ ->
	    Attribute = highest_gain_attribute(Examples),
	    lists:foldl(fun(Value, A) ->
				orddict:store({Attribute, Value}, tree(subset(Examples, Attribute, Value)), A)
			end,
			Tree,
			values(Examples, Attribute))
				
    end.




gain(Examples, Attribute) ->
    lists:foldl(fun(Value, A) ->
			Matching = subset(Examples, Attribute, Value),
			A - (length(Matching)/length(Examples)) * entropy(Matching)
		end,
		entropy(Examples),
		values(Examples, Attribute)).

subset(Examples, Attribute, Value) ->
    lists:filter(fun(#example{attributes = Attributes}) ->
			 case orddict:find(Attribute, Attributes) of
			     {ok, Value} ->
				 true;
			     _ ->
				 false
			 end
		 end, Examples).

values(Examples, Attribute) ->
    ordsets:to_list(lists:foldl(fun(#example{attributes = Attributes}, A) ->
					case orddict:find(Attribute, Attributes) of
					    {ok, Value} ->
						ordsets:add_element(Value, A);
					    error ->
						A
					end
				end, 
				ordsets:new(),
				Examples)).

attributes(Examples) ->
    ordsets:to_list(lists:foldl(fun(#example{attributes = Attributes}, A) ->
					ordsets:union(A, ordsets:from_list(orddict:fetch_keys(Attributes)))
				end,
				ordsets:new(),
				Examples)).

highest_gain_attribute(Examples) ->
    {_, HighestGain} = gb_trees:largest(lists:foldl(fun(Attribute, A) ->
							    gb_trees:enter(gain(Examples, Attribute), Attribute, A)
						    end,
						    gb_trees:empty(),
						    attributes(Examples))),
    HighestGain.
		      
				

classify(Attributes, Classification) ->
    #example{attributes = orddict:from_list(Attributes), classification = Classification}.


classification(#example{classification = Classification}) ->
    Classification.


-spec entropy(list(#example{})) -> float().
entropy(Examples) ->
    Log2 = logN(2),
    ordsets:fold(fun(Example, A) ->
			 Frequency = f(Example, Examples),
			 A - Frequency * Log2(Frequency)
		end, 0, m(Examples)).

m(S) ->
    ordsets:from_list([Classification || #example{classification = Classification} <- S]).

f(J, S) ->
    length(matching(J, S)) / length(S).

matching(J, S) ->
    lists:filter(match(J), S).

match(X) ->
    fun(#example{classification = Classification}) ->
	    X == Classification
    end.

logN(N) ->
    Denominator = math:log(N),
    fun(X) ->
	    math:log(X) / Denominator
    end.
			
    


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

n(N, X) ->
    lists:map(fun(_) -> X end, lists:seq(1, N)).

entropy_test() ->
    Yes = classify([], yes),
    No = classify([], no),
    ?assertEqual(0.9402859586706309, entropy(n(9, Yes) ++ n(5, No))).

entropy_perfectly_classified_test() ->
    Yes = classify([], yes),
    ?assertEqual(0.0, entropy([Yes])),
    ?assertEqual(0.0, entropy([Yes, Yes])).

entropy_totally_random_test() ->
    Yes = classify([], yes),
    No = classify([], no),
    ?assertEqual(1.0, entropy([Yes, No])).



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

wind_gain_test() ->
    ?assertEqual(0.04812703040826927, gain(wind_examples(), wind)).

entropy_wind_weak_test() ->
    ?assertEqual(0.8112781244591328, entropy(subset(wind_examples(), wind, weak))).

entropy_wind_strong_test() ->
    ?assertEqual(1.0, entropy(subset(wind_examples(), wind, strong))).
    



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

baseball_attributes_test() ->
    ?assertEqual([humidity, outlook, temperature, wind], attributes(baseball_examples())).

baseball_entropy_test() ->
    ?assertEqual(0.9402859586706309, entropy(baseball_examples())).

baseball_gain_outlook_test() ->
    ?assertEqual(0.2467498197744391, gain(baseball_examples(), outlook)).

baseball_gain_temperature_test() ->
    ?assertEqual(0.029222565658954647, gain(baseball_examples(), temperature)).

baseball_gain_humidity_test() ->
    ?assertEqual(0.15183550136234136, gain(baseball_examples(), humidity)).

baseball_gain_wind_test() ->
    ?assertEqual(0.04812703040826927, gain(baseball_examples(), wind)).

baseball_outlook_sunny_examples() ->
    subset(baseball_examples(), outlook, sunny).

baseball_outlook_sunny_gain_humidity_test() ->
    ?assertEqual(0.9709505944546686, gain(baseball_outlook_sunny_examples(), humidity)).

baseball_outlook_sunny_gain_temperature_test() ->
    ?assertEqual(0.5709505944546686, gain(baseball_outlook_sunny_examples(), temperature)).

baseball_outlook_wind_gain_temperature_test() ->
    ?assertEqual(0.01997309402197489, gain(baseball_outlook_sunny_examples(), wind)).



baseball_highest_gain_attribute_test() ->
    ?assertEqual(outlook, highest_gain_attribute(baseball_examples())).

baseball_highest_gain_attribute_with_outlook_rain_test() ->
    ?assertEqual(wind, highest_gain_attribute(subset(baseball_examples(), outlook, rain))).

baseball_highest_gain_attribute_with_outlook_sunny_test() ->
    ?assertEqual(humidity, highest_gain_attribute(subset(baseball_examples(), outlook, sunny))).

%%
%% Sunburn examples
%%
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


sunburn_highest_gain_attribute_test() ->
    ?assertEqual(hair, highest_gain_attribute(sunburn_examples())).

sunburn_with_hair_blonde_test() ->
    Blonde = subset(sunburn_examples(), hair, blonde),
    ?assertEqual(1.0, entropy(Blonde)),
    ?assertEqual(lotion, highest_gain_attribute(subset(sunburn_examples(), hair, blonde))).

sunburn_with_hair_red_test() ->
    Red = subset(sunburn_examples(), hair, red),
    ?assertEqual(0.0, entropy(Red)).

sunburn_with_hair_brown_test() ->
    Brown = subset(sunburn_examples(), hair, brown),
    ?assertEqual(0.0, entropy(Brown)).


%%
%% Golfing
%%

golfing_examples() ->
    [classify([{outlook, Outlook},
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

golfing_entropy_test() ->
    ?assertEqual(0.9402859586706309, entropy(golfing_examples())).

golfing_outlook_gain_test() ->
    ?assertEqual(0.2467498197744391, gain(golfing_examples(), outlook)).

golfing_windy_gain_test() ->
    ?assertEqual(0.04812703040826932, gain(golfing_examples(), windy)).


%%
%% Stock market
%%
stock_examples() ->
    [classify([{age, Age},
	       {competition, Competition},
	       {type, Type}],
	      Profit) || {Age,
			  Competition,
			  Type,
			  Profit} <- [{old, yes, swr, down},
				      {old, no, swr, down},
				      {old, no, hwr, down},
				      {mid, yes, swr, down},
				      {mid, yes, hwr, down},
				      {mid, no, hwr, up},
				      {mid, no, swr, up},
				      {new, yes, swr, up},
				      {new, no, hwr, up},
				      {new, no, swr, up}]].


stock_entropy_test() ->
    ?assertEqual(1.0, entropy(stock_examples())).


adm_examples() ->
    [quinlan_id3:classify([{macd_is_bearish_divergence, BearishDivergence},
			   {macd_is_bullish_divergence, BullishDivergence},
			   {macd_trigger,Trigger},
			   {macd_value,Value},
			   {macd_value_vs_trigger, ValueVsTrigger},
			   {macd_value_vs_zero,ValueVsZero},
			   {price_close, Close},
			   {price_high, High},
			   {price_low, Low},
			   {price_open, Open},
			   {price_volume, Volume}], 
			  Decision) || {BearishDivergence,
					BullishDivergence,
					Trigger,
					Value,
					ValueVsTrigger,
					ValueVsZero,
					Close,
					High,
					Low,
					Open,
					Volume,
					Decision} <-  [{true,false,falling,falling,below,below,rising,falling,rising,falling,falling, [{buy,1},{neutral,1}]},
						       {true,false,falling,falling,below,below,rising,falling,rising,rising,falling, [{neutral,1}]},
						       {true,false,falling,falling,below,below,rising,rising,rising,falling,falling, [{neutral,1}]},
						       {true,false,falling,falling,below,below,rising,rising,rising,rising,falling, [{buy,1},{neutral,1}]}]].



adm_entrophy_test() ->
    ?assertEqual(1.0, entropy(adm_examples())).

adm_highest_gain_test() ->
    ?assertEqual(price_volume, highest_gain_attribute(adm_examples())).

adm_highest_gain_values_test() ->
    ?assertEqual([falling], values(adm_examples(), price_volume)).

adm_highest_gain_value_subset_test() ->
    ?assertEqual(adm_examples(), subset(adm_examples(), price_volume, falling)).

adm_attributes_test() -> 
    ?assertEqual([macd_is_bearish_divergence,
		  macd_is_bullish_divergence, macd_trigger, macd_value,
		  macd_value_vs_trigger, macd_value_vs_zero, price_close,
		  price_high, price_low, price_open, price_volume],
		 attributes(adm_examples())).

adm_gain_macd_is_bearish_divergence_test() ->
    ?assertEqual(0.0, gain(adm_examples(), macd_is_bearish_divergence)).

adm_gain_macd_is_bullish_divergence_test() ->
    ?assertEqual(0.0, gain(adm_examples(), macd_is_bullish_divergence)).

adm_gain_macd_trigger_test() ->
    ?assertEqual(0.0, gain(adm_examples(), macd_trigger)).

adm_gain_macd_value_test() ->
    ?assertEqual(0.0, gain(adm_examples(), macd_value)).

adm_gain_macd_value_vs_trigger_test() ->
    ?assertEqual(0.0, gain(adm_examples(), macd_value_vs_trigger)).

adm_gain_macd_value_vs_zero_test() ->
    ?assertEqual(0.0, gain(adm_examples(), macd_value_vs_zero)).

adm_gain_price_close_test() ->
    ?assertEqual(0.0, gain(adm_examples(), price_close)).

adm_gain_price_high_test() ->
    ?assertEqual(0.0, gain(adm_examples(), price_high)).

adm_gain_price_low_test() ->
    ?assertEqual(0.0, gain(adm_examples(), price_low)).

adm_gain_price_open_test() ->
    ?assertEqual(0.0, gain(adm_examples(), price_open)).

adm_gain_price_volume_test() ->
    ?assertEqual(0.0, gain(adm_examples(), price_volume)).

-endif.
