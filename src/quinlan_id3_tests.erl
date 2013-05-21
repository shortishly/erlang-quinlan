-module(quinlan_id3_tests).
-include_lib("eunit/include/eunit.hrl").

baseball_examples() ->
    [quinlan_id3:classify([{outlook, Outlook}, 
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

baseball_tree_test() ->
    Tree = quinlan_id3:tree(baseball_examples()),
    ?assertEqual(yes, quinlan_id3:walk([{outlook, overcast}], Tree)),
    ?assertEqual(no, quinlan_id3:walk([{outlook, rain}, {wind, strong}], Tree)),
    ?assertEqual(yes, quinlan_id3:walk([{outlook, rain}, {wind, weak}], Tree)),
    ?assertEqual(no, quinlan_id3:walk([{humidity, high}, {outlook, sunny}], Tree)),
    ?assertEqual(yes, quinlan_id3:walk([{humidity, normal}, {outlook, sunny}], Tree)).

sunburn_examples() ->
    [quinlan_id3:classify([{hair, Hair},
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


sunburn_tree_test() ->
    Tree = quinlan_id3:tree(sunburn_examples()),
    ?assertEqual(sunburned, quinlan_id3:walk([{hair, red}], Tree)),
    ?assertEqual(none, quinlan_id3:walk([{hair, brown}], Tree)),
    ?assertEqual(sunburned, quinlan_id3:walk([{hair, blonde}, {lotion, no}], Tree)),
    ?assertEqual(none, quinlan_id3:walk([{hair, blonde}, {lotion, yes}], Tree)).

wmh_example() ->
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
					Decision} <-  [{false,false,rising,rising,above,above,rising,rising,rising,rising,rising,neutral},
						       {false,false,rising,rising,above,above,rising,rising,rising,rising,rising,neutral},
						       {false,false,rising,rising,above,above,rising,rising,rising,rising,rising,neutral},
						       {false,false,rising,rising,below,above,rising,rising,rising,rising,rising,neutral},
						       {false,false,rising,rising,below,above,rising,rising,rising,rising,falling,neutral},
						       {false,false,rising,rising,below,above,rising,rising,rising,rising,falling,neutral},
						       {false,false,rising,rising,below,above,rising,rising,rising,rising,falling,neutral},
						       {false,false,rising,rising,below,above,rising,rising,rising,rising,falling,neutral},
						       {false,false,rising,rising,below,above,rising,rising,rising,rising,falling,neutral},
						       {false,false,rising,rising,above,above,rising,rising,rising,rising,rising,sell},
						       {false,false,rising,rising,above,above,rising,rising,rising,rising,rising,sell},
						       {false,false,rising,rising,above,above,rising,rising,rising,rising,rising,sell},
						       {false,false,rising,rising,above,above,rising,rising,rising,rising,rising,sell},
						       {false,false,rising,rising,above,above,rising,rising,rising,rising,rising,sell},
						       {false,false,rising,rising,above,above,rising,rising,rising,rising,rising,sell},
						       {false,false,rising,rising,above,above,rising,rising,rising,rising,rising,sell},
						       {false,false,rising,rising,below,above,rising,rising,rising,rising,rising,neutral}]].
					 

wmh_15_test() ->
    Tree = quinlan_id3:tree(lists:nthtail(15, wmh_example())),
    ?assertEqual(sell, quinlan_id3:walk([{macd_value_vs_trigger, above}], Tree)),
    ?assertEqual(neutral, quinlan_id3:walk([{macd_value_vs_trigger, below}], Tree)).

wmh_10_test() ->
    Tree = quinlan_id3:tree(lists:nthtail(10, wmh_example())),
    ?assertEqual(sell, quinlan_id3:walk([{macd_value_vs_trigger, above}], Tree)),
    ?assertEqual(neutral, quinlan_id3:walk([{macd_value_vs_trigger, below}], Tree)).

wmh_5_test() ->
    Tree = quinlan_id3:tree(lists:nthtail(5, wmh_example())),
    ?assertEqual(sell, quinlan_id3:walk([{macd_value_vs_trigger, above}], Tree)),
    ?assertEqual(neutral, quinlan_id3:walk([{macd_value_vs_trigger, below}], Tree)).

wmh_4_test() ->
    Tree = quinlan_id3:tree(lists:nthtail(4, wmh_example())),
    ?assertEqual(sell, quinlan_id3:walk([{macd_value_vs_trigger, above}], Tree)),
    ?assertEqual(neutral, quinlan_id3:walk([{macd_value_vs_trigger, below}], Tree)).

wmh_3_test() ->
    Tree = quinlan_id3:tree(lists:nthtail(3, wmh_example())),
    ?assertEqual(sell, quinlan_id3:walk([{macd_value_vs_trigger, above}], Tree)),
    ?assertEqual(neutral, quinlan_id3:walk([{macd_value_vs_trigger, below}], Tree)).

wmh_2_test() ->
    Tree = quinlan_id3:tree(lists:nthtail(2, wmh_example())),
    ?assertEqual([neutral, sell, sell, sell, sell, sell, sell, sell], quinlan_id3:walk([{macd_value_vs_trigger, above}], Tree)),
    ?assertEqual(neutral, quinlan_id3:walk([{macd_value_vs_trigger, below}], Tree)).




adm_test() ->
    ?assertEqual({decision_leaf, [[{buy,1},{neutral,1}],
				  [{neutral,1}],
				  [{neutral,1}],
				  [{buy,1},{neutral,1}]]}, 
		 quinlan_id3:tree([{example,[{macd_is_bearish_divergence,true},
					     {macd_is_bullish_divergence,false},
					     {macd_trigger,falling},
					     {macd_value,falling},
					     {macd_value_vs_trigger,below},
					     {macd_value_vs_zero,below},
					     {price_close,rising},
					     {price_high,falling},
					     {price_low,rising},
					     {price_open,falling},
					     {price_volume,falling}],
				    [{buy,1},{neutral,1}]},
				   {example,[{macd_is_bearish_divergence,true},
					     {macd_is_bullish_divergence,false},
					     {macd_trigger,falling},
					     {macd_value,falling},
					     {macd_value_vs_trigger,below},
					     {macd_value_vs_zero,below},
					     {price_close,rising},
					     {price_high,falling},
					     {price_low,rising},
					     {price_open,rising},
					     {price_volume,falling}],
				    [{neutral,1}]},
				   {example,[{macd_is_bearish_divergence,true},
					     {macd_is_bullish_divergence,false},
					     {macd_trigger,falling},
					     {macd_value,falling},
					     {macd_value_vs_trigger,below},
					     {macd_value_vs_zero,below},
					     {price_close,rising},
					     {price_high,rising},
					     {price_low,rising},
					     {price_open,falling},
					     {price_volume,falling}],
				    [{neutral,1}]},
				   {example,[{macd_is_bearish_divergence,true},
					     {macd_is_bullish_divergence,false},
					     {macd_trigger,falling},
					     {macd_value,falling},
					     {macd_value_vs_trigger,below},
					     {macd_value_vs_zero,below},
					     {price_close,rising},
					     {price_high,rising},
					     {price_low,rising},
					     {price_open,rising},
					     {price_volume,falling}],
				    [{buy,1},{neutral,1}]}])).
