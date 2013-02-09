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
    ?assertEqual([{{outlook,overcast},yes},
		  {{outlook,rain},
		   [{{wind,strong},no},{{wind,weak},yes}]},
		  {{outlook,sunny},
		   [{{humidity,high},no},
		    {{humidity,normal},yes}]}], quinlan_id3:tree(baseball_examples())).

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
    ?assertEqual([{{hair,blonde},
		   [{{lotion,no},sunburned},{{lotion,yes},none}]},
		  {{hair,brown},none},
		  {{hair,red},sunburned}], quinlan_id3:tree(sunburn_examples())).


    
    
    
	
    


    
    
