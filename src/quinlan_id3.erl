-module(quinlan_id3).
-export([tree/1, classify/2, walk/2]).

-export([entropy/1, gain/2, subset/3, attributes/1, highest_gain_attribute/1]).

-record(example, {attributes = [], classification}).

-record(decision_node, {branch}).
-record(decision_leaf, {classification}).
-record(decision_tag, {attribute, value}).

-type tree() :: #decision_node{} | #decision_leaf{}.


-spec tree(list(#example{})) -> tree().
tree(Examples) ->
    tree(Examples, orddict:new()).

tree([#example{classification = Classification} | _] = Examples, Tree) ->
    case {entropy(Examples), lists:sum([gain(Examples, Attribute) || Attribute <- attributes(Examples)])} of
	{0.0, _} ->
	    #decision_leaf{classification = Classification};

	{_, 0.0} ->
	    #decision_leaf{classification = [classification(Example) || Example <- Examples]};
	    
	_ ->
	    Attribute = highest_gain_attribute(Examples),
	    #decision_node{branch = lists:foldl(fun(Value, A) ->
							orddict:store(#decision_tag{attribute = Attribute, value = Value}, tree(subset(Examples, Attribute, Value)), A)
						end,
						Tree,
						values(Examples, Attribute))}
				
    end.

walk(_, #decision_leaf{classification = Classification}) ->
    Classification;
walk(Example, #decision_node{branch = Branch}) ->
    [#decision_tag{attribute = Key} | _] = orddict:fetch_keys(Branch),
    Value = orddict:fetch(Key, Example),
	    
    case orddict:find(#decision_tag{attribute = Key, value = Value}, Branch) of
	{ok, #decision_leaf{} = Leaf} ->
	    walk(Example, Leaf);
	
	{ok, #decision_node{} = Node} ->
	    walk(Example, Node);
	
	error ->
	    []
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
