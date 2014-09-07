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

-module(quinlan_id3).

-export([
	 tree/1, 
	 classify/2, 
	 walk/2
	]).

-export([
	 entropy/1, 
	 gain/2, 
	 subset/3, 
	 attributes/1, 
	 highest_gain_attribute/1
	]).

-export_type([
	      example/0,
	      tree/0
	     ]).


-record(example, {
	  attributes = orddict:new() :: orddict:orddict(), 
	  classification :: term()
	 }).

-opaque example() :: #example{}.

-record(decision_node, {
	  branch
	 }).

-type decision_node() :: #decision_node{}.

-record(decision_leaf, {
	  classification
	 }).

-type decision_leaf() :: #decision_leaf{}.

-record(decision_tag, {
	  attribute, 
	  value
	 }).

-opaque tree() :: decision_node() | decision_leaf().


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



-spec walk(orddict:orddict(), tree()) -> term().
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

-spec gain(list(example()), term()) -> float().
gain(Examples, Attribute) ->
    lists:foldl(fun(Value, A) ->
			Matching = subset(Examples, Attribute, Value),
			A - (length(Matching)/length(Examples)) * entropy(Matching)
		end,
		entropy(Examples),
		values(Examples, Attribute)).


-spec subset(list(example()), term(), term()) -> list(example()).
subset(Examples, Attribute, Value) ->
    lists:filter(fun(#example{attributes = Attributes}) ->
			 case orddict:find(Attribute, Attributes) of
			     {ok, Value} ->
				 true;
			     _ ->
				 false
			 end
		 end, Examples).


-spec values(list(example()), term()) -> list(term()).
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


-spec attributes(list(example())) -> list(term()).
attributes(Examples) ->
    ordsets:to_list(lists:foldl(fun(#example{attributes = Attributes}, A) ->
					ordsets:union(A, ordsets:from_list(orddict:fetch_keys(Attributes)))
				end,
				ordsets:new(),
				Examples)).


-spec highest_gain_attribute(list(example())) -> term().
highest_gain_attribute(Examples) ->
    {_, HighestGain} = gb_trees:largest(lists:foldl(fun(Attribute, A) ->
							    gb_trees:enter(gain(Examples, Attribute), Attribute, A)
						    end,
						    gb_trees:empty(),
						    attributes(Examples))),
    HighestGain.
		      


-spec classify(orddict:orddict(), term()) -> example().
classify(Attributes, Classification) ->
    #example{attributes = orddict:from_list(Attributes), classification = Classification}.



-spec classification(example()) -> term().
classification(#example{classification = Classification}) ->
    Classification.


-spec entropy(list(example())) -> float().
entropy(Examples) ->
    Log2 = logN(2),
    ordsets:fold(fun(Example, A) ->
			 Frequency = f(Example, Examples),
			 A - Frequency * Log2(Frequency)
		end, 0, m(Examples)).

-spec m(list(example())) -> ordsets:ordset(term()).
m(S) ->
    ordsets:from_list([Classification || #example{classification = Classification} <- S]).


-spec f(example(), list(example())) -> float().
f(J, S) ->
    length(matching(J, S)) / length(S).


-spec matching(example(), list(example())) -> list(example()).
matching(J, S) ->
    lists:filter(match(J), S).


-spec match(term()) -> fun((example()) -> boolean()).
match(X) ->
    fun(#example{classification = Classification}) ->
	    X == Classification
    end.


-spec logN(number()) -> fun((number()) -> float()).
logN(N) ->
    Denominator = math:log(N),
    fun(X) ->
	    math:log(X) / Denominator
    end.
