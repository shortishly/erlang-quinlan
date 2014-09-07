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

-module(vote_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() ->
    common:all().

groups() ->
    common:groups(?MODULE).



with_training_data_highest_gain_attribute_test(Config) ->
    physician_fee_freeze = quinlan_id3:highest_gain_attribute(classify(training_data(Config))).

with_test_data_highest_gain_attribute_test(Config) ->
    physician_fee_freeze = quinlan_id3:highest_gain_attribute(classify(testing_data(Config))).

train_and_check_against_original_training_data_test(Config) ->
    {300, 300} = evaluate_tree(quinlan_id3:tree(classify(training_data(Config))), training_data(Config)).

train_and_check_against_test_data_test(Config) ->
    {126, 135} = evaluate_tree(quinlan_id3:tree(classify(training_data(Config))), testing_data(Config)).

train_with_test_data_but_check_against_original_training_data_test(Config) ->
    {279, 300} = evaluate_tree(quinlan_id3:tree(classify(testing_data(Config))), training_data(Config)).

evaluate_tree(Tree, Data) ->
    {lists:foldl(score(Tree), 0, Data), length(Data)}.

score(Tree) ->
    fun({Sample, Classification}, A) ->
	    case quinlan_id3:walk(Sample, Tree) of
		Classification ->
		    A+1;
		_ ->
		    A
	    end
    end.
    

read_file(Config, Filename) ->
    {ok, Binary} = file:read_file(?config(data_dir, Config) ++ Filename),
    Binary.

init_per_suite(Config) ->
    [{training, training_data(Config)}, {testing, testing_data(Config)} | Config].


classify([{_Sample, _Classification} | _] = Data) ->
    [quinlan_id3:classify(Sample, Classification) || {Sample, Classification} <- Data].

training_data(Config) ->
    vote(read_file(Config, "vote.data")).

testing_data(Config) ->
    vote(read_file(Config, "vote.test")).


vote(<<Attributes:31/bytes, ",republican.\n", Remainder/binary>>) ->
    [vote(Attributes, republican) | vote(Remainder)];
vote(<<Attributes:31/bytes, ",democrat.\n", Remainder/binary>>) ->
    [vote(Attributes, democrat) | vote(Remainder)];
vote(<<>>) ->
    [].


vote(<<Attributes:31/bytes>>, Classification) ->
    {orddict:from_list(lists:zip([handicapped_infants, 
				  water_project_cost_sharing,
				  adoption_of_the_budget_resolution,
				  physician_fee_freeze,
				  el_salvador_aid,
				  religious_groups_in_schools,
				  anti_satellite_test_ban,
				  aid_to_nicaraguan_contras,
				  mx_missile,
				  immigration,
				  synfuels_corporation_cutback,
				  education_spending,
				  superfund_right_to_sue,
				  crime,
				  duty_free_exports,
				  export_administration_act_south_africa],
				 binary:split(Attributes, <<",">>, [global]))),
     Classification}.
