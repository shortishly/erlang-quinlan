%% Copyright (c) 2014 Peter Morgan <peter.james.morgan@gmail.com>
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

-module(common).
-include_lib("common_test/include/ct.hrl").

-export([
	 all/0,
	 groups/1,
	 tests/1,
	 init_per_suite/1,
	 examples/1
	]).

all() ->
    [{group, main}].

groups(Module) ->
    [{main, [parallel], common:tests(Module)}].


tests(Module) ->
    [Function || {Function, Arity} <- Module:module_info(exports), Arity == 1, is_a_test(Function)].

is_a_test(is_a_test) ->
    false;
is_a_test(Function) ->
    hd(lists:reverse(string:tokens(atom_to_list(Function), "_"))) == "test".




init_per_suite(Config) ->
    [{examples, classify(Config)} | Config].

classify(Config) ->
    [quinlan_id3:classify(Sample, Classification) || {Sample, Classification} <- consult(Config, "sample.terms")].

consult(Config, Filename) ->
    {ok, Terms} = file:consult(?config(data_dir, Config) ++ Filename),
    Terms.

examples(Config) ->
    ?config(examples, Config).
