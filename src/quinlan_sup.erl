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


-module(quinlan_sup).

-behaviour(supervisor).

-export([
	 start_link/0
	]).

-export([
	 init/1
	]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).



-spec init([]) -> {ok, {{supervisor:strategy(), 5, 10}, [supervisor:child_spec()]}}.
init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.

