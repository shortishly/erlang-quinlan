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

-module(ipcam_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() ->
    common:all().

groups() ->
    common:groups(?MODULE).


highest_gain_attribute_test(Config) ->
    <<"upgrade_htmls.cgi">> = quinlan_id3:highest_gain_attribute(cameras(Config)).

upgrade_htmls_ok_entropy_test(Config) ->
    0.0 = quinlan_id3:entropy(quinlan_id3:subset(cameras(Config), <<"upgrade_htmls.cgi">>, ok)).


cameras(Config) ->
    [quinlan_id3:classify(tenvis_jpt3815w_2013(Config), tenvis), 
     quinlan_id3:classify(foscam_f18906w(Config), foscam),
     quinlan_id3:classify(loftek_nexus_543(Config), foscam)].


loftek_nexus_543(Config) ->
    consult(Config, "Loftek-Nexus-543.terms").

tenvis_jpt3815w_2013(Config) ->
    consult(Config, "Tenvis-JPT3815W-2013.terms").

foscam_f18906w(Config) ->
    consult(Config, "Foscam-F18906W.terms").

consult(Config, Name) ->
    {ok, [Terms]} = file:consult(?config(data_dir, Config) ++ Name),
    Terms.
    
