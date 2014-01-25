-module(ipcam_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0]).

-export([highest_gain_attribute_test/1, upgrade_htmls_ok_entropy_test/1]).

-import(quinlan_id3, [classify/2, entropy/1, subset/3, highest_gain_attribute/1]).

all() ->
    [highest_gain_attribute_test,
     upgrade_htmls_ok_entropy_test].



highest_gain_attribute_test(Config) ->
    ?assertEqual(<<"upgrade_htmls.cgi">>, highest_gain_attribute(cameras(Config))).

upgrade_htmls_ok_entropy_test(Config) ->
    ?assertEqual(0.0, entropy(subset(cameras(Config), <<"upgrade_htmls.cgi">>, ok))).


cameras(Config) ->
    [classify(tenvis_jpt3815w_2013(Config), tenvis), 
     classify(foscam_f18906w(Config), foscam),
     classify(loftek_nexus_543(Config), foscam)].


loftek_nexus_543(Config) ->
    consult(Config, "Loftek-Nexus-543.terms").

tenvis_jpt3815w_2013(Config) ->
    consult(Config, "Tenvis-JPT3815W-2013.terms").

foscam_f18906w(Config) ->
    consult(Config, "Foscam-F18906W.terms").

consult(Config, Name) ->
    {ok, [Terms]} = file:consult(?config(data_dir, Config) ++ Name),
    Terms.
    
