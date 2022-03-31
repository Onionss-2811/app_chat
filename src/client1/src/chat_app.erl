%% @author tukna
%% @doc @todo Add description to chat_app.


-module(chat_app).

-behaviour(application).

-export([start/2, stop/1]).

%% ====================================================================
%% API functions
%% ====================================================================

start(_Type, _StartArgs) ->
	chat_sup:start_link().

stop(_State) ->
	ok.


