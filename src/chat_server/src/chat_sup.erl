%% @author tukna
%% @doc @todo Add description to chat_sup.


-module(chat_sup).

-behaviour(supervisor).

-define(SERVER, ?MODULE).

-export([start_link/0]).

-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).



%% ====================================================================
%% Internal functions
%% ====================================================================

init([]) ->
	AChild = {chat_server,{chat_server, start_link, []},
	permanent, 2000, worker, [chat_server]},
	{ok,{{one_for_all,1,1}, [AChild]}}.

