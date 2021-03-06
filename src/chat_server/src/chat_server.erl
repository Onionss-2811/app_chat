%% @author tukna
%% @doc @todo Add description to chat_server.


-module(chat_server).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(USER_TABLE, users).

-record(users, {name, node}).
-record(state, {}).


-export([start_link/0, get_users/0, send_msg/4, install/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================

start_link() ->
	io:format("Server started ~n"),
	application:set_env(mnesia, dir, "database"),
	gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

install() ->
	ok = mnesia:create_schema([node()]),
	application:start(mnesia),
	mnesia:create_table(?USER_TABLE,
						[{attributes, record_info(fields, users)},
						{disc_copies, [node()]}
						]).

get_users() ->
	gen_server:cast({global, ?SERVER}, {get_users, self()}).

send_msg(From, ToNode, ToClt, Msg) ->
	io:format("Message from ~p to ~p~n", [From, ToClt]),
	gen_server:call({ToClt, ToNode}, {message, From, Msg}).


%% ====================================================================
%% Internal functions
%% ====================================================================

init([]) ->
	try
		install()
	catch _:_ ->
		io:format("Error when create table ~p~n", [?USER_TABLE])
	end,
	{ok, #state{}}.

%%--------------------------------------------------------------------

handle_call({connect, #users{name = DestClt, node = DestNode} = User},
			_From, State) ->
	io:format("____~p from ~p connecting...~n", [DestClt, DestNode]),

	case mnesia:dirty_read({users, DestClt}) of
		[User] 	->	io:format("~p already exists~n", [DestClt]),
					{reply, {connected, DestClt}, State};

		[] 		-> 	try 
						F = fun() ->
							mnesia:write(User)
						end,
					    mnesia:activity(transaction, F)
					catch _:_ ->
						io:format("~p error~n", [DestClt])
					end,

					io:format("~p join the server", [DestClt]),
					{reply, {connected, DestClt}, State}
	end;

handle_call({send_msg, #users{name = ToClt}, FromU, Msg}, _From, State) ->
	try
	    case [To_user] = mnesia:dirty_read({users, ToClt}) of
		    [{users, ToClt, _}]	->	send_msg(FromU, To_user#users.node, ToClt, Msg),
									{reply, ok, State};

		    []	->	io:format("~p Not found~n", [ToClt]),
		   			{reply, {send_msg, FromU}, State};

		   	_ 	->	io:format("Bad request"),
		   			{reply, {send_msg, FromU}, State}
	    end
	catch _:_ ->
		io:format("Error when send message.~n")
	end;

handle_call({exit, Pid}, _From, State) ->
    {reply, {exit, Pid}, State};

handle_call(Request, _From, State) ->
    error_logger:warning_msg("Bad message: ~p~n", [Request]),
    {reply, {error, unknown_call}, State}.

%%--------------------------------------------------------------------

handle_cast({get_users, _From}, State) ->
	Users = mnesia:dirty_all_keys(?USER_TABLE),
	io:format("List users: ~p~n", [Users]),
	{noreply, State};

handle_cast(Msg, State) ->
    error_logger:warning_msg("Bad message: ~p~n", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------

handle_info(Info, State) ->
    error_logger:warning_msg("Bad message: ~p~n", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------

terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.













