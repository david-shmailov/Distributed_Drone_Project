
-module(gui_interface_server).

-behaviour(gen_server).

-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {socket = undefined}).
-define(RETRY_DELAY, 5000).



start_link(Host, Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Host, Port], []).



init([Host, Port]) ->
    connect_with_retry(Host, Port, ?RETRY_DELAY).

connect_with_retry(Host, Port, Delay) ->
    case gen_tcp:connect(Host, Port, [binary, {active, false}]) of
        {ok, Socket} -> 
            {ok, #state{socket = Socket}};
        {error, econnrefused} ->
            io:format("Connection refused. Retrying in ~p milliseconds.~n", [Delay]),
            timer:sleep(Delay),
            connect_with_retry(Host, Port, Delay);
        {error, OtherReason} ->
            io:format("Error: ~p~n", [OtherReason]),
            {stop, OtherReason}
    end.


handle_call({send_data, Data}, _From, #state{socket = Socket} = State) ->
    case gen_tcp:send(Socket, Data) of
        ok -> {reply, ok, State};
        {error, Reason} -> {reply, {error, Reason}, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{socket = Socket}) ->
    gen_tcp:close(Socket),
    ok.



code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions