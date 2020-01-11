%%%-------------------------------------------------------------------
%%% Created : 11 Jan 2020 by Thiago Esteves <thiagocalori@gmail.com>
%%%
%%% @doc
%%% This is a generic gen server that will have multiple instances
%%% @end
%%%-------------------------------------------------------------------

-module(mserver).

-behaviour(gen_server).

-author('Thiago Esteves').

%%====================================================================
%% API functions
%%====================================================================
-export([start_link/1]).

%% gen server exported functions
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% API functions
-export([find_me/1, show_yourself/1]).

%%--------------------------------------------------------------------
%% Definitions
%%--------------------------------------------------------------------

-define(SERVER, ?MODULE).

%%%===================================================================
%%% INTERFACE API
%%%===================================================================

start_link([MserverName, Args]) ->
  gen_server:start_link({local, MserverName}, ?MODULE, [Args], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Args]) ->
  %% Register Gproc name
  gproc:reg({p, l, {?MODULE, Args}}),
% comment this line to stop trapping exits
  process_flag(trap_exit, true), % comment this line to stop trapping exits
  {ok, Args}.

handle_cast({find_me}, State) ->
  io:format("[CAST] I'm here, State: ~p \n\r ", [State]),
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_call({find_me}, _From, State) ->
  io:format("[CALL] I'm here, State: ~p \n\r ", [State]),
  {reply, ok, State};

handle_call(_Msg, _From, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

%% @private
terminate(_Reason, _State) ->
  gproc:goodbye().

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Exported Mserver functions
%%%===================================================================

-spec find_me(integer()) -> { ok | error , integer() }.
find_me(Args) ->
  gproc_send(call, Args, {find_me}).

-spec show_yourself(integer()) -> { ok | error , integer() }.
show_yourself(Args) ->
  gproc_send(cast, Args, {find_me}).

%%--------------------------------------------------------------------
%% @private Send a gen_server:call message if the PID is found
%%--------------------------------------------------------------------
gproc_send(call, Args, Msg) ->
  Key = {?MODULE, Args},
  case gproc:lookup_pids({p, l, Key}) of
    [Pid] -> gen_server:call(Pid, Msg);
    _ -> {error, invalid_mserver}
  end;

gproc_send(cast, Args, Msg) ->
  Key = {?MODULE, Args},
  case gproc:lookup_pids({p, l, Key}) of
    [Pid] -> gen_server:cast(Pid, Msg);
    _ -> {error, invalid_mserver}
  end.
