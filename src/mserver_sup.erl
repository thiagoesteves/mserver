%%%-------------------------------------------------------------------
%%% Created : 27 Dec 2019 by Thiago Esteves <calori@gmail.com>
%%%
%%% @doc
%%% This is the XFP top level supervisor where the user can create
%%% or remove mserver gen-servers.
%%% @end
%%%-------------------------------------------------------------------

-module(mserver_sup).

-behaviour(supervisor).

-author('Thiago Esteves').

%%====================================================================
%% API functions
%%====================================================================

-export([start_link/0]).

-export([init/1, create_mserver/1, remove_mserver/1]).

%%--------------------------------------------------------------------
%% Definitions
%%--------------------------------------------------------------------

-define(SERVER, ?MODULE).
-define(MSERVER_TIMEOUT, 5000). % ms
-define(MSERVER_NAME, mserver).

%%====================================================================
%% API functions implementation
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 4,
                 period => 30},
    % comment this line to stop trapping exits
    process_flag(trap_exit, true),
    {ok, {SupFlags, []}}.

%%====================================================================
%% Internal functions
%%====================================================================

create_mserver(Args) ->
  MserverName = compose_mserver_name(Args),
  MserverSpec = {MserverName, { ?MSERVER_NAME, start_link, [[MserverName, Args]]},
        permanent, ?MSERVER_TIMEOUT, worker, [?MSERVER_NAME]},
  supervisor:start_child(?MODULE, MserverSpec).

remove_mserver(Args) ->
  MserverName = compose_mserver_name(Args),
  supervisor:terminate_child(?MODULE, MserverName),
  supervisor:delete_child(?MODULE, MserverName).

%%====================================================================
%% Internal functions
%%====================================================================

i2l(I) -> erlang:integer_to_list(I).
l2a(L) -> erlang:list_to_atom(L).

%% compose gen server name
compose_mserver_name(Id) ->
  Name = "Mserver:"++i2l(Id),
  l2a(Name).
