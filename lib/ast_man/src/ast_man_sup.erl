%%%-------------------------------------------------------------------
%%% File    : ast_man_sup.erl
%%% Author  : Anders Nygren <anders@telteq.com.mx>
%%% Description : 
%%%
%%% Created : 29 Mar 2006 by Anders Nygren <anders@telteq.com.mx>
%%%-------------------------------------------------------------------
%% @private

-module(ast_man_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link(_StartArgs) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    AstEvt = {ast_evt,{ast_man_events,start_link,[]},
	      permanent,2000,worker,[ast_man_events]},
    AstMan = {ast_man,{ast_manager,start_link,[]},
	      permanent,2000,worker,[ast_manager]},
    AstDrv = {ast_drv,{ast_man_drv,start_link,[]},
	      permanent,2000,worker,[ast_man_drv]},
    {ok,{{one_for_all,100,1000}, [AstDrv,AstEvt,AstMan]}}.

%%====================================================================
%% Internal functions
%%====================================================================
