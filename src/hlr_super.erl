-module(hlr_super).
-behaviour(supervisor).

% Supervisor for the HLR

-export([start_link/1]).

% internal exports
-export([init/1]).

-define(SERVER, ?MODULE).

% externals
start_link(Args) ->
    supervisor:start_link({global, ?SERVER}, ?MODULE, []).


% server bits

% init/1
% returns: {ok, {SupFlags, [ChildSpec]}}
%          ignore
%          {error, Reason}
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    RestartInterval = 3600, % max 1000 restarts in 3600 seconds
    
    SupFlags = {RestartStrategy, MaxRestarts, RestartInterval},
    
    ChildSpecs = [
		  {hlr_server,
		   {hlr_server, start_link, []},
		   permanent,
		   1000, % watchdog timer, in milliseconds
		   worker,
		   [hlr_server]}
		 ],
     {ok, {SupFlags, ChildSpecs}}.
