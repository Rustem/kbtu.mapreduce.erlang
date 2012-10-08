-module(map_reduce).
-export([start_map_reduce/3]).
-import(lists, [foreach/2]).


%% Input = [{K, V}]
%% Map(K, V, Emit) -> Emit a stream of {K2, V2} tuples
%% Reduce(K2, List[V2], Emit) -> Emit a stream of {K2, V2} tuples
start_map_reduce(Input, Map, Reduce) -> 
    ParentPid = self(),
    Pid = spawn(fun() -> master(ParentPid, Map, Reduce, Input) end),
    receive
        {Pid, Result} -> Result % wait for mapreduce result and returns
    end.

%% master is responsible for:
%%  - delegate job to mappers
%%  - collect all mapper results
%%  - delegate job to reducers
%%  - collect all reduce results
%%  - send result to a master
%%  - link all mappers and reducers (fault - tolerance)
print(Text) ->
    io:format("Notification: ~p", [Text]).

master(ParentPid, Map, Reduce, Input) ->
    process_flag(trap_exit, true), % when we trap_exit the process is not terminate
    MasterPid = self(),
    
    % create mappers and delegate job one per each
    spawn_mappers(MasterPid, Map, Input), 
    M = length(Input),

    % Wait for mappers to terminate
    MapResult = supervise(M, dict:new(), {Map, Input}),
    print("Map finished. ~n"),
    % create reducers and delegate job one per each
    spawn_reducers(MasterPid, Reduce, dict:to_list(MapResult)),
    R = dict:size(MapResult),
    % Wait for reducers to terminate
    ReduceResult = supervise(R, dict:new(), {Reduce, dict:to_list(MapResult)}),
    print("Reduce finished. ~n"),
   % print("Reduce finished."),
    ParentPid ! {self(), ReduceResult}.

% spawn_mappers is a decorator to spawn_workers
spawn_mappers(MasterPid, Map, Input) -> 
    spawn_workers(MasterPid, Map, Input).

%% spawn_reducers is a decorator to spawn_workers
spawn_reducers(MasterPid, Reduce, Input) ->
    spawn_workers(MasterPid, Reduce, Input).

%% spawn_workers is responsible for:
%%   - emit intermediate key, value
spawn_workers(MasterPid, Fun, Input) ->
    foreach(fun({K, V}) -> 
                spawn_link(fun() -> worker(MasterPid, Fun, {K, V}) end)
            end, Input).
    
% Must send {K2, V2} and terminate
worker(MasterPid, Fun, {K, V}) ->
    Fun(K, V, fun(K2, V2) -> MasterPid ! {K2, V2} end).

%% collect and merge {K, V} messages from N-num of processes
%% Process type is Map or Reduce, depending on task type
supervise(0, Dict, _) -> Dict;
supervise(N, Dict, {Fun, Input}) ->
    receive
        {K, V} ->
            Dict1 = dict:append(K, V, Dict),
            supervise(N, Dict1, {Fun, Input});
        {'EXIT', _Who, normal} ->
            supervise(N-1, Dict, {Fun, Input});
        {'EXIT', _Who, _Why} ->
            spawn_link(fun() -> worker(self(), Fun, Input) end),
            io:format("process restarted!~n"),
            supervise(N, Dict, {Fun, Input})
    end.