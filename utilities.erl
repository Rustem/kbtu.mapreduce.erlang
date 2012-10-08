-module(utilities).
-export([list_files/1, read/1, read/2, printDict/1, query_index/2]).

-import(lists, [foreach/2]).
-import(string, [to_lower/1, tokens/2]).

-define(PUNCT_RE, ",!-=+|.? \\'").

printDict(Dict) ->
    List = dict:to_list(Dict),
    foreach(    
        fun({K, V}) -> 
            io:format("Item: {~p, ~p} ~n", [K, V]) 
        end, List).


list_files(DirName) ->
    {ok, Files} = file:list_dir(DirName),
    % joins each file with dir by dir separator(/)
    FilesWithNames = [filename:join(DirName, File) || File <- Files],
    Indexes = lists:seq(1, length(FilesWithNames)),
    % concat each pair {Index, File} to array of such pairs
    lists:zip(Indexes, FilesWithNames).

%% parse a File to words by removing punctuation
read(File) ->
    {ok, IODevice} = file:open(File, [read]),
    TokensToProcess = read(IODevice, []),
    file:close(IODevice),
    {ok, TokensToProcess}.
read(File, L) ->
    try
        case file:read_line(File) of
            {ok, Data} ->
                Tokens = tokens(to_lower(Data), ?PUNCT_RE),
                read(File, Tokens ++ L);
            eof ->
                L
        end
    catch
        Ex:Type -> {Ex, Type, erlang:get_stacktrace()}
    end.

% utility funct
query_index(Index, Word) ->
    dict:find(Word, Index).