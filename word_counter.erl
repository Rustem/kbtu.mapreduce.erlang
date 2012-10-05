-module(word_counter).
-export([count/1]).
% define import libraries
-import(utilities, [list_files/1, read/1, read/2, printDict/1]).
-import(map_reduce, [start_map_reduce/3]).
%% define constants
-define(PUNCT_RE, ",!-=+|.? \\'").

%% Word frequency word_counter
count(DirName) ->
    CountFiles = list_files(DirName),
    Result = start_map_reduce(CountFiles, fun map_text/3, fun reduce_words/3),
    printDict(Result).

map_text(_Index, FileName, Emit) ->
    io:format("FileName ~p", [FileName]),
    {ok, Words} = read(FileName),
    lists:foreach(fun (Word) -> Emit(Word, 1) end, Words).
% Emit all [K, V] pairs: [K, V] -> [Word, freq] ->[Word, 1]
map_words(_Index, FileName, Emit) ->
    {ok, [Words]} = file:consult(FileName),
    lists:foreach(fun (Word) -> Emit(Word, 1) end, Words).

% Emit for all [K', L[V]] -> [K', Sum(x, L[V])]
reduce_words(Word, Counts, Emit) ->
    Total = lists:foldl(fun(CurTotal, Item) -> CurTotal + Item end, 0, Counts),
    Emit(Word, Total).

