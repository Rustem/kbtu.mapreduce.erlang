-module(inverted_index).
-export([index/1]).

-import(map_reduce, [start_map_reduce/3]).
-import(utilities, [list_files/1, read/1, read/2, printDict/1, query_index/2]).

% FP is the ability to make abstractions on a very low level of code. Basic concepts
% such as looping can thus be ignored.

index(DirName) ->
    CountFiles = list_files(DirName),
    start_map_reduce(CountFiles, fun find_words/3, fun remove_duplicates/3).

% hook methods
find_words(_Index, FileName, Emit) ->
    {ok, Words} = read(FileName),
    lists:foreach(fun (Word) -> 
            if
                Word /= "" -> Emit(Word, FileName);
                Word == "" -> false
            end
        end, Words).

remove_duplicates(Word, FileNames, Emit) ->
    % remove duplicate file names per each word
    UniqueFiles = lists:sort(sets:to_list(sets:from_list(FileNames))),
    lists:foreach(fun (FileName) -> Emit(Word, FileName) end, UniqueFiles).

% very interesting ex
PrepareAlarm = fun(Room) ->
                    io:format("Alarm set in ~s.~n", [Room]),
                    fun() -> io:format("Alarm tripped in ~s! Call Batman!~n", [Room] )
                    end 
               end.

AlarmRead = PrepareAlarm("bathroom").
AlarmRead().