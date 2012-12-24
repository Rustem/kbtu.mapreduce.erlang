-module(tag_generator).
-export([count/1, token_iterator/2]).
% define import libraries
-import(utilities, [list_files/1, read/1, read/2, printDict/1, get_documents/1, text_to_words/1]).
-import(map_reduce, [start_map_reduce/3]).
%% define constants
-define(PUNCT_RE, " \t\n\r,.;:-!?\"'()").

%% Word frequency word_counter
% 
count(ListOfDocuments) ->
    CountFiles = get_documents(ListOfDocuments),
    Result = start_map_reduce(CountFiles, fun get_mapping_tags/3, fun tag_popularity/3).
    % printDict(Result).


token_iterator([], Emit) -> false;

token_iterator([Token | Words], Emit) ->
    if
        Token /= "" -> Emit(Token, 1);
        Token == "" -> false
    end,
    token_iterator(Words, Emit).

get_mapping_tags(_Index, EssayText, Emit) ->
    {ok, Words} = text_to_words(EssayText),
    io:format("~n Words ~p ~n ", [Words]),
    token_iterator(Words, Emit).

% Emit for all [K', L[V]] -> [K', Sum(x, L[V])]
tag_popularity(Word, Counts, Emit) ->
    io:format("Word ~p", [Word]),
    Total = lists:foldl(fun(CurTotal, Item) -> CurTotal + Item end, 0, Counts),
    Emit(Word, Total).

