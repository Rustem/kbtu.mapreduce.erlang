-module(utilities).
-export([list_files/1, read/1, read/2, printDict/1, query_index/2, get_documents/1, text_to_words/1]).

-import(lists, [foreach/2]).
-import(string, [to_lower/1, tokens/2]).

-define(PUNCT_RE, " \t\n\r,.;:-!?\"'()").

printDict(Dict) ->
    List = dict:to_list(Dict),
    foreach(    
        fun({K, V}) -> 
            io:format("Item: {~p, ~p} ~n", [K, V]) 
        end, List).

% [{ID, value}]
get_documents(Documents) ->
    [{1, "tur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. "}, 
    {2, "tur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. "}].


list_files(DirName) ->
    {ok, Files} = file:list_dir(DirName),
    % joins each file with dir by dir separator(/)
    FilesWithNames = [filename:join(DirName, File) || File <- Files],
    Indexes = lists:seq(1, length(FilesWithNames)),
    % concat each pair {Index, File} to array of such pairs
    lists:zip(Indexes, FilesWithNames).

prepositions() -> ["aboard", "is", "are", "about", "above", "across", "after", "against", "along", "amid", "among", "anti", "around", "as", "at", "before", "behind", "below", "beneath", "beside", "besides", "between", "beyond", "but", "by", "concerning", "considering", "despite", "down", "during", "except", "excepting", "excluding", "following", "for", "from", "in", "inside", "into", "like", "minus", "near", "of", "off", "on", "onto", "opposite", "outside", "over", "past", "per", "plus", "regarding", "round", "save", "since", "than", "through", "to", "toward", "towards", "under", "underneath", "unlike", "until", "up", "upon", "versus", "via", "with", "within", "without"].

text_to_words(Text) -> 
    CleanData = re:replace(Text, "[^A-Za-z\s]", "", [global, {return, list}]),
    Tokens = tokens(to_lower(CleanData), ?PUNCT_RE),
    Filtered_Tokens = lists:filter(fun (Token) -> not lists:member(Token, prepositions()) end, Tokens),
    {ok, Filtered_Tokens}.

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