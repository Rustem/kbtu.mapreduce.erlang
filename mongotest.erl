% Я взял репо с https://github.com/TonyGen/mongodb-erlang
% По установке
% надо сделать следующее
% git clone git://github.com/TonyGen/bson-erlang.git bson
% git clone git://github.com/TonyGen/mongodb-erlang.git mongodb
% cd bson
% erlc -o ebin -I include src/*.erl
% cd ../mongodb
% erlc -o ebin -I include -I .. src/*.erl
% cd ..

% для того чтоб erl запускался без зависимостей (erl -pa bson/ebin mongodb/ebin) надо сделать
% erl
% code:lib_dir().
% потом в терминале
% mkdir <path>/bson
% cp -R bson/ebin/ <path>/bson/
% mkdir <path>/mongodb
% cp -R mongodb/ebin/ <path>/mongodb/

% потом erl и вперед, только надо сразу делать application:start (mongodb).



-module(mongotest).
% -import(mongo, [connect/1]).
-export([select/1, put/3, get_value/2, extract_value/2, clear/2, delete_preps/2, prepositions/0]).

prepositions() -> ["aboard", "about", "above", "across", "after", "against", "along", "amid", "among", "anti", "around", "as", "at", "before", "behind", "below", "beneath", "beside", "besides", "between", "beyond", "but", "by", "concerning", "considering", "despite", "down", "during", "except", "excepting", "excluding", "following", "for", "from", "in", "inside", "into", "like", "minus", "near", "of", "off", "on", "onto", "opposite", "outside", "over", "past", "per", "plus", "regarding", "round", "save", "since", "than", "through", "to", "toward", "towards", "under", "underneath", "unlike", "until", "up", "upon", "versus", "via", "with", "within", "without"].

% select all from collection C
select(C) ->
	Host = {localhost, 27017},
	{ok, Conn} = mongo:connect (Host),
	{ok, Cursor} = mongo:do(safe, master, Conn, erlang, fun() -> 
		mongo:find(C, {}) end),
	mongo_cursor:rest(Cursor). % produces list of documents

% insert into collection C document with key K and value V
put(C, K, V) ->
	Host = {localhost, 27017},
	{ok, Conn} = mongo:connect (Host),
	{ok, Data} = mongo:do(safe, master, Conn, erlang, fun() -> 
		mongo:insert(C, {K, V}) end),
	Data.

% concatenating all the values to one text
get_value(C, K) ->
	Docs = select(C),
	lists:foldl(fun(X, Sum) -> string:concat(Sum, string:concat(extract_value(K, X), " ")) end, "", Docs).

% getting value with key K from document Doc
extract_value(K, Doc) ->
	bson:at(K, Doc).

% delete punctuation
clear(C, K) ->
	Text = get_value(C, K),
	% re:replace(Text, "[^\w\s]", "").
	re:replace(Text, "[^A-Za-z\s]", "", [global, {return, list}]).

% delete prepositions
delete_preps(C, K) -> 
	Clear_text = clear(C, K),
	lists:foldl(fun(X, Sum) -> re:replace(Sum, X, "", [global, {return, list}]) end, Clear_text, prepositions()).