-module(rib).
% -export([parse/1]).
-compile(export_all).




%% Print, to stdout, each character in the named RIB file.
print_chars(Filename) -> 
	Action = fun(Char) -> io:format("Char: '~s'~n", [Char]) end,
	
	stream:scan(Filename, Action).




%% Print, to stdout, each character in the named RIB file.
print_bytes(Filename) -> 
	Action = fun(Byte) -> io:format("Byte: '~w'~n", [Byte]) end,
	
	stream:scan(Filename, Action).




%% Validates the named RIB file.
validate(Filename) -> 
	Action = fun(_X) -> ok end,
	
	stream:scan(Filename, Action).




%% Read the RIB file and reconstruct it as a string.
read(Filename) -> 
	Action = fun(X) -> read_stream(X) end,
	
	{buffer,Buffer} = stream:scan(Filename, Action),
	lists:flatten(lists:reverse(Buffer)).




%% Initialises the stream processor.
%% Returns the required ParamList data structure.
read_stream(init) -> 
	io:format("Initializing the Action~n"),
	{buffer,[]};

%% Main stream processor action.
%% Returns a copy of the updated structure.
read_stream({{data,Data},{buffer,Buffer}}) -> {buffer,[Data|Buffer]}.




%% Parse the RIB file, creating a list of commands.
parse(Filename) -> 
	Action = fun(X) -> parse_stream(X) end,
	
	case stream:scan(Filename, Action) of 
		{_, {command,Command}, _}	->	{ok,lists:reverse(Command)};
		Error									->	Error
	end.




%% Initialises the stream processor.
%% Returns the required ParamList data structure.
parse_stream(init) -> 
	io:format("Initializing the Parser~n"),
	{{buffer,[]}, {command,[]}, {mode,default}};

%% Main stream processor action.
%% Returns a copy of the 
parse_stream({{data,[[Char]]},{{buffer,Buffer}, {command,Command}, {mode,Mode}}}) -> 
	parse_buffer(Char, Buffer, Command, Mode).




%% 
parse_buffer(Char, Buffer, Command, default) when 
		(Char == $\s) or (Char == $\t) or (Char == $\r) or (Char == $\n) -> 
	
	String = lists:reverse(Buffer),
	
	case mode(String) of 
		command	->	{{buffer,[]}, {command,[{erlang:list_to_atom(String), {params,[]}}|Command]}, {mode,command}};
		_		->	{{buffer,[]}, {command,Command}, {mode,default}}
	end;




% === Commands. ================================================================


% ====== Strings.======
parse_buffer($", Buffer, Command, command) -> 
	{{buffer,Buffer}, {command,Command}, {mode,string}};

parse_buffer($", Buffer, Command, string) -> 
	{{buffer,Buffer}, {command,Command}, {mode,command}};


% ====== Arrays. ======
parse_buffer($[, Buffer, Command, command) -> 
	io:format("Array begin~n"),
	{{buffer,[Buffer]}, {command, {Command, {array,[]}}}, {mode,array}};


parse_buffer(Char, Buffer, CommandAndArray, array) when 
		(Char == $\s) or (Char == $\t) or (Char == $\r) or (Char == $\n) -> 
		
	% io:format("~nAdd Field: '~s'~n", [Field]),
	{Command, {array,Array}} = CommandAndArray,
	{{buffer,[]}, {command, {Command, {array,[Buffer|Array]}}}, {mode,array}};


parse_buffer($], _Buffer, CommandAndArray, array) -> 
	io:format("Array end~n"),
	{[{Name, {params,Params}}|T], {array,Array}} = CommandAndArray,
	{{buffer,[]}, {command,[{Name, {params,[lists:reverse(Array)|Params]}}|T]}, {mode,command}};


parse_buffer(Char, [Buffer], Command, array) -> 
	io:format("Parse Field: ~s", [[Char]]),
	{{buffer,[Char|Buffer]}, {command,Command}, {mode,array}};


% ====== Names. ======
parse_buffer(Char, Buffer, Command, command) when 
		(Char == $\s) or (Char == $\t) or (Char == $\r) or (Char == $\n) -> 
	
	String = lists:reverse(Buffer),
	
	case mode(String) of 
		command	->	[{Name, {params,Params}}|T] = Command,
					parse_buffer(Char, Buffer, [{Name, {params,lists:reverse(Params)}}|T], default);
		skip	->	parse_buffer(Char, Buffer, Command, skip);
		_		->	[{Name, {params,Params}}|T] = Command,
					{{buffer,[]}, {command,[{Name, {params,[String|Params]}}|T]}, {mode,command}}
	end;




% === Comments. ================================================================

parse_buffer($#, _Buffer, Command, _Mode) -> 
	{{buffer,[]}, {command,Command}, {mode,comment}};


parse_buffer(Char, Buffer, Command, comment) when (Char == $\r) or (Char == $\n) -> 
	{{buffer,Buffer}, {command,Command}, {mode,default}};


parse_buffer(_Char, Buffer, Command, comment) -> 
	{{buffer, Buffer}, {command,Command}, {mode,comment}};




% === Fallbacks. ===============================================================

parse_buffer(_Char, Buffer, Command, skip) -> 
	{{buffer,Buffer}, {command,Command}, {mode,command}};

parse_buffer(Char, Buffer, Command, Mode) -> 
	{{buffer,[Char|Buffer]}, {command,Command}, {mode,Mode}}.




% ===  =========================================================================

%% Returns an atom representing the scanning mode for the context buffer string.
%% @spec mode(Buffer::string()) -> atom()
mode("#") -> comment;

mode("Display") -> command();
mode("Format") -> command();
mode("Projection") -> command();
mode("ShadingRate") -> command();

mode("Translate") -> command();
mode("Scale") -> command();
mode("Rotate") -> command();
mode("Skew") -> command();
mode("ReverseOrientation") -> command();

mode("WorldBegin") -> command();
mode("WorldEnd") -> command();
mode("TransformBegin") -> command();
mode("TransformEnd") -> command();

mode("Surface") -> command();

mode("LightSource") -> command();
	
mode("Polygon") -> command();

mode([]) -> skip;

mode(_) -> 
	default.




%% Returns the 'command' atom() for RIB commands.
%% @spec command() -> command
command() -> command.




printTuple(Tuple) ->
	lists:foreach(fun({Tag, Value}) -> io:format("{~w, \"~s\"}", [Tag, Value]) end, Tuple),
	io:format("~n").
