-module(stream).
-export([scan/2]).




%% Opens a file for reading as a stream.
%% @spec open(Filename::string()) -> {ok,io_device()} | {error,Reason}
open(Filename) -> 
	file:open(Filename, [raw, read]).




%% Closes IoDevice.
%% @spec close(io_device()) -> ok | {error,Reason}
close(IoDevice) -> 
	file:close(IoDevice).




%% Scans the named file and applies the passed Action to each character the 
%% stream.
%% @spec scan(Filename::string(), Action::fun()) -> ok | {error,string()}
scan(Filename, Action) -> 
	case open(Filename) of 
		{ok,IoDevice}	->	Result = scan(IoDevice, 0, Action, Action(init)),
							close(IoDevice),
							Result;
		Error			->	Error
	end.


%% @spec scan(Stream::io_device(), Posn::int(), Action::fun()) -> eof | {error,string()}
scan(Stream, Posn, Action, ParamList) -> 
	case get_char(file:pread(Stream, Posn, 1)) of 
		{error,Reason}	->	{error,Reason};
		eof				->	ParamList;
		Data			->	scan(Stream, Posn + 1, Action, Action({{data,Data}, ParamList}))
	end.




%% Returns the character data from a file:pread.
%% @spec get_char({ok,Data}) -> string()
get_char({ok,Data}) -> [Data];


%% Returns the error generated by file:pread.
%% @spec get_char({error,Reason}) -> error,Reason
get_char({error,Reason}) -> {error,Reason};


%% Returns an end-of-file atom (eof) generated by file:pread.
%% @spec get_char(eof) -> eof
get_char(eof) -> eof.

