%%% Copyright (c) 2017 Erik Reitsma <development@ernovation.com>

-module(decoder_tools).

-export([decode_for_record/4,
		 decode_attempt/3]).

decode_attempt(_Module, Message, []) ->
	{todo, Message};
decode_attempt(Module, Message, [{Record, Fields} | More]) ->
	case decode_for_record(Module, Message, Record, Fields) of
		false ->
			decode_attempt(Module, Message, More);
		Result ->
			Result
	end.

decode_for_record(Module, Message, Record, Fields) ->
	decode_for_record(Module, Message, Record, Fields, 2).

decode_for_record(_Module, <<>>, Record, _Fields, _Index) ->
	Record;
decode_for_record(_Module, _Data, Record, [], _Index) ->
	%% we have data left over: assume ok
	Record;
decode_for_record(Module, Data, Record, [Field | MoreFields], Index) ->
	case Module:decode_element(Field, Data) of
		{Value, MoreData} ->
			case element(Index, Record) of
				undefined ->
					%% ok, no need to check, set it
					decode_for_record(Module,
									  MoreData,
									  setelement(Index, Record, Value),
									  MoreFields,
									  Index+1);
				Value ->
					%% it matches the predefined value, ok
					decode_for_record(Module,
									  MoreData,
									  Record,
									  MoreFields,
									  Index+1);
				_ ->
					%% it does not match, this is not a record of this type
					false
			end;
		 Other ->
			error_logger:info_msg("Error decoding field ~p: ~p~n",[Field, Other]),
			%% something wrong decoding the element: must be wrong record
			false
	end.
