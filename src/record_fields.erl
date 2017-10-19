%%% Copyright (c) 2017 Erik Reitsma <development@ernovation.com>

-module(record_fields).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([get_fields/2, fill_record/3, fill_record/2, fill_record_recurse/2]).
-export([record_to_json/1, json_to_record/1]).
-export([record_diff/2]).
-export([json_subtract/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {record_table}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

json_to_record({struct, Vals}) ->
    case proplists:get_value(erl_type, Vals) of
	"tuple" ->
	    {array, Elements} =
		proplists:get_value(erl_value, Vals, {array,[]}),
	    list_to_tuple(
	      [ json_to_record(E) ||
		  E <- Elements]);
	"record" ->
	    RecordType = 
		list_to_atom(proplists:get_value(erl_record, Vals)),
	    case get_fields(RecordType,
			    length(Vals)-2) of
		{_Empty, FieldNames} ->
		    RecordValues =
			[ json_to_record(
			    proplists:get_value(FieldName, Vals)) ||
			    FieldName <- FieldNames ],
		    list_to_tuple(
			[RecordType | RecordValues]
		     );
		_ ->
		    RecordValues =
			[ json_to_record(Val) ||
			    {_FieldName, Val} <- Vals -- [{erl_record, list_to_atom(RecordType)},
							  {erl_type, "record"}] ],
		    list_to_tuple(
		      [RecordType | RecordValues])
	    end;
	"number" ->
	    case proplists:get_value(erl_value, Vals) of
		N when is_number(N) ->
		    N;
		N when is_list(N) ->
		    case catch list_to_integer(N) of
			I when is_integer(I) ->
			    I;
			_ ->
			    list_to_float(N)
		    end
	    end;
	"string" ->
	    proplists:get_value(erl_value, Vals);
	"list" ->
	    {array, Elements} = proplists:get_value(erl_value, Vals, {array, []}),
	    [json_to_record(Element) || Element <- Elements];
	"atom" ->
	    list_to_atom(proplists:get_value(erl_value, Vals, "undefined"));
	"pid" ->
	    list_to_pid(proplists:get_value(erl_value, Vals));
	"binary" ->
	    {array, Elements} = proplists:get_value(erl_value, Vals, {array, []}),
	    list_to_binary([json_to_record(Element) || Element <- Elements]);
	_ ->
	    %% no tuple or record, make a hash
	    [{Tag, json_to_record(Value)} ||
		{Tag, Value} <- Vals]
    end;
json_to_record({array, Values}) ->
    [json_to_record(Value) || Value <- Values];
json_to_record(Other) ->
    Other.

record_to_json(Record) when is_tuple(Record) ->
    case get_fields(element(1,Record), size(Record)-1) of
	undefined ->
	    {struct,
	     [{erl_type,"tuple"},
	      {erl_value, {array, 
			   [record_to_json(R) || 
			       R <- tuple_to_list(Record)]}}]};
	{_Empty, FieldNames} ->
	    {struct,
	     [{erl_type, "record"},
	      {erl_record, atom_to_list(element(1, Record))}
	      |
	      lists:zip(
		FieldNames,
		[
		 record_to_json(V)
		 ||
		    V <- tl(tuple_to_list(Record))])]}
    end;
record_to_json(Num) when is_number(Num) ->
    {struct, [{erl_type, "number"},
	      {erl_value, Num}]};
record_to_json(L) when is_list(L) ->
    case is_string(L) of
	true ->
	    {struct, 
	     [{erl_type, "string"},
	      {erl_list_value, {array,
			   [record_to_json(V)
			    ||
			       V <- L]}},
	      {erl_value, L}]};
	false ->
	    {struct,
	     [{erl_type, "list"},
	      {erl_value, {array,
			   [record_to_json(V)
			    ||
			       V <- L]}}]}
    end;
record_to_json(A) when is_atom(A) ->
    {struct,
     [{erl_type, "atom"},
      {erl_value, atom_to_list(A)}]};
record_to_json(P) when is_pid(P) ->
    {struct,
     [{erl_type, "pid"},
      {erl_value, pid_to_list(P)}]};
record_to_json(B) when is_binary(B) ->
    {struct,
     [{erl_type, "binary"},
      {erl_hex_value, {array, [byte_to_list(Byte) || <<Byte>> <= B ]}}, 
      {erl_hex_string, lists:append([byte_to_list(Byte) || <<Byte>> <= B ])}, 
      {erl_value, {array, binary_to_list(B)}}]};
record_to_json(Other) ->
    lists:flatten(io_lib:format("~w",[Other])).
	  
byte_to_list(B) when B < 16 ->
    [$0 | erlang:integer_to_list(B, 16)];
byte_to_list(B) ->
    erlang:integer_to_list(B, 16).

get_fields(Type, Size) ->
    case whereis(?SERVER) of
	undefined ->
	    ?MODULE:start_link();
	_ ->
	    ok
    end,
    gen_server:call(?SERVER, {get_fields, Type, Size}).

fill_record(Empty, Fields) ->
    case get_fields(element(1, Empty), size(Empty)-1) of
	undefined ->
	    Empty;
	{_, Names} ->
	    fill_record_fields(Empty, Names, Fields, 2)
    end.

fill_record(Type, Size, Fields) ->
    case get_fields(Type, Size) of
	undefined ->
	    undefined;
	{Empty, Names} ->
	    fill_record_fields(Empty, Names, Fields, 2)
    end.

fill_record_fields(Record, [Name|More], Fields, Index) ->
    case lists:keysearch(Name, 1, Fields) of
	{value, {_, Value}} ->
	    fill_record_fields(setelement(Index, Record, Value),
			       More,
			       Fields,
			       Index+1);
	_ ->
	    fill_record_fields(Record, More, Fields, Index+1)
    end;
fill_record_fields(Record, [], _Fields, _Index) ->
    Record.

fill_record_recurse(Type, Size, Fields) ->
    case get_fields(Type, Size) of
	undefined ->
	    undefined;
	{Empty, Names} ->
	    fill_record_fields_recurse(Empty, Names, Fields, 2)
    end.

fill_record_recurse(Empty, Fields) ->
    %% when a value is {{record, RecordName}, Fields}, construct
    %% another record recursively
    case get_fields(element(1, Empty), size(Empty)-1) of
	undefined ->
	    Empty;
	{_, Names} ->
	    fill_record_fields_recurse(Empty, Names, Fields, 2)
    end.
    
fill_record_fields_recurse(Record, [Name|More], Fields, Index) ->
    case lists:keysearch(Name, 1, Fields) of
	{value, {_, {{record, RecordType},
		     RecordValues}}} when is_list(RecordValues) ->
	    Value = fill_record_recurse(RecordType,
					any,
					RecordValues),
	    fill_record_fields_recurse(setelement(Index, Record, Value),
				       More,
				       Fields,
				       Index+1);	    
	{value, {_, Value}} ->
	    fill_record_fields_recurse(setelement(Index, Record, Value),
				       More,
				       Fields,
				       Index+1);
	_ ->
	    fill_record_fields_recurse(Record, More, Fields, Index+1)
    end;
fill_record_fields_recurse(Record, [], _Fields, _Index) ->
    Record.


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    Table = ets:new(record_fields, []),
    {ok, #state{record_table=Table}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({get_fields, Type, Size}, _From, State) ->
    Reply =
	case ets:lookup(State#state.record_table, {Type, Size}) of
	    [{_, Module}|_] ->
		Module:record_fields(Type);
	    [] ->
		case find_module(Type, Size) of
		    undefined ->
			undefined;
		    Module ->
			ets:insert(State#state.record_table,
				   {{Type, Size}, Module}),
			ets:insert(State#state.record_table,
				   {{Type, any}, Module}),
			Module:record_fields(Type)
		end
	end,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
find_module(Type, Size) ->
    Modules = code:all_loaded(),
    find_module(Type, Size, Modules).

find_module(Type, Size, [{Module, _} | More]) ->
    case catch Module:record_fields(Type) of
	{'EXIT', _Reason} ->
	    find_module(Type, Size, More);
	{Record, _Fields} ->
	    RecordSize = size(Record)-1,
	    if
		Size == any ->
		    Module;
		Size == RecordSize ->
		    Module;
		true ->
		    find_module(Type, Size, More)
	    end
    end;
find_module(_, _, []) ->
    undefined.

is_string([]) ->
    true;
is_string([H|T]) when H>31, H<256 ->
    is_string(T);
is_string(_) ->
    false.

record_diff(R1, R2) ->
    record_diff(R1, R2, []).

record_diff(R1, R1, _) ->
    [];
record_diff(R1, R2, Path) when is_tuple(R1),
			       is_tuple(R2),
			       size(R1) == size(R2),
			       size(R1) > 0,
			       element(1, R1) == element(1, R2) ->
    %% structure itself is good
    case record_fields:get_fields(element(1, R1),
				  size(R1)-1) of
	undefined ->
	    %% record not known, it is just a different tuple
	    [{lists:reverse(Path), R1, R2}];
	{_Empty, Fields} ->
	    lists:append(
	      [record_diff(element(I, R1),
			   element(I, R2),
			   [{element(1, R1), lists:nth(I-1, Fields)} | Path])
	       ||
		  I <- lists:seq(2, size(R1))])
    end;
record_diff(R1, R2, Path) ->
    [{lists:reverse(Path), {R1, R2}}].

json_subtract(R1, R1) ->
    null;
json_subtract({struct, R1}, {struct, R2}) ->
    Subtracted = R1--R2,
    {struct,
     [ {Tag, json_subtract(R1Value, proplists:get_value(Tag, R2))} 
       ||
	 {Tag, R1Value} <- Subtracted
     ]};
json_subtract({array, R1}, {array, R2}) ->
    R2Length = length(R2),
    {array,
     [ json_subtract(lists:nth(I, R1),
		     if
			 I > R2Length ->
			     null;
			 true ->
			     lists:nth(I, R2)
		     end)
       ||
	 I <- lists:seq(1, length(R1))]};
json_subtract(R1, _R2) ->
    R1.

	 
    
