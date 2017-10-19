%%% Copyright (c) 2017 Erik Reitsma <development@ernovation.com>

-module(codec_transform).

-export([parse_transform/2]).

parse_transform(Tree, _Options) ->
    transform_nodes(Tree).

transform_nodes(Nodes) ->
    transform_nodes(Nodes, [], []).

transform_nodes([], Nodes, ExtraExports) ->
    {Before, After} = 
	lists:splitwith(fun({function, _, _, _, _}) ->
				false;
			   (_) -> 
				true
			end,
			lists:append(lists:reverse(Nodes))),
    Before ++ lists:append(ExtraExports) ++ After;
transform_nodes([Node|More], Res, ResExports) ->
    {NewExports, NewNodes} =
	transform_node(Node),
    transform_nodes(More, [NewNodes | Res], [NewExports | ResExports]).

transform_node({attribute, Line, multicodec, {Name, Depth, Tree}}) ->
    EncodeName = list_to_atom("encode_"++atom_to_list(Name)),
    DecodeName = list_to_atom("decode_"++atom_to_list(Name)),
    EncodeFunctions = 
	{function, Line, EncodeName, Depth, 
	 make_clauses(Line, encode, Depth, Tree)},
    DecodeFunctions =
	{function, Line, DecodeName, Depth, 
	 make_clauses(Line, decode, Depth, Tree)},
    {[{attribute, Line, export, [
				 {EncodeName,Depth},
				 {DecodeName,Depth}
				]}],
     [
      EncodeFunctions,
      DecodeFunctions
     ]};

transform_node({attribute, Line, codec, {Name, Pairs}}) ->
    EncodeName = list_to_atom("encode_"++atom_to_list(Name)),
    DecodeName = list_to_atom("decode_"++atom_to_list(Name)),

    {[{attribute, Line, export, [{EncodeName,1},{DecodeName,1}]}],

     [{function, Line, EncodeName, 1,
       [{clause, Line, 
	 [make_value(To, Line)], 
	 [], 
	 [make_value(From, Line)]}
	|| {From, To} <- Pairs]
       ++ [{clause, Line, 
	    [{tuple, Line, [{atom, Line, other}, {var, Line, 'Other'}]}],
	    [],
	    [{var, Line, 'Other'}]}]},

      {function, Line, DecodeName, 1,
       [
	{clause, Line, 
	 [make_value(To, Line)], 
	 [], 
	 [make_value(From, Line)]}
	|| {To, From} <- Pairs]
       ++ [{clause, Line, 
	    [{var, Line, 'Other'}], 
	    [], 
	    [{tuple, Line, [{atom, Line, other}, {var, Line, 'Other'}]}]}]}
     ]};
transform_node(Other) ->
    {[],[Other]}.

make_value(T, Line) when is_tuple(T) ->
    {tuple, Line,
     [make_value(V, Line) || V <- tuple_to_list(T)]};
make_value(I, Line) when is_integer(I) ->
    {integer, Line, I};
make_value(A, Line) when is_atom(A) ->
    {atom, Line, A};
make_value(F, Line) when is_float(F) ->
    {float, Line, F}.

make_clauses(Line, Direction, Level, Tree) ->
    make_clauses(Line, Direction, Level, Tree, [], []).

make_clauses(Line, encode, 1, Pairs, FromArgs, ToArgs) ->
    OtherClause = make_other_clause(Line, encode, 1, FromArgs, ToArgs),
    [{clause, Line, 
      lists:reverse([make_value(To, Line) | ToArgs]),
      [],
      [{tuple, Line, lists:reverse([make_value(From, Line) | FromArgs])}]}
     || {From, To} <- Pairs] ++ [OtherClause];

make_clauses(Line, decode, 1, Pairs, FromArgs, ToArgs) ->
    OtherClause = make_other_clause(Line, decode, 1, FromArgs, ToArgs),
    [{clause, Line, 
      lists:reverse([make_value(From, Line) | FromArgs]),
      [],
      [{tuple, Line, lists:reverse([make_value(To, Line) | ToArgs])}]}
     || {From, To} <- Pairs] ++ [OtherClause];

make_clauses(Line, Direction, Level, Tree, FromArgs, ToArgs) ->
    OtherClause = make_other_clause(Line, Direction, Level, FromArgs, ToArgs),
    lists:append([make_clauses(Line,
			       Direction,
			       Level-1,
			       ChildNodes,
			       [make_value(From, Line) | FromArgs],
			       [make_value(To, Line) | ToArgs]) ||
		     {From, To, ChildNodes} <- Tree]) ++ [OtherClause].

make_other_clause(Line, decode, Level, FromArgs, ToArgs) ->
    OtherVars =
	[{var, Line, list_to_atom("Other"++integer_to_list(Count))}
	 ||
	    Count <- lists:seq(1, Level)],

    OtherTuples =
	[{tuple, Line, [{atom, Line, other}, OtherVar]}
	 || OtherVar <- OtherVars],
    {clause, Line,
     lists:reverse(OtherVars ++ FromArgs),
     [],
     [{tuple, Line, lists:reverse(OtherTuples ++ ToArgs)}]};
make_other_clause(Line, encode, Level, FromArgs, ToArgs) ->
    OtherVars =
	[{var, Line, list_to_atom("Other"++integer_to_list(Count))}
	 ||
	    Count <- lists:seq(1, Level)],

    OtherTuples =
	[{tuple, Line, [{atom, Line, other}, OtherVar]}
	 || OtherVar <- OtherVars],

    {clause, Line,
     lists:reverse(OtherTuples ++ ToArgs),
     [],
     [{tuple, Line, lists:reverse(OtherVars ++ FromArgs)}]}.
