%%% Copyright (c) 2017 Erik Reitsma <development@ernovation.com>

-module(record_fields_transform).

-export([parse_transform/2]).

parse_transform(Tree, _Options) ->
	Records = 
		[R ||
			{attribute, _Line, record,
			 {R, _}} <- Tree],
    transform_nodes(Tree, Records).

transform_nodes(Nodes, Records) ->
	lists:append([transform_node(Node, Records) || Node <- Nodes]).

transform_node({attribute, Line, record_fields, _}, Records) ->
	[{function, Line, record_fields, 1,
	  [{clause, Line, 
		[{atom, Line, Record}],
		[],
		[{tuple, Line,
		  [{record, Line, Record, []},
		   {call, Line, 
			{atom, Line, record_info},
			[{atom, Line, fields}, {atom, Line, Record}]}]}]
	   } ||
		  Record <- Records]}];
transform_node(Other, _Records) ->
	[Other].
