%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2015 - 2016, Jim Rosenblum
%%% @doc Parse transform implementing a pipe-line function.
%%% pipe(P1, Fn1(...), Fn2(...), Fn3(...)) will result in
%%% Fn1 being called with the initial parameter, P1, being used as a 
%%% parameter for Fn1. That output is used as an input parameter for Fn2 and 
%%% so on.
%%% 
%%% In all cases, a result is used as the first free parameter slot in the
%%% next function. An example should help descipher that last sentance.
%%%
%%%    pipe("test", 
%%%	    list_to_binary(), 
%%%	    byte_size(_), 
%%%	    add(4,_,5),
%%%	    integer_to_list())
%%% will be transformed to
%%% integer_to_list(add(4,(byte_size(list_to_binary("test"))),5))
%%%
%%% An alternative syntax has been  hacked-in such that if the compile option
%%% {pipe, true} is supplied, the source file will be re-scanned converting
%%%
%%% |> "test" |> list_to_binary |> etc() :| 
%%% into 
%%%
%%% pipe("test", list_to_binary(), etc())
%%%
%%% This might not work depending on other parse transforms and included
%%% macros. Be careful.
%%%
%%% @version {@version}
%%% @end
%%% Created : 14 October 2015 by Jim Rosenblum
%%% ----------------------------------------------------------------------------

-module(pipeline).
-export([parse_transform/2, format_error/1]).


%% Typedefs
-type options() :: [{atom(), any()}].


%% -----------------------------------------------------------------------------
%% Implements the actual transformation at compile time.
%%
-spec parse_transform(erl_parse:abstract_forms(), options()) -> 
			     erl_parse:abstract_forms().

parse_transform(Forms, Options) ->
    NewForms = case alternative_syntax(Options) of
	true ->
	    FileName = get_file_name(Forms),
	    transform(FileName);
	false ->
	    Forms
    end,
    lists:append(lists:map(fun transform_form/1, NewForms)).


alternative_syntax(Options) ->
    proplists:lookup(pipe, Options) == {pipe, true}.


%% -----------------------------------------------------------------------------
%% Takes an error code returned by one of the other functions in the module 
%% and creates a textual description of the error. 
%%
-spec format_error({error, module(), term()}) -> io_lib:chars().

format_error(E) ->
    case io_lib:deep_char_list(E) of
        true ->
            E;
        _ ->
            io_lib:write(E)
    end.


%% -----------------------------------------------------------------------------
%% Only concerned with looking within 'function' forms.
%%
transform_form({function, Line, Name, Arity, Body}) ->
    [{function, Line, Name, Arity, trx_function(Body)}];

transform_form(Form) ->
    [Form].


%% -----------------------------------------------------------------------------
%% The pipe directive is a function, so only look for it in 'function clauses', 
%% leave the rest alone.
%%
trx_function(BodyForms) ->
    lists:map(fun({clause, Line, L1, L2, Body}) ->
		      {clause, Line, L1, L2, trx_exp(Body,[])};
		 (Other)  ->
		      Other
	      end, BodyForms).


%% -----------------------------------------------------------------------------
%% Currently one can use pipe in: block, call, case, catch, if, fun, list 
%% comprehension, match, operator, try, try of, and unary operator form 
%% expressions. Look for pipe in one of those places.
%%
trx_exp([], Acc) ->
    lists:reverse(Acc);

trx_exp([{match, Line, P, E_0} | Tl], Acc) ->
    trx_exp(Tl, [{match, Line, 
		   hd(trx_exp([P],[])), 
		   hd(trx_exp([E_0],[]))} | Acc]);

% Skipping var, tuple, nil, cons, bin, 
trx_exp([{op, Line, Op, E_1, E_2} | Tl], Acc) ->
    trx_exp(Tl, [{op, Line, Op, 
		   hd(trx_exp([E_1],[])), 
		   hd(trx_exp([E_2],[]))} | Acc]);

trx_exp([{op, Line, Op, E_0} | Tl], Acc) ->
    trx_exp(Tl, [{op, Line, Op, 
		   hd(trx_exp([E_0],[]))} | Acc]);

% Skipping record, map

trx_exp([{'catch', Line, E_0} | Tl], Acc) ->
    trx_exp(Tl, [{'catch', Line, hd(trx_exp([E_0], []))} | Acc]);

% The Call we care about is the pipe(...) call. Re-write it as
% nested functions.
%
trx_exp([{call, Line, {atom, _Line2, pipe}, [Ps |Fns]}|Tl], Acc) ->
    put(args, Ps),
    trx_exp(Tl, [hd(pipe_body(lists:reverse(Fns), Line))| Acc]);

% skip other calls 

trx_exp([{lc, Line, E_0, Generators} | Tl], Acc) ->
    trx_exp(Tl, [{lc, Line, 
		   hd(trx_exp([E_0],[])), 
		   Generators} | Acc]);

% skip binary comprehensions

trx_exp([{block, Line, B} | Tl], Acc) ->
    trx_exp(Tl, [{block, Line, 
		  trx_exp(B, [])} | Acc]);

trx_exp([{'if', Line, Ics} | Tl], Acc) ->
    trx_exp(Tl, [{'if', Line, trx_exp(Ics, [])} | Acc]);

trx_exp([{'case', Line, E_0, Cs} | Tl], Acc) ->
    trx_exp(Tl, [{'case', Line, 
		  hd(trx_exp([E_0],[])), 
		  trx_exp(Cs, [])} | Acc]);

trx_exp([{'try', Line, B, Cs, Ds, []} | Tl], Acc) ->
    trx_exp(Tl, [{'try', Line,
		  trx_exp(B, []),
		  trx_exp(Cs, []),
		  trx_exp(Ds, []),
		  []} | Acc]);

% skip receive

trx_exp([{'fun', Line, {clauses, Clauses}}|Tl], Acc) ->
    trx_exp(Tl, [{'fun', Line, {clauses, 
				trx_exp(Clauses,[])}} | Acc]);

% skip named fun, query and field

trx_exp([{clause, L, Ps, Gs, B} | Tl], Acc) ->
    trx_exp(Tl, [{clause, L, Ps, Gs, trx_exp(B, [])} | Acc]);

trx_exp([Elt|Tl], Acc) ->
    trx_exp(Tl, [Elt|Acc]);

trx_exp(Elt, Acc) ->
    trx_exp([], [Elt|Acc]).


%% -----------------------------------------------------------------------------
%% Nest the return value of one Fn into the free parameter slot of the next Fn.
%% Two types of function calls: F() and M:F()
%%

pipe_body([{call, _Line, {remote, _Line, F, M}, A} | []], NewLine) ->
    Args = get(args),
    [{call, NewLine, {remote, NewLine, F, M}, insert(A, [], Args, NewLine)}];

pipe_body([{call, _Line,{atom, _Line, F}, A}|[]], NewLine) ->
    Args = get(args),
    [{call, NewLine, {atom, NewLine, F}, insert(A, [], Args, NewLine)}];

pipe_body([{call, _Line, {remote, _Line, F, M}, A}|Tl], NewLine) ->
    [{call, NewLine, {remote, NewLine, F, M}, 
      insert(A, [], hd(pipe_body(Tl, NewLine+1)), NewLine)}];

pipe_body([{call, _Line,{atom, _Line, F}, A}|Tl], NewLine)->
    [{call, NewLine, {atom, NewLine, F}, 
      insert(A, [], hd(pipe_body(Tl, NewLine + 1)), NewLine)}].


%% -----------------------------------------------------------------------------
%% Iterate through prameters looking for the first one that is _. Replace its
%% value with P and update its line number. If there are not parameters, then
%% the fn must be arity 1 and create the parameter tuple.
%%%
insert([], [], P, NewLine) ->
    [setelement(2, P, NewLine)];

insert([], Acc, _P, _NewLine) ->
    lists:reverse(Acc);

insert([{var, _Line, '_'} | Tl], Acc, P, NewLine) ->
    insert(Tl, [setelement(2, P, NewLine) | Acc], P, NewLine);

insert([H|Tl], Acc, P, NewLine) ->
    insert(Tl, [setelement(2, H, NewLine)|Acc], P, NewLine).



%%% -----------------------------------------------------------------------------
%%% Functions to support the alternative suntax which requires re-tokenizing the
%%% file, replacing the |> ... :| tokens with pipe(....).
%%% -----------------------------------------------------------------------------


% Parse out the name of the source file so that it can be scanned.
get_file_name([{attribute, 1, file, {FileName,_}} | _]) ->    
    FileName;
get_file_name([_|Tl]) -> 
    get_file_name(Tl).
		     
    
%% -----------------------------------------------------------------------------
%% Re-scan the file converting the list of tokens such that the |> ... :|
%% syntax is re-written into the pipe(....) tokens
%%
transform(File) ->
    {ok, B} = file:read_file(File),
    Forms = scan(erl_scan:tokens([],binary_to_list(B),1),[]),
    F2 = lists:map(fun(AForm) -> form_transform(AForm, [], false) end, Forms),
    F = fun(X) ->
		{ok,Y} = erl_parse:parse_form(X),
		Y end,
    [F(X) || X <- F2].


scan({done, {ok, Tokens, EndLoc}, LeftOver}, Acc) ->
    scan(erl_scan:tokens([], LeftOver, EndLoc), [Tokens | Acc]);
scan(_, Acc) ->
    lists:reverse(Acc).

% looke for the token and rewrite them as necessary.
form_transform([], Acc, _Started) ->    
    lists:reverse(Acc);

form_transform([{'|', Line}, {'>' ,_} | Tl], Acc, false) -> 
    form_transform(Tl, [{'(', Line}, {atom, Line, pipe}	| Acc], true);

form_transform([{'|', Line},{'>',_} |Tl], Acc, true) -> 
    form_transform(Tl, [{',', Line} | Acc], true);

form_transform([{':', Line}, {'|', _} | Tl], Acc, true) -> 
    form_transform(Tl, [{')', Line} | Acc], false);

form_transform([H | Tl], Acc, Started) ->
    form_transform(Tl, [H | Acc], Started).
