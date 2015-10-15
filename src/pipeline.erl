%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2015 - 2016, Jim Rosenblum
%%% @doc Parse transform implementing a pipe-lining operator.
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

parse_transform(Forms, _Options) ->
    io:format("~p~n",[Forms]),
    A = lists:append(lists:map(fun transform_form/1, Forms)),
    io:format("~p~n",[A]),
    A.



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
%% Only concerned with looking within 'function' declarations
%%
transform_form({function, Line, Name, Arity, Body}) ->
    [{function, Line, Name, Arity, [trx_function(Body, [])]}];

transform_form(Form) ->
    [Form].


%% -----------------------------------------------------------------------------
%% Only concenred about 'function clauses', ignore the rest
%%
trx_function([], Acc) -> 
    hd(lists:reverse(Acc));

trx_function([{clause, Line, L1, L2, Body} | Tl], Acc) ->
    trx_function(Tl, [{clause, Line, L1, L2, trx_exp(Body,[])} | Acc]);

trx_function([Hd|Tl], Acc) ->
    trx_function(Tl, [Hd|Acc]).


%% -----------------------------------------------------------------------------
%% pipe is valid in: try, try of, list comprehension, case, block, operator,
%% match, fun and call forms expressions.
%%
trx_exp([], Acc) ->
    lists:reverse(Acc);

trx_exp([{clause, L, V, [], B} | Tl], Acc) ->
    trx_exp(Tl, [{clause, L, V, [], trx_exp(B, [])} | Acc]);

trx_exp([{'try', Line, B, [], Cs, []} | Tl], Acc) ->
    trx_exp(Tl, [{'try', Line,
		  trx_exp(B, []),
		  [],
		  trx_exp(Cs, []),
		  []} | Acc]);

trx_exp([{'try', Line, B, Cs, Ds, []} | Tl], Acc) ->
    trx_exp(Tl, [{'try', Line,
		  trx_exp(B, []),
		  trx_exp(Cs, []),
		  trx_exp(Ds, []),
		  []} | Acc]);
     
trx_exp([{lc, Line, Thing, Generators} | Tl], Acc) ->
    trx_exp(Tl, [{lc, Line, 
		   hd(trx_exp([Thing],[])), 
		   Generators} | Acc]);

trx_exp([{'case', Line, Exp, Exps} | Tl], Acc) ->
    trx_exp(Tl, [{'case', Line, 
		  hd(trx_exp([Exp],[])), 
		  trx_exp(Exps, [])} | Acc]);

trx_exp([{block, Line, Thing} | Tl], Acc) ->
    trx_exp(Tl, [{block, Line, 
		  trx_exp(Thing, [])} | Acc]);

trx_exp([{op, Line, Op, L1, L2} | Tl], Acc) ->
    trx_exp(Tl, [{op, Line, Op, 
		   hd(trx_exp([L1],[])), 
		   hd(trx_exp([L2],[]))} | Acc]);

trx_exp([{match, Line, L1, L2} | Tl], Acc) ->
    trx_exp(Tl, [{match, Line, 
		   hd(trx_exp([L1],[])), 
		   hd(trx_exp([L2],[]))} | Acc]);

trx_exp([{'fun', Line, {clauses, Clauses}}|Tl], Acc) ->
    trx_exp(Tl, [{'fun', Line, {clauses, 
				trx_exp(Clauses,[])}} | Acc]);

trx_exp([{call, Line, {atom, _Line2, pipe}, [Ps |Fns]}|Tl], Acc) ->
    put(param, Ps),
    trx_exp(Tl, [hd(pipe_body(lists:reverse(Fns), Line))| Acc]);

trx_exp([Elt|Tl], Acc) ->
    trx_exp(Tl, [Elt|Acc]);

trx_exp(Elt, Acc) ->
    trx_exp([], [Elt|Acc]).


%% -----------------------------------------------------------------------------
%% Nest the return value of one Fn into the free parameter slot of the next Fn.
%%
pipe_body([{call, _Line,{atom, _Line2, Fn}, Params}|[]], Ref) ->
    [{call, Ref, {atom, Ref, Fn}, insert(Params, [], get(param), Ref)}];

pipe_body([{call, _Line,{atom, _Line2, Fn}, Params}|Tl], Ref)->
    [{call, Ref, {atom, Ref, Fn}, 
      insert(Params, [], hd(pipe_body(Tl, Ref+1)), Ref)}].


%% -----------------------------------------------------------------------------
%% when inserting a parameter into the form, do so in the first '_' space
%% updating its line number as appropriate.
%%
insert([], [], P, Ref) ->
    [setelement(2, P, Ref)];

insert([], Acc, _P, _Ref) ->
    lists:reverse(Acc);

insert([{var, _Line, '_'} | Tl], Acc, P, Ref) ->
    insert(Tl, [setelement(2,P,Ref)|Acc], P, Ref);

insert([H|Tl], Acc, P, Ref) ->
    insert(Tl, [setelement(2, H, Ref)|Acc], P, Ref) .



    






    
    
    




