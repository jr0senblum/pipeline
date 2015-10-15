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
    lists:append(lists:map(fun transform_form/1, Forms)).



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
%% Only concenred about 'function' abstract form, ignore the rest
%%
transform_form({function, Line, Name, Arity, Body}) ->
    [{function, Line, Name, Arity, [mod_clause(Body, [])]}];

transform_form(Form) ->
    [Form].


%% -----------------------------------------------------------------------------
%% Only concenred about 'clauses', ignore the rest
%%
mod_clause([], Acc) -> 
    hd(lists:reverse(Acc));

mod_clause([{clause, Line, L1, L2, Body} | Tl], Acc) ->
    mod_clause(Tl, [{clause, Line, L1, L2, mod_body(Body,[])} | Acc]);

mod_clause([Hd|Tl], Acc) ->
    mod_clause(Tl, [Hd|Acc]).


%% -----------------------------------------------------------------------------
%% pipe can be part of lc, operator, match, fun or call forms.
%%
mod_body([], Acc) ->
    lists:reverse(Acc);


mod_body([{lc, Line, Thing, Generators} | Tl], Acc) ->
    mod_body(Tl, [{lc, Line, 
		   hd(mod_body([Thing],[])), 
		   Generators} | Acc]);

mod_body([{op, Line, Op, L1, L2} | Tl], Acc) ->
    mod_body(Tl, [{op, Line, Op, 
		   hd(mod_body([L1],[])), 
		   hd(mod_body([L2],[]))} | Acc]);

mod_body([{match, Line, L1, L2} | Tl], Acc) ->
    mod_body(Tl, [{match, Line, 
		   hd(mod_body([L1],[])), 
		   hd(mod_body([L2],[]))} | Acc]);

mod_body([{'fun', Line, {clauses, Clause}}|Tl], Acc) ->
    mod_body(Tl, [{'fun', Line, {clauses, 
				 [{clause, L, V, [], 
				   mod_body(Rest,[])} || 
				 {clause, L, V, [], Rest} <- Clause]}} | Acc]);

mod_body([{call, Line, {atom, _Line2, pipe}, [Ps |Fns]}|Tl], Acc) ->
    put(param, Ps),
    mod_body(Tl, [hd(pipe_body(lists:reverse(Fns), Line))| Acc]);

mod_body([Elt|Tl], Acc) ->
    mod_body(Tl, [Elt|Acc]);

mod_body(Elt, Acc) ->
    mod_body([], [Elt|Acc]).


%% -----------------------------------------------------------------------------
%% nest the return value of one as the parameter of the next
%%
pipe_body([{call, _Line,{atom, _Line2, Fn}, Params}|[]], Ref) ->
    [{call, Ref, {atom, Ref, Fn}, insert(Params, [], get(param), Ref)}];

pipe_body([{call, _Line,{atom, _Line2, Fn}, Params}|Tl], Ref)->
    [{call, Ref, {atom, Ref, Fn}, 
      insert(Params, [], hd(pipe_body(Tl, Ref+1)), Ref)}].


%% -----------------------------------------------------------------------------
%% when inserting one as the parameter of the next, do so in the first
%% '_' space
%%
insert([], [], P, Ref) ->
    [setelement(2,P,Ref)];

insert([], Acc, _P, _Ref) ->
    lists:reverse(Acc);

insert([{var, _Line, '_'} | Tl], Acc, P, Ref) ->
    insert(Tl, [setelement(2,P,Ref)|Acc], P, Ref);

insert([H|Tl], Acc, P, Ref) ->
    insert(Tl, [setelement(2, H, Ref)|Acc], P, Ref) .



    






    
    
    




