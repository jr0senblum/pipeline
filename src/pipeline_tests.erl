-module(pipeline_tests).
-compile({parse_transform, pipeline}).
-include_lib("eunit/include/eunit.hrl").

paren_test() ->
    A = (pipe("test", list_to_binary(), byte_size())),
    ?_assert(A == 4).

if_test() ->
    Cond = 1,
    A = if 
	    Cond == 1 -> 
		pipe("test", list_to_binary(), byte_size());
	    false -> 
		pipe("testnot", list_to_binary(), byte_size())
	end,
    ?_assert(A == 4).

catch_test()->
	Catch = (catch pipe("test",
			    list_to_binary(), 
			    byte_size(), 
			    add(_,0))),
	?_assert(Catch == 4).

catch_error_test() ->
    {'EXIT', {Bad, _}} = (catch pipe("test",
				   list_to_binary(), 
				   byte_size(), 
				   add(_,0)) + a),
	?_assert(Bad == badaarith).

neg_test() ->
   Neg = -pipe("test",
		  list_to_binary(), 
		  byte_size(), 
		  add(_,0)),
    ?_assert(Neg == -4).

try_test()->
    Result = try 
		 case pipe("test",
			   list_to_binary(), 
			   byte_size(), 
			   add(_,0)) of
		     2 -> true;
		     4 -> 	pipe("test",
				     list_to_binary(), 
				     byte_size(), 
				     add(_,0)) + a
		 end
	     catch
		 _:_ -> true
	     end,

    R2 = try pipe("test",
		  list_to_binary(), 
		  byte_size(), 
		  add(_,0))  of
	     1 -> oops;
	     4-> pipe("test",
		      list_to_binary(), 
		      byte_size(), 
		      add(_,0))
	 catch
	     _:_ -> io:format("error")
	 end,
    ?_assert(R2 == 4 andalso Result == true).

case_test()->
    Result = case 
		 pipe("test",
		      list_to_binary(), 
		      byte_size(), 
		      add(_,0)) of
		 2 -> true;
		 _ -> 	pipe("test",
			     list_to_binary(), 
			     byte_size(), 
			     add(_,0)) 
	     end,
    ?_assert(Result == 4).

assignment_test() ->
    A = pipe("test",
	     list_to_binary(_), 
	     byte_size(), 
	     add(11,_),
	     integer_to_list()),

    ?_assert(A == "15").

equality_test() ->    
    B = (pipe("test",
	    list_to_binary(), 
	    byte_size(), 
	    add(_,4),
	    integer_to_list()) == "8"),
    ?_assert(B == 8).

lc_test()->
    C = [pipe("test",
	  list_to_binary(), 
	  byte_size(), 
	  add(4, _)) + D || D <- lists:seq(1,10)],

     C1 = [begin 
 	     A1 = pipe("test",
 		  list_to_binary(), 
 		  byte_size(), 
 		  add(D1, _)),
 	     A1 + D1
 	 end || D1 <- lists:seq(1,10)],
    io:format("List Comprehension with block: ~p, [~p]~n", 
	      [C1, C1 == lists:seq(6,24,2)]),
    ?_assert(C1 == lists:seq(6,24,2) andalso C == lists:seq(9,18)).

fun_test() ->
    D = fun(E, E) ->
		E;
	   (E, F) -> 
		pipe("test",
		     list_to_binary(), 
		     byte_size(), 
		     add(_, E + F))
	end,
    ?_assert(D(10,1)==15).

math_exp_test() ->
    G = 1 + pipe("test",
	     list_to_binary(), 
	     byte_size(), 
		 add(_,0)) + 2,
    ?_assert(G == 7).


add(A,B) ->
     A + B.



