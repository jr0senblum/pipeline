-module(test).
-export([start/0]).
-compile({parse_transform, pipeline}).

start() ->
    A = pipe("test",
	     list_to_binary(_), 
	     byte_size(), 
	     add(11,_),
	     integer_to_list()),

    io:format("it is ~p: ~p~n",[A, A == "15"]),

    
    B = (pipe("test",
	    list_to_binary(), 
	    byte_size(), 
	    add(_,4),
	    integer_to_list()) == "8"),
    io:format("... and ~p: ~p~n",[B, true]),


    C = [pipe("test",
	  list_to_binary(), 
	  byte_size(), 
	  add(4, _)) + D || D <- lists:seq(1,10)],
    io:format("... and ~p: ~p.~n",[C, C == lists:seq(9,18)]),

    D = fun(E, E) ->
		E;
	   (E, F) -> 
		pipe("test",
		     list_to_binary(), 
		     byte_size(), 
		     add(_,E+F))
	end,
    io:format("... and ~p ~p~n",[D(10,1), D(10,1)==15]),
    1 + pipe("test",
	     list_to_binary(), 
	     byte_size(), 
	     add(_,0)) + 2.



add(A,B) ->
     A + B.








