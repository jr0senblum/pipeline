-module(test).
-export([start/0]).
-compile({parse_transform, pipeline}).


start() ->


    try 
	case pipe("test",
		  list_to_binary(), 
		  byte_size(), 
		  add(_,0)) of
	    2 -> true;
	    _ -> 	pipe("test",
			     list_to_binary(), 
			     byte_size(), 
			     add(_,0)) + a
	end
    catch
	_:_ -> io:format("try works works~n")
    end,


    try pipe("test",
	     list_to_binary(), 
	     byte_size(), 
	     add(_,0))  of
	1 -> oops;
	4-> Atry = pipe("test",
	     list_to_binary(), 
	     byte_size(), 
	     add(_,0)),
	    io:format("Try of works: ~p [~p]~n",[Atry, Atry ==4])
    catch
	_:_ -> io:format("error")
    end,

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
    io:format("Case:~p, [~p]~n",[Result, Result == 4]),

    A = pipe("test",
	     list_to_binary(_), 
	     byte_size(), 
	     add(11,_),
	     integer_to_list()),

    io:format("Assignment: ~p, [~p]~n",[A, A == "15"]),

    
    B = (pipe("test",
	    list_to_binary(), 
	    byte_size(), 
	    add(_,4),
	    integer_to_list()) == "8"),
    io:format("A == B: ~p [~p]~n",[B, true]),


    C = [pipe("test",
	  list_to_binary(), 
	  byte_size(), 
	  add(4, _)) + D || D <- lists:seq(1,10)],
    io:format("List Comprehension: ~p, [~p]~n", [C, C == lists:seq(9,18)]),


     C1 = [begin 
 	     A1 = pipe("test",
 		  list_to_binary(), 
 		  byte_size(), 
 		  add(D1, _)),
 	     A1 + D1
 	 end || D1 <- lists:seq(1,10)],
    io:format("List Comprehension with block: ~p, [~p]~n", 
	      [C1, C1 == lists:seq(6,24,2)]),

    D = fun(E, E) ->
		E;
	   (E, F) -> 
		pipe("test",
		     list_to_binary(), 
		     byte_size(), 
		     add(_,E+F))
	end,
    io:format("Function definition: ~p [~p]~n",[D(10,1), D(10,1)==15]),

    G = 1 + pipe("test",
	     list_to_binary(), 
	     byte_size(), 
		 add(_,0)) + 2,
    io:format("In-line mathmatical expression: ~p, [~p].~n",[G, G==7]).


add(A,B) ->
     A + B.








