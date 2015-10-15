##Pipeline parse transform
    pipe(InitParameter,
         fun1(),
         fun2(7,_),
         fun3(_,4))
         
 will be transformed into
 
     fun3(fun2(7, fun1(InitParameter)), 4)
     
 Currently one can use pipe in: block, call, case, if, fun, list comprehension, 
 match, operator, try, try of, and unary opertor form expressions. Some examples
 include:

    pipe(InitParameter,
         fun1(),
         fun2(7,_),
         fun3(_,4))

	A = pipe(InitParameter,
         fun1(),
         fun2(7,_),
         fun3(_,4))
         
    pipe(InitParameter,
         fun1(),
         fun2(7,_),
         fun3(_,4)) == Value
    
    A = fun(X) ->    
            pipe(InitParameter,
                fun1(),
                fun2(7,_),
                fun3(_,4));
        (Y) ->   
                etc
        end
        
    [pipe(InitParameter,
          fun1(),
          fun2(7,_),
          fun3(_,X)) || X <- lists:seq(1,10)]

    Catch = (catch pipe("test",
                        list_to_binary(),
                        byte_size(),
                        add(_,0)))

 
 
##Usage
Make sure the parse_transform file, pipeline.erl, is compiled as part of your project
or in some other way ensure that the pipeline.beam is in your path.

Either include a compile directive in your .erl file

      -compile({parse_transform, pipeline}).
 
 
 
 or use the compile directive
     
    c(test, [{parse_transform, pipeline}]).