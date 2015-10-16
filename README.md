##Pipeline parse transform
    pipe(InitialParameter,
         fun1(),
         fun2(7,_),
         fun3(_,4),
	 funN())
         
 will be transformed into
 
     funN(fun3(fun2(7, fun1(InitialParameter)), 4))

Given

    pipe(IP, Fn1, Fn2, ..., FnN)

The result of Fn1(P) will be given to Fn2 as its first _free_ parameter for
all FnK where 1 <= K <= N. By _free_ parameter, I mean the only parameter
in functions with arity 1, and the first parameter marked with _ for functions
with arity greater than 1.

Examples will make this more clear. It's an easy concept whose explanation
I have butchered.

Currently, one can use pipe in: block, call, catch, case, if, fun,
list comprehension, match, operator, try, try of, and unary operator
form expressions.

Some examples include:

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