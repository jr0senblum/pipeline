##Pipeline parse transform
    pipe(InitParameter,
         fun1(),
         fun2(7,_),
         fun3(_,4))
         
 will be transformed into
 
     fun3(fun2(7, fun1(InitParameter)), 4)
     
 Currently the following scenarios are supported

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
          fun3(_,X)) || X <- lists:seq(1,10)];
 
 