**THIS DOCUMENT IS STILL A WORK IN PROGRESS**

<img src="https://github.com/justinethier/husk-scheme/raw/master/docs/design-notes-husk-scheme.png" width="500" height="44">

# Continuations

The original husk code was based on the code from the tutorial [Write Yourself a Scheme in 48 Hours](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours) by Jonathan Tang. Although that book is a great starting point for writing a Scheme, its interpreter does not include many of features required by R<sup>5</sup>RS Scheme such as continuations. This article provides a brief overview of Scheme continuations and presents the corresponding code from husk to show how they could be implemented.

This article is intended as an introductory article for continuations. It will be most helpful to a reader with previous experience with Lisp and/or functional programming. In addition, you may want to at least skim through Jonathan Tang's book to get a basic understanding of the Haskell interpreter.

## Introduction
Scheme is a minimalistic language that does not include many common control constructs such as return, try/catch, or even goto. Instead Scheme provides continuations - a powerful, general-purpose construct which may be used to build more specific control structures. The [R<sup>5</sup>RS specification](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.4) gives the following summary:

>Whenever a Scheme expression is evaluated there is a continuation wanting the result of the expression. The continuation represents an entire (default) future for the computation. If the expression is evaluated at top level, for example, then the continuation might take the result, print it on the screen, prompt for the next input, evaluate it, and so on forever. Most of the time the continuation includes actions specified by user code, as in a continuation that will take the result, multiply it by the value stored in a local variable, add seven, and give the answer to the top level continuation to be printed. Normally these ubiquitous continuations are hidden behind the scenes and programmers do not think much about them. On rare occasions, however, a programmer may need to deal with continuations explicitly. Call-with-current-continuation allows Scheme programmers to do that by creating a procedure that acts just like the current continuation.

To see how this works in practice, we can walk through an implementation of `return` using Scheme:

    > (call-with-current-continuation
        (lambda (return)
          (for-each (lambda (x)
                (if (negative? x)
                    (return x)))
              '(54 0 37 -3 245 19))
        #t))
    -3

Let's break this down. As the R<sup>5</sup>RS spec describes, `call-with-current-continuation` (or `call/cc` for short) expects a single function as its only argument - an anonymous `lambda` function in this example. When Scheme executes `call-with-current-continuation`, it packs up the current continuation and passes it in as the `return` argument. This continuation can be called just like a function, at which point Scheme will abandon whatever continuation is in effect and will resume execution at this previous continuation.

As the code above loops over the list of numbers, it finds a negative number and calls into the `return` continuation. Execution immediately jumps back to where `call-with-current-continuation` left off, and the whole construct evaluates to `-3`.

Continuations are first-class objects, which means they can be assigned to variables, passed to functions, etc, just like any other data type. [Phillip Wright](http://tech.phillipwright.com/2010/05/23/continuations-in-scheme/) wrote an excellent article about this in which he presented the following code snippet: 

    > (define handle #f)
    > (+ 2 (call/cc (lambda (k) (set! handle k) 2)))
    4
    > (handle 6)
    8
    > (handle 20)
    22

This example illustrates many important points. By storing the continuation up in a variable we can use it later on in the program: in this case, it is used several times to add `2` to arbitrary expressions. A proper Scheme implementation allows a continuation to be invoked like this multiple times. We can also see that a continuation may be captured at any point in the code, even while evaluating part of a larger expression. The linked article is brief and I recommend reading through for a more detailed explanation.

## Continuation Passing Style
The [Portland Pattern Repository's Continuation Implementation](http://c2.com/cgi/wiki?ContinuationImplementation) wiki page provides several possible approaches for implementing continuations. In particular, the Scheme runtime can use continuation passing style (CPS). This is also what Jonathan recommends at the end of his [tutorial](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Conclusion). 

In CPS, a function (b) is passed as a parameter to another function (a), with the intent that when (a) is finished it will pass control to (b). So (b) in essence becomes a future computation of (a). This pattern is used extensively in modern programming - for instance, the node.js JavaScript framework allows one to pass a callback to an asynchronous function that is later executed once the asynchronous operation completes.

An example may be helpful. The original husk `eval` functions were written in direct style, like this:

    eval env (List [Atom "if", pred, conseq, alt]) =
        do result <- eval env pred
           case result of
             Bool False -> eval env alt
             otherwise -> eval env conseq

We have already seen that a Scheme continuation may be captured at any point within an expression. But in the above code, `eval` is always called twice - once when computing `result` and again after `result` is inspected. What would happen if that first `eval` contained a continuation? Eventually, once the continuation finished executing, the code would return and control would *incorrectly* pass to one of the second `eval`'s. Oops! 

Consider the same code written using CPS:

    eval env cont (List [Atom "if", predic, conseq, alt]) = do
      eval env (makeCPS env cont cps) (predic)
      where   cps :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
              cps e c result _ = 
                case (result) of
                  Bool False -> eval e c alt
                  _ -> eval e c conseq

Here we use the `makeCPS` helper function to pack up `cps` as a continuation object that is passed into `eval`. The evaluator will then call back into `cps` when it is ready. But since `cps` is being passing in as a function, the interpreter also has the freedom to call into another continuation instead!

The [Call With Current Continuation](http://c2.com/cgi/wiki?CallWithCurrentContinuation) wiki offers a keen observation:

>If your program evaluator/interpreter is implemented using Continuation Passing Style you get call-with-current-continuation for free. CALL/CC is then a very natural operation since every function is called with the current continuation anyway.

So CPS is a very natural way to implement continuations if you are fortunate enough to be using a language that can take advantage of this pattern. Looking back, it seems obvious to use CPS to implement continuations in husk, as Haskell supports both first-class functions and tail call optimization.

While researching CPS, one thing that really threw me for a loop was the [quote](http://c2.com/cgi/wiki?ContinuationPassingStyle]):

>CPS is a programming style where no function is ever allowed to return

But eventually `eval` has to return *something*, right? Well, yes, but since Haskell supports proper tail recursion we can call through as many CPS functions as necessary without fear of overflowing the stack. husk's `eval` function will eventually transform an expression into a single value that is returned to its caller. But that is just our application; another program written in CPS might keep calling into functions forever. 

Languages that do not optimize tail calls - such as JavaScript - can support CPS style, but eventually a value must be returned since the stack will keep growing larger with each call into a new function.

## Implementation
In order to transform the husk evaluator into CPS, a new `cont` (continuation) parameter had to be added to `eval`, and threaded through each call. This was a time consuming change as there were perhaps a hundred calls to `eval` within the core husk code, but each change by itself was straightforward. One of the reasons it took me so long to realize an approach using CPS was that I originally looked at the problem from a different perspective. Although CPS works great for Haskell code, what about all of those Scheme functions that need to be evaluated? Surely they will not be transformed into Haskell functions - so how do we handle them?

In order to support Scheme functions, the original husk implementation of continuations passed around a list of Scheme code to be executed as the body of the function. Each time a line of code is executed, the body is reduced by one line and the evaluator calls into the next line. This works great for supporting a certain class of continuations such as `return`, but cannot handle special forms built into the core evaluator such as `if`, `begin`, etc. So over time the use of higher-level Haskell functions was incorporated as well. Although this complicates the implementation somewhat, both approaches still use CPS - just in the Scheme case, you can think of each expression within the function as being its own continuation.

Let's walk through each part of the implementation to get an understanding of how it all fits together.

###Data types
The following container allows us to pass around either Scheme or Haskell code:

    -- |Container to hold code that is passed to a continuation for deferred execution 
    data DeferredCode =
        SchemeBody [LispVal] | -- ^A block of Scheme code
        HaskellBody {
           contFunction :: (Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal)
         , contFunctionArgs :: (Maybe [LispVal]) -- Arguments to the higher-order function 
        } -- ^A Haskell function

Since continuations are first-class objects, the `LispVal` data type was extended to include a new `Continuation` member:

    Continuation {  closure :: Env                          -- Environment of the continuation
                  , currentCont     :: (Maybe DeferredCode) -- Code of current continuation
                  , nextCont        :: (Maybe LispVal)      -- Code to resume after body of cont
                  , extraReturnArgs :: (Maybe [LispVal])    -- Extra return arguments, to support (values) and (call-with-values)
                  , dynamicWind :: (Maybe [DynamicWinders]) -- Functions injected by (dynamic-wind) 
                 }

This member contains a closure to capture the state of the program, a continuation chain, and auxillary data. The current continuation will be executed immediately. Execution then passes to the next continuation if it is present. If there is no more code to execute, the continuation members may be set to `Nothing` to instruct the evaluator to return its current value. 

The auxillary data members allow a continuation to keep track of data for special cases. Multiple return values may be stored to support `call-with-values`. `DynamicWinders` was added to store pairs of `before` and `after` functions to support `dynamic-wind`:

    -- |Container to store information from a dynamic-wind
    data DynamicWinders = DynamicWinders {
        before :: LispVal -- ^Function to execute when resuming continuation within extent of dynamic-wind
      , after :: LispVal  -- ^Function to execute when leaving extent of dynamic-wind
    }

###Helper functions
The following helper functions are provided as a convenience to package up Haskell functions, but they also serve another purpose. By encapsulating how the `Continuation` object is built they allow us to change its type structure with no impact to the evaluator's code - a wonderful aid for refactoring:

    -- Make an "empty" continuation that does not contain any code
    makeNullContinuation :: Env -> LispVal
    makeNullContinuation env = Continuation env Nothing Nothing Nothing Nothing 
    
    -- Make a continuation that takes a higher-order function (written in Haskell)
    makeCPS :: Env -> LispVal -> (Env -> LispVal -> LispVal -> Maybe [LispVal]-> IOThrowsError LispVal) -> LispVal
    makeCPS env cont@(Continuation _ _ _ _ dynWind) cps = Continuation env (Just (HaskellBody cps Nothing)) (Just cont) Nothing dynWind
    makeCPS env cont cps = Continuation env (Just (HaskellBody cps Nothing)) (Just cont) Nothing Nothing -- This overload just here for completeness; it should never be used 
    
    -- Make a continuation that stores a higher-order function and arguments to that function
    makeCPSWArgs :: Env -> LispVal -> (Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal) -> [LispVal] -> LispVal
    makeCPSWArgs env cont@(Continuation _ _ _ _ dynWind) cps args = Continuation env (Just (HaskellBody cps (Just args))) (Just cont) Nothing dynWind
    makeCPSWArgs env cont cps args = Continuation env (Just (HaskellBody cps (Just args))) (Just cont) Nothing Nothing -- This overload just here for completeness; it should never be used 

###Eval helper function
After the evaluation function is finished with an expression, it calls into `continueEval` to pick up execution of the next continuation:

    continueEval :: Env -> LispVal -> LispVal -> IOThrowsError LispVal

There are many versions of `continueEval`, depending upon the input pattern. We will briefly discuss each one in turn. The first one below accepts a higher-order Haskell function, which is just call into it directly:

    continueEval _  (Continuation cEnv 
                                 (Just (HaskellBody func funcArgs)) 
                                 (Just (Continuation cce cnc ccc _ cdynwind)) 
                                  xargs 
                                  _)
             val = func cEnv (Continuation cce cnc ccc xargs cdynwind) val funcArgs

We may also receive a list containing Scheme code. In this case the function checks how much code is left. If the Scheme code is all finished the resultant value is returned; otherwise we keep going:

    continueEval _ (Continuation cEnv (Just (SchemeBody cBody)) (Just cCont) extraArgs dynWind) val = do
        case cBody of
            [] -> do
              case cCont of
                Continuation nEnv ncCont nnCont _ nDynWind -> 
                  -- Pass extra args along if last expression of a function, to support (call-with-values)
                  continueEval nEnv (Continuation nEnv ncCont nnCont extraArgs nDynWind) val 
                _ -> return (val)
            [lv]       -> eval cEnv (Continuation cEnv (Just (SchemeBody []))  (Just cCont) Nothing dynWind) (lv)
            (lv : lvs) -> eval cEnv (Continuation cEnv (Just (SchemeBody lvs)) (Just cCont) Nothing dynWind) (lv)

Finally, there are two edge cases where a current continuation may not be present:

    -- No current continuation, but a next cont is available; call into it
    continueEval _ (Continuation cEnv Nothing (Just cCont) _ _) val = continueEval cEnv cCont val
    
    -- There is no continuation code, just return value
    continueEval _ (Continuation _ Nothing Nothing _ _) val = return val

###Apply
Apply is used to execute a Scheme function; it needs to know both how to call a function as well as how to execute a continuation:

    apply :: LispVal -> LispVal -> [LispVal] -> IOThrowsError LispVal

There are several patterns to consider. Let's start with the first, which handles function application of a continuation:

    apply _ cont@(Continuation env ccont ncont _ ndynwind) args = do
      case ndynwind of
        -- Call into dynWind.before if it exists...
        Just ([DynamicWinders beforeFunc _]) -> apply (makeCPS env cont cpsApply) beforeFunc []
        _ ->  doApply env cont
     where
       cpsApply :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
       cpsApply e c _ _ = doApply e c
       doApply e c = 
          case (toInteger $ length args) of 
            0 -> throwError $ NumArgs 1 [] 
            1 -> continueEval e c $ head args
            _ ->  -- Pass along additional arguments, so they are available to (call-with-values)
                 continueEval e (Continuation env ccont ncont (Just $ tail args) ndynwind) $ head args 

We first check to see if there is a `before` function stored in the continuation from a previous `dynamic-wind` operation. Such a function is guaranteed to execute each time the continuation is called, so we execute it if present. Then husk checks the number of arguments to the continuation, and hands them - along with the continuation itself - to `continueEval` to resume execution at the new continuation. We have just replaced the current continuation!

_Please note that the implementation of `dynamic-wind` as-is is not completely correct; the implementation needs to take into account a *stack* of before (and after) functions. A future version of husk will have a more complete solution._

A primitive function such as `+`, `-`, etc cannot call into a continuation because primitives just execute "pure" code that processes their arguments and returns a value. So husk just calls a primitive directly:

    apply cont (PrimitiveFunc func) args = do
      result <- liftThrows $ func args
      case cont of
        Continuation cEnv  _ _ _ _ -> continueEval cEnv cont result
        _ -> return result

The same code is also used to apply primitive IO functions.

The following case is a bit more interesting; here we execute a Scheme function:

    apply cont (Func aparams avarargs abody aclosure) args =
      if num aparams /= num args && avarargs == Nothing
         then throwError $ NumArgs (num aparams) args
         else (liftIO $ extendEnv aclosure $ zip (map ((,) varNamespace) aparams) args) >>= bindVarArgs avarargs >>= (evalBody abody)
      where remainingArgs = drop (length aparams) args
            num = toInteger . length
            --
            -- Continue evaluation within the body, preserving the outer continuation.
            --
            -- This link was helpful for implementing this, and has a *lot* of other useful information:
            -- http://icem-www.folkwang-hochschule.de/~finnendahl/cm_kurse/doc/schintro/schintro_73.html#SEC80
            --
            -- What we are doing now is simply not saving a continuation for tail calls. For now this may
            -- be good enough, although it may need to be enhanced in the future in order to properly
            -- detect all tail calls. 
            --
            -- See: http://icem-www.folkwang-hochschule.de/~finnendahl/cm_kurse/doc/schintro/schintro_142.html#SEC294
            --
            evalBody evBody env = case cont of
                Continuation _ (Just (SchemeBody cBody)) (Just cCont) _ cDynWind -> if length cBody == 0
                    then continueWCont env (evBody) cCont cDynWind
    --                else continueWCont env (evBody) cont (trace ("cDynWind = " ++ show cDynWind) cDynWind) -- Might be a problem, not fully optimizing
                    else continueWCont env (evBody) cont cDynWind -- Might be a problem, not fully optimizing
                Continuation _ _ _ _ cDynWind -> continueWCont env (evBody) cont cDynWind
                _ -> continueWCont env (evBody) cont Nothing 
    
            -- Shortcut for calling continueEval
            continueWCont cwcEnv cwcBody cwcCont cwcDynWind = 
                continueEval cwcEnv (Continuation cwcEnv (Just (SchemeBody cwcBody)) (Just cwcCont) Nothing cwcDynWind) $ Nil ""
    
            bindVarArgs arg env = case arg of
              Just argName -> liftIO $ extendEnv env [((varNamespace, argName), List $ remainingArgs)]
              Nothing -> return env

This function uses a series of helper functions, organized into a single pipeline:

    (liftIO $ extendEnv aclosure $ zip (map ((,) varNamespace) aparams) args) 
        >>= bindVarArgs avarargs 
        >>= (evalBody abody)

In a nutshell, we create a copy of the function's closure (input environment), bind the function arguments to that copy, and pass the Scheme code to `continueEval` to begin the evaluation process.

TODO: do we need to break this down a bit more?

###call/cc
Here is the implementation of `call/cc`. Husk uses CPS, so the code is actually quite simple:

    evalfuncCallCC [cont@(Continuation _ _ _ _ _), func] = do
       case func of
         PrimitiveFunc f -> do
             result <- liftThrows $ f [cont]
             case cont of 
                 Continuation cEnv _ _ _ _ -> continueEval cEnv cont result
                 _ -> return result
         Func aparams _ _ _ ->
           if (toInteger $ length aparams) == 1 
             then apply cont func [cont] 
             else throwError $ NumArgs (toInteger $ length aparams) [cont] 
         other -> throwError $ TypeMismatch "procedure" other
    evalfuncCallCC (_ : args) = throwError $ NumArgs 1 args -- Skip over continuation argument
    evalfuncCallCC _ = throwError $ NumArgs 1 []

Since `call/cc` accepts a single function as an argument, we simply call into that function, passing the current continuation as the only argument. There are two cases since a primitive function may be called directly.

## Lessons Learned
Initially I thought a lower-level construct might be required to implement continuations and proper tail recursion. For example, many [Schemes written in C use a trampoline](http://home.pipeline.com/~hbaker1/CheneyMTA.html):

>A popular method for achieving proper tail recursion in a non-tail-recursive C implementation is a trampoline.[2] A trampoline is an outer function which iteratively calls an inner function. The inner function returns the address of another function to call, and the outer function then calls this new function. In other words, when an inner function wishes to call another inner function tail-recursively, it returns the address of the function it wants to call back to the trampoline, which then calls the returned function. By returning before calling, the stack is first popped so that it does not grow without bound on a simple iteration. Unfortunately, the cost of such a trampoline function call is 2-3 times slower than a normal C call, and it requires that arguments be passed in global variables [Tarditi92].
>
>Appel's unpublished suggestion for achieving proper tail recursion in C uses a much larger fixed-size stack, continuation-passing style, and also does not put any arguments or data on the C stack. When the stack is about to overflow, the address of the next function to call is longjmp'ed (or return'ed) to a trampoline. Appel's method avoids making a large number of small trampoline bounces by occasionally jumping off the Empire State Building.

But it turns out this is not necessary since Haskell already supports proper tail recursion. So as explained earlier, husk can just use higher order functions and CPS to implement continuations directly. If husk were implemented in C it would be much more difficult to implement an interpreter of equal complexity. 

It is possible that husk's continuation could have be written in a more clever, compact form. For example we may have been able to leverage the continuation monad as part of this implementation. But any compactness gains would come at the expense of readability. One of the main goals of this implementation is as a learning project, so it is undesirable to make the code *too* clever. As such it could always be used as a blueprint to implement Scheme in a lower-level form.

## Conclusion

For me it is much easier to understand continuations now after having implemented support for them in husk. To really understand why continuations are such a general purpose concept it helps to look at them not only from the perspective of the application programmer, but to also consider how they are implemented in the Scheme runtime itself. 

Husk was my first major Haskell project, so please do not be too critical of the code presented here. It could probably be prettied up a bit and made more concise. But for the purposes of this project I tried to keep things as simple as possible.

Anyway, I hope this article helped you understand a bit more about what continuations are and how they work. If you would like to suggest any corrections or improvements - or if you just enjoyed reading - please drop me a line. 

Thanks!

## References

- http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours
- http://c2.com/cgi/wiki?ContinuationImplementation
- http://tech.phillipwright.com/2010/05/23/continuations-in-scheme/
- http://community.schemewiki.org/?call-with-current-continuation
- http://icem-www.folkwang-hochschule.de/~finnendahl/cm_kurse/doc/schintro/schintro_73.html#SEC80
- http://home.pipeline.com/~hbaker1/CheneyMTA.html
- http://matt.might.net/articles/cps-conversion/
