<img src="https://github.com/justinethier/husk-scheme/raw/master/docs/husk-scheme.png" width="250" height="44">

When implementing husk one of the most difficult concepts to wrap my head around was continuations. After a fair amount of research, trial, error, and  hacking I was finally able to put together a working implementation in husk while doing quite a bit of learning along the way. This article takes that learning process - and the code that came out of it - to introduce the basics of continuations and explains in depth how they are implemented in husk.

## Introduction
Scheme is a minimalistic language and does not include many common control constructs such as return, try/catch, or even goto. Instead Scheme provides continuations - a powerful, general-purpose construct which may be used to build any number of specific control structures. The [R<sup>5</sup>RS specification](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.4) gives the following background information:

>Whenever a Scheme expression is evaluated there is a continuation wanting the result of the expression. The continuation represents an entire (default) future for the computation. If the expression is evaluated at top level, for example, then the continuation might take the result, print it on the screen, prompt for the next input, evaluate it, and so on forever. Most of the time the continuation includes actions specified by user code, as in a continuation that will take the result, multiply it by the value stored in a local variable, add seven, and give the answer to the top level continuation to be printed. Normally these ubiquitous continuations are hidden behind the scenes and programmers do not think much about them. On rare occasions, however, a programmer may need to deal with continuations explicitly. Call-with-current-continuation allows Scheme programmers to do that by creating a procedure that acts just like the current continuation.

To see how this works, we can walk through an implementation of `return` using Scheme:

    (call-with-current-continuation
      (lambda (return)
        (for-each (lambda (x)
                (if (negative? x)
                    (return x)))
              '(54 0 37 -3 245 19))
        #t))
    -> -3

Let's break this down. As the spec describes, `call-with-current-continuation` (or `call/cc` for short) expects a single function as its only argument - an anonymous `lambda` function in this example. When Scheme executes `call-with-current-continuation`, it packs up the current continuation and passes it in as the `return` argument. This continuation can be called just like a function, at which point Scheme will abandon whatever continuation is in effect and will resume execution at this previous continuation.

As the code above loops over the list of numbers, it finds a negative number and calls into the `return` continuation. Execution immediately jumps back to where `call-with-current-continuation` left off, and the whole construct evaluates to `-3`.

Scheme continuations are first-class objects, which means they can be assigned to variables, passed to functions, etc, just like any other data type. To give you an idea how this might be useful, here is a quick example from [Phillip Wright - Tech](http://tech.phillipwright.com/2010/05/23/continuations-in-scheme/): 

	(define handle #f)
	(+ 2 (call/cc (lambda (k) (set! handle k) 2)))
	 -> 4
	(handle 6)
	 -> 8
	(handle 20)
	 -> 22

This example illustrates many important points. By storing the continuation up in a variable we can use it later on in the program. In this case, it is used several times to add `2` to arbitrary expressions. A proper Scheme implementation allows a continuation to be invoked multiple times. We also see that 
a continuation may be captured at any point in the code, even while evaluating part of a larger expression. The linked article is brief and I recommend reading through for a more detailed explanation.

## Continuation Passing Style
The [Continuation Implementation](http://c2.com/cgi/wiki?ContinuationImplementation) page provides several possible approaches for implementing continuations. One such approach is to write the Scheme 'runtime' using continuation passing style (CPS).

In CPS, a function (b) is passed as a parameter to another function (a), with the intent that when (a) is finished it will pass control to (b). So (b) in essence becomes a future computation of (a). This is used extensively in modern programming - for instance, the node.js JavaScript framework allows one to pass a callback to an asynchronous function that is later executed once the asynchronous operation completes.

Here is an example of husk code written in its original direct style:

    eval env (List [Atom "if", pred, conseq, alt]) =
        do result <- eval env pred
           case result of
             Bool False -> eval env alt
             otherwise -> eval env conseq

We have already seen that a continuation may be captured at any point of an expression. But in the above code, `eval` is called twice - once when computing `result` and again after result is inspected. But what would happen if that first `eval` contained a continuation? Eventually, once the continuation was finished, the code would return and control would incorrectly pass to one of the second `eval`'s. Ooops! 

Observe the same code written in CPS:

    eval env cont (List [Atom "if", predic, conseq, alt]) = do
      eval env (makeCPS env cont cps) (predic)
      where   cps :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
              cps e c result _ = 
                case (result) of
                  Bool False -> eval e c alt
                  _ -> eval e c conseq

Here we use the `makeCPS` helper function to pack up `cps` as a continuation object that is passed into `eval`. The evaluator can then call back into `cps` when it is ready. But since the "next step" is being passing in as a function, the interpreter also has the freedom to instead call into another continuation!

The [Call With Current Continuation wiki at c2.com](http://c2.com/cgi/wiki?CallWithCurrentContinuation) offers a keen observation:

>If your program evaluator/interpreter is implemented using ContinuationPassingStyle you get call-with-current-continuation for free. CALL/CC is then a very natural operation since every function is called with the current continuation anyway.

CPS is a very natural way to implement continuations, if you are fortunate enough to be using a language that can take advantage of this pattern. 
Looking back, it seems obvious to use CPS to implement continuations in husk, as Haskell supports higher order functions (that is, a function can be assigned to a variable just like any other data type).

While researching CPS, one thing that really threw me for a loop was the [quote](http://c2.com/cgi/wiki?ContinuationPassingStyle]):

>CPS is a programming style where no function is ever allowed to return

But eventually `eval` has to return *something*, right? Well, yes, but since Haskell supports proper tail recursion we can call through as many CPS functions as necessary without fear of overflowing the stack. In husk, eventually `eval` will evaluate to a single value that is returned to its caller. But another program written in CPS might keep calling into functions forever. 

Languages that do not support proper tail recursion - such as JavaScript - can support CPS style, but eventually a value must be returned since the stack will keep growing larger with each call into a new function.

## Implementation
In order to transform the husk evaluator into CPS, a new `cont` parameter had to be added to `eval`, and threaded through each call. This was a time consuming change as there are so many calls to `eval` with the core husk code. But each change was straightforward - many to the point of being mechanical. However, one of the reasons that it took me so long to realize CPS was the right choice was that I was looking at the problem from a different perspective. Although CPS works great for Haskell code, what about all of those Scheme functions that need to be evaluated? Surely they will not be transformed into Haskell functions - so how to handle them?

The original husk implementation for continuations passed around a list of Scheme code to be executed as the body of the function. Each time a line of code is executed, the body is reduced by one line and the evaluator calls into the next one. This works great for supporting a certain class of continuations such as `return`. Anyway, the use of higher-level Haskell functions was incorporated into that code. This complicates the implementation somewhat, although in a sense both approaches are using CPS - in the Scheme case, we are simply passing around a scheme function. In fact, you could think of each line of the function as being its own CPS function.

Enough talk. Let's walk through each part of the implementation to get an understanding of how it all works.

###Data types
The following container allows us to pass around either Scheme or Haskell code:

    -- |Container to hold code that is passed to a continuation for deferred execution 
    data DeferredCode =
        SchemeBody [LispVal] | -- ^A block of Scheme code
        HaskellBody {
           contFunction :: (Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal)
         , contFunctionArgs :: (Maybe [LispVal]) -- Arguments to the higher-order function 
        } -- ^A Haskell function

Since continuations are first-class object, We then extend the `LispVal` data type to include a new `Continuation` member:

    Continuation {  closure :: Env                     -- Environment of the continuation
                  , currentCont :: (Maybe DeferredCode)-- Code of current continuation
                  , nextCont    :: (Maybe LispVal)     -- Code to resume after body of cont
                 }

This member contains a closure to capture the state of the program. It also includes a continuation 'chain'. The current continuation will be executed immediately; if present, execution will then pass to the next continuation. If there is no more code to execute, the continuation members may be set to `Nothing` to instruct the evaluator to return its current value. 

###Helper functions
The following helper functions are provided as a convenience to package up Haskell functions, but they also serve another purpose. By encapsulating how the `Continuation` object is built they allow us to change its type structure with no impact to the evaluator's code - a wonderful aid for refactoring:

    -- Make an "empty" continuation that does not contain any code
    makeNullContinuation :: Env -> LispVal
    makeNullContinuation env = Continuation env Nothing Nothing 
    
    -- Make a continuation that takes a higher-order function (written in Haskell)
    makeCPS :: Env -> LispVal -> (Env -> LispVal -> LispVal -> Maybe [LispVal]-> IOThrowsError LispVal) -> LispVal
    makeCPS env cont cps = Continuation env (Just (HaskellBody cps Nothing)) (Just cont)
    
    -- Make a continuation that stores a higher-order function and arguments to that function
    makeCPSWArgs :: Env -> LispVal -> (Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal) -> [LispVal] -> LispVal
    makeCPSWArgs env cont cps args = Continuation env (Just (HaskellBody cps (Just args))) (Just cont)
    
###Eval helper function
After the evaluation function is finished with an expression, it calls into `continueEval` to pick up execution of the next continuation:

    continueEval :: Env -> LispVal -> LispVal -> IOThrowsError LispVal

There are many versions of `continueEval`, depending upon the input pattern. We will briefly discuss each one in turn. The first one below accepts a higher-order Haskell function, which is just call into it directly:

    continueEval _ (Continuation cEnv (Just (HaskellBody func funcArgs)) (Just cCont)) val = func cEnv cCont val funcArgs

We may also receive a list containing Scheme code. In this case the function sees how much code is left. If the Scheme code is all finished the resultant value is returned; otherwise we keep going:

    continueEval _ (Continuation cEnv (Just (SchemeBody cBody)) (Just cCont)) val = do
        case cBody of
            [] -> do
              case cCont of
                Continuation nEnv _ _ -> continueEval nEnv cCont val
                _ -> return (val)
            [lv] -> eval cEnv (Continuation cEnv (Just (SchemeBody [])) (Just cCont)) (lv)
            (lv : lvs) -> eval cEnv (Continuation cEnv (Just (SchemeBody lvs)) (Just cCont)) (lv)

Finally, there are two edge cases where a current continuation may not be present:

    -- No current continuation, but a next continuation is available; call into it
    continueEval _ (Continuation cEnv Nothing (Just cCont)) val = continueEval cEnv cCont val
    
    -- There is no continuation code, just return value
    continueEval _ (Continuation _ Nothing Nothing) val = return val

###Apply
Apply is used to execute a Scheme function; it needs to know both how to call a function as well as how to execute a continuation:

    apply :: LispVal -> LispVal -> [LispVal] -> IOThrowsError LispVal

There are several patterns to consider. Let's start with the first, which handles function application of a continuation. As of now, husk only supports sending a single argument to a continuation, so there is simple validation for the number of arguments. Once that is complete, we simply call into `continueEval`:

    apply _ c@(Continuation env _ _) args = do
      if (toInteger $ length args) /= 1 
        then throwError $ NumArgs 1 args
        else continueEval env c $ head args

A primitive function cannot call into a continuation, so we simply call into it directly, obtain a result, and then call into a continuation if present. The same code is also used to apply primitive IO functions:

    apply cont (PrimitiveFunc func) args = do
      result <- liftThrows $ func args
      case cont of
        Continuation cEnv  _ _ -> continueEval cEnv cont result
        _ -> return result

This case is a bit more interesting; here we execute a Scheme function.

TODO: explain this in more detail

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
                Continuation _ (Just (SchemeBody cBody)) (Just cCont) -> if length cBody == 0
                    then continueWCont env (evBody) cCont
                    else continueWCont env (evBody) cont -- Might be a problem, not fully optimizing
                _ -> continueWCont env (evBody) cont
    
            -- Shortcut for calling continueEval
            continueWCont cwcEnv cwcBody cwcCont = 
                continueEval cwcEnv (Continuation cwcEnv (Just (SchemeBody cwcBody)) (Just cwcCont)) $ Nil ""
    
            bindVarArgs arg env = case arg of
              Just argName -> liftIO $ extendEnv env [((varNamespace, argName), List $ remainingArgs)]
              Nothing -> return env

###call/cc
Here is the implementation of `call/cc`. Since husk uses CPS, the code is actually quite simple, though perhaps more verbose than it needs to be:

TODO: can this code be simplified further?

    eval e c (List [Atom "call/cc", proc]) = eval e (makeCPS e c cpsEval) proc
     where
       cpsEval :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
       cpsEval _ cont func _ = 
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
    
TODO: an explanation of what is going on here...
(Basically the code above )

## Lessons Learned
Initially I thought that we might have to use a lower-level construct to implement continuations. One such construct is a trampoline, which is used by many C implementations.

TODO: link to what trampolines are, C example, etc...

    {- Possible implementation in haskell (not sure how complete it is??) 
      Did not need this function, since we are using Haskell
    trampoline :: Env -> LispVal -> IOThrowsError LispVal
    trampoline env val = do
      result <- eval env val
      case result of
           -- If a form is not fully-evaluated to a value, bounce it back onto the trampoline...
           func@(Func params vararg body closure True) -> trampoline env func -- next iteration, via tail call (?)
           val -> return val
    -}

 - shift/reset???


 - Some of my notes:
 - as simple as using CPS to evaluate lists of "lines" (body)? Then could pass the next part of the CPS as the cont arg to eval. Or is this too simple to work? need to think about this - http://en.wikipedia.org/wiki/Continuation-passing_style
 -
 - Possible design approach:
 -
 -  * thread cont through eval
 -  * instead of returning, call into next eval using CPS style, with the cont parameter.
 -    this replaces code in evalBody (possibly other places?) that uses local CPS to execute a function
 -  * parameter will consist of a lisp function
 -  * eval will call into another function to deal with details of manipulating the cont prior to next call
 -    need to work out details of exactly how that would work, but could for example just go to the next line
 -    of body. 
 -  * To continue above point, where is eval'd value returned to? May want to refer to R5RS section that describes call/cc:

## Conclusion
For me it is much easier to understand continuations now after having implemented support for them in husk. To really understand why continuations are such a general purpose concept one must look at them not only from the perspective of the application programmer, but also from the perspective of how they are implemented in the Scheme runtime itself. In a way we took the easy way out in husk, as Haskell provides many constructs required by a Scheme. If husk were implemented in C it would be much more difficult to implement an interpreter of equal complexity. However, the husk code is written to be readable and easy to follow. As such, my goal is to provide a working implementation that is easy to follow. It could be used as a blueprint to implement Scheme in a lower-level form.

It is possible that much of the Haskell code in husk could be written in a more clever, compact form. For example the continuation monad may have been able to be used...
But this compactness would come at the expense of readability...

## References

 -  http://tech.phillipwright.com/2010/05/23/continuations-in-scheme/
 -  http://community.schemewiki.org/?call-with-current-continuation

