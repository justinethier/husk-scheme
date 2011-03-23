When implementing husk one of the most difficult concepts to wrap my head around was continuations. After a fair amount of research, trial, error, and  hacking I was finally able to put together a working implementation in husk while doing quite a bit of learning along the way. This article takes that learning process - and the code that came out of it - to introduce the basics of continuations and explains in depth how they are implemented in husk.

## Introduction
Scheme is a minimalistic language and does not provide many common control constructs such as return, try/catch, or even goto. Instead Scheme provides the more powerful concept of continuations, which may be used to build any specific type of control construct. The [R<sup>5</sup>RS specification](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.4) gives the following background information:

>Whenever a Scheme expression is evaluated there is a continuation wanting the result of the expression. The continuation represents an entire (default) future for the computation. If the expression is evaluated at top level, for example, then the continuation might take the result, print it on the screen, prompt for the next input, evaluate it, and so on forever. Most of the time the continuation includes actions specified by user code, as in a continuation that will take the result, multiply it by the value stored in a local variable, add seven, and give the answer to the top level continuation to be printed. Normally these ubiquitous continuations are hidden behind the scenes and programmers do not think much about them. On rare occasions, however, a programmer may need to deal with continuations explicitly. Call-with-current-continuation allows Scheme programmers to do that by creating a procedure that acts just like the current continuation.

Which is all well and good, but just how do you use these things? Fortunately they also provided some example code - for instance, here is how you can implement return in Scheme:

    (call-with-current-continuation
      (lambda (return)
        (for-each (lambda (x)
                (if (negative? x)
                    (return x)))
              '(54 0 37 -3 245 19))
    #t))                                ===>  -3

Let's break this down. As the spec describes, call-with-current-continuation (or call/cc for short) expects a single function as its only argument. When Scheme executes call-with-current-continuation above, it takes the current continuation and passes it to this function as the `return` argument. At any time, this continuation can be called into just like a function - at which point Scheme will abandon whatever continuation is in effect and will resume execution using the previous continuation.

So as the code above loops over the list of numbers, it finds a negative number and calls into the `return` continuation. So execution immediately jumps back to where (call-with-current-continuation) left off.




TODO - arg example from here: http://tech.phillipwright.com/2010/05/23/continuations-in-scheme/

(define handle #f)
(+ 2 (call/cc (lambda (k) (set! handle k) 2)))
 -> 4
(handle 6)
 -> 8
(handle 20)
 -> 22



## Continuation Passing Style
The [Continuation Implementation](http://c2.com/cgi/wiki?ContinuationImplementation) page provides several possible approaches for implementing continuations. Looking back, it seems obvious to use continuation passing style (CPS) to implement them in husk, as Haskell supports higher order functions.

In CPS , a function is 
( example of using CPS for jQuery callback)

Original code in direct (non-CPS) style:
    eval env (List [Atom "if", pred, conseq, alt]) =
        do result <- eval env pred
           case result of
             Bool False -> eval env alt
             otherwise -> eval env conseq

Sample code using CPS in the evaluator:
    eval env cont (List [Atom "if", predic, conseq, alt]) = do
      eval env (makeCPS env cont cps) (predic)
      where   cps :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
              cps e c result _ = 
                case (result) of
                  Bool False -> eval e c alt
                  _ -> eval e c conseq


 - Need to rethink below and come up with a clear, top-level design approach. Some starting points
 - for this are:

 -
 - ALSO, consider the following quote: 
 -  [http://c2.com/cgi/wiki?ContinuationPassingStyle]("CPS is a programming style where no function is ever allowed to return.") - of course this is not quite right, eventually eval has to return *something*
 - So, this would mean that when evaluating a simple integer, string, etc eval should call into
 - the continuation instead of just returning.
 - Need to think about how this will be handled, how functions will be called using CPS, and what
 - the continuation data type needs to contain.



 -  http://c2.com/cgi/wiki?CallWithCurrentContinuation (the link to this book may be helpful as well: http://c2.com/cgi/wiki?EssentialsOfProgrammingLanguages - apparently if the interpreter is written using CPS, then call/cc is free)
"If your program evaluator/interpreter is implemented using ContinuationPassingStyle you get call-with-current-continuation for free. CALL/CC is then a very natural operation since every function is called with the current continuation anyway. "


## Implementation
what are they, what do they do
- overview of code:
 - 2 approaches
 - code for each
 - actual continuation code

Data type:

	| Continuation {  closure :: Env    -- Environment of the continuation
                        , body :: [LispVal] -- Code in the body of the continuation
                        , continuation :: LispVal    -- Code to resume after body of cont
                        , contFunctionArgs :: (Maybe [LispVal]) -- Arguments to a higher-order function 
                        , continuationFunction :: (Maybe (Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal))
                        -- FUTURE: stack (for dynamic wind)

Helper functions:

    -- Make an "empty" continuation that does not contain any code
    makeNullContinuation :: Env -> LispVal
    makeNullContinuation env = Continuation env [] (Nil "") Nothing Nothing 
    
    -- Make a continuation that takes a higher-order function (written in Haskell)
    makeCPS :: Env -> LispVal -> (Env -> LispVal -> LispVal -> Maybe [LispVal]-> IOThrowsError LispVal) -> LispVal
    makeCPS env cont cps = Continuation env [] cont Nothing (Just cps)
    
    -- Make a continuation that stores a higher-order function and arguments to that function
    makeCPSWArgs :: Env -> LispVal -> (Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal) -> [LispVal] -> LispVal
    makeCPSWArgs env cont cps args = Continuation env [] cont (Just args) (Just cps)
    
Eval helper function:

    {- continueEval is a support function for eval, below.
     -
     - Transformed eval section into CPS by calling into this instead of returning from eval.
     - This function uses the cont argument to determine whether to keep going or to finally
     - return a result.
     - -}
    continueEval :: Env -> LispVal -> LispVal -> IOThrowsError LispVal
    -- Passing a higher-order function as the continuation; just evaluate it. This is 
    -- done to enable an 'eval' function to be broken up into multiple sub-functions,
    -- so that any of the sub-functions can be passed around as a continuation.
    --
    -- This perhaps shows cruft as we also pass cBody (scheme code) as a continuation.
    -- We could probably just use higher-order functions instead, but both are used for
    -- two different things.
    continueEval _ (Continuation cEnv _ cCont funcArgs (Just func)) val = func cEnv cCont val funcArgs
    
    -- No higher order function, so:
    --
    -- If there is Scheme code to evaluate in the function body, we continue to evaluate it.
    --
    -- Otherwise, if all code in the function has been executed, we 'unwind' to an outer
    -- continuation (if there is one), or we just return the result. Yes technically with
    -- CPS you are supposed to keep calling into functions and never return, but eventually
    -- when the computation is complete, you have to return something.
    continueEval _ (Continuation cEnv cBody cCont Nothing Nothing) val = do
        case cBody of
            [] -> do
              case cCont of
                Continuation nEnv _ _ _ _ -> continueEval nEnv cCont val
                _ -> return (val)
            [lv] -> eval cEnv (Continuation cEnv [] cCont Nothing Nothing) (lv)
            (lv : lvs) -> eval cEnv (Continuation cEnv lvs cCont Nothing Nothing) (lv)
    continueEval _ _ _ = throwError $ Default "Internal error in continueEval"
    
Implementation within apply:
    -- Call into a Scheme function
    apply :: LispVal -> LispVal -> [LispVal] -> IOThrowsError LispVal
    apply _ c@(Continuation env _ _ _ _) args = do
      if (toInteger $ length args) /= 1 
        then throwError $ NumArgs 1 args
        else continueEval env c $ head args
    apply cont (IOFunc func) args = do
      result <- func args
      case cont of
        Continuation cEnv _ _ _ _ -> continueEval cEnv cont result
        _ -> return result
    apply cont (PrimitiveFunc func) args = do
      result <- liftThrows $ func args
      case cont of
        Continuation cEnv _ _ _ _ -> continueEval cEnv cont result
        _ -> return result
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
                Continuation _ cBody cCont _ Nothing -> if length cBody == 0
                    then continueWCont env (evBody) cCont
                    else continueWCont env (evBody) cont -- Might be a problem, not fully optimizing
                _ -> continueWCont env (evBody) cont
    
            -- Shortcut for calling continueEval
            continueWCont cwcEnv cwcBody cwcCont = 
                continueEval cwcEnv (Continuation cwcEnv cwcBody cwcCont Nothing Nothing) $ Nil ""
    
            bindVarArgs arg env = case arg of
              Just argName -> liftIO $ extendEnv env [((varNamespace, argName), List $ remainingArgs)]
              Nothing -> return env
    apply _ func args = throwError $ BadSpecialForm "Unable to evaluate form" $ List (func : args)
    
Implementation of call/cc:
    eval env cont (List (Atom "call-with-current-continuation" : args)) = 
      eval env cont (List (Atom "call/cc" : args))
    eval _ _ (List [Atom "call/cc"]) = throwError $ Default "Procedure not specified"
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
    



    




## Lessons Learned
 - trampolines (not necessary due to haskell)

{- Did not need this function, since we are using Haskell
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
For me it is much easier to understand continuations now after having implemented support for them in husk. In fact, one of the best ways to really understand why continuations are such a general purpose concept is to look at them not only from the perspective of the application programmer, but rather from the perspective of how they are implemented in the Scheme runtime itself.

## References


 -  http://tech.phillipwright.com/2010/05/23/continuations-in-scheme/
 -  http://community.schemewiki.org/?call-with-current-continuation

