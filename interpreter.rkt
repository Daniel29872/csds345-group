#lang racket

; Group Members: Ulises Soto, Daniel Arnholt, Jimmy Ngo

(require "classParser.rkt")

; Outer layer of the interpreter which reads in the global variables and function definitions
(define interpret
  (lambda (filename classname)
    (interpret-outer-acc (parser filename) classname (new-state))))

(define get-methods cadr)
(define get-static-methods caddr)
(define no-type 'NoType)

; Adds class closures for all classses in given file to the state
(define interpret-outer-acc
  (lambda (syntax-tree classname state)
    (if (null? syntax-tree)
        ;(interpret-main (get-classname-main-body state classname) state classname classname) ; Just get the body of the main function
        state
        (interpret-outer-acc (rest-of-tree syntax-tree) classname (M_statement (curr-statement syntax-tree) state returnError breakError continueError throwError no-type no-type)))))

(define interpret-main
  (lambda (main-body state compileType runtimeType)
    (interpret-inner main-body state returnError breakError continueError throwError compileType runtimeType)))

; Interprets the statements in function. Each statement will return a state that will be used to evalute
; the next statement. When return is called in the program, then then return continuation is called,
; moving out of the function. 
(define interpret-inner
  (lambda (syntax-tree state return break continue throw compileType runtimeType)
    (call/cc (lambda (newReturn) (interpret-inner-acc syntax-tree state newReturn break continue throw compileType runtimeType)))))

(define interpret-inner-acc
  (lambda (syntax-tree state return break continue throw compileType runtimeType)
    (if (null? syntax-tree)
        (return "error")
        (interpret-inner-acc (rest-of-tree syntax-tree) (M_statement (curr-statement syntax-tree) state return break continue throw compileType runtimeType) return break continue throw compileType runtimeType))))

#| ---------PART _ ADDITIONS--------- |#

(define get-classname-main-body
  (lambda (state classname)
    (cadr (getBinding (list-of-class-static-methods (getBinding state classname)) 'main))))

(define list-of-class-static-methods caddr)

#|------------------------------------|#

; Return, break, continue, and throw continuations to be passed at the start. Will throw errors when called
; outside of the appropriate locations such as a while loop body or the try-body of a try-catch-finally
; statement.
(define returnError
  (lambda (s)
    (error "Invalid use of return outside of main")))

(define breakError
  (lambda (s)
    (error "Invalid use of break outside of loop")))

(define continueError
  (lambda (s)
    (error "Invalid use of continue outside of loop")))

(define throwError
  (lambda (e s)
    (error "Invalid use of throw outside of try block")))

(define new-state
  (lambda ()
    (list (new-layer))))

(define add-layer
  (lambda (state)
    (cons (new-layer) state)))

(define remove-layer
  (lambda (state)
    (rest-of-layers state)))

(define new-layer
  (lambda ()
    (list (list) (list))))

; --------------------- BINDING FUNCTIONS ---------------------

(define addBinding
  (lambda (state var val)
    (if (var-in-layer-vars? (top-layer-vars state) var)
        (error "already declared variable " var)
        (cons (add-to-layer (top-layer state) var val) (rest-of-layers state)))))

(define getBinding
  (lambda (state var)
    (cond
      [(null? state)                                   (error "using before declaring " var)]
      [(var-in-layer-vars? (top-layer-vars state) var) (get-binding-from-layer (top-layer state) var)]
      [else                                            (getBinding (rest-of-layers state) var)])))

(define updateBinding
  (lambda (state var val)
    (cond
      [(null? state)                                   (error "using before declaring" var)]
      [(var-in-layer-vars? (top-layer-vars state) var) (cons (update-layer-binding (top-layer state) var val) (rest-of-layers state))]
      [else                                            (cons (top-layer state) (updateBinding (rest-of-layers state) var val))])))



; --------------------- EVALUATION STATE FUNCTIONS ---------------------

; Handles the evaluation of a boolean expression.
(define M_boolean
  (lambda (exp state throw compileType runtimeType)
    (cond
      [(eq? exp 'true)                #t]
      [(eq? exp 'false)               #f]
      [(not (list? exp))
            (if (boolean? (getBinding state exp))
                (getBinding state exp)
                (error "type error"))]
      [(eq? (operator exp) '==)       (eq? (M_integer (leftoperand exp) state throw compileType runtimeType) (M_integer (rightoperand exp) state throw compileType runtimeType))]
      [(eq? (operator exp) '!=)       (not (eq? (M_integer (leftoperand exp) state throw compileType runtimeType) (M_integer (rightoperand exp) state throw compileType runtimeType)))]
      [(eq? (operator exp) '<)        (<   (M_integer (leftoperand exp) state throw compileType runtimeType) (M_integer (rightoperand exp) state throw compileType runtimeType))]
      [(eq? (operator exp) '>)        (>   (M_integer (leftoperand exp) state throw compileType runtimeType) (M_integer (rightoperand exp) state throw compileType runtimeType))]
      [(eq? (operator exp) '<=)       (<=  (M_integer (leftoperand exp) state throw compileType runtimeType) (M_integer (rightoperand exp) state throw compileType runtimeType))]
      [(eq? (operator exp) '>=)       (>=  (M_integer (leftoperand exp) state throw compileType runtimeType) (M_integer (rightoperand exp) state throw compileType runtimeType))]
      [(eq? (operator exp) '&&)       (and (M_boolean (leftoperand exp) state throw compileType runtimeType) (M_boolean (rightoperand exp) state throw compileType runtimeType))]
      [(eq? (operator exp) '||)       (or  (M_boolean (leftoperand exp) state throw compileType runtimeType) (M_boolean (rightoperand exp) state throw compileType runtimeType))]
      [(eq? (operator exp) '!)        (not (M_boolean (leftoperand exp) state throw compileType runtimeType))]
      [(eq? (operator exp) 'funcall)  (M_value exp state throw compileType runtimeType)]
      [else                           (error "Not a Boolean")])))

; Handles the evaluation of an integer expression.
(define M_integer
  (lambda (exp state throw compileType runtimeType)
    (cond
      [(number? exp)                                                  exp]
      [(not (list? exp))
            (if (number? (getBinding state exp))
                (getBinding state exp)
                (error "type error"))]
      [(and (eq? (operator exp) '-) (null? (rightoperand-list exp)))  (- 0 (M_integer (leftoperand exp) state throw))]
      [(eq? (operator exp) '+)                                        (+ (M_integer (leftoperand exp) state throw compileType runtimeType) (M_integer (rightoperand exp) state throw compileType runtimeType))]
      [(eq? (operator exp) '-)                                        (- (M_integer (leftoperand exp) state throw compileType runtimeType) (M_integer (rightoperand exp) state throw compileType runtimeType))]
      [(eq? (operator exp) '*)                                        (* (M_integer (leftoperand exp) state throw compileType runtimeType) (M_integer (rightoperand exp) state throw compileType runtimeType))]
      [(eq? (operator exp) '/)                                        (quotient (M_integer (leftoperand exp) state throw compileType runtimeType) (M_integer (rightoperand exp) state throw compileType runtimeType))]
      [(eq? (operator exp) '%)                                        (remainder (M_integer (leftoperand exp) state throw compileType runtimeType) (M_integer (rightoperand exp) state throw compileType runtimeType))]
      [(eq? (operator exp) 'funcall)                                  (M_value exp state throw compileType runtimeType)]
      [(eq? (operator exp) 'dot)                                      (get-val-from-instance-fields-of-runtime-type (caddr exp) (getBinding state compileType) runtimeType)]; create a dot function to should determine if need to use the compileType or the runtimeType
      [else                                                           (error "Not an Integer: " exp)])))

(define get-val-from-instance-fields-of-runtime-type
  (lambda (var class-closure runtimeType)
    (getBinding (cadddr class-closure) var))) ;<------

(define find-me
  (lambda (var lis)
    (cond
      [(null? lis) (error "not here" var)]
      [(eq? (cadar lis) var) (car (cddar lis))]
      [else (find-me var (cdr lis))])))
        

; Handles the evaluation of any general expression.
(define M_value
  (lambda (statement state throw compileType runtimeType)
    (cond
      [(number? statement)                 statement]
      [(eq? statement 'true)               #t]
      [(eq? statement 'false)              #f]
      [(not (list? statement))             (getBinding state statement)]
      [(eq? (operator statement) '+)       (M_integer statement state throw compileType runtimeType)]
      [(eq? (operator statement) '-)       (M_integer statement state throw compileType runtimeType)]
      [(eq? (operator statement) '*)       (M_integer statement state throw compileType runtimeType)]
      [(eq? (operator statement) '/)       (M_integer statement state throw compileType runtimeType)]
      [(eq? (operator statement) '%)       (M_integer statement state throw compileType runtimeType)]
      [(eq? (operator statement) '==)      (M_boolean statement state throw compileType runtimeType)]
      [(eq? (operator statement) '!=)      (M_boolean statement state throw compileType runtimeType)]
      [(eq? (operator statement) '<)       (M_boolean statement state throw compileType runtimeType)]
      [(eq? (operator statement) '>)       (M_boolean statement state throw compileType runtimeType)]
      [(eq? (operator statement) '<=)      (M_boolean statement state throw compileType runtimeType)]
      [(eq? (operator statement) '>=)      (M_boolean statement state throw compileType runtimeType)]
      [(eq? (operator statement) '&&)      (M_boolean statement state throw compileType runtimeType)]
      [(eq? (operator statement) '||)      (M_boolean statement state throw compileType runtimeType)]
      [(eq? (operator statement) '!)       (M_boolean statement state throw compileType runtimeType)]
                                                 ;v---- something like (return (dot this x) should lead here. Should return the field x from "this"
                                                 ;getBinding (vars-list of instance-closure) var
      [(eq? (operator statement) 'dot)      (getBinding (cadr (getBinding state (leftoperand statement))) (rightoperand statement))] 
      [(eq? (operator statement) 'funcall) (M_func_value (M_dot (function-name statement) state compileType runtimeType) (cons (getBinding state (leftoperand (function-name statement))) (var-value-list statement)) state
                                                         (lambda (a) a) breakError continueError (lambda (e s) (throw e state)) compileType runtimeType)]
      [(eq? (operator statement) 'new)     (instance-closure (cadr statement) state)]
      [else                                (error "invalid operator")])))

; Interprets a function body and returns the value after going through the body of the function.
(define M_func_value
  (lambda (closure argList state return break continue throw compileType runtimeType)
    (interpret-inner
     (closure_body closure)
     (bindParameters (closure_formal_params closure) argList (add-layer ((closure_func closure) state)) state compileType runtimeType)
     return break continue throw compileType runtimeType)))

; Interprets a function body, but returns the state after going through the body of the function.
(define M_func_state
  (lambda (closure argList state return break continue throw compileType runtimeType)
    (begin
      (interpret-inner
       (closure_body closure)
       (bindParameters (closure_formal_params closure) argList (add-layer ((closure_func closure) state)) state compileType runtimeType)
       return break continue throw compileType runtimeType)
     state)))

; Bind each passed parameter with its variable name in the state
(define bindParameters
  (lambda (params args fstate state compileType runtimeType)
    (if (or (null? params) (null? args))
        (bothListsEmpty params args fstate)
        (if (eq? (car params) 'this)
            (bindParameters (cdr params) (cdr args) (addBinding fstate (car params) (car args)) state compileType runtimeType)  ;< ---- Update to not call M_value on the instance closure of 'this (currently just skiping the first param and arg check)
            (bindParameters (cdr params) (cdr args) (addBinding fstate (car params) (M_value (car args) state throwError compileType runtimeType)) state compileType runtimeType)))))

; Evaluate a statement, and modify the state accordingly
(define M_statement
  (lambda (statement state return break continue throw compileType runtimeType)
    (cond
      [(eq? (operator statement) 'var)      (M_declare statement state throw compileType runtimeType)]
      [(eq? (operator statement) 'function) (M_function statement state compileType runtimeType)]
      [(eq? (operator statement) 'funcall)  (M_func_state (M_dot (function-name statement) state compileType runtimeType) (cons (getBinding state (leftoperand (function-name statement))) (var-value-list statement)) state
                                                         return break continue throw compileType runtimeType)]
      [(eq? (operator statement) '=)        (M_assignment statement state throw compileType runtimeType)]
      [(eq? (operator statement) 'return)   (M_return statement state return throw compileType runtimeType)]
      [(eq? (operator statement) 'if)       (M_if statement state return break continue throw compileType runtimeType)]
      [(eq? (operator statement) 'while)    (M_while statement state return break continue throw compileType runtimeType)]
      [(eq? (operator statement) 'begin)    (M_block (block-stmts statement) state return break continue throw compileType runtimeType)]
      [(eq? (operator statement) 'try)      (M_try_catch_finally statement state return break continue throw compileType runtimeType)]
      [(eq? (operator statement) 'continue) (continue state)]
      [(eq? (operator statement) 'break)    (break state)]
      [(eq? (operator statement) 'throw)    (throw (M_value (throw-value statement) state throw compileType runtimeType) state)]
      [(eq? (operator statement) 'class)    (M_class statement state compileType runtimeType)])))

(define instance-class car)
(define get-class-name cadr)

; returns the closure of the method from using either the runtime type or the compile time type
(define M_dot
  (lambda (statement state compileType runtimeType)
    (get-method-from-class
     (rightoperand statement)
     (instance-class (getBinding state (leftoperand statement)))
     (getBinding state (instance-class (getBinding state (leftoperand statement)))))))

; Create a binding for a class definition
(define M_class
  (lambda (statement state compileType runtimeType)
    (if (and (eq? compileType no-type) (eq? runtimeType no-type)) ; Do not allow class creation within another class
         (addBinding state (get-class-name statement) (make-class-closure statement))
         (error "Nested classes are not permitted."))))

; Takes in a function definition and creates a closure of the function to be added to the state.
(define M_function
  (lambda (static class-name statement state compileType runtimeType)
    (addBinding state (function-name statement) (make-closure static class-name (function-name statement) (formal-params statement) (function-body statement) state))))


; --------------------- STATEMENT STATE FUNCTIONS ---------------------

(define unbox-and-copy
  (lambda (list)
    (if (null? list)
        '()
        (cons (box (unbox (first-element list))) (unbox-and-copy (rest-of-list list))))))

(define deep-copy
  (lambda (state)
    (cons (unbox-and-copy (vars-list state)) (list (unbox-and-copy (vals-list state))))))

(define fields-list
  (lambda (closure)
    (deep-copy (car (cadddr closure)))))

(define classname-from-closure car)

#|

         The instance closure needs to be a copy of instance variables from the class-closure.
         Currently just copies the from class-closure instance-fields which uses boxes. Meaning changing for one
         instance changes for all the others.

|#


(define instance-closure
  (lambda (classname state)
    ; a list of: classname (fields)
    (list classname (fields-list (getBinding state classname)))))

; Used when calling a function to reduce the state to the scope of which it was defined.
(define restore-scope
  (lambda (state func-name)
    state))
    ;(if (var-in-layer-vars? (top-layer-vars state) func-name)
    ;    state
    ;    (restore-scope (rest-of-state state) func-name))))

; Generates the function closure using the given function name, parameters, body, and the state.
(define make-closure
  (lambda (static classname funcname formalparams body state)
    (if (eq? static 'static)
        (list formalparams body (lambda (s) (restore-scope s funcname)) classname)
        (list (cons 'this formalparams) body (lambda (s) (restore-scope s funcname)) classname))))

(define super-class-list caddr)
(define super-class-name cdr)
(define class-name cadr)

(define get-super-class
  (lambda (super-class-lst)
    (if (null? super-class-lst)
        'None
        (super-class-name super-class-lst))))

(define make-class-closure
  (lambda (class)
    ; a list of: superclass (methods) (static-methods) (fields) (static-fields)
    (list
     (get-super-class (super-class-list class))
     (get-class-methods (class-name class) (get-class-body class) (new-state))
     (get-class-static-methods (class-name class) (get-class-body class) (new-state))
     (get-class-instance-fields (get-class-body class) (new-state))
     (get-class-static-fields (get-class-body class)))))

(define get-class-body
  (lambda (class)
    (cadddr class)))

(define get-class-methods
  (lambda (class-name class-body state)
    (cond
      [(null? class-body) state]
      [(eq? (caar class-body) 'function) (get-class-methods class-name (cdr class-body) (M_function 'not-static class-name (car class-body) state no-type no-type))]
      [else                       (get-class-methods class-name (cdr class-body) state)])))

(define get-class-static-fields
  (lambda (class-body)
    (cond
      [(null? class-body) '()]
      [(eq? (caar class-body) 'static-var) (cons (car class-body) (get-class-static-fields (cdr class-body)))]
      [else                               (get-class-static-fields (cdr class-body))])))

(define get-class-static-methods
  (lambda (class-name class-body state)
    (cond
      [(null? class-body) state]
      [(eq? (caar class-body) 'static-function) (get-class-static-methods class-name (cdr class-body) (M_function 'static class-name (car class-body) state no-type no-type))]
      [else                               (get-class-static-methods class-name (cdr class-body) state)])))

;--
;-- Needs to be updated to call M_value on the instance field value so that a value is stored and not any expressions ---;
;--
(define get-class-instance-fields
  (lambda (class-body state)
    (cond
      [(null? class-body)           state]
      [(eq? (caar class-body) 'var) (get-class-instance-fields (cdr class-body) (addBinding state (cadar class-body) (caddar class-body)))]
      [else                         (get-class-instance-fields (cdr class-body) state)])))

; Processes a list of statements and returns the state after interpreting each statement.
; Begins by adding a new layer to the state before interpreting the first statement and removes the
; layer after the last statement is interpreted.
(define M_block
  (lambda (statement state return break continue throw)
    (remove-layer (M_block-list statement
                                (add-layer state) return
                                (lambda (s) (break (remove-layer s)))
                                (lambda (s) (continue (remove-layer s)))
                                (lambda (e s) (throw e (remove-layer s)))))))

; Interprets a list of statements and returns the state after reaching the end.
(define M_block-list
  (lambda (stmt-list state return break continue throw)
    (if (null? stmt-list)
        state
        (M_block-list (rest-of-tree stmt-list) (M_statement (curr-statement stmt-list) state return break continue throw) return break continue throw))))

; Processes a statement in the form (while (condition-stmt) (body-stmts)) and retuns an updated state
; While the condition-stmt is true, the body-stmts are interpreted. A new break continuation is passed which
; immediately return the state at that point.
(define M_while
  (lambda (statement state return break continue throw)
    (call/cc
     (lambda (newBreak) (loop (condition statement) (body-stmt statement) state return newBreak throw)))))

; Processes the body-stmts of the while loop and returns an updated state.
; A new continue continuation is passed into the statement being interpreted which will immediately 
; move on to the next iteration. 
(define loop
  (lambda (condition body state return break throw)
    (if (M_boolean condition state throw)
        (loop condition body (M_statement body state return break (lambda (s) (break (loop condition body s return break throw))) throw) return break throw)
        state)))

; Process and if statement and returns an updated state
; If the condition statemnt is true, the the body statemtn is interpreted. Multiple if statmenets can
; be inclded and will be interpreted if condition was false.
(define M_if
  (lambda (statement state return break continue throw)
    (cond
      [(M_boolean (condition statement) state throw) (M_statement (body-stmt statement) state return break continue throw)]
      [(null? (else-stmts statement))                state]
      [(eq? (first-else-stmt statement) 'if)         (M_if (else-stmts statement) state return break continue throw)]
      [else                                          (M_statement (first-else-stmt statement) state return break continue throw)])))

; Processes statement and retuns an updated state.
; (try (try-block) (catch (e) (catch-block)) (finally (finally-block))): 
;     try-block is interpreted with new continuations for break, continue, and throw. If throw is called, 
;     then catch-block and/or finally-block are called. Neither catch nor finally blocks are required.
(define M_try_catch_finally
  (lambda (statement state return break continue throw)
    (call/cc
     (lambda (newThrow) (M_try (try-block statement)
                            (finally-block statement)
                            state
                            return
                            (lambda (s) (break (M_finally (finally-block statement) s return break continue throw)))
                            (lambda (s) (continue (M_finally (finally-block statement) s return break continue throw)))
                            (lambda (e s) (newThrow (M_finally (finally-block statement) (M_catch (catch-block statement) (addBinding s (catch-stmt-var (catch-block statement)) e) return break continue throw) return break continue throw))))))))

; Processes in statement in the form (try-block) and returns an updated state.
; The list of statements in the try-block is interpreted.
(define M_try
  (lambda (try-stmt finally-block state return newBreak newContinue newThrow)
    (if (null? finally-block)
         (M_block try-stmt state return newBreak newContinue newThrow)
         (M_finally finally-block (M_block try-stmt state return newBreak newContinue newThrow) return newBreak newContinue newThrow))))

; Processes in a statement in the form (catch-block) and retuns an updated state.
; The list of statement in the catch-block is interpreted with the state containing the variable which
; contains the error value thrown.
(define M_catch
  (lambda (catch-stmt state return break continue throw)
    (if (null? catch-stmt)
        state
        (M_block (rest-of-catch-stmt catch-stmt) state return break continue throw))))

; Processes in a statement in the form (finally-block) and returns an updated state.
; The list of statements in the finally-block is interpreted. This is the last block which is interpreted
; in a try-catch-finally statement if the original statement had a finally block. 
(define M_finally
  (lambda (finally-stmt state return break continue throw)
    (if (null? finally-stmt)
        state
        (M_block (rest-of-finally-stmt finally-stmt) state return break continue throw))))

; Processes statement in the form (return val). Calls return continuation with val
(define M_return
  (lambda (statement state return throw compileType runtimeType)
    (cond
      [(eq? (M_value (var-name statement) state throw compileType runtimeType) #t) (return 'true)]
      [(eq? (M_value (var-name statement) state throw compileType runtimeType) #f) (return 'false)]
      [else                                                (return (M_value (var-name statement) state throw compileType runtimeType))])))
    
; Processes statement in the form (= var val) and retuns an updated state.
; Updates binding of var with val in the state.
(define M_assignment
   (lambda (statement state throw compileType runtimeType)
     (if (eq? (caadr statement) 'dot) ;check if assinging to a dot (dot this a) which is (dot class-instance-name instance-var)
         (updateBinding (cadr (getBinding state (cadr (cadr statement)))) (caddr (cadr statement)) (M_value (caddr statement) state throw compileType runtimeType)); (= (dot this a) 10)
         (updateBinding state (var-name statement) (M_value (var-value statement) state throw compileType runtimeType))))); (= var val)
                                                                                                                      

; Processes statement in one of two forms and returns an updated state.
; (var x): Adds binding x to the state with initial value 'error.
; (var x val): Adds binding x to the state with initial value val.
(define M_declare
  (lambda (statement state throw compileType runtimeType)
    (if (null? (var-value-list statement))
        (addBinding state (var-name statement) 'error)
        (addBinding state (var-name statement) (M_value (var-value statement) state throw compileType runtimeType)))))



; --------------------- HELPER FUNCTIONS ---------------------

; Used with getBinding to get a method closure from a class closure stored in the state.
(define get-method-from-class
  (lambda (func classname closure)
    (get-method-from-class-acc func classname (append (get-methods closure) (get-static-methods closure)))))

(define get-method-from-class-acc
  (lambda (func classname methods)
    (cond
      [(null? methods) (error "Class " classname " does not have function " func)]
      [(eq? (unbox (method-name methods)) func) (unbox (method-closure methods))]
      [else (get-method-from-class-acc func classname (rest-of-list methods))])))

(define method-name caaar)
(define method-closure caadar)

; Add the variable/value pair to the layer of the state
(define add-to-layer
  (lambda (layer var val)
    (cons (add-last (vars-list layer) var) (list (add-last (vals-list layer) val)))))

; Return true if the varibale is declared in the layer, false otherwise
(define var-in-layer-vars?
  (lambda (layer-vars var)
    (cond
      [(null? layer-vars)                             #f]
      [(eq? (unbox (first-element layer-vars)) var)   #t]
      [else                                           (var-in-layer-vars? (rest-of-list layer-vars) var)])))

; Get the binding of a variable from the given layer
(define get-binding-from-layer
  (lambda (layer var)
    (cond
      [(null? layer)                               (error "using before declaring" var)]
      [(not (eq? (unbox (first-var layer)) var))   (get-binding-from-layer (cons (rest-of-vars layer) (list (rest-of-vals layer))) var)]
      [(eq? (unbox (first-val layer)) 'error)      (error "using before assigning " var)]
      [else                                        (unbox (first-val layer))])))

; Update the value of a variable in the layer
(define update-layer-binding
  (lambda (layer var val)
    (update-layer-binding-cps layer var val (lambda (vars vals) (cons vars (list vals))))))

(define update-layer-binding-cps
  (lambda (layer var val return)
    (cond
      [(null? layer)   (error "using before declaring" var)]
      [(and (eq? (unbox (first-var layer)) var) (same-type (unbox (first-val layer)) val))
                       (return (vars-list layer) (cons (begin (set-box! (first-val layer) val) (first-val layer)) (rest-of-vals layer)))]
      [(eq? (unbox (first-var layer)) var)
                       (error "incorrect type assignment" var)]
      [else            (update-layer-binding-cps (cons (rest-of-vars layer) (list (rest-of-vals layer))) var val
                                               (lambda (vars vals) (return (cons (first-var layer) vars) (cons (first-val layer) vals))))])))

; Add the value to the end of the list and return the updated list
(define add-last
  (lambda (ls value)
    (if (null? ls)
        (list (box value))
        (cons (first-element ls) (add-last (rest-of-list ls) value)))))

; Check if two values a and b are the same type
(define same-type
  (lambda (a b)
    (cond
      [(or (eq? a 'error) (eq? b 'error)) #t]
      [(and (number? a) (number? b))      #t]
      [(and (boolean? a) (boolean? b))    #t]
      [else                               #f])))

; Generates a copy of the given list. Used to copy the state for functions.
(define copy
  (lambda (state)
    (cond
      [(null? state)       (list)]
      [(list? (car state)) (cons (copy (car state)) (copy (cdr state)))]
      [else                (cons (car state) (copy (cdr state)))])))

; Check if both lists provided are empty. Used to check if formal parameters count matches provided arguments count.
(define bothListsEmpty
  (lambda (params args fstate)
    (cond
      [(not (null? params)) (error "not enough arguments")]
      [(not (null? args))   (error "too many arguments")]
      [else                 fstate])))



; --------------------- ABSTRACTION... ---------------------
(define top-layer car)
(define rest-of-layers cdr)
(define rest-of-tree cdr)
(define curr-statement car)
(define vars-list car)
(define vals-list cadr)
(define top-layer-vars caar)
(define first-var caar)
(define first-val caadr)
(define rest-of-vars cdar)
(define rest-of-vals cdadr)
(define first-element car)
(define rest-of-list cdr)
(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)
(define rightoperand-list cddr)
(define block-stmts cdr) 
(define throw-value cadr)
(define closure_formal_params car)
(define closure_arg_list cddr)
(define closure_body cadr)
(define closure_func caddr)
(define condition cadr)
(define body-stmt caddr)
(define else-stmts cdddr)
(define first-else-stmt cadddr)
(define rest-of-finally-stmt cadr)
(define rest-of-catch-stmt caddr)
(define catch-stmt-var caadr)
(define try-block cadr)
(define catch-block caddr)
(define finally-block cadddr)
(define function-name cadr)
(define formal-params caddr)
(define function-body cadddr)
(define var-name cadr)
(define var-value caddr)
(define var-value-list cddr)
(define rest-of-state cdr)

; (define state (interpret "tests/1" 'A)
; (define cclosure (getBinding state 'A))
