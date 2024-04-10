#lang racket

; Group Members: Ulises Soto, Daniel Arnholt, Jimmy Ngo

(require "functionParser.rkt")


(define interpret
  (lambda (filename)
    (interpret-outer-acc (parser filename) (new-state) returnError breakError continueError throwError)))

(define interpret-outer-acc
  (lambda (syntax-tree state return break continue throw)
    (if (null? syntax-tree)
        (M_func_value (getBinding state 'main) '() state return break continue throw)
        (interpret-outer-acc (rest-of-tree syntax-tree) (M_statement (curr-statement syntax-tree) state return break continue throw) return break continue throw))))

(define interpret-inner
  (lambda (syntax-tree state return break continue throw)
    (call/cc (lambda (newReturn) (interpret-inner-acc syntax-tree state newReturn break continue throw)))))

(define interpret-inner-acc
  (lambda (syntax-tree state return break continue throw)
    (if (null? syntax-tree)
        (return "error")
        (interpret-inner-acc (rest-of-tree syntax-tree) (M_statement (curr-statement syntax-tree) state return break continue throw) return break continue throw))))

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
    (cdr state)))

(define new-layer
  (lambda ()
    '(() ())))

(define top-layer car)
(define rest-of-layers cdr)

(define rest-of-tree cdr)
(define curr-statement car)


; --------------------- BINDING FUNCTIONS ---------------------

(define addBinding
  (lambda (state var val)
    (if (var-in-layer-vars? (top-layer-vars state) var)
        (error "already declared variable " var)
        (cons (add-to-layer (top-layer state) var val) (rest-of-layers state)))))

(define getBinding
  (lambda (state var)
    (cond
      [(null? state) (error "using before declaring " var)]
      [(var-in-layer-vars? (top-layer-vars state) var)
                     (get-binding-from-layer (car state) var)]
      [else          (getBinding (rest-of-layers state) var)])))

(define updateBinding
  (lambda (state var val)
    (cond
      [(null? state) (error "using before declaring" var)]
      [(var-in-layer-vars? (top-layer-vars state) var)
                     (cons (update-layer-binding (top-layer state) var val) (rest-of-layers state))]
      [else          (cons (top-layer state) (updateBinding (rest-of-layers state) var val))])))

; --------------------- HELPER FUNCTIONS ---------------------

(define vars-list car)
(define vals-list cadr)
(define top-layer-vars caar)
(define first-var caar)
(define first-val caadr)
(define rest-of-vars cdar)
(define rest-of-vals cdadr)
(define first-element car)
(define rest-of-list cdr)

; Add the variable/value pair to the layer of the state
(define add-to-layer
  (lambda (layer var val)
    (cons (add-last (vars-list layer) var) (list (add-last (vals-list layer) val)))))

; Return true if the varibale is declared in the layer, false otherwise
(define var-in-layer-vars?
  (lambda (layer-vars var)
    (cond
      [(null? layer-vars)                   #f]
      [(eq? (unbox (car layer-vars)) var)   #t]
      [else                                 (var-in-layer-vars? (cdr layer-vars) var)])))

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


; --------------------- EVALUATION STATE FUNCTIONS ---------------------

(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)
(define rightoperand-list cddr)
(define block-stmts cdr) 
(define throw-value cadr)

(define M_boolean
  (lambda (exp state)
    (cond
      [(eq? exp 'true)          #t]
      [(eq? exp 'false)         #f]
      [(not (list? exp))
            (if (boolean? (getBinding state exp))
                (getBinding state exp)
                (error "type error"))]
      [(eq? (operator exp) '==) (eq? (M_integer (leftoperand exp) state) (M_integer (rightoperand exp) state))]
      [(eq? (operator exp) '!=) (not (eq? (M_integer (leftoperand exp) state) (M_integer (rightoperand exp) state)))]
      [(eq? (operator exp) '<)  (<   (M_integer (leftoperand exp) state) (M_integer (rightoperand exp) state))]
      [(eq? (operator exp) '>)  (>   (M_integer (leftoperand exp) state) (M_integer (rightoperand exp) state))]
      [(eq? (operator exp) '<=) (<=  (M_integer (leftoperand exp) state) (M_integer (rightoperand exp) state))]
      [(eq? (operator exp) '>=) (>=  (M_integer (leftoperand exp) state) (M_integer (rightoperand exp) state))]
      [(eq? (operator exp) '&&) (and (M_boolean (leftoperand exp) state) (M_boolean (rightoperand exp) state))]
      [(eq? (operator exp) '||) (or  (M_boolean (leftoperand exp) state) (M_boolean (rightoperand exp) state))]
      [(eq? (operator exp) '!)  (not (M_boolean (leftoperand exp) state))]
      [(eq? (operator exp) 'funcall)                                  (M_value exp state)]
      [else                     (error "Not a Boolean")])))

(define M_integer
  (lambda (exp state)
    (cond
      [(number? exp)                                                  exp]
      [(not (list? exp))
            (if (number? (getBinding state exp))
                (getBinding state exp)
                (error "type error"))]
      [(and (eq? (operator exp) '-) (null? (rightoperand-list exp)))  (- 0 (M_integer (leftoperand exp) state))]
      [(eq? (operator exp) '+)                                        (+ (M_integer (leftoperand exp) state) (M_integer (rightoperand exp) state))]
      [(eq? (operator exp) '-)                                        (- (M_integer (leftoperand exp) state) (M_integer (rightoperand exp) state))]
      [(eq? (operator exp) '*)                                        (* (M_integer (leftoperand exp) state) (M_integer (rightoperand exp) state))]
      [(eq? (operator exp) '/)                                        (quotient (M_integer (leftoperand exp) state) (M_integer (rightoperand exp) state))]
      [(eq? (operator exp) '%)                                        (remainder (M_integer (leftoperand exp) state) (M_integer (rightoperand exp) state))]
      [(eq? (operator exp) 'funcall)                                  (M_value exp state)]
      [else                                                           (error "Not an Integer: " exp)])))

(define M_value
  (lambda (statement state)
    (cond
      [(number? statement)                 statement]
      [(eq? statement 'true)                         #t]
      [(eq? statement 'false)                        #f]
      [(not (list? statement))             (getBinding state statement)]
      [(eq? (operator statement) '+)       (M_integer statement state)]
      [(eq? (operator statement) '-)       (M_integer statement state)]
      [(eq? (operator statement) '*)       (M_integer statement state)]
      [(eq? (operator statement) '/)       (M_integer statement state)]
      [(eq? (operator statement) '%)       (M_integer statement state)]
      [(eq? (operator statement) '==)      (M_boolean statement state)]
      [(eq? (operator statement) '!=)      (M_boolean statement state)]
      [(eq? (operator statement) '<)       (M_boolean statement state)]
      [(eq? (operator statement) '>)       (M_boolean statement state)]
      [(eq? (operator statement) '<=)      (M_boolean statement state)]
      [(eq? (operator statement) '>=)      (M_boolean statement state)]
      [(eq? (operator statement) '&&)      (M_boolean statement state)]
      [(eq? (operator statement) '||)      (M_boolean statement state)]
      [(eq? (operator statement) '!)       (M_boolean statement state)]
      [(eq? (operator statement) 'funcall) (M_func_value (getBinding state (cadr statement)) (cddr statement) state (lambda (a) a) (lambda (a) a) (lambda (a) a) (lambda (a) a))] 
      [else                                (error "invalid operator")])))

(define closure_formal_params car)
(define closure_body cadr)
(define closure_func caddr)

; Interprets a function body and returns the value after going through the body of the function.
(define M_func_value
  (lambda (closure argList state return break continue throw)
    (interpret-inner
     (closure_body closure)
     (bindParameters (closure_formal_params closure) argList (add-layer ((closure_func closure) state)) state)
     return break continue throw)))

(define M_func_state
  (lambda (closure argList state return break continue throw)
    (begin
      (interpret-inner
       (closure_body closure)
       (bindParameters (closure_formal_params closure) argList (add-layer ((closure_func closure) state)) state)
       return break continue throw)
     state)))

(define bothListsEmpty
  (lambda (params args fstate)
    (cond
      [(not (null? params)) (error "not enough arguments")]
      [(not (null? args))   (error "too many arguments")]
      [else                 fstate])))

(define bindParameters
  (lambda (params args fstate state)
    (if (or (null? params) (null? args))
        (bothListsEmpty params args fstate)
        (bindParameters (cdr params) (cdr args) (addBinding fstate (car params) (M_value (car args) state)) state))))

(define M_statement
  (lambda (statement state return break continue throw)
    (cond
      [(eq? (operator statement) 'var)      (M_declare statement state)]
      [(eq? (operator statement) 'function) (M_function statement state)]
      [(eq? (operator statement) 'funcall)  (M_func_state (getBinding state (cadr statement)) (cddr statement) state (lambda (a) a) (lambda (a) a) (lambda (a) a) (lambda (a) a))]
      [(eq? (operator statement) '=)        (M_assignment statement state)]
      [(eq? (operator statement) 'return)   (M_return statement state return)]
      [(eq? (operator statement) 'if)       (M_if statement state return break continue throw)]
      [(eq? (operator statement) 'while)    (M_while statement state return break continue throw)]
      [(eq? (operator statement) 'begin)    (M_block (block-stmts statement) state return break continue throw)]
      [(eq? (operator statement) 'try)      (M_try_catch_finally statement state return break continue throw)]
      [(eq? (operator statement) 'continue) (continue state)]
      [(eq? (operator statement) 'break)    (break state)]
      [(eq? (operator statement) 'throw)    (throw (throw-value statement) state)])))


; --------------------- STATEMENT STATE FUNCTIONS ---------------------

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

(define copy
  (lambda (state)
    (cond
      [(null? state)       '()]
      [(list? (car state)) (cons (copy (car state)) (copy (cdr state)))]
      [else                (cons (car state) (copy (cdr state)))])))

(define rest-of-state cdr)

(define restore-scope
  (lambda (state func-name)
    (if (var-in-layer-vars? (top-layer-vars state) func-name)
        state
        (restore-scope (rest-of-state state) func-name))))

(define make-closure
  (lambda (funcname formalparams body state)
    (list formalparams body (lambda (s) (restore-scope (copy s) funcname)))))

; Takes in a function definition and creates a closure of the function to be added to the state.
(define M_function
  (lambda (statement state)
    (addBinding state (function-name statement) (make-closure (function-name statement) (formal-params statement) (function-body statement) state))))

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
    (if (M_boolean condition state)
        (loop condition body (M_statement body state return break (lambda (s) (break (loop condition body s return break throw))) throw) return break throw)
        state)))

(define M_if
  (lambda (statement state return break continue throw)
    (cond
      [(M_boolean (condition statement) state) (M_statement (body-stmt statement) state return break continue throw)]
      [(null? (else-stmts statement))          state]
      [(eq? (first-else-stmt statement) 'if)   (M_if (else-stmts statement) state return break continue throw)]
      [else                                    (M_statement (first-else-stmt statement) state return break continue throw)])))

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


; --------------------- VARIABLE / VALUE STATE FUNCTIONS ---------------------

(define var-name cadr)
(define var-value caddr)
(define var-value-list cddr)

; Processes statement in the form (return val). Calls return continuation with val
(define M_return
  (lambda (statement state return)
    (cond
      [(eq? (M_value (var-name statement) state) #t) (return 'true)]
      [(eq? (M_value (var-name statement) state) #f) (return 'false)]
      [else                                          (return (M_value (var-name statement) state))])))
    
; Processes statement in the form (= var val) and retuns an updated state.
; Updates binding of var with val in the state.
(define M_assignment
   (lambda (statement state)
     (updateBinding state (var-name statement) (M_value (var-value statement) state))))

; Processes statement in one of two forms and returns an updated state.
; (var x): Adds binding x to the state with initial value 'error.
; (var x val): Adds binding x to the state with initial value val.
(define M_declare
  (lambda (statement state)
    (if (null? (var-value-list statement))
        (addBinding state (var-name statement) 'error)
        (addBinding state (var-name statement) (M_value (var-value statement) state)))))