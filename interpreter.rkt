#lang racket

; Group Members: Ulises Soto, Daniel Arnholt, Jimmy Ngo

(require "simpleParser.rkt")

(define rest-of-tree cdr)
(define curr-statement car)

(define interpret
  (lambda (filename)
    (call/cc (lambda (return) (interpret-acc (parser filename) (new-state) return breakError continueError throwError))))) ;<-- Proper errors should be shown for calling continuations in invalid places

(define interpret-acc
  (lambda (syntax-tree state return break continue throw)
    (if (null? syntax-tree)
        ; something to be done if no return call is acceptable
        (return "void")
        (interpret-acc (rest-of-tree syntax-tree) (M_statement (curr-statement syntax-tree) state return break continue throw) return break continue throw))))

(define breakError
  (lambda (s)
    (error "Invalid break call")))

(define continueError
  (lambda (s)
    (error "Invalid continue call")))

(define throwError
  (lambda (s)
    (error "Invalid throw call")))


#|
    New functions for updating environment
|#

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


; --------------------- BINDING FUNCTIONS ---------------------

(define vars-list car)
(define vals-list cadr)
(define top-layer-vars caar)

(define first-var caar)
(define first-val caadr)

(define rest-of-vars cdar)
(define rest-of-vals cdadr)

#| TO BE UPDATED |#
(define removeBinding ; currently not used, but might be useful later
  (lambda (state var)
    (if (eq? (index-of (vars-list state) -1))
        (error "using before declaring")
        (cons (remove-n (vars-list state) (index-of (vars-list state)) (list (remove-n (vals-list state) (index-of (vars-list state)))))))))


#|
    New functions for state operations
|#

(define addBinding
  (lambda (state var val)
    (if (var-in-layer-vars? (top-layer-vars state) var)
        (error "already declared variable " var)
        (cons (add-to-layer (top-layer state) var val) (rest-of-layers state)))))

(define add-to-layer
  (lambda (layer var val)
    (cons (add-last (vars-list layer) var) (list (add-last (vals-list layer) val)))))


(define getBinding
  (lambda (state var)
    (cond
      [(null? state) (error "using before declaring " var)]
      [(var-in-layer-vars? (top-layer-vars state) var)
                     (get-binding-from-layer (car state) var)]
      [else          (getBinding (rest-of-layers state) var)])))

(define var-in-layer-vars?
  (lambda (layer-vars var)
    (cond
      [(null? layer-vars)         #f]
      [(eq? (car layer-vars) var) #t]
      [else                       (var-in-layer-vars? (cdr layer-vars) var)])))

(define get-binding-from-layer
  (lambda (layer var)
    (cond
      [(null? layer)                     (error "using before declaring" var)]
                                                                 ;removes first var and val of layer to get rest of layer
      [(not (eq? (first-var layer) var)) (get-binding-from-layer (cons (rest-of-vars layer) (list (rest-of-vals layer))) var)]
      [(eq? (first-val layer) 'error)    (error "using before assigning " var)]
      [else                              (first-val layer)])))

; Updates var binding in state. Searches for var in top layer and moves down layers until var found
; Retuns error if var is not found in any layer
(define updateBinding
  (lambda (state var val)
    (cond
      [(null? state) (error "using before declaring" var)]
      [(var-in-layer-vars? (top-layer-vars state) var)
                     (cons (update-layer-binding (top-layer state) var val) (rest-of-layers state))]
      [else          (cons (top-layer state) (updateBinding (rest-of-layers state) var val))])))

; Returns a layer with update binding for var
(define update-layer-binding
  (lambda (layer var val)
    (update-layer-binding-cps layer var val (lambda (vars vals) (cons vars (list vals))))))

(define update-layer-binding-cps
  (lambda (layer var val return)
    (cond
      [(null? layer) (error "using before declaring" var)]
      [(and (eq? (first-var layer) var) (same-type (first-val layer) val))
                     (return (vars-list layer) (cons val (rest-of-vals layer)))]
      [(eq? (first-var layer) var) (error "incorrect type assignment" var)]
                                               ;removes first var and val of layer to get rest of layer
      [else          (update-layer-binding-cps (cons (rest-of-vars layer) (list (rest-of-vals layer))) var val
                                               (lambda (vars vals) (return (cons (first-var layer) vars) (cons (first-val layer) vals))))])))

; --------------------- HELPER FUNCTIONS ---------------------

(define first-element car)
(define rest-of-list cdr)

; Add the value to the end of the list and return the updated list
(define add-last
  (lambda (ls value)
    (if (null? ls)
        (list value)
        (cons (first-element ls) (add-last (rest-of-list ls) value)))))

; Return the first index n where list[n] = value
(define index-of (lambda (ls value) (index-of-acc ls value 0)))

(define index-of-acc
  (lambda (ls value acc)
    (cond
      [(null? ls)                     -1]
      [(eq? (first-element ls) value) acc]
      [else                           (index-of-acc (rest-of-list ls) value (+ acc 1))])))

; -------------UNUSED------------- ;
; Get the nth element of the list and return its value
(define get-n
  (lambda (ls n)
    (cond
      [(null? ls) -1]
      [(zero? n)  (first-element ls)]
      [else       (get-n (rest-of-list ls) (- n 1))])))

; Remove the nth element of the list and return the updated list
(define remove-n
  (lambda (ls n)
    (if (zero? n)
        (rest-of-list ls)
        (cons (first-element ls) (remove-n (rest-of-list ls) (- n 1))))))

; -------------UNUSED------------- ;
; Update the nth element of the list and return the updated list
(define update-n
  (lambda (ls n value)
    (if (zero? n)
        (cons value (rest-of-list ls))
        (cons (first-element ls) (update-n (rest-of-list ls) (- n 1) value)))))

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
      [else                     (error "Not a Boolean")])))

(define M_integer
  (lambda (exp state)
    (cond
      [(number? exp)                                                  exp]
      [(not (list? exp))
            (if (number? (getBinding state exp))
                (getBinding state exp)
                (error "type error"))]
      [(and (eq? (operator exp) '-) (null? (rightoperand-list exp)))  (- 0 (M_integer (leftoperand exp) state))] ; unary -
      [(eq? (operator exp) '+)                                        (+ (M_integer (leftoperand exp) state) (M_integer (rightoperand exp) state))]
      [(eq? (operator exp) '-)                                        (- (M_integer (leftoperand exp) state) (M_integer (rightoperand exp) state))]
      [(eq? (operator exp) '*)                                        (* (M_integer (leftoperand exp) state) (M_integer (rightoperand exp) state))]
      [(eq? (operator exp) '/)                                        (quotient (M_integer (leftoperand exp) state) (M_integer (rightoperand exp) state))]
      [(eq? (operator exp) '%)                                        (remainder (M_integer (leftoperand exp) state) (M_integer (rightoperand exp) state))]
      [else                                                           (error "Not an Integer")])))

(define M_value
  (lambda (statement state)
    (cond
      [(number? statement)            statement]
      [(not (list? statement))        (getBinding state statement)]
      [(eq? (operator statement) '+)  (M_integer statement state)]
      [(eq? (operator statement) '-)  (M_integer statement state)]
      [(eq? (operator statement) '*)  (M_integer statement state)]
      [(eq? (operator statement) '/)  (M_integer statement state)]
      [(eq? (operator statement) '%)  (M_integer statement state)]
      [(eq? (operator statement) '==) (M_boolean statement state)]
      [(eq? (operator statement) '!=) (M_boolean statement state)]
      [(eq? (operator statement) '<)  (M_boolean statement state)]
      [(eq? (operator statement) '>)  (M_boolean statement state)]
      [(eq? (operator statement) '<=) (M_boolean statement state)]
      [(eq? (operator statement) '>=) (M_boolean statement state)]
      [(eq? (operator statement) '&&) (M_boolean statement state)]
      [(eq? (operator statement) '||) (M_boolean statement state)]
      [(eq? (operator statement) '!)  (M_boolean statement state)]
      [else                           (error "invalid operator")])))

(define M_statement
  (lambda (statement state return break continue throw)
    (cond
      [(eq? (operator statement) 'var)      (M_declare statement state)]
      [(eq? (operator statement) '=)        (M_assignment statement state)]
      [(eq? (operator statement) 'return)   (M_return statement state return)]
      [(eq? (operator statement) 'if)       (M_if statement state return break continue)]
      [(eq? (operator statement) 'while)    (M_while statement state return throw)]
      [(eq? (operator statement) 'begin)    (M_block statement state return break continue)]
      [(eq? (operator statement) 'continue) (continue state)]
      [(eq? (operator statement) 'break)    (break state)])))


; --------------------- STATEMENT STATE FUNCTIONS ---------------------

(define condition cadr)
(define body-stmt caddr)
(define else-stmts cdddr)
(define first-else-stmt cadddr)
(define block-stmts cdr)

(define M_block
  (lambda (statement state return break continue)
    (remove-layer (M_block-list (block-stmts statement) (add-layer state) return (lambda (s) (break (remove-layer s))) (lambda (s) (continue (remove-layer s)))))))

(define M_block-list
  (lambda (stmt-list state return break continue)
    (if (null? stmt-list)
        state
        (M_block-list (rest-of-tree stmt-list) (M_statement (curr-statement stmt-list) state return break continue 'throw) return break continue))))

(define M_while
  (lambda (statement state return throw)
    (call/cc
     (lambda (break) (loop (condition statement) (body-stmt statement) state return break throw)))))

(define loop
  (lambda (condition body state return break throw)
    (if (M_boolean condition state)
        (loop condition body (M_statement body state return break (lambda (s) (break (loop condition body s return break throw))) throw) return break throw)
        state)))

(define M_if
  (lambda (statement state return break continue)
    (cond
      [(M_boolean (condition statement) state) (M_statement (body-stmt statement) state return break continue 'throw)]
      [(null? (else-stmts statement))          state]
      [(eq? (first-else-stmt statement) 'if)   (M_if (else-stmts statement) state return)]
      [else                                    (M_statement (first-else-stmt statement) state return break continue 'throw)])))

(define M_try
  (lambda (statement state return break continue)
    (call/cc (throw) (M_try_catch (try statement)
                                  (catch statement)
                                  (finally statement)
                                  state return
                                  (lambda (s) (break (M_finally (finally statement) state return break continue)))
                                  (lambda (s) (continue (M_finally (finally statement) state return break continue)))
                                  (lambda (s) (throw (M_finally (finally statement) state return break continue)))))))

; --------------------- VARIABLE / VALUE STATE FUNCTIONS ---------------------

(define var-name cadr)
(define var-value caddr)
(define var-value-list cddr)

(define M_return
  (lambda (statement state return)
    (cond
      [(eq? (M_value (var-name statement) state) #t) (return 'true)]
      [(eq? (M_value (var-name statement) state) #f) (return 'false)]
      [else                                          (return (M_value (var-name statement) state))])))
    
(define M_assignment
   (lambda (statement state)
     (updateBinding state (var-name statement) (M_value (var-value statement) state))))

(define M_declare
  (lambda (statement state)
    (if (null? (var-value-list statement))
        (addBinding state (var-name statement) 'error)
        (addBinding state (var-name statement) (M_value (var-value statement) state)))))