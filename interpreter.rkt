#lang racket

; Group Members: Ulises Soto, Daniel Arnholt, Jimmy Ngo

(require "simpleParser.rkt")

(define rest-of-tree cdr)
(define curr-statement car)
(define new-state '(() ()))

(define interpret
  (lambda (filename)
    (interpret-acc (parser filename) new-state)))

(define interpret-acc
  (lambda (syntax-tree state)
    (if (null? syntax-tree)
        (M_value 'return state)
        (interpret-acc (rest-of-tree syntax-tree) (M_statement (curr-statement syntax-tree) state)))))


; --------------------- BINDING FUNCTIONS ---------------------

(define vars-list car)
(define vals-list cadr)

(define first-var caar)
(define first-val caadr)

(define rest-of-vars cdar)
(define rest-of-vals cdadr)

(define addBinding
  (lambda (state var value)
    (if (eq? (index-of (vars-list state) var) -1)
        (cons (add-last (vars-list state) var) (list (add-last (vals-list state) value)))
        (error "already declared variable"))))

(define getBinding
  (lambda (state var)
    (cond
      [(null? (vars-list state))         (error "using before declaring")]
      [(not (eq? (first-var state) var)) (getBinding (cons (rest-of-vars state) (list (rest-of-vals state))) var)]
      [(eq? (first-val state) 'error)    (error "using before assigning")]
      [else                              (first-val state)])))

(define removeBinding ; currently not used, but might be useful later
  (lambda (state var)
    (if (eq? (index-of (vars-list state) -1))
        (error "using before declaring")
        (cons (remove-n (vars-list state) (index-of (vars-list state)) (list (remove-n (vals-list state) (index-of (vars-list state)))))))))

(define updateBinding
  (lambda (state var value)
      (cond
        [(eq? (index-of (vars-list state) var) -1)                                          (error "using before declaring")]
        [(not (same-type (get-n (vals-list state) (index-of (vars-list state) var)) value)) (error "incorrect type assignment")]
        [else                                                                               (cons (vars-list state) (list (update-n (vals-list state) (index-of (vars-list state) var) value)))])))


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
      [(eq? exp 'true) #t]
      ((eq? exp 'false) #f)
      ((not (list? exp))
       (let ((value (getBinding state exp)))
              (if (or (eq? value #t) (eq? value #f))
                  value
                  (error "type error"))))
      ((eq? (operator exp) '==) (eq? (M_integer (leftoperand exp) state) (M_integer (rightoperand exp) state)))
      [(eq? (operator exp) '!=) (not (eq? (M_integer (leftoperand exp) state) (M_integer (rightoperand exp) state)))]
      [(eq? (operator exp) '<)  (<   (M_integer (leftoperand exp) state) (M_integer (rightoperand exp) state))]
      [(eq? (operator exp) '>)  (>   (M_integer (leftoperand exp) state) (M_integer (rightoperand exp) state))]
      [(eq? (operator exp) '<=) (<=  (M_integer (leftoperand exp) state) (M_integer (rightoperand exp) state))]
      [(eq? (operator exp) '>=) (>=  (M_integer (leftoperand exp) state) (M_integer (rightoperand exp) state))]
      [(eq? (operator exp) '&&) (eq? (M_boolean (leftoperand exp) state) (M_boolean (rightoperand exp) state))]
      [(eq? (operator exp) '||) (or  (M_boolean (leftoperand exp) state) (M_boolean (rightoperand exp) state))]
      [(eq? (operator exp) '!)  (not (M_boolean (leftoperand exp) state))]
      [else (error "Not a Boolean")])))

(define M_integer
  (lambda (exp state)
    (cond
      [(number? exp) exp]
      [(not (list? exp))
       (let ((value (getBinding state exp)))
              (if (number? value)
                  value
                  (error "type error")))]
      [(and (eq? (operator exp) '-) (null? (rightoperand-list exp))) (- 0 (M_integer (leftoperand exp) state))] ; unary -
      [(eq? (operator exp) '+) (+ (M_integer (leftoperand exp) state) (M_integer (rightoperand exp) state))]
      [(eq? (operator exp) '-) (- (M_integer (leftoperand exp) state) (M_integer (rightoperand exp) state))]
      [(eq? (operator exp) '*) (* (M_integer (leftoperand exp) state) (M_integer (rightoperand exp) state))]
      [(eq? (operator exp) '/) (quotient (M_integer (leftoperand exp) state) (M_integer (rightoperand exp) state))]
      [(eq? (operator exp) '%) (remainder (M_integer (leftoperand exp) state) (M_integer (rightoperand exp) state))]
      [else (error "Not an Integer")])))

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
  (lambda (statement state)
    [cond
      [(equal? (operator statement) 'var)    (M_declare statement state)]
      [(equal? (operator statement) '=)      (M_assignment statement state)]
      [(equal? (operator statement) 'return) (M_return statement state)]
      [(equal? (operator statement) 'if)     (M_if statement state)]
      [(equal? (operator statement) 'while)  (M_while statement state)]]))


; --------------------- STATEMENT STATE FUNCTIONS ---------------------

(define condition cadr)
(define body-stmt caddr)
(define else-stmts cdddr)
(define first-else-stmt cadddr)

(define M_while
  (lambda (statement state)
    (if (M_boolean (condition statement) state)
        (M_while statement (M_statement (body-stmt statement) state))
        state)))

(define M_if
  (lambda (statement state)
    (cond
      [(M_boolean (condition statement) state) (M_statement (body-stmt statement) state)]
      [(null? (else-stmts statement))          state]
      [(eq? (first-else-stmt statement) 'if)   (M_if (else-stmts statement) state)]
      [else                                    (M_statement (first-else-stmt statement) state)])))

; --------------------- VARIABLE / VALUE STATE FUNCTIONS ---------------------

(define var-name cadr)
(define var-value caddr)
(define var-value-list cddr)

(define M_return
  (lambda (statement state)
    (cond
      [(eq? (M_value (var-name statement) state) #t) (updateBinding (M_declare '(var return) state) 'return 'true)]
      [(eq? (M_value (var-name statement) state) #f) (updateBinding (M_declare '(var return) state) 'return 'false)]
      [else                                          (updateBinding (M_declare '(var return) state) 'return (M_value (var-name statement) state))])))

(define M_assignment
   (lambda (statement state)
     (updateBinding state (var-name statement) (M_value (var-value statement) state))))

(define M_declare
  (lambda (statement state)
    (if (null? (var-value-list statement))
        (addBinding state (var-name statement) 'error)
        (addBinding state (var-name statement) (M_value (var-value statement) state)))))