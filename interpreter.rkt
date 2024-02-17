#lang racket

(require "simpleParser.rkt")

(define interpret
  (lambda (filename)
    (interpret-acc (parser filename) '(() ()))))

(define interpret-acc
  (lambda (syntax-tree state)
    (if (null? syntax-tree)
        (getBinding state 'return)
        (interpret-acc (cdr syntax-tree) (M_statement (car syntax-tree) state)))))

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
      [(eq? (operator exp) '!)  (not (M_boolean (leftoperand exp) state))])))

(define M_integer
  (lambda (exp state)
    (cond
      [(number? exp) exp]
      [(not (list? exp))
       (let ((value (getBinding state exp)))
              (if (number? value)
                  value
                  (error "type error")))]
      [(eq? (operator exp) '+) (+ (M_integer (leftoperand exp) state) (M_integer (rightoperand exp) state))]
      [(eq? (operator exp) '-) (- (M_integer (leftoperand exp) state) (M_integer (rightoperand exp) state))]
      [(eq? (operator exp) '*) (* (M_integer (leftoperand exp) state) (M_integer (rightoperand exp) state))]
      [(eq? (operator exp) '/) (quotient (M_integer (leftoperand exp) state) (M_integer (rightoperand exp) state))]
      [(eq? (operator exp) '%) (remainder (M_integer (leftoperand exp) state) (M_integer (rightoperand exp) state))])))

(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)


; --------------------- BINDING FUNCTIONS ---------------------

(define addBinding
  (lambda (state var value)
    (let ([n (index-of (car state) var)])
      (if (eq? n -1)
          (cons (add-last (car state) var) (cons (add-last (cadr state) value) '()))
          (error "already declared variable")))))

(define getBinding
  (lambda (state var)
    (cond
      [(null? (car state)) (error "using before declaring")]
      [(not (eq? (caar state) var)) (getBinding (cons (cdar state) (cons (cdadr state) '())) var)]
      [(eq? (caadr state) 'error) (error "using before assigning")]
      [else (caadr state)])))

(define removeBinding
  (lambda (state var)
    (let ([n (index-of (car state) var)])
      (if (eq? n -1)
          (error "using before declaring")
          (cons (remove-n (car state) n) (cons (remove-n (cadr state) n) '()))))))

(define updateBinding
  (lambda (state var value)
    (let* ([n (index-of (car state) var)]
          [prev (get-n (cadr state) n)])
      (cond
        [(eq? n -1) (error "using before declaring")]
        [(or (eq? prev 'error) (same-type prev value)) (cons (car state) (cons (update-n (cadr state) n value) '()))]
        [else (error (format "assigning incorrect type ~a ~a" prev value))]))))


; --------------------- HELPER FUNCTIONS ---------------------

; Add the value to the end of the list and return the updated list
(define add-last
  (lambda (ls value)
    (if (null? ls)
        (cons value '())
        (cons (car ls) (add-last (cdr ls) value)))))

; Return the first index n where list[n] = value
(define index-of (lambda (ls value) (index-of-acc ls value 0)))

(define index-of-acc
  (lambda (ls value acc)
    (cond
      [(null? ls) -1]
      [(eq? (car ls) value) acc]
      [else (index-of-acc (cdr ls) value (+ acc 1))])))

; Get the nth element of the list and return its value
(define get-n
  (lambda (ls n)
    (cond
      [(null? ls) (error "list index out of range")]
      [(zero? n) (car ls)]
      [else (get-n (cdr ls) (- n 1))])))

; Remove the nth element of the list and return the updated list
(define remove-n
  (lambda (ls n)
    (if (zero? n)
        (cdr ls)
        (cons (car ls) (remove-n (cdr ls) (- n 1))))))

; Update the nth element of the list and return the updated list
(define update-n
  (lambda (ls n value)
    (if (zero? n)
        (cons value (cdr ls))
        (cons (car ls) (update-n (cdr ls) (- n 1) value)))))

; Check if two values a and b are the same type
(define same-type
  (lambda (a b)
    (cond
      [(and (number? a) (number? b)) #t]
      [(and (boolean? a) (boolean? b)) #t]
      [else #f])))


; --------------------- STATE FUNCTIONS ---------------------

(define M_statement
  (lambda (statement state)
    [cond
      [(equal? (car statement) 'var)    (M_declare statement state)]
      [(equal? (car statement) '=)      (M_assignment statement state)] ;(M_assign statement state)]
      [(equal? (car statement) 'return) (M_return statement state)] ;TODO return statement
      [(equal? (car statement) 'if)     (M_if statement state)]
      [(equal? (car statement) 'while)  (M_while statement state)]]))

(define M_return
  (lambda (statement state)
    (updateBinding (M_declare '(var return) state) 'return (cadr statement))))

(define M_assignment
  (lambda (statement state)
    (let ((var (cadr statement))
          (value (M_integer (caddr statement) state))) ; the value from evaluating the expression
                                                       ; TODO: only evalues exp returning an int
      (updateBinding state var value))))

(define M_while
  (lambda (statement state)
    (let [(condition (cadr statement))
          (body-stmt (caddr statement))]
      (if (M_boolean condition state)
          (M_while statement (M_statement body-stmt state))
          state))))

(define M_if
  (lambda (statement state)
    (let ((condition (cadr statement))
          (body-stmt (caddr statement)))
      (if (M_boolean condition state)
          (M_while statement (M_statement body-stmt state))
          state))))

(define M_declare
  (lambda (statement state)
    (let ((name (cadr statement)))
      (if (null? (cddr statement))
          (addBinding state name 'error)
          (addBinding state name 'TODO))))) ; TODO: Handle cases like var x = 2 * 5 + 4
  