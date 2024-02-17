#lang racket

(require "simpleParser.rkt")

(define interpret
  (lambda (filename)
    (let ((syntax-tree (parser filename)))
          syntax-tree)))

(define M_boolean
  (lambda (exp state)
    (cond
      [(eq? exp 'true) #t]
      ((eq? exp 'false) #f)
      ((eq? (operator exp) '==) (eq? (M_integer (leftoperand exp) state) (M_integer (rightoperand exp) state)))
      [(eq? (operator exp) '!=) (not (M_integer (leftoperand exp) state) (M_integer (rightoperand exp) state))]
      [(eq? (operator exp) '<)  (<   (M_integer (leftoperand exp) state) (M_integer (rightoperand exp) state))]
      [(eq? (operator exp) '>)  (>   (M_integer (leftoperand exp) state) (M_integer (rightoperand exp) state))]
      [(eq? (operator exp) '<=) (<=  (M_integer (leftoperand exp) state) (M_integer (rightoperand exp) state))]
      [(eq? (operator exp) '>=) (>=  (M_integer (leftoperand exp) state) (M_integer (rightoperand exp) state))]
      [(eq? (operator exp) '&&) (eq? (M_boolean (leftoperand exp) state) (M_boolean (rightoperand exp) state))]
      [(eq? (operator exp) '||) (or  (M_boolean (leftoperand exp) state) (M_boolean (rightoperand exp) state))]
      [(eq? (operator exp) '!)  (not (M_boolean (leftoperand exp) state))]
      [else (let ((value (getBinding state exp)))
              (if (or (eq? value #t) (eq? value #f))
                  value
                  (error "incorrect type")))])))

(define M_integer
  (lambda (exp state)
    (cond
      [(number? exp) exp]
      [(eq? (operator exp) '+) (+ (M_integer (leftoperand exp)) (M_integer (rightoperand exp)))]
      [(eq? (operator exp) '-) (- (M_integer (leftoperand exp)) (M_integer (rightoperand exp)))]
      [(eq? (operator exp) '*) (* (M_integer (leftoperand exp)) (M_integer (rightoperand exp)))]
      [(eq? (operator exp) '/) (quotient (M_integer (leftoperand exp)) (M_integer (rightoperand exp)))]
      [(eq? (operator exp) '%) (remainder (M_integer (leftoperand exp)) (M_integer (rightoperand exp)))]
      [else (let ((value (getBinding state exp)))
              (if (number? value)
                  value
                  (error "incorrect type")))])))

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
    (let ([n (index-of (car state) var)])
      (if (eq? n -1)
          (error "using before declaring")
          (cons (car state) (cons (update-n (cadr state) n value) '()))))))


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


; --------------------- STATE FUNCTIONS ---------------------

(define M_statement
  (lambda (statement state)
    [cond
      [(equal? (car statement) 'var)    (M_declare statement state)]
      [(equal? (car statement) '=)      'TODO] ;(M_assign statement state)]
      [(equal? (car statement) 'return) 'TODO] ;TODO return statement
      [(equal? (car statement) 'if)     (M_if statement state)]
      [(equal? (car statement) 'while)  (M_while statement state)]]))

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

;(define M_assign
;  (lambda (statement state)
;    (let ((var (cadr statement))
;          (value (caddr statement)))
;      (if (

(define M_declare
  (lambda (statement state)
    (let ((name (cadr statement)))
      (addBinding state name 'error))))