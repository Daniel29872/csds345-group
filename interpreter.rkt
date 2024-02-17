#lang racket

(require "simpleParser.rkt")

(define interpret
  (lambda (filename)
    (let ((syntax-tree (parser filename)))
          syntax-tree)))

(define M_boolean
  (lambda (exp)
    (cond
      [(eq? exp 'true) #t]
      ((eq? exp 'false) #f)
      ((eq? (operator exp) '==) (eq? (M_integer (leftoperand exp)) (M_integer (rightoperand exp))))
      [(eq? (operator exp) '!=) (not (M_integer (leftoperand exp)) (M_integer (rightoperand exp)))]
      [(eq? (operator exp) '<)  (<   (M_integer (leftoperand exp)) (M_integer (rightoperand exp)))]
      [(eq? (operator exp) '>)  (>   (M_integer (leftoperand exp)) (M_integer (rightoperand exp)))]
      [(eq? (operator exp) '<=) (<=  (M_integer (leftoperand exp)) (M_integer (rightoperand exp)))]
      [(eq? (operator exp) '>=) (>=  (M_integer (leftoperand exp)) (M_integer (rightoperand exp)))]
      [(eq? (operator exp) '&&) (eq? (M_boolean (leftoperand exp)) (M_boolean (rightoperand exp)))]
      [(eq? (operator exp) '||) (or  (M_boolean (leftoperand exp)) (M_boolean (rightoperand exp)))]
      [(eq? (operator exp) '!)  (not (M_boolean (leftoperand exp)))])))

(define M_integer
  (lambda (exp)
    (cond
      [(number? exp) exp]
      [(eq? (operator exp) '+) (+ (M_integer (leftoperand exp)) (M_integer (rightoperand exp)))]
      [(eq? (operator exp) '-) (- (M_integer (leftoperand exp)) (M_integer (rightoperand exp)))]
      [(eq? (operator exp) '*) (* (M_integer (leftoperand exp)) (M_integer (rightoperand exp)))]
      [(eq? (operator exp) '/) (quotient (M_integer (leftoperand exp)) (M_integer (rightoperand exp)))]
      [(eq? (operator exp) '%) (remainder (M_integer (leftoperand exp)) (M_integer (rightoperand exp)))])))

(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)

(define getBinding
  (lambda (state var)
    (cond
      [(null? (car state)) (error "using before declaring")]
      [(not (eq? (caar state) var)) (getBinding (cons (cdar state) (cons (cdadr state) '())) var)]
      [(eq? (caadr state) 'error) (error "using before assigning")]
      [else (caadr state)])))

(define addBinding
  (lambda (state var value)
    (cons (addBinding-rec (car state) var) (cons (addBinding-rec (car (cdr state)) value) '()))))

(define addBinding-rec
  (lambda (ls value)
    (if (null? ls)
        (cons value '())
        (cons (car ls) (addBinding-rec (cdr ls) value)))))
