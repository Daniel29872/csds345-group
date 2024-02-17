#lang racket

(require "simpleParser.rkt")

(define interpret
  (lambda (filename)
    (interpret-rec (parser filename))))

(define interpret-rec
  (lambda (syntax-tree)
    (cond
      [(null? syntax-tree) '()]
      [else '()])))

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

(define operator (lambda (exp) (car exp)))
(define leftoperand cadr)
(define rightoperand caddr)
