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


(define addBinding
  (lambda (state var value)
    (cons (addBinding-rec (car state) var) (cons (addBinding-rec (car (cdr state)) value) '()))))

(define addBinding-rec
  (lambda (ls value)
    (if (null? ls)
        (cons value '())
        (cons (car ls) (addBinding-rec (cdr ls) value)))))
      
    


