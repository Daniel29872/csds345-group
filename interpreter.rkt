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