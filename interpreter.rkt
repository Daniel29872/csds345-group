#lang racket

(require "simpleParser.rkt")

(define interpret
  (lambda (filename)
    (parser filename)))