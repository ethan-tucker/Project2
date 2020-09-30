#lang typed/racket

(require typed/rackunit)

; Defining The ExprC Language
(struct numC ([n : Real]) #:transparent)
(struct ifleq0C ([test : ExprC][then : ExprC][else : ExprC]) #:transparent)
(struct binop ([op : Symbol][l : ExprC][r : ExprC]) #:transparent)
(struct idC ([s : Symbol]) #:transparent)
(struct appC ([fun : Symbol][arg : ExprC]) #:transparent)
(define-type ExprC (U numC ifleq0C binop idC appC))

; Defining the FunDefC structure
(struct FunDefC ([name : Symbol][arg : Symbol][body : ExprC]) #:transparent)

; takes binop operator names and maps them to their meaning
(define (binopLookup [b : binop]) : Real
  (match b
    [(binop '+ l r) (+ (interp l) (interp r))]
    [(binop '* l r) (* (interp l) (interp r))]
    [(binop '/ l r) (/ (interp l) (interp r))]
    [(binop '- l r) (- (interp l) (interp r))]
    [other ((error 'parse2 "not a valid binary operator"))]))

; interp takes in a ArithC expression and outputs the numeric result of that expression
(define (interp [a : ExprC]) : Real
  (match a
    [(numC n) n]
    [(ifleq0C test then els)
     (cond
       [(> (interp test) 0) (interp els)]
       [else (interp then)])]
    [(binop op l r) (binopLookup a)]
    [(appC fun arg) (error 'interp "appC not implemented yet")]))

(check-equal? (interp (numC 5)) 5)
(check-equal? (interp (binop '+ (binop '+ (numC 5) (numC 5)) (numC 5))) 15)
(check-equal? (interp (binop '* (binop '+ (numC 5) (numC 5)) (numC 5))) 50)
(check-equal? (interp (ifleq0C (numC 5) (numC 1) (numC -1))) -1)

; Maps s-expressions directly to ExprC form
(define (parse [s : Sexp]) : ExprC
  (match s 
    [(? real? num) (numC num)]
    [(? symbol? sym) (idC sym)]
    ; Attempting to catch the application case
    ; I definitely dont think this is a correct way of handling this
    [(list (? symbol? sym) arg) (appC sym (parse arg))]
    [(list (? symbol? sym) l r) (binop sym (parse l) (parse r))]
    [(list 'ifleq0 test then else)(ifleq0C (parse test) (parse then) (parse else))]
    [other (error 'parse2 "input ~e was not well formed" s)]))

(check-equal? (parse 5) (numC 5))
(check-equal? (parse '(+ 5 6)) (binop '+ (numC 5) (numC 6)))
(check-equal? (parse '(* 2 2)) (binop '* (numC 2) (numC 2)))
(check-equal? (parse '(ifleq0 7 5 6)) (ifleq0C (numC 7) (numC 5) (numC 6)))
(check-equal? (parse '(ifleq0 (+ 7 1) (* 5 2) 6))
              (ifleq0C (binop '+ (numC 7) (numC 1)) (binop '* (numC 5) (numC 2)) (numC 6)))
(check-exn (regexp (regexp-quote "input \"huh\" was not well formed"))
           (lambda () (parse "huh")))

; Parses a function definition
; Takes in an s-expression and outputs function defintitions
(define (parse-fundef [f : Sexp]) : FunDefC
  (match f
    [(list 'fundef (list (? symbol? name) (? symbol? arg)) body)
     (FunDefC name arg (parse body))]))

(check-equal? (parse-fundef '{fundef {f x} {+ x 14}}) (FunDefC 'f 'x (binop '+ (idC 'x) (numC 14))))
(check-equal? (parse-fundef '{fundef {quadruple x} {double {double x}}})
              (FunDefC 'quadruple 'x (appC 'double (appC 'double (idC 'x)))))

; top-interp: accepts an s-expression and calls the parser and then the desugar function for the assignment
(define (top-interp [s : Sexp]) : Number
  (interp (parse s)))

(check-equal? (top-interp '(+ 5 6)) 11)
(check-equal? (top-interp '(/ 5 5)) 1)
(check-equal? (top-interp '(- 5 5)) 0)
(check-equal? (top-interp 5) 5)
(check-equal? (top-interp '(* 2 2)) 4)
(check-equal? (top-interp '(ifleq0 (+ 5 6) 5 6)) 6)
(check-equal? (top-interp '(ifleq0 -5 5 (* 5 6))) 5)