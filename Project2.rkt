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

; subst: takes an in expression a symbol to look for and replacement expression. In the in expression
; any time the symbol to look for exists, replace that with the replacement expression.
(define (subst-helper [what : ExprC][for : Symbol][in : ExprC]) : ExprC
  (match in
    [(numC n) in]
    [(idC s) (cond
               [(eq? s for) what]
               [else in])]
    [(appC fun arg)(appC fun (subst-helper what for arg))]
    [(binop op l r) (binop op (subst-helper what for l)(subst-helper what for r))]
    [(ifleq0C test then else) (ifleq0C (subst-helper what for test)
                                       (subst-helper what for then)
                                       (subst-helper what for else))]))

(check-equal? (subst-helper (numC 2) 'x (binop '+ (idC 'x) (numC 5))) (binop '+ (numC 2) (numC 5)))
(check-equal? (subst-helper (numC 5) 'x (appC 'quadrouple (appC 'double (appC 'double (idC 'x)))))
                     (appC 'quadrouple (appC 'double (appC 'double (numC 5)))))
(check-equal? (subst-helper (numC 2) 'x (binop '+ (idC 'z) (numC 5))) (binop '+ (idC 'z) (numC 5)))
(check-equal? (subst-helper (numC 2) 'x (ifleq0C (idC 'x) (idC 'x) (binop '- (idC 'x) (numC 1))))
              (ifleq0C (numC 2) (numC 2) (binop '- (numC 2) (numC 1))))

(define (subst [what : Real][for : Symbol][in : ExprC]) : ExprC
  (subst-helper (numC what) for in))

(check-equal? (subst 4
                     'x
                     (binop '+ (idC 'x) (numC 5)))
              (binop '+ (numC 4) (numC 5)))

; get-fundef: takes a symbol and a list of functions and returns the function that has the symbol for a name
; If no functions match return "reference to undefined function"
(define (get-fundef [sym : Symbol][fds : (Listof FunDefC)]) : FunDefC
(cond
    [(empty? fds) (error 'get-fundef "reference to undefined function")]
    [(cons? fds) (cond
                   [(equal? sym (FunDefC-name (first fds))) (first fds)]
                   [else (get-fundef sym (rest fds))])]))

; This test case is definitely a little fucky, but it proves that get-fundef works. I dont think the functions
; defined in the list are necesarily formed in the correct way, but that shouldnt matter
(check-equal? (get-fundef 'f (list (FunDefC 'f 'x (binop '+ (idC 'x) (numC 14))) (FunDefC 'main 'init (appC 'f (numC 2)))))
              (FunDefC 'f 'x (binop '+ (idC 'x) (numC 14))))
(check-equal? (get-fundef 'main (list (FunDefC 'f 'x (binop '+ (idC 'x) (numC 14))) (FunDefC 'main 'init (appC 'f (numC 2)))))
              (FunDefC 'main 'init (appC 'f (numC 2))))
(check-exn (regexp (regexp-quote "reference to undefined function"))
           (lambda ()
             (get-fundef 'bad (list (FunDefC 'f 'x (binop '+ (idC 'x) (numC 14))) (FunDefC 'main 'init (appC 'f (numC 2)))))))

; takes binop operator names and maps them to their meaning
(define (binopLookup [b : binop][funList : (Listof FunDefC)]) : Real
  (match b
    [(binop '+ l r) (+ (interp l funList) (interp r funList))]
    [(binop '* l r) (* (interp l funList) (interp r funList))]
    [(binop '/ l r) (/ (interp l funList) (interp r funList))]
    [(binop '- l r) (- (interp l funList) (interp r funList))]
    [other ((error 'parse2 "not a valid binary operator"))]))

; interp takes in a ArithC expression and outputs the numeric result of that expression
(define (interp [a : ExprC][funList : (Listof FunDefC)]) : Real
  (match a
    [(numC n) n]
    [(ifleq0C test then els)
     (cond
       [(> (interp test funList) 0) (interp els funList)]
       [else (interp then funList)])]
    [(binop op l r) (binopLookup a funList)]
    [(appC fun arg) (interp (subst (interp arg funList)
                                    (FunDefC-arg (get-fundef fun funList))
                                    (FunDefC-body (get-fundef fun funList)))
                            funList)]
    [(idC n) (error 'interp "Shouldn't get here")]))

(check-equal? (interp (numC 5)
                      (list (FunDefC 'f 'x (binop '+ (idC 'x) (numC 14)))
                            (FunDefC 'main 'init (appC 'f (numC 2))))) 5)
(check-equal? (interp (binop '+ (binop '+ (numC 5) (numC 5)) (numC 5))
                      (list (FunDefC 'f 'x (binop '+ (idC 'x) (numC 14)))
                            (FunDefC 'main 'init (appC 'f (numC 2))))) 15)
(check-equal? (interp (binop '* (binop '+ (numC 5) (numC 5)) (numC 5))
                      (list (FunDefC 'f 'x (binop '+ (idC 'x) (numC 14)))
                            (FunDefC 'main 'init (appC 'f (numC 2))))) 50)
(check-equal? (interp (binop '- (binop '/ (numC 5) (numC 5)) (numC 1))
                      (list (FunDefC 'f 'x (binop '+ (idC 'x) (numC 14)))
                            (FunDefC 'main 'init (appC 'f (numC 2))))) 0)
(check-equal? (interp (ifleq0C (numC 5) (numC 1) (numC -1))
                      (list (FunDefC 'f 'x (binop '+ (idC 'x) (numC 14)))
                            (FunDefC 'main 'init (appC 'f (numC 2))))) -1)
(check-equal? (interp (appC 'f (numC 5))
                      (list (FunDefC 'f 'x (binop '+ (idC 'x) (numC 14)))
                            (FunDefC 'main 'init (appC 'f (numC 2))))) 19)

; Maps s-expressions directly to ExprC form
(define (parse [s : Sexp]) : ExprC
  (match s 
    [(? real? num) (numC num)]
    [(? symbol? sym) (idC sym)]
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

; pars-prog: Parse functions and outputs a list of functions
(define (parse-prog [s : (Listof Sexp)]) : (Listof FunDefC)
  (match s
    ['() '()]
    [else (cons (parse-fundef (first s)) (parse-prog (rest s)))]))

(check-equal? (parse-prog '{{fundef {f x} {+ x 14}}{fundef {main init} {f 2}}})
              (list (FunDefC 'f 'x (binop '+ (idC 'x) (numC 14)))
                    (FunDefC 'main 'init (appC 'f (numC 2)))))

; interp-fns: takes a list of FunDefCs and outputs the result of running the main function
(define (interp-fns [funList : (Listof FunDefC)]) : Real
  (interp (FunDefC-body (get-fundef 'main funList)) funList))

  
(check-equal? (interp-fns (list (FunDefC 'f 'x (binop '+ (idC 'x) (numC 14)))
                                (FunDefC 'main 'init (appC 'f (numC 2)))))
              16)

;top-interp: accepts an s-expression and calls the parser and then the desugar function for the assignment
(define (top-interp [s : (Listof Sexp)]) : Real
  (interp-fns (parse-prog s)))

(check-equal? (top-interp '{{fundef {f x} {+ x 14}}{fundef {main init} {f 2}}}) 16)