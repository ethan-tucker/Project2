#lang typed/racket

(require typed/rackunit)

; We completed the entire project

; Defining The ExprC Language
(struct numC ([n : Real]) #:transparent)
(struct ifleq0C ([test : ExprC][then : ExprC][else : ExprC]) #:transparent)
(struct binop ([op : Symbol][l : ExprC][r : ExprC]) #:transparent)
(struct idC ([s : Symbol]) #:transparent)
(struct appC ([fun : Symbol][arg : ExprC]) #:transparent)
(define-type ExprC (U numC ifleq0C binop idC appC))

; Defining the FunDefC structure. This structure represents a function with a name,
; argument and body
(struct FunDefC ([name : Symbol][arg : Symbol][body : ExprC]) #:transparent)

; top-interp: accepts an s-expression and calls the parser function for the assignment
(define (top-interp [s : (Listof Sexp)]) : Real
  (interp-fns (parse-prog s)))

; interp: takes in a ExprC expression and a list of FunDefC and outputs the
; numeric result of that expression
(define (interp [a : ExprC][funList : (Listof FunDefC)]) : Real
  (match a
    [(numC n) n]
    [(ifleq0C test then els)
     (cond
       [(> (interp test funList) 0) (interp els funList)]
       [else (interp then funList)])]
    [(binop op l r) (binopLookup a funList)]
    [(appC fun arg) (define func : FunDefC (get-fundef fun funList))
                    (interp (subst (interp arg funList)
                                    (FunDefC-arg func)
                                    (FunDefC-body func))
                            funList)]
    [(idC n) (error 'interp "DXUQ Shouldn't get here")]))

; pars-prog: Takes in an Sexp, parses each function in that Sexp and outputs a list of functions
(define (parse-prog [s : Sexp]) : (Listof FunDefC)
  (for/list : (Listof FunDefC) ([fun (in-list s)]) (parse-fundef fun)))

; parse-fundef: Parses a function definition
; Takes in an s-expression and outputs a FunDefC
(define (parse-fundef [f : Sexp]) : FunDefC
  (match f
    [(list 'fundef (list (? symbol? name) (? symbol? arg)) body)
     (FunDefC name arg (parse body))]
    [else (error 'parse-fundef "DXUQ function formed incorrectly")]))

; interp-fns: takes a list of FunDefCs and outputs the result of running the main function with
; an init value of 0
(define (interp-fns [funList : (Listof FunDefC)]) : Real
  (interp (appC 'main (numC 0)) funList))

; get-fundef: takes a symbol and a list of functions and returns the function that has the symbol for a name
; If no functions match that symbol return "reference to undefined function"
(define (get-fundef [sym : Symbol][fds : (Listof FunDefC)]) : FunDefC
(cond
    [(empty? fds) (error 'get-fundef "DXUQ reference to undefined function")]
    [(cons? fds) (cond
                   [(equal? sym (FunDefC-name (first fds))) (first fds)]
                   [else (get-fundef sym (rest fds))])]))

; parse: Maps s-expressions directly from Sexp to ExprC
(define (parse [s : Sexp]) : ExprC
  (match s 
    [(? real? num) (numC num)]
    [(? symbol? sym) (validate-symbol sym)]
    [(list (? symbol? sym) arg) (appC sym (parse arg))]
    [(list (? symbol? sym) l r) (binop sym (parse l) (parse r))]
    [(list 'ifleq0 test then else)(ifleq0C (parse test) (parse then) (parse else))]
    [other (error 'parse2 "DXUQ input ~e was not well formed" s)]))

; validate-symbol: Takes in a symbol and throws an error if it is one of our operations. If it is not return
; the symbol wrapped in with idC
(define (validate-symbol [sym : Symbol]) : ExprC
  (cond
    [(member sym '(+ * / - ifleq0 fundef)) (error 'invalid-symbol "DXUQ invalid symbol")]
    [else (idC sym)]
   ))

; subst: takes in a Real (what you are going to insert) a Symbol (what is going to be replaced) and an
; expression to look for the symbol to replace. Subst wraps the Real in a numC and calls the subst-
; helper function
(define (subst [what : Real][for : Symbol][in : ExprC]) : ExprC
  (subst-helper (numC what) for in))

; subst-helper: takes an in expression a symbol to look for and replacement expression. In the
; in expression any time the symbol to look for exists, replace that with the replacement expression.
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

; binopLookup: takes binop operator names and a list of functions and maps them each to their
; intended functionality. Evaluates based on that functionality.
(define (binopLookup [b : binop][funList : (Listof FunDefC)]) : Real
  (match b
    [(binop '+ l r) (+ (interp l funList) (interp r funList))]
    [(binop '* l r) (* (interp l funList) (interp r funList))]
    [(binop '- l r) (- (interp l funList) (interp r funList))]
    [(binop '/ l r) (cond
                      [(= (interp r funList) 0) (error 'binopLookup "DXUQ divide by 0 error")]
                      [else (/ (interp l funList) (interp r funList))])]
    [other ((error 'binopLookup "DXUQ not a valid binary operator"))]))

; subst-helper test cases
(check-equal? (subst-helper (numC 2) 'x (binop '+ (idC 'x) (numC 5))) (binop '+ (numC 2) (numC 5)))
(check-equal? (subst-helper (numC 5) 'x (appC 'quadrouple (appC 'double (appC 'double (idC 'x)))))
                     (appC 'quadrouple (appC 'double (appC 'double (numC 5)))))
(check-equal? (subst-helper (numC 2) 'x (binop '+ (idC 'z) (numC 5))) (binop '+ (idC 'z) (numC 5)))
(check-equal? (subst-helper (numC 2) 'x (ifleq0C (idC 'x) (idC 'x) (binop '- (idC 'x) (numC 1))))
              (ifleq0C (numC 2) (numC 2) (binop '- (numC 2) (numC 1))))

; subst test cases
(check-equal? (subst 4
                     'x
                     (binop '+ (idC 'x) (numC 5)))
              (binop '+ (numC 4) (numC 5)))

; This test case is definitely a little fucky, but it proves that get-fundef works. I dont think the functions
; defined in the list are necesarily formed in the correct way, but that shouldnt matter
(check-equal? (get-fundef 'f (list (FunDefC 'f 'x (binop '+ (idC 'x) (numC 14)))
                                   (FunDefC 'main 'init (appC 'f (numC 2)))))
              (FunDefC 'f 'x (binop '+ (idC 'x) (numC 14))))
(check-equal? (get-fundef 'main (list (FunDefC 'f 'x (binop '+ (idC 'x) (numC 14)))
                                      (FunDefC 'main 'init (appC 'f (numC 2)))))
              (FunDefC 'main 'init (appC 'f (numC 2))))
(check-exn (regexp (regexp-quote "DXUQ reference to undefined function"))
           (lambda ()
             (get-fundef 'bad (list (FunDefC 'f 'x (binop '+ (idC 'x) (numC 14)))
                                    (FunDefC 'main 'init (appC 'f (numC 2)))))))

; binopLookup tests
(check-exn (regexp (regexp-quote "DXUQ divide by 0 error"))
           (lambda ()
             (binopLookup (binop '/ (numC 6) (numC 0))
                          (list (FunDefC 'f 'x (binop '+ (idC 'x) (numC 14)))
                                (FunDefC 'main 'init (appC 'f (numC 2)))))))
(check-exn (regexp (regexp-quote "DXUQ not a valid binary operator"))
           (lambda ()
             (binopLookup (binop 'l (numC 6) (numC 9))
                          (list (FunDefC 'f 'x (binop '+ (idC 'x) (numC 14)))
                                (FunDefC 'main 'init (appC 'f (numC 2)))))))

; interp tests
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
(check-equal? (interp (ifleq0C (numC -1) (numC 1) (numC -1))
                      (list (FunDefC 'f 'x (binop '+ (idC 'x) (numC 14)))
                            (FunDefC 'main 'init (appC 'f (numC 2))))) 1)
(check-equal? (interp (appC 'f (numC 5))
                      (list (FunDefC 'f 'x (binop '+ (idC 'x) (numC 14)))
                            (FunDefC 'main 'init (appC 'f (numC 2))))) 19)
(check-exn (regexp (regexp-quote "DXUQ Shouldn't get here"))
           (lambda ()
             (interp (idC 'fail)
                      (list (FunDefC 'f 'x (binop '+ (idC 'x) (numC 14)))
                            (FunDefC 'main 'init (appC 'f (numC 2)))))))

; parse test cases
(check-equal? (parse 5) (numC 5))
(check-exn (regexp (regexp-quote "invalid-symbol: DXUQ invalid symbol"))
           (lambda () (parse '(+ - 5))))
(check-equal? (parse '(+ 5 6)) (binop '+ (numC 5) (numC 6)))
(check-equal? (parse '(* 2 2)) (binop '* (numC 2) (numC 2)))
(check-equal? (parse '(ifleq0 7 5 6)) (ifleq0C (numC 7) (numC 5) (numC 6)))
(check-equal? (parse '(ifleq0 (+ 7 1) (* 5 2) 6))
              (ifleq0C (binop '+ (numC 7) (numC 1)) (binop '* (numC 5) (numC 2)) (numC 6)))
(check-exn (regexp (regexp-quote "DXUQ input \"huh\" was not well formed"))
           (lambda () (parse "huh")))

; parse fundef test cases
(check-equal? (parse-fundef '{fundef {f x} {+ x 14}}) (FunDefC 'f 'x (binop '+ (idC 'x) (numC 14))))
(check-equal? (parse-fundef '{fundef {quadruple x} {double {double x}}})
              (FunDefC 'quadruple 'x (appC 'double (appC 'double (idC 'x)))))

; parse prog test cases
(check-equal? (parse-prog '{{fundef {f x} {+ x 14}}{fundef {main init} {f 2}}})
              (list (FunDefC 'f 'x (binop '+ (idC 'x) (numC 14)))
                    (FunDefC 'main 'init (appC 'f (numC 2)))))

; interp fns test cases
(check-equal? (interp-fns (list (FunDefC 'f 'x (binop '+ (idC 'x) (numC 14)))
                                (FunDefC 'main 'init (appC 'f (numC 2)))))
              16)

; top-interp test cases
(check-equal? (top-interp '{{fundef {f x} {+ x 14}}{fundef {main init} {f 2}}}) 16)
(check-exn (regexp (regexp-quote "DXUQ function formed incorrectly"))
           (lambda () (top-interp '{{fundef {main} {+ 2 2}}})))
(check-equal? (top-interp '{{fundef {double x} {+ x x}}
                            {fundef {quadrouple x} {double {double x}}}
                            {fundef {f x} {quadrouple x}}{fundef {main init} {f 2}}}) 8)

; *The* big test case. This actually took me longer than I would like to admit to get working.
(check-equal? (top-interp (quote ((fundef (minus-five x) (+ x (* -1 5)))
                                  (fundef (main init) (minus-five (+ 7 init)))))) 2)
(check-equal? (top-interp '{{fundef {round x}
                                    {ifleq0 {- x .5} 0 {+ 1 {round {- x 1}}}}}
                            {fundef {main init} {round 2.6}}}) 3)
(check-equal? (top-interp '{{fundef {round x}
                                    {ifleq0 {- x .5} 0 {+ 1 {round {- x 1}}}}}
                            {fundef {main init} {round 2.1}}}) 2)
(check-equal? (top-interp '{{fundef {round x}
                                    {ifleq0 {- x .5} 0 {+ 1 {round {- x 1}}}}}
                            {fundef {main init} {round .1}}}) 0)