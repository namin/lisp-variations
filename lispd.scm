(define-record-type fun (fields (immutable f)))
(define-record-type cont (fields (immutable f)))
(define-record-type fsubr (fields (immutable f)))
(define-record-type fexpr (fields (immutable f)))

(define (apply-cont cont v)
  ((cont-f cont) v))

(define (base-eval exp env cont)
  (cond
    ((or (number? exp) (boolean? exp)) (apply-cont cont exp))
    ((symbol? exp) (eval-var exp env cont))
    ((pair? exp) (base-apply exp env cont))
    (error 'base-eval "unknown exp" exp)))

(define (base-apply exp env cont)
  (assert (list? exp))
  (let ((fun (car exp))
        (args (cdr exp)))
    (base-eval
     fun env
     (make-cont
      (lambda (vf)
        (cond
          ((fun? vf)
           (evlist args env (make-cont (lambda (vas) ((fun-f vf) vas cont)))))
          ((cont? vf)
           (evlist args env (make-cont (lambda (vas)
                                         (assert (= 1 (length vas)))
                                         (apply-cont vf (car vas))))))
          ((fsubr? vf)
           ((fsubr-f vf) (list exp env cont)))
          ((fexpr? vf)
           (apply-cont cont ((fexpr-f vf) args)))
          (else 'base-apply "unknown function" vs)))))))

(define (eval-call/cc exp env cont)
  (let ((f (cadr exp)))
    (base-apply `(,f (quote ,cont)) env cont)))

(define (eval-var exp env cont)
  (assert (symbol? exp))
  (apply-cont cont (get env exp)))

(define (eval-quote exp env cont)
  (apply-cont cont (cadr exp)))

(define (eval-if exp env cont)
  (let ((c (cadr exp))
        (a (caddr exp))
        (b (cadddr exp)))
    (base-eval
     c env
     (make-cont
      (lambda (cv)
        (if cv
            (base-eval a env cont)
            (base-eval b env cont)))))))

(define (eval-set! exp env cont)
  (let ((x (cadr exp))
        (rhs (caddr exp)))
    (assert (symbol? x))
    (base-eval
     rhs env
     (make-cont
      (lambda (v)
        (apply-cont cont (set env x v)))))))

(define (eval-fun c)
  (lambda (exp env cont)
    (let ((params (cadr exp))
          (body (cddr exp)))
      (apply-cont cont (c (lambda (args)
                            (eval-begin body (extend env params args)
                                        (make-cont (lambda (v) v)))))))))

(define (eval-lambda exp env cont)
  (let ((params (cadr exp))
        (body (cddr exp)))
    (apply-cont cont (make-fun (lambda (args c)
                                 (eval-begin body (extend env params args) c))))))

(define eval-fsubr (eval-fun make-fsubr))
(define eval-fexpr (eval-fun make-fexpr))

(define (eval-begin-exp exp env cont)
  (let ((body (cdr exp)))
    (eval-begin body env cont)))

(define (eval-begin exp env cont)
  (if (null? (cdr exp))
      (base-eval (car exp) env cont)
      (base-eval (car exp) env (make-cont (lambda (_) (eval-begin (cdr exp) env cont))))))

(define (eval-define exp env cont)
  (let ((name (cadr exp))
        (body (cddr exp)))
    (let ((p (cons name (void))))
      (set-car! env (cons p (car env)))
      (eval-begin body env (make-cont (lambda (v)
                                        (set-cdr! p v)
                                        (apply-cont cont name)))))))
(define (evlist exp env cont)
  (if (null? exp)
      (apply-cont cont '())
      (base-eval (car exp) env (make-cont (lambda (v) (evlist (cdr exp) env (make-cont (lambda (vs) (apply-cont cont (cons v vs))))))))))

(define (get env x)
  (cdr (lookup env x)))
(define (set env x v)
  (let ((p (lookup env x)))
    (set-cdr! p v)
    v))
(define (lookup env x)
  (assq x (find (lambda (frame) (assq x frame)) env)))
(define (extend env params args)
  (cons (map cons params args) env))

(define (fc f) (make-fun (lambda (v c) (apply-cont c (f v)))))

(define (fsubr-of f)
  (make-fsubr (lambda (v)
                (let ((exp (car v))
                      (env (cadr v))
                      (cont (caddr v)))
                  (f exp env cont)))))

(define (make-init-env)
  (letrec
      ((init-env
        (delay
          (list
           (list
            (cons '<        (fc (lambda (args) (< (car args) (cadr args)))))
            (cons '+        (fc (lambda (args) (+ (car args) (cadr args)))))
            (cons '*        (fc (lambda (args) (* (car args) (cadr args)))))
            (cons '-        (fc (lambda (args) (- (car args) (cadr args)))))
            (cons 'eq?      (fc (lambda (args) (eq? (car args) (cadr args)))))
            (cons 'cons     (fc (lambda (args) (cons (car args) (cadr args)))))
            (cons 'car      (fc (lambda (args) (car (car args)))))
            (cons 'cdr      (fc (lambda (args) (cdr (car args)))))
            (cons 'list     (fc (lambda (args) args)))
            (cons 'quote    (fsubr-of eval-quote))
            (cons 'if       (fsubr-of eval-if))
            (cons 'set!     (fsubr-of eval-set!))
            (cons 'lambda        (fsubr-of eval-lambda))
            (cons 'fsubr    (fsubr-of eval-fsubr))
            (cons 'fexpr    (fsubr-of eval-fexpr))
            (cons 'begin    (fsubr-of eval-begin-exp))
            (cons 'define   (fsubr-of eval-define))
            (cons 'call/cc  (fsubr-of eval-call/cc))
            (cons 'eval-var (fsubr-of eval-var))
            (cons 'base-eval
                  (make-fun (lambda (args c)
                              (let ((exp (car args))
                                    (e (cadr args))
                                    (k (caddr args)))
                                (let ((env (if (null? e) (force init-env) e))
                                      (cont
                                       (cond
                                         ((fun? k) (make-cont (lambda (v) (f (list v) c))))
                                         ((cont? k) k)
                                         (else (error 'init-env "not a valid cont" k)))))
                                  (base-eval exp env cont))))))
            (cons 'eval (make-fun (lambda (args c)
                                    (let ((exp (car args)))
                                      (base-eval exp (force init-env) c))))))))))
    (force init-env)))

(load "test-check.scm")

(define (repl)
  (let ((global-env (make-init-env)))
    (lambda (exp) (base-eval exp global-env (make-cont (lambda (v) v))))))

(test
  (let ((ev (repl)))
    (ev '(define factorial (lambda (n) (if (< n 2) n (* n (factorial (- n 1)))))))
    (ev '(factorial 6)))
  720)

(test
  (let ((ev (repl)))
    (ev '(begin
          (define even (lambda (n) (if (eq? n 0) #t (odd (- n 1)))))
          (define odd (lambda (n) (if (eq? n 0) #f (even (- n 1)))))
          ))
    (ev '(odd 7)))
  #t)

(test
  (let ((ev (repl)))
    (ev '(define x 1))
    (list (ev '(eval 'x))
          (ev '(* (eval 'x) 2))))
  (list 1 2))

(test
  (let ((ev (repl)))
    (ev '(* 2 (call/cc (lambda (k) 3)))))
  6)

(test
  (let ((ev (repl)))
    (ev '(* 2 (call/cc (lambda (k) (k 3))))))
  6)

(test
  (let ((ev (repl)))
    (ev '(* 2 (call/cc (lambda (k) (k (k 3)))))))
  6)

(test
  (let ((ev (repl)))
    (ev '(* 2 (call/cc (lambda (k) (* 5 (k 3)))))))
  6)

(test
  (let ((ev (repl)))
    (ev '(define my-if (fexpr (c a b) (if (eval c) (eval a) (eval b)))))
    (ev '(my-if #t 1 bad)))
  1)

(test
  (let ((ev (repl)))
    (ev '()))
  '())

(test
  (let ((ev (repl)))
    (ev '(list)))
  '())

(test
  (let ((ev (repl)))
    (ev '(list 10)))
  '(10))

(test
  (let ((ev (repl)))
    (ev '(define history '()))
    (ev '(list history (cons 4 history))))
  '(() (4)))

(test
  (let ((ev (repl)))
    (define r '())
    (ev '(define history '()))
    (ev '(define save!
           (fexpr (lhs rhs)
                  ((lambda (old-val)
                     (eval (list 'set! lhs rhs))
                     (set! history (cons (list
                                          lhs
                                          old-val (eval lhs)) history)))
                   (eval lhs)))))
    (ev '(define test 1))
    (ev '(save! test (* test 2)))
    (set! r (cons (ev 'test) r))
    (set! r (cons (ev 'history) r))
    (ev '(save! test (* test 2)))
    (set! r (cons (ev 'test) r))
    (set! r (cons (ev 'history) r))
    (reverse r))
  '(2 ((test 1 2)) 4 ((test 2 4) (test 1 2))))

(test
  (let ((ev (repl)))
    (ev '(define my-exp (fsubr (exp env cont) exp)))
    (ev '(my-exp x)))
  '(my-exp x))

(test
  (let ((ev (repl)))
    (ev '(define jump (fsubr (exp env cont) (eval (car (cdr exp))))))
    (ev '(- 1 (jump 2))))
  2)

(test
  (let ((ev (repl)))
    (ev '(define fall (fsubr (exp env cont) 1)))
    (ev '(* 2 (fall))))
  1)

(test
  (let ((ev (repl)))
    (define r '())
    (ev '(define old-set! set!))
    (ev '(define history '()))
    (ev '(define save!
           (fexpr (lhs rhs)
                  ((lambda (old-val)
                     (eval (list 'old-set! lhs rhs))
                     (old-set! history (cons (list
                                              lhs
                                              old-val (eval lhs)) history)))
                   (eval lhs)))))
    (ev '(set! set!
           (fsubr (exp env cont)
                  (eval (list 'save! (car (cdr exp)) (car (cdr (cdr exp)))))
                  (cont (car (cdr exp))))))
    (ev '(define test 1))
    (ev '(set! test (* test 2)))
    (set! r (cons (ev 'test) r))
    (set! r (cons (ev 'history) r))
    (ev '(set! test (* test 2)))
    (set! r (cons (ev 'test) r))
    (set! r (cons (ev 'history) r))
    (reverse r))
  '(2 ((test 1 2)) 4 ((test 2 4) (test 1 2))))

(test
  (let ((ev (repl)))
    (define r '())
    (ev '(define assq
           (lambda (k m)
             (if (eq? m '())
                 #f
                 (if (eq? (car (car m)) k)
                     (car m)
                     (assq k (cdr m)))))))
    (ev '(begin
           (define t '())
           (define traced-fns '())
           (define trace
             (fexpr (fn)
                    (set! traced-fns (cons (cons fn (eval fn)) traced-fns))
                    ((lambda (old-fn)
                       ((lambda (tf)
                          ((fsubr (_ env cont)
                                  (base-eval (list 'set! fn 'tf) env cont))))
                        (lambda (x) (begin
                                 (set! t (cons (list fn x) t))
                                 ((lambda (v) (begin
                                           (set! t (cons (list fn x '=> v) t))
                                           v))
                                  (old-fn x))
                                 ))))
                     (eval fn))))
           (define untrace
             (fexpr (fn)
                    ((lambda (old-fn)
                       ((fsubr (_ env cont)
                               (base-eval (list 'set! fn 'old-fn) env cont))))
                     (cdr (assq fn traced-fns)))))))
    (ev '(define factorial (lambda (n) (if (< n 2) n (* n (factorial (- n 1)))))))
    (ev '(trace factorial))
    (set! r (cons (ev '(factorial 3)) r))
    (set! r (cons (ev 't) r))
    (ev '(set! t '()))
    (ev '(untrace factorial))
    (set! r (cons (ev '(factorial 3)) r))
    (set! r (cons (ev 't) r))
    (reverse r))
  '(6 ((factorial 3 => 6) (factorial 2 => 2) (factorial 1 => 1) (factorial 1) (factorial 2) (factorial 3)) 6 ()))

(test
  (let ((ev (repl)))
    (define r '())
    (ev '(define reflect-steps
           (fexpr (total default fn arg)
                  ((lambda (old-fn steps)
                     ((lambda (new-fn)
                        (begin
                          ((fsubr (_ env cont)
                                  (base-eval (list 'set! fn 'new-fn) env cont)))
                          ((lambda (ret) (begin
                                      ((fsubr (_ env cont)
                                              (base-eval (list 'set! fn 'old-fn) env cont)))
                                      ret))
                           (eval (list fn arg)))))
                      (lambda (n)
                        (begin (set! steps (+ steps 1))
                               (if (< steps total)
                                   ((fsubr (_ env cont)
                                           (base-eval (list 'old-fn n) env cont)))
                                   default)))))
                   (eval fn) 0))))
    (ev '(define fib (lambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))))
    (set! r (cons (ev '(fib 6)) r))
    (set! r (cons (ev '(reflect-steps 3 1 fib 6)) r))
    (set! r (cons (ev '(reflect-steps 3 0 fib 6)) r))
    (set! r (cons (ev '(reflect-steps 1000 1 fib 6)) r))
    (set! r (cons (ev '(fib 6)) r))
    (reverse r))
  (list 8 3 0 8 8))

(test
  (let ((ev (repl)))
    (define r '())
    (ev '(begin
           (define expand (lambda (binding body)
                            (list 'define (car binding)
                                  (list 'fexpr (cdr binding) (list 'eval body)))
                            ))
           (define define-macro (fexpr (binding body)
                                       (eval (expand binding body))
                                       ))
           ))
    (set! r (cons (ev '(expand '(quote-it x) '(list 'quote x))) r))
    (set! r (cons (ev '(begin
                         (define-macro (quote-it x) (list 'quote x))
                         (quote-it y)
                         )) r))
    (set! r (cons (ev '(begin
                         (define-macro (var-val x) (list 'list (list 'quote x) x))
                         (define y 1)
                         (var-val y)
                         )) r))
    (ev '(begin
           (define-macro (foo x) x)
           (define-macro (outer-foo x) (list 'foo x))
           (define-macro (outer-foo2 x) (foo x))
           ))
    (set! r (cons (ev '(outer-foo 1))  r))
    (reverse r))
  (list '(define quote-it (fexpr (x) (eval (list (quote quote) x))))
        'y
        '(y 1)
        1))
#|

(test
  (let ((ev (repl)))
    (ev '(define my-call/cc (fsubr (exp env cont) (base-eval (car (cdr exp)) env (lambda (f) (cont (f cont)))))))
    (list
     (ev '(* 2 (my-call/cc (lambda (k) 3))))
     (ev '(* 2 (my-call/cc (lambda (k) (k 3)))))
     (ev '(* 2 (my-call/cc (lambda (k) (k (k 3))))))
     (ev '(* 2 (my-call/cc (lambda (k) (* 5 (k 3))))))))
  (list 6 6 6 6))

|#
