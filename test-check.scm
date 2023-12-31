(define test-failed #f)
(define (set-test-failed!)
  (set! test-failed #t))

(define-syntax test
  (syntax-rules ()
    ((_ tested-expression expected-result)
     (begin
       (printf "Testing ~a\n" 'tested-expression)
       (let* ((expected expected-result)
              (produced tested-expression))
         (or (equal? expected produced)
             (begin
               (set-test-failed!)
               (error 'test
                      (format "Failed: ~a~%Expected: ~a~%Computed: ~a~%"
                              'tested-expression expected produced))
               ;; (format #t "Failed: ~a~%Expected: ~a~%Computed: ~a~%"
               ;;         'tested-expression expected produced)
               )))))))

(define-syntax test-error
  (syntax-rules ()
    ((_ tested-expression)
     (test
       (guard (x (else #f)) (begin tested-expression #t))
       #f))))

(define-syntax time-test
  (syntax-rules ()
    ((_ tested-expression expected-result)
     (test
       (time tested-expression)
       expected-result))))

(define-syntax todo
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (printf "TODO ~s\n" title))))

;; `expect-exception` is used for testing code that *should* raise an
;; exception (but not an error).
;;
;; `expect-exception` takes a procedure of no arguments (a "thunk"),
;; `p`, and invokes `p`.  If `p` raises an error, or if `p` is
;; evaluated without an error or exception being raised,
;; `expect-exception` raises an error.  If `p` raises an exception
;; (but not an error), `expect-exception` returns the symbol 'ok'.
;;
;; https://scheme.com/tspl4/exceptions.html
(define (expect-exception thunk)
  (call/cc
   (lambda (k)
     (with-exception-handler
       (lambda (x) (if (error? x) (raise x) (k 'ok)))
       (lambda ()
         (thunk)
         (raise (make-error)))))))

;; `expect-error` is used for testing code that *should* raise an
;; error (but not an exception).
;;
;; `expect-error` takes a procedure of no arguments (a "thunk"), `p`,
;; and invokes `p`.  If `p` raises an exception, or if `p` is
;; evaluated without an error or exception being raised,
;; `expect-error` raises an error.  If `p` raises an error (but not an
;; exception), `expect-error` returns the symbol 'ok'.
;;
;; https://scheme.com/tspl4/exceptions.html
(define (expect-error thunk)
  (call/cc
   (lambda (k)
     (with-exception-handler
       (lambda (x) (if (error? x) (k 'ok) (raise x)))
       (lambda ()
         (thunk)
         (raise (make-violation)))))))
