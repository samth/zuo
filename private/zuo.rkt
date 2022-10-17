#lang racket/base
(require racket/file
         racket/match
         racket/string
         racket/format)

(provide zuo-eval-from-bridge)

(define modules (make-hash))

(define (zuo-eval-from-bridge lang-name content-bytes in-file-path)
  (define mod-path
    (path->bytes (path->complete-path
                  (cond
                    [(path? in-file-path) in-file-path]
                    [(string? in-file-path) (string->path in-file-path)]
                    [else "zuo"])
                  (or (current-load-relative-directory)
                      (current-directory)))))
  (define invented-prefix (string->bytes/utf-8 (format "#lang ~a" lang-name)))
  (zuo-eval-module in-file-path (bytes-append invented-prefix content-bytes)))

(define (zuo-read-language bytes)
  (unless (regexp-match? #rx"^#lang " bytes)
    (zuo-fail "read: expected #lang followed by a space"))
  (define m (regexp-match #px"^#lang ([-_+/a-zA-Z0-9]+)\\s" bytes))
  (unless m
    (zuo-fail "read: expected module library path after #lang"))
  (values (read (open-input-bytes (cadr m)))
          (bytes-length (car m))))
           
(define (zuo-eval-module mod-path content-bytes)
  (define-values (lang start) (zuo-read-language content-bytes))
  (define mod
    (cond
      [(eq? lang 'zuo/kernel)
       (zuo-eval (zuo-read-one (subbytes content-bytes start)))]
      [else
       (define env (zuo-dynamic-require lang))
       (define proc (hash-ref env 'read-and-eval #f))
       (unless (procedure? proc)
         (zuo-fail1 "not a language module path" lang))
       (proc content-bytes
             start
             mod-path)]))
  (unless (hash? mod)
    (zuo-fail1 "module did not produce a hash table" mod-path))
  (hash-set! modules mod-path mod)
  mod)

(define (zuo-dynamic-require mod-path)
  (check-module-path 'dynamic-require mod-path)
  (or (hash-ref modules mod-path #f)
      (let ()
        (define file-path
          (cond
            [(symbol? mod-path) (library-path->file-path mod-path)]
            [else (path->complete-path (bytes->path mod-path))]))
        (zuo-eval-module mod-path (file->bytes file-path)))))

(define (library-path->file-path mod-path)
  (define mod-str (format "~a" mod-path))
  (define norm-mod-str (if (regexp-match? #rx"/" mod-str)
                           (string-append mod-str ".zuo")
                           (string-append mod-str "/main.zuo")))
  (define-values (base name dir?) (split-path norm-mod-str))
  (collection-file-path name base))

(define (zuo-read-one bytes)
  (define in (open-input-bytes bytes))
  (define v (read in))
  (when (eof-object? v)
    (zuo-fail "read: no S-expression in input"))
  (unless (eof-object? (read in))
    (zuo-fail "read: extra input after S-expression"))
  (s-exp-string->bytes v))

(define (s-exp-string->bytes v)
  (cond
    [(string? v) (string->bytes/utf-8 v)]
    [(pair? v) (cons (s-exp-string->bytes (car v))
                     (s-exp-string->bytes (cdr v)))]
    [else v]))

(define (module-path-join a b)
  (cond
    [(symbol? a) a]
    [else (let ([pa (bytes->path a)])
            (cond
              [(absolute-path? pa) a]
              [(bytes? b)
               (let ([pb (bytes->path b)])
                 (define-values (base name dir?) (split-path pb))
                 (path->bytes (path->complete-path pa base)))]
              [else
               (let* ([bs (string-split (symbol->string b) "/")]
                      [rev-bs (if (null? (cdr bs))
                                  bs
                                  (cdr (reverse bs)))]
                      [a (regexp-replace #rx#"[.]zuo$" a #"")]
                      [as (string-split (bytes->string/utf-8 a) "/")])
                 (let loop ([rev-bs rev-bs] [as as])
                   (cond
                     [(equal? (car as) "..") (loop (cdr rev-bs) (cdr as))]
                     [(equal? (car as) ".") (loop rev-bs (cdr as))]
                     [else (string->symbol (string-join (append (reverse rev-bs) as) "/"))])))]))]))
                 

(struct variable (name [value #:mutable])
  #:omit-define-syntaxes
  #:constructor-name make-variable)

(define undefined (gensym 'undefined))
  
(define (variable name) (make-variable name undefined))
(define (variable-ref var)
  (let ([v (variable-value var)])
    (if (eq? v undefined)
        (zuo-fail1 "undefined" (variable-name var))
        v)))
(define (variable-set! var val)
  (unless (eq? undefined (variable-value var))
    (zuo-fail1w "variable-set!" "variable already has a value" var))
  (set-variable-value! var val))

(struct opaque (tag val))

(define zuo-namespace (make-base-empty-namespace))
(parameterize ([current-namespace zuo-namespace])
  (define kernel-ht (hasheq))
  (define (install! name value)
    (eval `(,#'define-values (,name) (,#'quote ,value)))
    (set! kernel-ht (hash-set kernel-ht name value)))
  (define-syntax-rule (install-all! n ...)
    (begin (install! 'n n) ...))
  (let ([eval (lambda (e) (zuo-eval e))]
        [dynamic-require (lambda (e) (zuo-dynamic-require e))]
        [hash hasheq]
        [string? bytes?]
        [string=? bytes=?]
        [string-length bytes-length]
        [substring subbytes]
        [string->symbol (lambda (s) (string->symbol (bytes->string/utf-8 s)))]
        [string->uninterned-symbol (lambda (s) (string->uninterned-symbol (bytes->string/utf-8 s)))]
        [symbol->string (lambda (s) (string->bytes/utf-8 (symbol->string s)))]
        [opaque-ref (lambda (tag v def)
                      (if (and (opaque? v) (eq? (opaque-tag v) tag))
                          (opaque-val v)
                          def))]
        [read-from-string-all (lambda (bstr)
                                (define i (open-input-bytes bstr))
                                (let loop ()
                                  (define v (read i))
                                  (if (eof-object? v)
                                      '()
                                      (cons (s-exp-string->bytes v) (loop)))))]
        [path->complete-path (lambda (bstr wrt-bytes)
                               (path->bytes (path->complete-path (bytes->path bstr)
                                                                 (bytes->path wrt-bytes))))]
        [split-path (lambda (bstr)
                      (let-values ([(base name dir?) (split-path (bytes->path bstr))])
                        (list (if (path? base) (path->bytes base) #f)
                              (path->bytes name))))]
        [alert (lambda args
                 (let ([args (if (and (pair? args) (string? (car args)))
                                 (begin
                                   (display (car args))
                                   (when (pair? (cdr args))
                                     (display ": "))
                                   (cdr args))
                                 args)])
                   (display (apply ~s args))
                   (newline)))]
        [kernel-env (lambda () kernel-ht)])
    (install-all!
     null?
     pair?
     list?
     procedure?
     symbol?
     string?
     integer?

     not
     eq?
     void eof

     car cdr cons list
     length append reverse

     + - * quotient remainder
     < <= = >= >
     bitwise-and
     bitwise-ior
     bitwise-xor

     string=? string-length substring
     string->symbol
     string->uninterned-symbol
     symbol->string

     path->complete-path
     split-path
     
     procedure-arity-mask apply

     opaque opaque-ref

     alert
     error

     hash hash-ref hash-set hash-remove
     hash-keys hash-count hash-keys-subset?

     read-from-string-all
     
     variable variable-ref variable-set!

     ~a ~s ~v

     eval
     dynamic-require
     module-path-join
     kernel-env)))

(define (zuo-eval e)
  (define compiled-e
    (let loop ([e e])
      (match e
        [`(quote ,e) `(,#'quote ,e)]
        [`(lambda ,formals ,body) `(,#'#%plain-lambda ,formals ,(loop body))]
        [`(lambda ,formals ,name-str ,body) (let ([name (string->symbol (bytes->string/utf-8 name-str))])
                                              `(,#'let-values ([(,name) (,#'#%plain-lambda ,formals ,(loop body))])
                                                              ,name))]
        [`(let ([,id ,rhs]) ,body) `(,#'let-values ([(,id) ,(loop rhs)]) ,(loop body))]
        [`(let/cc ,id ,body) `(,#'call/cc (,#'lambda (,id) ,(loop body)))]
        [`(begin ,e ...) `(,#'begin ,@(map loop e))]
        [`(if ,tst ,thn ,els) `(,#'if ,(loop tst) ,(loop thn) ,(loop els))]
        [`(,e ...) `(,#'#%plain-app ,@(map loop e))]
        [(? symbol?) e]
        [_ `(,#'quote ,e)])))
  (eval compiled-e zuo-namespace))

(define (check-module-path who v) (void))

(define (zuo-fail msg) (error msg))
(define (zuo-fail1 msg val) (error msg val))
(define (zuo-fail1w who msg val) (error (format "~a: ~a" who msg) val))
