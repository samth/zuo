#lang racket/base
(require racket/list (for-syntax racket/base syntax/parse racket/syntax racket/format)
         (prefix-in r: racket/base))
(provide define lambda #%module-begin hash hash-set provide #%app require
         rename-in only-in rename-out
         bytes list* dynamic-require #%top-interaction
         let let* letrec if cond null? car cdr cadr cddr equal? eq? #%datum =
         and >= <= char ~a + - * / else 
          unless
         (for-syntax (all-from-out racket/base) bad-syntax)
         lambda
         let
         let*
         letrec
         define-values
         caar
         cdar
         if
         and
         or
         when
         unless
         begin
         cond
         quote
         quasiquote
         unquote
         unquote-splicing
         quote-syntax

         define
         include
         require
         provide
         module+
         quote-module-path

         pair?
         null?
         integer?
         string?
         symbol?
         hash?
         list?
         procedure?
         path-string?
         module-path?
         relative-path?
         handle?
         boolean?
         void

         apply
         call/cc
         call/prompt
         continuation-prompt-available?
         context-consumer
         context-consumer?

         cons
         car
         cdr
         list
         append
         reverse
         length
         member
         assoc
         remove
         list-ref
         list-set

         andmap
         ormap
         map
         filter
         foldl
         for-each

         not
         eq?
         equal?
         void?

         +
         -
         *
         quotient
         modulo
         <
         <=
         =
         >=
         >
         bitwise-and
         bitwise-ior
         bitwise-xor
         bitwise-not

         string-length
         string-ref
         string-u32-ref
         substring
         string=?
         string-ci=?
         string->symbol
         string->uninterned-symbol
         symbol->string
         string
         string-sha256
         char
         string-split string-join string-trim
         string-tree?

         hash
         hash-ref
         ref
         hash-set
         hash-remove
         hash-keys
         hash-count
         hash-keys-subset?

         opaque
         opaque-ref

         build-path
         split-path
         at-source

         variable?
         variable
         variable-ref
         variable-set!

         identifier?
         syntax-e
         syntax->datum
         datum->syntax
         bound-identifier=?
         bad-syntax
         misplaced-syntax
         duplicate-identifier

         fd-open-input
         fd-open-output
         fd-close
         fd-read
         fd-write
         eof
         fd-terminal?
         file->string
         display-to-file

         stat
         ls rm mv mkdir rmdir symlink readlink cp
         current-time
         system-type
         file-exists?
         directory-exists?
         link-exists?
         explode-path
         simple-form-path
         find-relative-path
         build-raw-path
         path-replace-extension
         path-only
         file-name-from-path
         path->complete-path
         ls* rm* cp* mkdir-p
         :error :truncate :must-truncate :append :update :can-update
         cleanable-file
         cleanable-cancel

         process
         process-status
         process-wait
         find-executable-path
         shell->strings
         string->shell

         error
         alert
         ~v
         ~a
         ~s
         arity-error
         arg-error
         display displayln

         string-read
         module->hash
         build-module-path
         kernel-env
         kernel-eval

         runtime-env
         dump-image-and-exit
         exit
         suspend-signal resume-signal)



(define-syntax (-struct stx)
  (syntax-parse stx
    [(_ nm:id (flds:id ...))
     #:with (upd ...) (for/list ([f (syntax->list #'(flds ...))])
                        (format-id #'nm "~a-set-~a" #'nm f))
     
     #`(begin (r:struct nm (flds ...))
              (define upd void) ...)]))
(provide (rename-out [-struct struct]))


(require racket/format racket/include)
(define (char v) (char->integer (string-ref v 0)))
(define-syntax-rule (define-fake id ...)
  (begin (provide id ...) (define id void) ...))
(define-fake

  list-tail
  string->integer 
  quote-module-path handle?
  call/prompt

  context-consumer
  context-consumer?

  fd-valid?


  string-u32-ref
  at-source

  variable?
  variable
  variable-ref
  variable-set!
  string-sha256
  bad-syntax
  misplaced-syntax
  duplicate-identifier

  fd-open-input
  fd-open-output
  fd-close
  fd-read
  fd-write
  eof
  fd-terminal?
  file->string
  display-to-file

  stat
  ls rm mv mkdir rmdir symlink readlink cp
  current-time
  string-join string-trim
  string-split
  string-tree?
  ref opaque opaque-ref simple-form-path find-relative-path build-raw-path path-only file-name-from-path
  ls* rm* cp* mkdir-p
  :error :truncate :must-truncate :append :update :can-update
  cleanable-file
  cleanable-cancel

  process
  process-status
  process-wait
  find-executable-path
  shell->strings
  string->shell
  alert
  arity-error
  arg-error
  string-read
  module->hash
  build-module-path
  kernel-env
  kernel-eval
  runtime-env
  dump-image-and-exit
  suspend-signal
  resume-signal)

(define-syntax-rule (provide-targets id ...) (provide id ...))

(module stx racket/base
  (provide syntax-error)
  (define (syntax-error . _) (error 'syntax-error)))

(require (submod "." stx))

(provide syntax-error)
(require (for-syntax (submod "." stx) racket/list racket/syntax))

(provide (for-syntax syntax-error
                     ~a
                     check-duplicates))

(define-for-syntax (convert-in stx)
  (let loop ([_e stx])
    (define e (syntax-e _e))
    (cond
      [(list? e) (map loop e)]
      [(pair? e) (cons (loop (car e)) (loop (cdr e)))]
      [(symbol? e) _e]
      [else e])))

(define-for-syntax (convert-out i l)
  (datum->syntax i l))

(define-syntax (defstx stx)
  (syntax-case stx ()
    [(_ (i arg) e ... last)
     #'(define-syntax i
         (λ (_arg) (let ([arg (convert-in _arg)]) e ... (convert-out _arg last))))
     #;
     (syntax/loc stx (define-syntax-rule (i . _) (void)))]
    [(_ i e)
     #'(define-syntax i
         (λ (stx) (convert-out stx (e (convert-in stx)))))
     #;
     (syntax/loc stx (define-syntax-rule (i . _) (void)))]))

(provide (rename-out [defstx define-syntax]))

(define-for-syntax (bad-syntax x) (error 'bad-syntax))

(module reader syntax/module-reader
  #:language 'zuo/base)
