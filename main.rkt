#lang racket/base
(require racket/list (for-syntax racket/base))

(require (rename-in zuo/base [define-syntax -define-syntax]))

(provide (rename-out [-define-syntax define-syntax]))

(provide (all-from-out zuo/base))

; this is a horrible hack
(define-for-syntax bp
  (build-path (find-executable-path (find-system-path 'exec-file)
                                    (find-system-path 'collects-dir))
              'up "src/zuo/lib/zuo"))

(define-syntax (require-zuo stx)
  (syntax-case stx ()
    [(_ id)
     (let ([p (path->string (build-path bp (syntax-e #'id)))])
       #`(begin (require (file #,p))
                (provide (all-from-out (file #,p)))))]))

(require-zuo "cmdline.zuo")
(require-zuo "glob.zuo")
(require-zuo "build.zuo")
(require-zuo "c.zuo")
(require-zuo "jobserver.zuo")
(require-zuo "shell.zuo")
(require-zuo "thread.zuo")


(require racket/format racket/include)
(define (char v) (char->integer (string-ref v 0)))

(define-syntax-rule (provide-targets id ...) (provide id ...))

(define-syntax-rule (define-syntax x _ ...) (void 'x))

(define-for-syntax (bad-syntax x) x)

(module reader syntax/module-reader
  #:language 'zuo)
