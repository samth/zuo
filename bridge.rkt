#lang racket/base
(require (for-syntax racket/base)
         "private/zuo.rkt")

;; A language implemented with `#lang zuo/private/racket/bridge`
;; should have one S-exptession, which is a symbol representing a Zuo
;; module language path
(module reader syntax/module-reader
  #:language '(submod zuo/bridge reader-bridge))

(module reader-bridge racket/base
  (require (for-syntax racket/base))
  (provide (rename-out [module-begin #%module-begin]))

  ;; Implement a bridge Racket module as one that slurps in the
  ;; string, and sends that off (along with the Zuo language name and
  ;; the enclosing file's path) to the main module outside this
  ;; submodule
  (define-syntax (module-begin stx)
    (syntax-case stx ()
      [(_ zuo-lang-module-path)
       #'(#%module-begin
          (module reader racket/base
            (provide (rename-out [my-read read]
                                 [my-read-syntax read-syntax]))
            (define (read-all in)
              (apply bytes-append
                     (let loop ()
                       (let ([s (read-bytes 4096 in)])
                         (if (eof-object? s)
                             '()
                             (cons s (loop)))))))
            (define (my-read in)
              `(module name zuo/bridge
                 zuo-lang-module-path
                 ,(read-all in)
                 ,(object-name in)))
            (define (my-read-syntax name in)
              (datum->syntax #f (my-read in)))))])))

(provide (rename-out [module-begin #%module-begin]))

;; This is the `#%module-begin` for a Racket bridge module.
;; It jumps into the implementation of Zuo using 
(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ zuo-lang-module-path content-bytes in-file-path)
     #'(#%module-begin
        (void (zuo-eval-from-bridge 'zuo-lang-module-path 'content-bytes 'in-file-path)))]))
