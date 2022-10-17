#lang racket/base
(require racket/list (for-syntax racket/base))
(provide define lambda #%module-begin hash hash-set provide #%app struct require
         rename-in only-in
         bytes list* dynamic-require #%top-interaction
         let let* letrec if cond null? car cdr cadr cddr equal? eq? #%datum =
         and >= <= char ~a + - * / else target? target-path
         provide-targets unless
         (for-syntax (all-from-out racket/base) bad-syntax)
         lambda
         let
         let*
         letrec
         define-syntax
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
         syntax-error
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
         suspend-signal resume-signal

         command-line

         target
         rule
         phony-rule
         input-file-target
         input-data-target
         target-path
         target-name
         target?
         token?
         rule?
         phony-rule?
         sha256?
         file-sha256
         no-sha256
         sha256-length
         build
         build/command-line
         build/command-line*
         build/dep
         build/no-dep
         find-target
         make-at-dir
         make-targets
         command-target?
         command-target->target
         bounce-to-targets

         shell
         shell/wait
         build-shell

         call-in-main-thread
         thread? thread channel? channel channel-put channel-get
         thread-process-wait
         config-file->hash)


(require racket/format racket/include)
(define (char v) (char->integer (string-ref v 0)))
(define-syntax-rule (define-fake id ...)
  (begin (provide id ...) (define id 'id) ...))
(define-fake

  quote-module-path handle?
  call/prompt

  context-consumer
  context-consumer?
  
  target
  rule
  phony-rule
  input-file-target
  input-data-target
  target-path
  target-name
  target?
  token?
  rule?
  phony-rule?
  sha256?
  file-sha256
  no-sha256
  sha256-length
  build
  build/command-line
  build/command-line*
  build/dep
  build/no-dep
  find-target
  make-at-dir
  make-targets
  command-target?
  command-target->target
  bounce-to-targets

  shell
  shell/wait
  build-shell

  call-in-main-thread
  thread? thread channel? channel channel-put channel-get
  thread-process-wait
  string-u32-ref
         at-source

         variable?
         variable
         variable-ref
         variable-set!
         string-sha256
         syntax-error
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
  resume-signal
  command-line
  .exe shell-subst config-merge glob->matcher
  string->integer fd-valid?  config-file->hash)

(define-syntax-rule (provide-targets id ...) (provide id ...))

(define-syntax-rule (define-syntax x _ ...) (void 'x))

(define-for-syntax (bad-syntax x) x)

(module reader syntax/module-reader
  #:language 'zuo)
