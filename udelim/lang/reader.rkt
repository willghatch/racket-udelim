(module reader racket/base
  (require syntax/module-reader
           udelim
           )

  (provide (rename-out [at-read read]
                       [at-read-syntax read-syntax]
                       [at-get-info get-info]))

  (define udelim-table
    (make-list-delim-readtable/wrap
     #\🌜 #\🌛 '#%moon-faces
     #:base-readtable
     (make-list-delim-readtable/wrap
      #\⦕ #\⦖ '#%double-inequality-brackets
      #:base-readtable
      (make-list-delim-readtable/wrap
       #\⦓ #\⦔ '#%inequality-brackets
       #:base-readtable
       (make-list-delim-readtable/wrap
        #\﴾ #\﴿ '#%ornate-parens
        #:base-readtable
        (make-list-delim-readtable/wrap
         #\⸨ #\⸩ '#%double-parens
         #:base-readtable
         (make-string-delim-readtable
          #\“ #\”
          #:base-readtable
          (make-string-delim-readtable #\« #\»))))))))
  (define (wrap-reader p)
    (lambda args
      (parameterize ([current-readtable udelim-table])
        (apply p args))))

  (define-values (at-read at-read-syntax at-get-info)
    (make-meta-reader
     'udelim
     "language path"
     lang-reader-module-paths
     wrap-reader
     (lambda (orig-read-syntax)
       (define read-syntax (wrap-reader orig-read-syntax))
       (lambda args
         (define stx (apply read-syntax args))
         ;(define old-prop (syntax-property stx 'module-language))
         ;(define new-prop `#(at-exp/lang/language-info get-language-info ,old-prop))
         ;(syntax-property stx 'module-language new-prop)
         stx
         ))
     (lambda (proc) proc)
     #;(lambda (proc)
       (lambda (key defval)
         (define (fallback) (if proc (proc key defval) defval))
         (define (try-dynamic-require mod export)
           (or (with-handlers ([exn:fail? (λ (x) #f)])
                 (dynamic-require mod export))
               (fallback)))
         (case key
           [(color-lexer)
            (try-dynamic-require 'syntax-color/scribble-lexer 'scribble-lexer)]
           [(definitions-text-surrogate)
            'scribble/private/indentation]
           [(drracket:indentation)
            (dynamic-require 'scribble/private/indentation 'determine-spaces)]
           [else (fallback)]))))))
