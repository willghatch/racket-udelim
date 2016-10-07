(module reader racket/base
  (require syntax/module-reader
           udelim
           )

  (provide (rename-out [u-read read]
                       [u-read-syntax read-syntax]
                       [u-get-info get-info]))

  (define (wrap-reader p)
    (lambda args
      (parameterize ([current-readtable (udelimify (current-readtable))])
        (apply p args))))

  (define-values (u-read u-read-syntax u-get-info)
    (make-meta-reader
     'udelim
     "language path"
     ;lang-reader-module-paths
     (lambda (bstr)
       (let* ([str (bytes->string/latin-1 bstr)]
              [sym (string->symbol str)])
         (and (module-path? sym)
              (vector
               ;; try submod first:
               `(submod ,sym reader)
               ;; fall back to /lang/reader:
               (string->symbol (string-append str "/lang/reader"))))))

     wrap-reader
     wrap-reader
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
