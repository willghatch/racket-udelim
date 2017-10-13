#lang racket/base

(require racket/contract/base)
(provide
 (contract-out
  [make-string-delim-readtable
   (->* (char? char?)
        (#:base-readtable readtable?
         #:wrapper (or/c false/c symbol?))
        readtable?)]
  [make-list-delim-readtable
   (->* (char? char?)
        (#:base-readtable readtable?
         #:wrapper (or/c false/c symbol?)
         #:inside-readtable any/c)
        readtable?)]
  [udelimify (->* ((or/c readtable? false/c)) readtable?)]
  [stx-string->port (->* (syntax?) input-port?)]
  [scribble-strings->string (->* (syntax?) syntax?)]
  ))

(require syntax/readerr)

(define (make-raise-balance-error l-paren r-paren)
  (define raise-balance-error
    (case-lambda
      [(ch port) (raise-balance-error ch port #f #f #f #f)]
      [(ch port src line col pos)
       (raise-read-error (format "unexpected closing delimiter ~a\n" r-paren)
                         src line col pos #f)]))
  raise-balance-error)

(define (make-string-reader l-paren r-paren #:wrapper [wrapper #f])
  ;; balance parens, and return the contents as a bare string with no escapes
  (define string-reader
    (case-lambda
      [(ch port) (syntax->datum (string-reader ch port #f #f #f #f))]
      [(ch port src line col pos)
       (define-values (n-line n-col n-pos) (port-next-location port))
       (define (loop ch cur-balance-level ch-so-far)
         (cond [(equal? eof ch)
                (raise-read-error
                 (format "unexpected eof, expected ~a more closing delimiter ~a\n"
                         cur-balance-level r-paren)
                 src line col pos #f)]
               [(equal? ch l-paren) (loop (read-char port)
                                          (add1 cur-balance-level)
                                          (cons ch ch-so-far))]
               [(not (equal? ch r-paren)) (loop (read-char port)
                                                cur-balance-level
                                                (cons ch ch-so-far))]
               [(> cur-balance-level 1) (loop (read-char port)
                                              (sub1 cur-balance-level)
                                              (cons ch ch-so-far))]
               ;; this shouldn't be possible... but I'll leave it here anyway.
               [(< cur-balance-level 1)
                ((make-raise-balance-error l-paren r-paren)
                 src line col pos)]
               [else
                (let* ([final-chs (cdr (reverse ch-so-far))]
                       [span (length final-chs)]
                       [unwrapped (datum->syntax
                                   #f (apply string final-chs)
                                   (list src n-line n-col n-pos span))])
                  (if wrapper
                      (datum->syntax #f (list wrapper unwrapped))
                      unwrapped))]))
       (loop ch 0 '())]))
  string-reader)

(define (make-list-reader l-paren r-paren
                          #:wrapper [wrapper #f]
                          #:inside-readtable [inside-readtable 'inherit])
  (define paren-reader
    (case-lambda
      [(ch port) (syntax->datum (paren-reader ch port #f #f #f #f))]
      [(ch port src line col pos)
       (define (loop stxs-rev)
         (let ([next-ch (read-char port)])
           (cond
             #| TODO - what if one of these whitespace characters is bound
             to something non-default in the readtable??  For now, just ignore
             them. |#
             [(member next-ch '(#\space #\tab #\newline #\vtab #\return))
              (loop stxs-rev)]
             [(equal? next-ch r-paren)
              (let ([unwrapped (reverse stxs-rev)])
                (if wrapper
                    (datum->syntax #f (cons wrapper unwrapped))
                    (datum->syntax #f unwrapped)))]
             [else
              (let ([one-stx (parameterize
                                      ([current-readtable
                                        (cond
                                          [(readtable? inside-readtable)
                                           inside-readtable]
                                          [(not inside-readtable)
                                           #f]
                                          [else (current-readtable)])])
                                       (read-syntax/recursive src
                                                              port
                                                              next-ch))])
                     (loop (cons one-stx stxs-rev)))])))
       (loop '())]))
  paren-reader)



(define (make-string-delim-readtable l-paren r-paren
                                     #:base-readtable [base-readtable #f]
                                     #:wrapper [wrapper #f])
  (make-readtable
   base-readtable
   l-paren 'terminating-macro (make-string-reader l-paren r-paren #:wrapper wrapper)
   r-paren 'terminating-macro (make-raise-balance-error l-paren r-paren)))

(define (make-list-delim-readtable l-paren r-paren
                                   #:base-readtable [base-readtable #f]
                                   #:wrapper [wrapper #f]
                                   #:inside-readtable [inside-readtable 'inherit])
  (make-readtable
   base-readtable
   l-paren 'terminating-macro (make-list-reader l-paren r-paren
                                                #:wrapper wrapper
                                                #:inside-readtable inside-readtable)
   r-paren 'terminating-macro (make-raise-balance-error l-paren r-paren)))


(define (udelimify table)
  (make-list-delim-readtable
   #\🌜 #\🌛 #:wrapper '#%moon-faces
   #:base-readtable
   (make-list-delim-readtable
    #\⦕ #\⦖ #:wrapper '#%double-inequality-brackets
    #:base-readtable
    (make-list-delim-readtable
     #\⦓ #\⦔ #:wrapper '#%inequality-brackets
     #:base-readtable
     (make-list-delim-readtable
      #\﴾ #\﴿ #:wrapper '#%ornate-parens
      #:base-readtable
      (make-list-delim-readtable
       #\⟅ #\⟆ #:wrapper '#%s-shaped-bag-delim
       #:base-readtable
       (make-string-delim-readtable
        #\◸ #\◹ #:wrapper '#%upper-triangles
        #:base-readtable
        (make-string-delim-readtable
         #\◺ #\◿ #:wrapper '#%lower-triangles
         #:base-readtable
         (make-string-delim-readtable
          #\◤ #\◥ #:wrapper '#%full-upper-triangles
          #:base-readtable
          (make-string-delim-readtable
           #\◣ #\◢ #:wrapper '#%full-lower-triangles
           #:base-readtable
           (make-string-delim-readtable
            #\｢ #\｣ #:wrapper '#%cjk-corner-quotes
            #:base-readtable
            (make-string-delim-readtable
             #\« #\»
             #:base-readtable table))))))))))))

(define (stx-string->port stx)
  (let ([str (syntax->datum stx)]
        [line (syntax-line stx)]
        [col (syntax-column stx)]
        [pos (syntax-position stx)]
        [src (syntax-source stx)])
    (if (not (string? str))
        (error 'stx-string->port "syntax is not a string: ~a\n" stx)
        (let ([p (open-input-string str src)])
          (port-count-lines! p)
          (set-port-next-location! p line col pos)
          p))))

(define (reconstitute-scribble-strings stx)
  ;; Based on `verb` macro in reader-internals docs.
  ;; If there is a newline after the opening brace, it doesn't
  ;; seem to show up, still.  Oh well.
  (syntax-case stx ()
    [(item ...)
     (datum->syntax
      stx
      (let loop ([items (syntax->list #'(item ...))])
        (if (null? items)
            '()
            (let* ([fst  (car items)]
                   [prop (syntax-property fst 'scribble)]
                   [rst  (loop (cdr items))])
              (cond [(not prop) (cons fst rst)]
                    [(eq? prop 'indentation) rst]
                    [(not (and (pair? prop)
                               (eq? (car prop) 'newline)))
                     (cons fst rst)]
                    [else (cons (datum->syntax
                                 fst (cadr prop) fst)
                                rst)])))))]))

(define (scribble-strings->string stx)
  (syntax-case stx ()
    [(arg ...)
     (let* ([error-if-not-str (λ (s) (or (string? (syntax->datum s))
                                         (raise-syntax-error 'scribble-strings->string
                                                             "expected string"
                                                             s)))]
            [all-strs? (map error-if-not-str (syntax->list stx))]
            [one-str (apply string-append
                            (map syntax->datum
                                 (syntax->list
                                  (reconstitute-scribble-strings #'(arg ...)))))]
            [s (car (syntax->list #'(arg ...)))]
            [loclist (list (syntax-source s) (syntax-line s) (syntax-column s)
                           (syntax-position s) (string-length one-str))])
       (datum->syntax s one-str loclist))]))

(module+ test
  (require rackunit)

  (define mytable (make-string-delim-readtable #\{ #\}))
  (define guillemet-table
    (make-string-delim-readtable #\« #\» #:wrapper '#%guillemets))
  (define ceil-table (make-list-delim-readtable #\⌈ #\⌉))
  (define ceil-table/wrap (make-list-delim-readtable #\⌈ #\⌉ #:wrapper '#%ceil))

  (parameterize ([current-readtable mytable])
    (let ([port (open-input-string "  \"in a string {here\" {hello @{testing 123}foo} }goodbye")])
      (check-equal? (syntax->datum (read-syntax "t1" port))
                    "in a string {here")
      (check-equal? (syntax->datum (read-syntax "t1" port))
                    "hello @{testing 123}foo")
      (check-exn exn? (λ () (read-syntax "t1" port)))))

  (parameterize ([current-readtable guillemet-table])
    (let ([port (open-input-string "  \"in a string «here\" «hello @«testing 123»foo» »goodbye")])
      (check-equal? (syntax->datum (read-syntax "t2" port))
                    "in a string «here")
      (check-equal? (syntax->datum (read-syntax "t2" port))
                    '(#%guillemets "hello @«testing 123»foo"))
      (check-exn exn? (λ () (read-syntax "t2" port)))))

  (parameterize ([current-readtable ceil-table])
    (let ([port (open-input-string "{testing ⌈foo bar ⌈⌉ () aoeu⌉ hello} foo")])
      (check-equal? (syntax->datum (read-syntax "t3" port))
                    '(testing (foo bar () () aoeu) hello))))

  (define ceil-table-with-inner-normal (make-list-delim-readtable #\⌈ #\⌉
                                                                  #:inside-readtable #f))
  ;; If this one had the closing delimiter attached to the aoeu symbol, it would error
  (parameterize ([current-readtable ceil-table-with-inner-normal])
    (let ([port (open-input-string "{testing ⌈foo bar ⌈⌉ () aoeu ⌉ hello} foo")])
      (check-equal? (syntax->datum (read-syntax "t3.2" port))
                    '(testing (foo bar ⌈⌉ () aoeu) hello))))

  (parameterize ([current-readtable ceil-table/wrap])
    (let ([port (open-input-string "(testing ⌈foo bar ⌈⌉ () aoeu⌉ hello) foo")])
      (check-equal? (syntax->datum (read-syntax "t4" port))
                    '(testing (#%ceil foo bar (#%ceil) () aoeu) hello))))

  (define weird-table (make-list-delim-readtable #\{ #\} #:inside-readtable mytable))
  (parameterize ([current-readtable weird-table])
    (let ([port (open-input-string "{testing a {b c} d }")])
      (check-equal? (syntax->datum (read-syntax "t5" port))
                    '(testing a "b c" d))))

  (define weird-table2 (make-list-delim-readtable #\{ #\} #:inside-readtable ceil-table))
  (parameterize ([current-readtable weird-table2])
    (let ([port (open-input-string "{testing a (b c) d }")])
      (check-equal? (syntax->datum (read-syntax "t6" port))
                    '(testing a (b c) d))))
  (parameterize ([current-readtable weird-table2])
    (let ([port (open-input-string "{testing a ⌈b c ⌉ d }")])
      (check-equal? (syntax->datum (read-syntax "t7" port))
                    '(testing a (b c) d))))
  (parameterize ([current-readtable weird-table2])
    ;; this one fails if you use read-syntax/recursive using the readtable argument rather than parameterizing current-readtable...
    (let ([port (open-input-string "{testing a ⌈b c⌉ d }")])
      (check-equal? (syntax->datum (read-syntax "t8" port))
                    '(testing a (b c) d))))

  (parameterize ([current-readtable weird-table2])
    (let ([port (open-input-string "{testing {1 2 3}foo (testing)a ⌈b aoeu c⌉test d }")])
      (check-equal? (syntax->datum (read-syntax "t9" port))
                    '(testing (1 2 3) foo (testing) a (b aoeu c) test d))))

  )
