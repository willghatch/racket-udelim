#lang racket/base

(require
 racket/contract/base
 racket/port
 )

(provide
 (contract-out
  [make-string-delim-readtable
   (->* (char? char?)
        (#:base-readtable readtable?
         #:wrapper (or/c false/c symbol? procedure?)
         #:as-dispatch-macro? any/c
         #:string-read-syntax (or/c false/c (-> any/c input-port? any/c))
         #:whole-body-readers? any/c)
        readtable?)]
  [make-list-delim-readtable
   (->* (char? char?)
        (#:base-readtable readtable?
         #:wrapper (or/c false/c symbol? procedure?)
         #:as-dispatch-macro? any/c
         #:inside-readtable (or/c false/c
                                  readtable?
                                  (-> (or/c false/c readtable?))
                                  'inherit))
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

(define (make-string-reader l-paren r-paren
                            #:wrapper [wrapper #f]
                            #:string-read-syntax [string-read-syntax #f]
                            #:whole-body-readers? [whole-body-readers? #f])
  ;; balance parens, and return the contents as a bare string with no escapes

  (define inner-read-syntax
    (and string-read-syntax
         (if whole-body-readers?
             string-read-syntax
             (λ (src port)
               (datum->syntax
                #f
                (port->list (λ (p) (string-read-syntax src p))
                            port))))))
  (define string-reader
    (case-lambda
      [(ch port) (syntax->datum (string-reader ch port #f #f #f #f))]
      [(ch port src line col pos)
       (define-values (n-line n-col n-pos) (port-next-location port))
       (define (loop ch cur-balance-level ch-so-far)
         (cond [(equal? eof ch)
                (raise-read-error
                 (format "unexpected eof, expected ~a more of closing delimiter ~a\n"
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
                ({make-raise-balance-error l-paren r-paren}
                 src line col pos)]
               [else
                (let* ([final-chs (cdr (reverse ch-so-far))]
                       [span (length final-chs)]
                       [str-stx (datum->syntax
                                 #f (apply string final-chs)
                                 (list src n-line n-col n-pos span))]
                       [read-stx (if inner-read-syntax
                                     (inner-read-syntax src
                                                        (stx-string->port str-stx))
                                     str-stx)])
                  (cond [(symbol? wrapper)
                         (datum->syntax
                          #f (list (datum->syntax #f wrapper
                                                  (list src line col pos span))
                                   read-stx))]
                        [(procedure? wrapper) (wrapper read-stx)]
                        [else read-stx]))]))
       (loop ch 0 '())]))
  string-reader)

(define whitespace-list '(#\space #\tab #\newline #\vtab #\return))

(define (make-list-reader l-paren r-paren
                          #:wrapper [wrapper #f]
                          #:inside-readtable [inside-readtable 'inherit])
  (define paren-reader
    (case-lambda
      [(ch port) (syntax->datum (paren-reader ch port #f #f #f #f))]
      [(ch port src line col pos)
       (define (loop stxs-rev)
         (let ([next-ch (read-char port)]
               [inner-readtable (cond
                                  [(procedure? inside-readtable)
                                   (let ([rt (inside-readtable)])
                                     (if (not rt)
                                         (make-readtable #f)
                                         rt))]
                                  [(readtable? inside-readtable)
                                   inside-readtable]
                                  [(not inside-readtable)
                                   (make-readtable #f)]
                                  [else (current-readtable)])])
           (cond
             ;; Ignore whitespace, unless it is bound in the inner readtable.
             [(and (member next-ch whitespace-list)
                   (let-values ([(macro-type reader-proc dispatch-proc)
                                 (readtable-mapping inner-readtable next-ch)])
                     (member macro-type whitespace-list)))

              (loop stxs-rev)]
             [(equal? next-ch r-paren)
              (let ([unwrapped (reverse stxs-rev)])
                (cond [(symbol? wrapper)
                       (datum->syntax
                        #f (cons (datum->syntax #f wrapper (list src line col pos 0))
                                 unwrapped))]
                      [(procedure? wrapper)
                       (wrapper (datum->syntax #f unwrapped
                                               (list src line col pos 0)))]
                      [else (datum->syntax #f unwrapped)]))]
             [else
              (let ([one-stx (parameterize
                                 ([current-readtable inner-readtable])
                               (read-syntax/recursive src
                                                      port
                                                      next-ch))])
                (loop (cons one-stx stxs-rev)))])))
       (loop '())]))
  paren-reader)



(define (make-string-delim-readtable l-paren r-paren
                                     #:base-readtable [base-readtable #f]
                                     #:wrapper [wrapper #f]
                                     #:as-dispatch-macro? [as-dispatch-macro? #f]
                                     #:string-read-syntax [string-read-syntax #f]
                                     #:whole-body-readers? [whole-body-readers? #f])
  (make-readtable base-readtable
                  l-paren
                  (if as-dispatch-macro? 'dispatch-macro 'terminating-macro)
                  (make-string-reader
                   l-paren r-paren
                   #:wrapper wrapper
                   #:string-read-syntax string-read-syntax
                   #:whole-body-readers? whole-body-readers?)
                  r-paren 'terminating-macro (make-raise-balance-error l-paren r-paren)))

(define (make-list-delim-readtable l-paren r-paren
                                   #:base-readtable [base-readtable #f]
                                   #:wrapper [wrapper #f]
                                   #:as-dispatch-macro? [as-dispatch-macro? #f]
                                   #:inside-readtable [inside-readtable 'inherit])
  (make-readtable base-readtable
                  l-paren
                  (if as-dispatch-macro? 'dispatch-macro 'terminating-macro)
                  (make-list-reader l-paren r-paren
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
  (define guillemet-table/wrap
    (make-string-delim-readtable #\« #\» #:wrapper '#%guillemets))
  (define guillemet-table/wrap2
    (make-string-delim-readtable
     #\« #\»
     #:wrapper (λ (stx) (datum->syntax #f (list'#%guillemets2 stx)))))
  (define guillemet-table/wrap3
    (make-string-delim-readtable #\« #\» #:wrapper '#%guillemets
                                 #:as-dispatch-macro? #t))
  (define ceil-table (make-list-delim-readtable #\⌈ #\⌉))
  (define ceil-table/wrap (make-list-delim-readtable #\⌈ #\⌉ #:wrapper '#%ceil))
  (define ceil-table/wrap2
    (make-list-delim-readtable
     #\⌈ #\⌉
     #:wrapper (λ (stx) (datum->syntax #f (cons '#%ceil2 (syntax-e stx))))))
  (define ceil-table/wrap3 (make-list-delim-readtable #\⌈ #\⌉ #:wrapper '#%ceil
                                                      #:as-dispatch-macro? #t))

  (parameterize ([current-readtable mytable])
    (let ([port (open-input-string "  \"in a string {here\" {hello @{testing 123}foo} }goodbye")])
      (check-equal? (syntax->datum (read-syntax "t1" port))
                    "in a string {here")
      (check-equal? (syntax->datum (read-syntax "t1" port))
                    "hello @{testing 123}foo")
      (check-exn exn? (λ () (read-syntax "t1" port)))))

  (parameterize ([current-readtable guillemet-table/wrap])
    (let ([port (open-input-string "  \"in a string «here\" «hello @«testing 123»foo» »goodbye")])
      (check-equal? (syntax->datum (read-syntax "t2" port))
                    "in a string «here")
      (check-equal? (syntax->datum (read-syntax "t2" port))
                    '(#%guillemets "hello @«testing 123»foo"))
      (check-exn exn? (λ () (read-syntax "t2" port)))))

  (parameterize ([current-readtable guillemet-table/wrap2])
    (let ([port (open-input-string "  \"in a string «here\" «hello @«testing 123»foo» »goodbye")])
      (check-equal? (syntax->datum (read-syntax "t2-wrap2" port))
                    "in a string «here")
      (check-equal? (syntax->datum (read-syntax "t2-wrap2" port))
                    '(#%guillemets2 "hello @«testing 123»foo"))
      (check-exn exn? (λ () (read-syntax "t2-wrap2" port)))))

  (parameterize ([current-readtable guillemet-table/wrap3])
    (let ([port (open-input-string "  \"in a string «here\" #«hello @#«testing 123»foo» »goodbye")])
      (check-equal? (syntax->datum (read-syntax "t2-wrap3" port))
                    "in a string «here")
      (check-equal? (syntax->datum (read-syntax "t2-wrap3" port))
                    '(#%guillemets "hello @#«testing 123»foo"))
      (check-exn exn? (λ () (read-syntax "t2-wrap3" port)))))

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

  (parameterize ([current-readtable ceil-table/wrap2])
    (let ([port (open-input-string "(testing ⌈foo bar ⌈⌉ () aoeu⌉ hello) foo")])
      (check-equal? (syntax->datum (read-syntax "t4-wrap2" port))
                    '(testing (#%ceil2 foo bar (#%ceil2) () aoeu) hello))))

  (parameterize ([current-readtable ceil-table/wrap3])
    (let ([port (open-input-string "(testing #⌈foo bar #⌈⌉ () aoeu⌉ hello) foo")])
      (check-equal? (syntax->datum (read-syntax "t4-wrap3" port))
                    '(testing (#%ceil foo bar (#%ceil) () aoeu) hello))))

  (define weird-table (make-list-delim-readtable #\{ #\} #:inside-readtable mytable))
  (parameterize ([current-readtable weird-table])
    (let ([port (open-input-string "{testing a {b c} d }")])
      (check-equal? (syntax->datum (read-syntax "t5" port))
                    '(testing a "b c" d))))

  (define weird-table2 (make-list-delim-readtable
                        #\{ #\} #:inside-readtable (λ () ceil-table)))
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

  (define tab-table (make-list-delim-readtable
                     #\{ #\} #:base-readtable (make-readtable #f #\tab #\a #f)))
  (parameterize ([current-readtable tab-table])
    (let ([port (open-input-string "(testing \tfoo \t {a b \t \tc} aoeu)")])
      (check-equal? (syntax->datum (read-syntax "t10" port))
                    `(testing ,(string->symbol "\tfoo")
                              ,(string->symbol "\t")
                              (a b
                                 ,(string->symbol "\t")
                                 ,(string->symbol "\tc"))
                              aoeu))))

  (define inner-read-test-table1
    (make-string-delim-readtable #\⟅ #\⟆ #:string-read-syntax read-syntax))
  (parameterize ([current-readtable inner-read-test-table1])
    (let ([port (open-input-string "⟅this (is ⟅a read⟆) test⟆")])
      (check-equal? (syntax->datum (read-syntax "t11" port))
                    '(this (is (a read)) test))))

  (define inner-read-test-table2
    (make-string-delim-readtable
     #\⟅ #\⟆
     #:wrapper 'test-wrap
     #:string-read-syntax (λ (src p) (string-upcase (port->string p)))
     #:whole-body-readers? #t))
  (parameterize ([current-readtable inner-read-test-table2])
    (let ([port (open-input-string "⟅this (is ⟅a read⟆) test⟆")])
      (check-equal? (syntax->datum (read-syntax "t12" port))
                    '(test-wrap "THIS (IS ⟅A READ⟆) TEST"))))
  )
