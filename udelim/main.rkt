#lang racket/base

(require racket/contract/base)
(provide
 (contract-out
  [make-string-delim-readtable
   (->* (char? char?) (#:base-readtable readtable?) readtable?)]
  [make-string-delim-readtable/wrap
   (->* (char? char? symbol?) (#:base-readtable readtable?) readtable?)]
  [make-list-delim-readtable
   (->* (char? char?) (#:base-readtable readtable?) readtable?)]
  [make-list-delim-readtable/wrap
   (->* (char? char? symbol?) (#:base-readtable readtable?) readtable?)]
  [stx-string->port (->* (syntax?) input-port?)]
  ))

(require syntax/readerr)

(define (make-raise-balance-error l-paren r-paren)
  (define raise-balance-error
    (case-lambda
      [(ch port) (raise-balance-error ch port #f #f #f #f)]
      [(ch port src line col pos)
       (raise-read-error (format "unexpected closing delimiter ~a~n" r-paren)
                         src line col pos #f)]))
  raise-balance-error)

(define (make-string-reader l-paren r-paren)
  ;; balance parens, and return the contents as a bare string with no escapes
  (define string-reader
    (case-lambda
      [(ch port) (syntax->datum (string-reader ch port #f #f #f #f))]
      [(ch port src line col pos)
       (define-values (n-line n-col n-pos) (port-next-location port))
       (define (loop ch cur-balance-level ch-so-far)
         (cond [(equal? eof ch)
                (raise-read-error
                 (format "unexpected eof, expected ~a more closing delimiter ~a~n"
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
                       [span (length final-chs)])
                  (datum->syntax
                   #f (apply string final-chs)
                   (list src n-line n-col n-pos span)))]))
       (loop ch 0 '())]))
  string-reader)

(define (make-list-reader l-paren r-paren)
  (define paren-reader
    (case-lambda
      [(ch port) (syntax->datum (paren-reader ch port #f #f #f #f))]
      [(ch port src line col pos)
       (define (loop stxs-rev)
         (let ([next-ch (read-char port)])
           (if (equal? next-ch r-paren)
               (datum->syntax #f (reverse stxs-rev))
               (let ([one-stx (read-syntax/recursive src port next-ch)])
                 (loop (cons one-stx stxs-rev))))))
       (loop '())]))
  paren-reader)

(define (make-delim-reader/wrap unwrapped-reader-maker list? delim-l delim-r wrapper-sym)
  (define reader (unwrapped-reader-maker delim-l delim-r))
  (define delim-reader-wrapped
    (case-lambda
      [(ch port) (syntax->datum (delim-reader-wrapped ch port #f #f #f #f))]
      [(ch port src line col pos)
       (with-syntax ([str-stx (reader ch port src line col pos)])
         (if list?
             #`(#,wrapper-sym . str-stx)
             #`(#,wrapper-sym str-stx)))]))
  delim-reader-wrapped)

(define (make-string-reader/wrap l r wrapper-sym)
  (make-delim-reader/wrap make-string-reader #f l r wrapper-sym))
(define (make-list-reader/wrap l r wrapper-sym)
  (make-delim-reader/wrap make-list-reader #t l r wrapper-sym))

(define (make-make-delim-readtable make-reader)
  (λ (l-paren r-paren #:base-readtable [base-readtable #f])
    (make-readtable
     base-readtable
     l-paren 'terminating-macro (make-reader l-paren r-paren)
     r-paren 'terminating-macro (make-raise-balance-error l-paren r-paren))))

(define (make-make-delim-readtable/wrap make-reader/wrap)
  (λ (l-paren r-paren wrapper-symbol #:base-readtable [base-readtable #f])
    (make-readtable
     base-readtable
     l-paren 'terminating-macro (make-reader/wrap l-paren r-paren wrapper-symbol)
     r-paren 'terminating-macro (make-raise-balance-error l-paren r-paren))))

(define make-string-delim-readtable
  (make-make-delim-readtable make-string-reader))
(define make-list-delim-readtable
  (make-make-delim-readtable make-list-reader))
(define make-string-delim-readtable/wrap
  (make-make-delim-readtable/wrap make-string-reader/wrap))
(define make-list-delim-readtable/wrap
  (make-make-delim-readtable/wrap make-list-reader/wrap))

(define (stx-string->port stx)
  (let ([str (syntax->datum stx)]
        [line (syntax-line stx)]
        [col (syntax-column stx)]
        [pos (syntax-position stx)]
        [src (syntax-source stx)])
    (if (not (string? str))
        (error 'stx-string->port "syntax is not a string: ~a~n" stx)
        (let ([p (open-input-string str src)])
          (port-count-lines! p)
          (set-port-next-location! p line col pos)
          p))))

(module+ test
  (require rackunit)

  (define mytable (make-string-delim-readtable #\{ #\}))
  (define guillemet-table
    (make-string-delim-readtable/wrap #\« #\» '#%guillemets))
  (define ceil-table (make-list-delim-readtable #\⌈ #\⌉))
  (define ceil-table/wrap (make-list-delim-readtable/wrap #\⌈ #\⌉ '#%ceil))

  (parameterize ([current-readtable mytable])
    (let ([port (open-input-string "  \"in a string {here\" {hello @{testing 123}foo} }goodbye")])
      (check-equal? (syntax->datum (read-syntax "foo" port))
                    "in a string {here")
      (check-equal? (syntax->datum (read-syntax "foo" port))
                    "hello @{testing 123}foo")
      (check-exn exn? (λ () (read-syntax "foo" port)))))

  (parameterize ([current-readtable guillemet-table])
    (let ([port (open-input-string "  \"in a string «here\" «hello @«testing 123»foo» »goodbye")])
      (check-equal? (syntax->datum (read-syntax "foo" port))
                    "in a string «here")
      (check-equal? (syntax->datum (read-syntax "foo" port))
                    '(#%guillemets "hello @«testing 123»foo"))
      (check-exn exn? (λ () (read-syntax "foo" port)))))

  (parameterize ([current-readtable ceil-table])
    (let ([port (open-input-string "(testing ⌈foo bar ⌈⌉ aoeu⌉ hello) foo")])
      (check-equal? (syntax->datum (read-syntax "foo" port))
                    '(testing (foo bar () aoeu) hello))))

  (parameterize ([current-readtable ceil-table/wrap])
    (let ([port (open-input-string "(testing ⌈foo bar ⌈⌉ aoeu⌉ hello) foo")])
      (check-equal? (syntax->datum (read-syntax "foo" port))
                    '(testing (#%ceil foo bar (#%ceil) aoeu) hello))))
  )
