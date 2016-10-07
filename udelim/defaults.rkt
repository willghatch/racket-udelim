#lang racket/base

(provide
 #%ornate-parens
 #%s-shaped-bag-delim
 #%inequality-brackets
 #%double-inequality-brackets
 #%moon-faces

 #%cjk-corner-quotes
 )

(require (for-syntax racket/base))

(define-syntax (pass-through-list stx)
  (syntax-case stx ()
    [(ptm e ...) #'(e ...)]))
(define-syntax #%ornate-parens (make-rename-transformer #'pass-through-list))
(define-syntax #%s-shaped-bag-delim (make-rename-transformer #'pass-through-list))
(define-syntax #%inequality-brackets (make-rename-transformer #'pass-through-list))
(define-syntax #%double-inequality-brackets (make-rename-transformer #'pass-through-list))
(define-syntax #%moon-faces (make-rename-transformer #'pass-through-list))

(define-syntax (pass-through-one stx)
  (syntax-case stx ()
    [(ptm e) #'e]))
(define-syntax #%cjk-corner-quotes (make-rename-transformer #'pass-through-one))

