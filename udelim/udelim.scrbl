#lang scribble/manual

@(require (for-label racket/base
                     racket/contract
                     syntax/strip-context
                     udelim))

@title[#:tag "udelim"]{udelim}
@author+email["William Hatch" "william@hatch.uno"]

@defmodule[udelim]

@section{Guide}
This is a library I wrote primarily to help make nestable embedding of different syntax in #lang rash, but is generally useful for adding extra types of parenthesis or string delimiters to a language.  After watching Jay McCarthy's talk at Sixth Racketcon, I also decided to steal his idea of making different types of parenthesis wrap their contents with an additional #%symbol.

You can use the udelim meta-language (eg #lang udelim racket/base) to add a few extra parenthesis types and string types to any language.  Specifically, «» and “” are nestable non-escaping string delimiters, ﴾foo bar﴿ reads as (#%ornate-parens foo bar), ⦓foo bar⦔ reads as (#%inequality-brackets foo bar), ⦕foo bar⦖ reads as (#%double-inequality-brackets foo bar), 🌜foo bar🌛 reads as (#%moon-faces foo bar), and ⸨foo bar⸩ reads as (#%double-parens foo bar).

@section{Reference}

@(require racket/sandbox
          scribble/eval)
@(define my-evaluator
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string])
     (make-evaluator 'racket/base)))

@defproc[(make-list-delim-readtable
[l-paren char?]
[r-paren char?]
[#:base-readtable base-readtable readtable? #f])
readtable?]{
Returns a new readtable based on @racket[base-readtable] that uses @racket[l-paren] and @racket[r-paren] like parenthesis.  IE they read into a list.

@examples[#:eval my-evaluator
          (require udelim)
          (parameterize ([current-readtable (make-list-delim-readtable #\⟦ #\⟧)])
            (read
             (open-input-string "(a b ⟦c d e ⟦f g⟧ h i⟧ j k)")))]
}

@defproc[(make-list-delim-readtable/wrap
[l-paren char?]
[r-paren char?]
[wrapper-sym symbol?]
[#:base-readtable base-readtable readtable? #f])
readtable?]{
Like @racket[make-list-delim-readtable], except it puts @racket[wrapper-sym] at the head of the list.

@examples[#:eval my-evaluator
          (require udelim)
          (parameterize ([current-readtable
                         (make-list-delim-readtable/wrap #\⟦ #\⟧ '#%white-brackets)])
            (read
             (open-input-string "(a b ⟦c d e ⟦f g⟧ h i⟧ j k)")))]
}


@defproc[(make-string-delim-readtable
[l-paren char?]
[r-paren char?]
[#:base-readtable base-readtable readtable? #f])
readtable?]{
Returns a new readtable based on @racket[base-readtable] that uses @racket[l-paren] and @racket[r-paren] as delimiters to a non-escapable string (with balanced internal delimiters).

In addition to simply being a nice additional option to make literal strings, it goes great with @racket[stx-string->port] to use in macros that read alternative syntax, such as are used in #lang rash.  Other things you might do are create macros that read interesting surface syntax for different data structures, list comprehensions, or common patterns that you use that would benefit from a different syntax.

@examples[#:eval my-evaluator
          (require udelim)
          (parameterize ([current-readtable (make-string-delim-readtable #\« #\»)])
            (read
             (open-input-string
              "«this is a string with nested «string delimiters.»  No \n escape interpreting.»")))]
}

@defproc[(make-string-delim-readtable/wrap
[l-paren char?]
[r-paren char?]
[wrapper-sym symbol?]
[#:base-readtable base-readtable readtable? #f])
readtable?]{
Like @racket[make-string-delim-readtable], except that the result will be wrapped in a form with @racket[wrapper-sym].  This makes it easy to use @racket[make-rename-transformer] to make #%guillemets an alias for your string-reading macro, allowing you to invoke it implicitly with strings wrapped in «».

@examples[#:eval my-evaluator
          (require udelim)
          (parameterize ([current-readtable
                          (make-string-delim-readtable/wrap #\« #\» '#%guillemets)])
            (read
             (open-input-string
              "«this is a string with nested «string delimiters.»  No \n escape interpreting.»")))]
}

@defproc[(stx-string->port [stx syntax?]) input-port?]{
@racket[stx] should only contain a string.  The location data on @racket[stx] is used to give the resulting port accurate location information when it is read.  This is useful for creating macros that allow embedding of alternate syntax, such as #lang rash does.

When you use @racket[read-syntax] on the resulting port, the syntax objects will have correct location information, but will be lacking lexical context.  To fix this, use @racket[replace-context].

@examples[#:eval my-evaluator
          (require udelim)
          (with-syntax
          ([str #'"#(this \"is a\" ((string that) can be) read with some reader function)"])
           (read-syntax (syntax-source #'str) (stx-string->port #'str)))]

}

@section{Code and License}

The code is available
@hyperlink["https://github.com/willghatch/racket-udelim"]{on github}.

This library is licensed under the terms of the LGPL version 3, or (at
your option) any later version published by the Free Software
Foundation (IE LGPL3+).
