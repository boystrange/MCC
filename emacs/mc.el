(require 'generic-x) ;; we need this

(define-generic-mode 'mailbox-calculus-mode   ;; name of the mode to create
  '("//")                           ;; comments start with '--'
  '("interface" "process" "new" "case" "of" "done" "in" "free" "fail") ;; keywords
  '(("#[[:alnum:]]+" . 'font-lock-type-face)
    ("[[:digit:]]+" . 'font-lock-constant-face)
    ("]" . 'font-lock-function-name-face)
    ("[@[{}|?!:.&;\u00AC\u25B8\u2192]" . 'font-lock-function-name-face)
    ("[+*\u00B7]" . 'font-lock-preprocessor-face)
    )
  '("\\.mc") ;; files for which to activate this mode
  nil                              ;; other functions to call
  "A mode for the Mailbox Calculus" ;; doc string for this mode
  )
