;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((dhall-mode
  (dhall-format-arguments "--ascii"))
 (purescript-mode
  ;; Ideally, this expression would be `(load-file "emacs/purty.el")',
  ;; but we're hampered by what `load-file' expects and what we can do in a Directory Local Variables File.
  ;;
  ;; We want to `load-file' on the `emacs/purty.el' file,
  ;; but have to reference it absolutely.
  ;; We find where we are `.dir-locals.el',
  ;; then `expand-file-name' on `emacs/purty.el' relative to us.
  ;; This gives us the full path to `emacs/purty.el'.
  ;;
  ;; Since we cannot execute functions directly (a restriction of Directory Local Variables Files),
  ;; we `eval' `load-file' to circumvent the restriction.
  ;;
  ;; If there's a simpler alternative, we should switch to that.
  (psc-ide-source-globs
   "src/**/*.purs"
   ".spago/**/*.purs")
  (eval load-file (expand-file-name
                   "emacs/purty.el"
                   (locate-dominating-file (buffer-file-name) ".dir-locals.el")))
  (psc-ide-flycheck-ignored-error-codes . ("HiddenConstructors"))
  (mode . purty-on-save)
  (psc-ide-use-npm-bin . t)))
