;;; purty.el --- Minor-mode to format PureScript code with purty on save

;; Original file: https://gitlab.com/joneshf/purty/-/blob/9561ae283e707733b8262c762288605ff3d63a36/purty.el

(require 'reformatter)

(defgroup purty nil
  "Minor mode for formatting PureScript buffers with purty."
  :group 'languages
  :link '(url-link "https://gitlab.com/joneshf/purty"))

(reformatter-define purty
  :program (purty-executable-path)
  :args '("-")
  :group 'purty
  :lighter " purty")

(defun purty-executable-path ()
  "Return the full path to the purty executable by invoking `npm bin`."
  (let* ((npm-bin (s-trim-right (shell-command-to-string "npm bin")))
         (formatter (expand-file-name "purty" npm-bin)))
    (if (and formatter (file-executable-p formatter))
        formatter
        (with-output-to-temp-buffer "*purty errors*"
          (print "Could not find `purty' executable. Please run `make' to install it")))))

(provide 'purty)
;;; purty.el ends here
