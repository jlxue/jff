;; http://www.emacswiki.org/emacs/ParEdit
(defvar electrify-return-match
  "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an \"electric\" return.")

(defun electrify-return-if-match (arg)
  "If the text after the cursor matches `electrify-return-match' then
open and indent an empty line between the cursor and the text.  Move the
cursor to the new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at electrify-return-match)
        (save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
(when (fboundp 'paredit-mode)
  (mapc (lambda (hook)
          (add-hook hook (lambda ()
                           (paredit-mode +1)
                           (local-set-key (kbd "RET") 'electrify-return-if-match)
                           (show-paren-mode t))))
        '(emacs-lisp-mode-hook lisp-mode-hook lisp-interaction-mode-hook slime-repl-mode-hook)))

;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

;; slime selector
(global-set-key "\C-cs" 'slime-selector)

;; use w3m to browse CLHS
;(require 'w3m)
(when (fboundp 'w3m-browse-url)
  (setq browse-url-browser-function '(("hyperspec" . w3m-browse-url)
                                      ("." . browse-url-default-browser))))

