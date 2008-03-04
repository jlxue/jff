;; ido.el
(require 'ido)
(ido-mode t)

;; auctex
;(setq load-path (cons load-path "/usr/share/emacs-snapshot/site-lisp"))
;(require 'tex-site)
(load "/usr/share/emacs21/site-lisp/tex-site.el")
(setq-default TeX-master nil)
(setq TeX-parse-self t)
(setq TeX-auto-save t)

(require 'tabbar)
(tabbar-mode t)
(global-set-key [(control shift tab)] 'tabbar-backward)
(global-set-key [(control tab)]       'tabbar-forward)

;; linum.el
(require 'linum)

;; emacs-w3m
(require 'w3m-load)
(require 'mime-w3m)

