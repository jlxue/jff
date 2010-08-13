;;; for CPerlMode
;; from http://www.emacswiki.org/cgi-bin/wiki/CPerlMode
;;
;; replace the standard perl-mode with cperl-mode 
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))
;;; cperl-mode is preferred to perl-mode                                        
;;; "Brevity is the soul of wit" <foo at acm.org>                               
(defalias 'perl-mode 'cperl-mode)
;; remove the trailing underline
(setq cperl-invalid-face nil)
;; make cperl-mode always highlight scalar variables
(setq cperl-highlight-variables-indiscriminately t)


;;; for cc-mode
(require 'cc-mode)
(defun my-c-mode-common-hook ()
  (c-set-style "k&r")             	;; 代码缩进风格
  (c-toggle-auto-hungry-state t)        ;; 删除时一口吃掉前或后所有的空格
  (c-toggle-hungry-state t)
  ;  (setq tab-width 4                     ;; tab width 控制
  ;	indent-tabs-mode nil                ;; 扩展tab为空格
  ;	c-basic-offset 4))
  )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;;; for Scheme
;(require 'quack)

;;; for Common Lisp
;(setq inferior-lisp-program "/usr/bin/sbcl")
;(setq inferior-lisp-program "/usr/bin/clisp")
;(require 'slime)
;(slime-setup)
;; for cldoc.el
;(autoload 'turn-on-cldoc-mode "cldoc" nil t)
;(dolist (hook '(lisp-mode-hook
;                slime-repl-mode-hook))
;  (add-hook hook 'turn-on-cldoc-mode))

