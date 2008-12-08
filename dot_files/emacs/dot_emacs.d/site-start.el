;;; cheat to use lisp plugins in Debian emacs packages
;(defconst debian-emacs-flavor 'emacs21)
;; see /usr/share/emacs/22.0.50/lisp/startup.el.gz
;; and /usr/share/emacs/site-lisp/debian-startup.el
;; and /usr/share/doc/emacsen-common/debian-emacs-policy.gz
;(if (load "debian-startup" t t nil)
;                 (debian-startup 'emacs-snapshot))
;(load  "/usr/share/emacs/site-lisp/debian-startup.el")
;(debian-startup debian-emacs-flavor)


;;; then personal stuff...
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp/tree")

(mapc 'load (directory-files "~/.emacs.d/site-start.d" t "\.el$"))

