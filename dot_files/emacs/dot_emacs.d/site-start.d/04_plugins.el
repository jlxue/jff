;; ido.el
(require 'ido)
(ido-mode t)

(require 'tabbar)
(tabbar-mode t)
(global-set-key [(control shift tab)] 'tabbar-backward)
(global-set-key [(control tab)]       'tabbar-forward)

;; linum.el
(require 'linum)

