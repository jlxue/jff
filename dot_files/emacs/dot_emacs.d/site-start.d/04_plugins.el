;; ido.el
(require 'ido)
(ido-mode t)

(require 'tabbar)
(tabbar-mode t)
(global-set-key [(control shift tab)] 'tabbar-backward)
(global-set-key [(control tab)]       'tabbar-forward)

;; linum.el
(require 'linum)

;; easypg
(require 'epa-file)
(epa-file-enable)
(setq epa-file-encrypt-to nil)
(setq epa-file-cache-passphrase-for-symmetric-encryption t)
(setq epa-file-inhibit-auto-save nil)

;; ccrypt
;(require 'jka-compr-ccrypt)

