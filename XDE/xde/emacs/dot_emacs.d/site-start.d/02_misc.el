;;;;;;;;;; from http://ann77.stu.cdut.edu.cn/Emacs/EmacsTweak.html
;;;
;;; 保存 session 和 desktop . 每次关闭 emacs 后, 在打开 emacs 时,
;;; 恢复很多很多东西, 就像你没有关闭 emacs 一样。
;;; 第一次使用的时候,要运行 M-x desktop-save 命令。
; (require 'session)
; (add-hook 'after-init-hook 'session-initialize)
; (load "desktop")
; (desktop-load-default)
; (desktop-read)
;;;
;;; 在窗口的标题栏上显示文件名称
;; C-h v to find topic "mode-line-format"
(setq frame-title-format "Emacs 23@%n%b | %f")
;;;
;;; Stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)
;;;
;;; 显示列号
(setq column-number-mode t)
;;;
;;; 设置很大的kill ring，以免删除重要的东西
(setq kill-ring-max 200)
;;;
;;; 把fill-column设成72
(setq default-fill-column 72)
;;;
;;; 设置 sentence-end 可以识别中文标点，不用在 fill 时在句号后插入两个空格
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)
;;;
;;; 可以递归的使用 minibuffer
(setq enable-recursive-minibuffers t)
;;;
;;; 把缺省的 major mode 设置为 text-mode, 而不是几乎什么功能
;;; 也没有的 fundamental-mode.
(setq default-major-mode 'text-mode)
;;;
;;; 括号匹配时显示另外一边的括号，而不是跳到另一个括号
(show-paren-mode t)
(setq show-paren-style 'parentheses)
;;;
;;; 光标靠近鼠标指针时，让鼠标指针自动让开，别挡住视线。
(mouse-avoidance-mode 'animate)
;;;
;;; 让 Emacs 可以直接打开和显示图片
(auto-image-file-mode)
;;;
;;; 设置有用的个人信息，这在很多地方有用
(setq user-full-name "Liu Yubao")
(setq user-mail-address "yubao.liu@gmail.com")
;;;
;;; 让 dired 可以递归的拷贝和删除目录
(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)
;;;
;;; 有tool bar和menu bar
(setq tool-bar-mode t)
(setq menu-bar-mode nil)
;;;
;;; use clipboard
(setq x-select-enable-clipboard t)
;;;
;;; 高亮当前行
;(require 'hl-line)
;(global-hl-line-mode t)
;; 让高亮的颜色自动和当前的配色方案配合, 每次改变背景颜色的时候, 执行
;; M-x wcy-color-theme-adjust-hl-mode-face.
;; list-faces-display可以显示所有的 face 的信息, describe-face可以显示
;; 某一个 face 的详细信息
; <--------白色背景下是灰色高亮，没有原来的绿色舒服
;# (or (facep 'my-hl-line-face) (make-face 'my-hl-line-face))
;# (setq hl-line-face 'my-hl-line-face)
;# (face-spec-set 'my-hl-line-face '((t (
;# 				      :background "DodgerBlue3"
;# 				      ; :bold 
;# 				      ; :weight nil
;# 				      :inverse-video nil
;# 				      ))))
;# (defun wcy-color-theme-adjust-hl-mode-face()
;#   (interactive)
;#   (let* ((color  (x-color-values (face-attribute 'default :background))))
;#     (if (null color)
;#       (error "not support.")
;#       (let ((my-color (mapcar 
;# 			(lambda (x)
;# 			  (let ((y (/ #XFFFF 4))
;# 				(delta #X8FF))
;# 			    (cond
;# 			      ((< x (* y 1))
;# 			       (+ x delta))
;# 			      ((< x (* y 2))
;# 			       (- x delta))
;# 			      ((< x (* y 3))
;# 			       (+ x delta))
;# 			      (t 
;# 				(- x delta)))))
;# 			color)))
;# 	(message "%S %S" color my-color)
;# 	(set-face-attribute
;# 	  'my-hl-line-face nil 
;# 	  :background 
;# 	  (concat "#"
;# 		  (mapconcat 
;# 		    (lambda (c) (format "%04X" c))
;# 		    my-color
;# 		    "")))))))
;# (wcy-color-theme-adjust-hl-mode-face)
; <-----------------------------------------
;;;;;;;;;; END from ann77  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq column-number-mode t)
(setq mouse-yank-at-point t)
(show-paren-mode t)
(setq show-paren-style 'parentheses)
(setq version-control t)
(setq kept-new-versions 3)
(setq delete-old-versions t)
(setq kept-old-versions 2)
(setq dired-kept-versions 1)
(setq visible-bell t)

(cond 
  (
   (fboundp 'global-font-lock-mode)
   ;; Turn on font-lock in all modes that support it
   (global-font-lock-mode t)
   ;; Maximum colors
   (setq font-lock-maximum-decoration t)
   )
  )

(global-set-key "\C-xg" 'goto-line)
(setq-default ispell-program-name "aspell")
(setq-default ispell-local-dictionary "american")
;;(global-set-key (kbd "") 'ispell-complete-word)

(fset 'yes-or-no-p 'y-or-n-p)
(transient-mark-mode t)
(column-number-mode t)
(display-time-mode t)

;; avoid jump to former paenthese 
(setq show-paren-mode 't)
(setq show-paren-style 'parentheses)    ; seems does not work
;; avoid jump when scrolling
(setq scroll-setp 1
      scroll-margin 0
      scroll-conservatively 10000
      )
;; auto show image
(auto-image-file-mode)
;; no  backup files
(setq make-backup-file nil)
;; big file highligh too slow;
(setq lazy-lock-defer-on-scrolling t)
; emacs 23 报错
;(setq font-lock-support-mode 'lazy-lock-mode)
(setq font-lock-maximum-decoration t)
;; auto complete function
(global-set-key "\M- " 'hippie-expand)
(setq hippie-expand-try-functions-list 
      '(try-complete-file-name-partially 
	 try-complete-file-name 
	 try-expand-all-abbrevs 
	 try-expand-list 
	 try-expand-line 
	 try-expand-dabbrev 
	 try-expand-dabbrev-all-buffers 
	 try-expand-dabbrev-from-kill 
	 try-complete-lisp-symbol-partially 
	 try-complete-lisp-symbol)) 


;; recentf
(require 'recentf)
(recentf-mode t)

;	;;gnuserv
;	;;;;;;;;;;;;(defun temp-directory () "/tmp")
;	(require 'gnuserv)
;	(gnuserv-start)
;	;; 在当前frame打开
;	(setq gnuserv-frame (selected-frame))
;	;; 打开后让Emacs跳到前面来
;	(setenv "GNUSERV_SHOW_EMACS" "1")

;;TODO mode
(setq todo-file-do "~/.emacs.d/todo-do")
(setq todo-file-done "~/.emacs.d/todo-done")
(setq todo-file-top "~/.emacs.d/todo-top")

;;diary-mode
(setq diary-file "~/.emacs.d/diary")
(add-hook 'diary-hook 'appt-make-list)

;;default-directory
(setq default-directory "~/")

;;; for tree*.el from ann77
(setq tr-image-directory "~/.emacs.d/site-lisp/tree/images/")
; M-x tr-show-directory
(require 'tree-dir)
; M-x tr-info
(require 'tree-info)
; M-x tr-imenu
(require 'tree-imenu)
; M-x tr-mg
(require 'tree-mode-group)

(if (fboundp 'color-theme-calm-forest)
  (color-theme-calm-forest))

