;;; tree-mode-group.el --- 

;; Copyright (C) 2005  Free Software Foundation, Inc.

;; Author: ChunYe Wang 
;; Keywords: 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; tr-mg-groups 定义了一个 modes 组，是一个 group 的定义,
;; group defination。一个 group defination 是一个 list ， 
;; 其中，第一个元素是 group definantion 的名字，后面的每一个 
;; element 是一个内容。

;; 如果 element 是一个 symbol 那么表示一个 mode 的名称,
;; 凡是符合 mode 的 buffer 都在属于这个 group

;; 如果 element 是一个 lambda 函数,那么就会调用这个函数.
;; 函数接受一个 buffer 作为参数.如果函数返回 t ,那么这样
;; 的 buffer 就属于这个组.

;; 如果 element 是一个 list , 那么这个 list 表示一个子
;; group .


;;; Code:
(require 'tree)

(defvar tr-mg-groups
  '("Mode Groups"
    ("Lisp" emacs-lisp-mode)
    ("C Program" c-mode c++-mode)
    ("HELP" help-mode Info-mode)
    ("Directory" dired-mode 
     ("HOME" (lambda (buffer)
               (with-current-buffer buffer
                 (string-match "^/home" (expand-file-name default-directory)))))
     ("PROJECTS" (lambda (buffer)
                   (with-current-buffer buffer
                     (string-match "^/project" (expand-file-name default-directory))))))
    ("Star Buffer" "^\\*.*\\*$")))
  
;;
(defun tr-mg-match-p (buffer mg)
  (with-current-buffer buffer
  (cond 
   ((symbolp mg)
    (or (eq major-mode mg)))
        ;;(and (fboundp mg) (funcall mg buffer))))
   ((functionp mg)
    (funcall mg buffer))
   ((stringp mg)
    (string-match mg (buffer-name buffer))))))

(defun tr-mg-match-mgs-p (buffer mgs)
  (let ((my-or (function (lambda(&rest x)
                           (if x (if (car x) t
                                   (apply my-or (cdr x))))))))
    (apply my-or 
           (mapcar 
            (lambda (mg)
              (tr-mg-match-p buffer mg))
            mgs))))

(defun tr-mg-match-a-node(buffer node)
  (let ((parent (tr-node-get-parent node)))
    (if (not parent) ;; root node
        t ;; match all buffers.
      (if (tr-mg-match-mgs-p buffer (tr-node-get-conditions node))
          (tr-mg-match-a-node buffer parent)
        nil))))

(defun tr-node-put-conditions (conditions node)
  (tr-node-put-property 'conditions conditions node))
(defun tr-node-get-conditions (node)
  (tr-node-get-property 'conditions node nil))
(defun tr-mg-on-click (node)
  (let ((buffer (tr-node-get-property 'buffer node)))
    (if (buffer-live-p buffer)
        (switch-to-buffer buffer))))
(defun tr-mg-update-function (node)
  (let* ((buffers (remove-if-not (lambda (b)
                                   (tr-mg-match-a-node b node))
                                 (buffer-list)))
         (add-buffer (function (lambda(b)
                                 (let ((n (tr-make-node (buffer-name b) node)))
                                   (tr-node-put-updated t n)
                                   (tr-node-put-property 'buffer b n)
                                   (tr-node-put-expended nil n)
                                   (tr-node-put-action-on-click 'tr-mg-on-click n))))))
    (tr-node-put-children nil node)
    (mapc add-buffer buffers)))

(defun tr-mg-make-nodes( groups parent)
  (let* ((g-name (car groups))
         (conditions (cdr groups))
         (node (tr-make-node g-name parent))
         sub-groups)
    (while conditions
      (let ((c (car conditions)))
        (cond
         ((functionp c) (tr-node-put-conditions (cons c (tr-node-get-conditions node))
                                                node))
         ((listp c) (setq sub-groups (cons c sub-groups)))
         (t (tr-node-put-conditions (cons c (tr-node-get-conditions node))
                                    node))))
      (setq conditions (cdr conditions)))
    (mapc (lambda (subgroup) (tr-mg-make-nodes subgroup node)) sub-groups)
    (when (not (tr-node-get-children node)) ;; leaf node
      (tr-node-put-update-function 'tr-mg-update-function node))
    (tr-node-put-updated t node)
    node))
(defun tr-mg ()
  (interactive "")
  (let* ((buffer (get-buffer-create "*tree mode groups*"))
         (oldbuffer (current-buffer))
         (node (tr-mg-make-nodes tr-mg-groups nil)))
    (tr-node-put-expended t node)
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (tr-mode node)
      (goto-char 1)
      (tr-mode-adjust-pos))
    (let* ((w1 (selected-window))
           (w2 (split-window w1 40 t)))
      (set-window-buffer w2 oldbuffer)
      (set-window-buffer w1 buffer))))


(provide 'tree-mode-group)
;;; tree-mode-group.el ends here
