;;; tree-dir.el --- 

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

;; 



;;; Code:
(require 'tree)
(defvar tr-dir-windows-width 40 
  "default windows size when show the directory tree buffer.")
(defun tr-dir-node-get-text (node)
  (file-name-nondirectory (tr-node-get-dirname node)))
(defun tr-node-get-dirname (node)
  (tr-node-get-property 'file-name node))
(defun tr-node-put-dirname (filename node)
  (tr-node-put-property 'file-name filename node))
(defun tr-dir-on-click (node)
  (let ((b (current-buffer)))
    (find-file-other-window (tr-node-get-dirname node))
    (pop-to-buffer b)))

(defvar tr-dir-ignore-dir-regexp "^\\.$\\|^\\.\\.$")
(defun tr-update-directory-node (node)
  (let* ((dir (tr-node-get-dirname node))
         (children (remove-if (lambda (n) 
                                (not
                                 (file-directory-p
                                  (tr-node-get-dirname n))))
                              (tr-node-get-children node)))
         (children-files (mapcar 'tr-node-get-dirname children))
         (files (remove-if-not (lambda (f)
                             (and (file-directory-p f)
                                  (not (string-match tr-dir-ignore-dir-regexp (file-name-nondirectory f)))
                                  (not (member f children-files))))
                           (directory-files dir t nil nil)))
         (nodes (mapcar (lambda (f)
                          (let ((n (tr-make-node 'tr-dir-node-get-text nil)))
                            (tr-node-put-update-function 'tr-update-directory-node n)
                            (tr-node-put-action-on-click 'tr-dir-on-click n)
                            (tr-node-put-dirname f n)
                            n))
                        files))
         (new-children (append nodes children)))
    (tr-node-put-updated t node)
    (tr-node-put-children
     (sort new-children (lambda (n1 n2)
                   (let ((f1 (file-name-nondirectory (tr-node-get-dirname n1)))
                         (f2 (file-name-nondirectory (tr-node-get-dirname n2))))
                     (string-lessp f1 f2))))
     node)))
    
(defun tr-make-directory-node (dir depth &optional parent)
  (let* ((default-directory (expand-file-name dir))
         (node (tr-make-node 'tr-dir-node-get-text parent))
         (files (sort (directory-files dir t nil nil) 
                      (lambda (n1 n2)
                        (let ((f1 (file-name-nondirectory n1))
                              (f2 (file-name-nondirectory n2)))
                          (string-lessp f2 f1))))))
    (mapc
     (lambda (file)
       (cond 
        ((string-match tr-dir-ignore-dir-regexp (file-name-nondirectory file)) nil)
        ((file-directory-p file)
         (if (> depth 0)
             (tr-make-directory-node file (1- depth) node)
           (let ((n (tr-make-node 'tr-dir-node-get-text node)))
             (tr-node-put-update-function 'tr-update-directory-node n)
             (tr-node-put-dirname file n)
             (tr-node-put-action-on-click 'tr-dir-on-click n)
             (tr-node-put-updated nil n))))))
     files)
    (tr-node-put-update-function 'tr-update-directory-node node)
    (tr-node-put-dirname dir node)
    (tr-node-put-updated t node)
    (tr-node-put-action-on-click 'tr-dir-on-click node)
    node))

(defun tr-show-directory(dir)
  (interactive "Ddir:")
  (let ((buffer (get-buffer-create (format "*tree dir:%s*" dir)))
        (oldbuffer (current-buffer))
        (node (tr-make-directory-node (substring dir 0 -1) 0)))
    (tr-node-put-expended t node)
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (tr-mode node)
      (goto-char 1)
      (tr-mode-adjust-pos))
    (delete-other-windows)
    (let* ((w1 (selected-window))
           (w2 (split-window w1 tr-dir-windows-width t)))
      (set-window-buffer w2 oldbuffer)
      (set-window-buffer w1 buffer))))

;;(tr-node-test-show-directory "d:/cygwin/usr/share/emacs" ;;2)

;; (setq a (tr-make-node "root"))
;; (tr-make-node "child 3" a)

;; (let ((default-directory "d"))
;;   (directory-files "."))
;; (mapc (lambda (n)
;;         (princ (format "%s\n" (tr-node-get-text n))))
;;       (tr-node-get-children a))

(provide 'tree-dir)
;;; tree-dir.el ends here
