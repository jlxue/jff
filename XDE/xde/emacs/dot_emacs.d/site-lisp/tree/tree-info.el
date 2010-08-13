;;; tree-info.el --- 

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
(defvar tr-info-windows-width 40 
  "default windows size when show the directory tree buffer.")
(defun tr-info-on-click (node)
  (let* ((buffer (get-buffer-create "*info*"))
         (nodespec (tr-node-get-property 'info-spec node))
         (junk (string-match "^(\\([^)]+\\))\\([^.]+\\)$" nodespec))
         (file (match-string 1 nodespec))
         (node (match-string 2 nodespec)))
    (pop-to-buffer buffer t)
    (with-current-buffer buffer
      (Info-find-node file node))))
(defun tr-info-update (node)
  (when (not (tr-node-get-updated node))
    (tr-node-put-updated t node)
    (let ((completion (reverse (Info-speedbar-fetch-file-nodes (tr-node-get-property 'info-spec node)))))
      (while completion
        (let* ((info-node (car completion))
               (sub-node (tr-make-node (car info-node) node))) 
          (tr-node-put-property 'info-spec (cdr info-node) sub-node)
          (tr-node-put-action-on-click 'tr-info-on-click sub-node)
          (tr-node-put-updated nil sub-node)
          (tr-node-put-update-function 'tr-info-update sub-node))
        (setq completion (cdr completion))))))
  
(defun tr-info-make-nodes ()
  (let ((node (tr-make-node "Info" nil)))
    (tr-node-put-property 'info-spec "(dir)top" node)
    (tr-info-update node)
    (tr-node-put-expended t node)
    (tr-node-put-action-on-click 'tr-info-on-click node)
    (tr-node-put-updated t node)
    node))
    
  
(defun tr-info ()
  (interactive "")
  (let* ((buffer (get-buffer-create "*tree Info*"))
         (oldbuffer (current-buffer))
         (node (tr-info-make-nodes)))
    (tr-node-put-expended t node)
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (tr-mode node)
      (goto-char 1)
      (tr-mode-adjust-pos))
    (delete-other-windows)
    (let* ((w1 (selected-window))
           (w2 (split-window w1 tr-info-windows-width t)))
      (set-window-buffer w2 oldbuffer)
      (set-window-buffer w1 buffer))))
(provide 'tree-info)
;;; tree-info.el ends here
