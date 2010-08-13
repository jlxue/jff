;;; tree-imenu.el --- 

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
(require 'imenu)

(defun tr-imenu-on-click (node)
  (let* ((m (tr-node-get-property 'marker node))
         (b (marker-buffer m)))
    (if (buffer-live-p b)
        (progn
          (pop-to-buffer b)
          (goto-char m))
      (error (format "%S buffer is dead." b)))))
(defun tr-imenu-make-node(parent indexes)
  (tr-node-put-expended t parent)
  (cond 
   ((imenu--subalist-p indexes)
    (let ((node (tr-make-node (car indexes) parent)))
      (mapc (lambda (index)
              (tr-imenu-make-node node index))
            (cdr indexes))))
   ((and (markerp (cdr-safe indexes)) (> (cdr indexes) 0))
    (let ((node (tr-make-node (car indexes) parent)))
      (tr-node-put-property 'marker (cdr indexes) node)
      (tr-node-put-action-on-click 'tr-imenu-on-click node)))))



(defun tr-imenu ()
  (interactive "")
  (let* ((indexes (imenu--make-index-alist))
         (oldbuffer (current-buffer))
         (buffer (get-buffer-create (format "*tree-imenu-%s*" (buffer-name))))
         (root (tr-make-node (format "FILE:%s" (or (buffer-file-name) "<NIL>")) nil)))
    (tr-node-put-expended t root)
    (mapc #'(lambda (index)
              (tr-imenu-make-node root index))
          (reverse indexes))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (tr-mode root)
      (goto-char 1)
      (tr-mode-adjust-pos))
    (let* ((w1 (selected-window))
           (w2 (split-window w1 tr-dir-windows-width t)))
      (set-window-buffer w2 oldbuffer)
      (set-window-buffer w1 buffer))))


(provide 'tree-imenu)
;;; tree-imenu.el ends here
