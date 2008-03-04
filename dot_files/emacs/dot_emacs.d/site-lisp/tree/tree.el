;;; tree.el --- a libary for tree view

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


(defun tr-node-new ()
  (make-symbol "a-tree-node"))
(defun tr-node-get-property (property node &optional default)
  (or (get node property) default))
(defun tr-node-put-property (property value node)
  (put node property value ))
(defun tr-make-node (text &optional parent  pos)
  "create a node. parent is nil, mean a root node. pos
is a number, telling where the new created node is inserted.
if pos is nil, then append the new created node to the
parent's children.
"
  (let ((r (tr-node-new)))
    (tr-node-put-property :text text r)
    (tr-node-put-property :children nil r)
    (tr-node-put-property :expended nil r)
    (if (not parent)
        (tr-node-put-property :depth 0 r)
      (tr-node-put-property :depth (1+ (tr-node-get-depth parent)) r)
      ;; insert this node into parent.
      (tr-node-insert-child parent r pos))
    r))

;; property children.
(defun tr-node-get-children(node)
  "return the children node list. nil if node is a leaf node."
  (tr-node-get-property :children node))
;; put parent of each elt in childres to node also. the depth of each child is
;; put correctly too.
(defun tr-node-put-children(children node)
  "put the children"
  (tr-node-put-property :children children node)
  (mapc (function (lambda (n)
          (tr-node-put-property :depth (1+ (tr-node-get-depth node)) n)
          (tr-node-put-property :parent node n)))
        children))
(defun tr-node-not-last-child-p(node)
  (let ((parent (tr-node-get-parent node)))
    (if parent
        (cdr (memq node (tr-node-get-children parent)))
      t)))
;; ;; is it function useful ?
;; (defun tr-node-sort-children (node sortfunc)
;;   (let ((children (tr-node-get-children node)))
;;     (tr-node-put-property :children 
;;                           (sort children sortfunc) node)))

;; whether the node is a leaf node.
(defun tr-node-leaf-node-p(node)
  (not (tr-node-get-children node)))

;; property parent. read only
(defun tr-node-get-parent (node)
  "return nil, if node is a root node."
  (tr-node-get-property :parent node))
;; if parent is nil, then the node is root node.
(defun tr-node-root-node-p(node)
  (not (tr-node-get-parent node)))
(defun tr-node-get-next-brother (node)
  (let ((parent (tr-node-get-parent node)))
    (if (not parent)
        nil ;; root node has no next brother.
      (cadr (memq node (tr-node-get-children parent))))))
;; property depth. read only. depth = 0 means root node.
(defun tr-node-get-depth (node)
  "return the depth of a node. read only."
  (tr-node-get-property :depth node 0))

;; property text the text property can be a string or a function. if function,
;; the function accept a NODE as a parameter and return a string as text of the
;; node. the text of node is displayed.
(defun tr-node-get-text (node)
  (let ((f (tr-node-get-property :text node)))
    (cond 
     ((stringp f) f)
     ((functionp f) (funcall f node))
     (t ""))))

;; the function is used to access the property of `update-function'
(defun tr-node-get-update-function (node)
  (or (tr-node-get-property :update-function node) 'ignore))
(defun tr-node-put-update-function (func node)
  (tr-node-put-property :update-function func node))
;; the following two function is used to access the property of `updated'
;; if click on a node, then update the node, i.e. get all
;; possible children of the node. for effiency, node is
;; updated on demand. so when a node is not updated, we can
;; not tell whether the node is a leaf node, even it has no
;; children.
(defun tr-node-get-updated (node)
  (tr-node-get-property :updated node))
(defun tr-node-put-updated (v node)
  (tr-node-put-property :updated v node))

;; if the children are visible, the expand is true.
;; if any parent of a node is not expanded, the node is visible.
(defun tr-node-get-expended (node)
  (if (not node)
      t ;; root node is expaned.
    (if (tr-node-get-property :expended node)
        (tr-node-get-expended (tr-node-get-parent node))
      nil)))
;; put the expaned property of a node.
(defun tr-node-put-expended (v node)
  (tr-node-put-property :expended v node))
;; check whether a node is visiable.
(defun tr-node-get-visible(node)
  (let ((parent (tr-node-get-parent node)))
    (if parent 
        (tr-node-get-expended parent)
      ;; root node should be always visiable.
      t)))
;; put all parent of a node expanded. so make the node is visible.
(defun tr-node-make-visible(v node)
  (let ((parent (tr-node-get-parent node)))
    (when parent
      (tr-node-put-expended t parent)
      (tr-node-make-visible t parent))))

    
(defun tr-node-get-action-on-click (node)
  (or (tr-node-get-property :on-click node) 'ignore))
(defun tr-node-put-action-on-click (func node)
  (tr-node-put-property :on-click func node))
(defun tr-node-insert-child (parent child pos)
  "insert child into parent's children list. at pos."
  (or parent (error "parent is nil. when insert a child."))
  (or child (error "it doesn't make sense to insert a nil child"))
  (let* ((children (tr-node-get-children parent)))
         ;;(rpos (or pos (length children)
    (tr-node-put-property :depth (1+ (tr-node-get-depth parent)) child)
    (tr-node-put-property :parent parent child)
    (tr-node-put-property :children (cons child children) parent )))

;; mode for view a tree
(defvar tr-mode-root-node nil)
(make-variable-buffer-local 'tr-mode-root-node)
(defvar tr-mode-map nil)
(if tr-mode-map
    ()
  (setq tr-mode-map (make-sparse-keymap))
  (define-key tr-mode-map (kbd "<RET>") 'tr-mode-click-node)
  (define-key tr-mode-map (kbd "n") 'tr-mode-node-at-point)
  (define-key tr-mode-map (kbd "<right>") 'tr-mode-expand-node)
  (define-key tr-mode-map (kbd "<left>") 'tr-mode-contract-node)
  (define-key tr-mode-map (kbd "<down>") 'tr-mode-next)
  (define-key tr-mode-map (kbd "<up>") 'tr-mode-prev)
  (define-key tr-mode-map (kbd "<down-mouse-1>") 'ignore)
  (define-key tr-mode-map (kbd "<mouse-1>") 'tr-mode-mouse-click)
  (define-key tr-mode-map (kbd "<drag-mouse-1>") 'ignore)
  (define-key tr-mode-map (kbd "g") 'tr-mode-redraw)
  (define-key tr-mode-map (kbd "<SPC>") 'scroll-up)
  (define-key tr-mode-map (kbd "<DEL>") 'scroll-down)
  (define-key tr-mode-map (kbd "q") 'kill-buffer))

(defun tr-mode(root-node)
  "Major mode for view a tree structure:
      Special commands: \\{tr-mode-map}
     Turning on text-mode runs the hook `text-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (setq tr-mode-root-node root-node)
  (set-syntax-table (make-syntax-table))
  (use-local-map tr-mode-map)
  (setq local-abbrev-table nil)
  (setq buffer-read-only nil)
  (setq mode-name "tree-view")
  (setq major-mode 'tr-mode)
  (tr-mode-draw-node tr-mode-root-node)
  (setq buffer-read-only t)
  (run-hooks 'tr-mode-hook))
;; functions related with overlay
(defun tr-make-overlay(start end)
  (let ((s (make-symbol "overlay"))
        (m1 (make-marker))
        (m2 (make-marker)))
    (set-marker m1 start (current-buffer))
    (set-marker m2 end (current-buffer))
    (set s (cons m1 m2))
    s))
         
(defun tr-move-overlay( overlay begin end)
  (let ((v (symbol-value overlay)))
    (set-marker (car v) begin)
    (set-marker (cdr v) end)))
(defun tr-overlay-put( overlay s v)
  nil)
(defun tr-overlay-buffer(overlay)
  (let ((v (symbol-value overlay)))
    (marker-buffer (car v))))
(defun tr-overlay-start(overlay)
  (car (symbol-value overlay)))
(defun tr-overlay-end(overlay)
  (cdr (symbol-value overlay)))

;; get a list of all node.
(defun tr-get-tree-node-list (node)
  (apply 'append (list node)
         (if (tr-node-get-expended node)
             (mapcar 'tr-get-tree-node-list 
                     (tr-node-get-children node))
           nil)))
(defvar tr-image-directory nil)


;; (defimage tr-image-open 
;;   '((:file "open" :type :xpm)


;; ))

(defconst tr-mode-tree-image-names
  `((open       "[-] " ,(and tr-image-directory (create-image (concat tr-image-directory "open.xpm"))))
    (close      "[+] " ,(and tr-image-directory (create-image (concat tr-image-directory "close.xpm"))))
    (no-updated "[x] " ,(and tr-image-directory (create-image (concat tr-image-directory "empty.xpm"))))
    (leaf       " *  " ,(and tr-image-directory (create-image (concat tr-image-directory "leaf.xpm"))))
    (guide      " |  " ,(and tr-image-directory (create-image (concat tr-image-directory "guide.xpm"))))
    (no-guide   "    " ,(and tr-image-directory (create-image (concat tr-image-directory "no_guide.xpm"))))
    (end-guide  " `- " ,(and tr-image-directory (create-image (concat tr-image-directory "end_guide.xpm"))))
    (handle     " |- ",(and tr-image-directory (create-image (concat tr-image-directory "handle.xpm")))))
  "contains images possible to draw a tree.")

(defun tr-mode-draw-a-node-get-preceding (node)
  (let ((parent (tr-node-get-parent node)))
    (if parent
        (cons (if (tr-node-not-last-child-p node) 
                  'guide
                'no-guide)
              (tr-mode-draw-a-node-get-preceding parent))
      nil)))

(defun tr-mode-draw-a-node-draw-a-symbol(type)
  (let* ((pair (assq type tr-mode-tree-image-names))
         (ascii (nth 0 (cdr pair)))
         (image (nth 1 (cdr pair)))
         start end overlay)
    (setq start (point))
    (insert ascii)
    (setq end (point))
    (when t ;; there are some problem when display adjance image.
      (put-text-property start end 'display (cons 'image (cdr image))))
    nil))

(defun tr-mode-draw-a-node (node)
  (let ((nodeflag (cond 
                   ((not (tr-node-get-updated node)) 'no-updated)
                   ((tr-node-leaf-node-p node) 'leaf )
                   ((tr-node-get-expended node) 'open)
                   (t 'close)))
        begin line-start
        (overlay (tr-node-get-property :overlay node)))
    (or overlay (setq overlay (tr-make-overlay 1 1)))
    (when (not (tr-node-root-node-p node))
      (insert-char ?\n 1)
      (mapc 'tr-mode-draw-a-node-draw-a-symbol 
            (reverse (tr-mode-draw-a-node-get-preceding (tr-node-get-parent node)))))
    (when (not (tr-node-root-node-p node))
      (if (tr-node-not-last-child-p node)
          (tr-mode-draw-a-node-draw-a-symbol 'handle)
        (tr-mode-draw-a-node-draw-a-symbol 'end-guide)))
    (tr-mode-draw-a-node-draw-a-symbol nodeflag)
    (setq line-start (point))
    (setq begin (point))
    (insert (tr-node-get-text node))
    (tr-move-overlay overlay begin (point))
    (put-text-property line-start (point) 'node-at-point node)
    (tr-node-put-property :overlay overlay node)))

;; clear display a node and all hist children
(defun tr-mode-clear-node(node)
  (let ((region (tr-node-get-region node))
        (buffer-read-only nil))
    (delete-region (car region) (cdr region))))
;; display node and all his children .
(defun tr-mode-draw-node(node)
  (let ((buffer-read-only nil)
        (node-list (tr-get-tree-node-list node)))
    (mapc 'tr-mode-draw-a-node node-list)))

;; this is a helper function which is used by tr-mode-node-at-point if node is
;; containded in the range (s e) then return t.
(defun tr-node-at-point-p (node s e)
  (let ((overlay (tr-node-get-property :overlay node)))
    (and overlay 
         (buffer-live-p (tr-overlay-buffer overlay))
         (<= s (tr-overlay-start overlay))
         (>= e (tr-overlay-end overlay)))))
;; return the node at a point POS. nil if there is no such node.
(defun tr-mode-node-at-point (&optional pos)
  (interactive)
  (setq pos (or pos (point)))
  (let ((s (tr-get-line-beginning-position pos))
        (e (1- (tr-get-line-end-position pos))))
    (let ((node (get-text-property e 'node-at-point)))
      (if (interactive-p)
          (if node
              (princ (format "%s is " (tr-node-get-text node)))
            (princ "none")))
      node)))

;; because the node is updated on demand, i.e. maybe a node has children, the
;; children doesn't exist until the node is updated. the node has a property
;; `updated', it is nil, even the node has no children, we can't tell whether it
;; is a leaf node or not until the node is updated. the node also has a property
;; 'update-function', it is a function accept one parameter, which is the NODE.
;; the function will be involved to update the node. it is reponsibility of the
;; update function to put the property `updated' of a node.
;; `tr-node-put-updated' is used to get the property `updated'.
;; `tr-node-get-update-function' and `tr-node-put-update-function' is used to
;; access the property `update-function'


(defun tr-mode-redraw ()
  (interactive)
  (let ((buffer-read-only nil)
        (node (tr-mode-node-at-point (point))))
    (erase-buffer)
    (tr-mode-draw-node tr-mode-root-node)
    (if node
        (goto-char (tr-overlay-start (tr-node-get-property :overlay node)))
      (goto-char 1))))

(defun tr-mode-expand-node-internal (node)
  (when node
    (save-excursion
      (tr-mode-clear-node node)
      (tr-node-put-expended t node)
      (funcall (tr-node-get-update-function node) node)
      (tr-mode-draw-node node))
    (forward-line)
    (tr-mode-adjust-pos)))
(defun tr-mode-contract-node-internal (node)
  (when node
    (save-excursion
      (tr-mode-clear-node node)
      (tr-node-put-expended nil node)
      (tr-mode-draw-node node))
    (forward-line)
    (tr-mode-adjust-pos)))

(defun tr-mode-expand-node()
  (interactive)
  (let ((node (tr-mode-node-at-point (point))))
    (tr-mode-expand-node-internal node)))
(defun tr-mode-contract-node()
  (interactive)
  (let* ((node (tr-mode-node-at-point (point))))
    (when node
      (tr-mode-contract-node-internal 
       (if (tr-node-get-expended node)
           node
         (tr-node-get-parent node))))))

(defun tr-mode-adjust-pos()
  "adjust the point to a beginning of a node."
  (let* ((node (tr-mode-node-at-point (point)))
         (overlay (and node (tr-node-get-property :overlay node)))
         (pos (and overlay (tr-overlay-start overlay))))
    (when pos 
      (goto-char pos)))) 
                          
(defun tr-mode-next (arg)
  (interactive "p")
  (forward-line (or arg 1))
  (tr-mode-adjust-pos))
(defun tr-mode-prev (arg)
  "Move to the previous ARGth line in a speedbar buffer."
  (interactive "p")
  (tr-mode-next (if arg (- arg) -1)))

(defun tr-mode-click-node ()
  (interactive)
  (let ((node (tr-mode-node-at-point)))
    (when node
      (funcall (tr-node-get-action-on-click node) node))))

(defun tr-mode-mouse-click(e)
  (interactive "e")
  (let* ((start-event (event-start e))
         (pos (posn-point start-event))
         (window (posn-window start-event))
         (buffer (window-buffer window))
         (current-buffer (current-buffer)))
    (if (not (eq current-buffer buffer))
        (progn
          ;; jump to the window
          (select-window window)
          (with-current-buffer buffer
            (goto-char pos)
            (tr-mode-adjust-pos)))
      (let* ((node (tr-mode-node-at-point pos))
             (parent (and node (tr-node-get-parent node)))
             (overlay (and node (tr-node-get-property :overlay node)))
             (start (and overlay (tr-overlay-start overlay)))
             (end (and overlay(tr-overlay-end overlay))))
        (when node
          (goto-char pos)
          (if (and (>= pos start) (< pos end))
              (tr-mode-click-node)
            ;; not click on the label of a tree node. expand or contract a node.
            ;; if not udpated, update it first.
            (if (not (tr-node-get-updated node))
                (progn 
                  (funcall (tr-node-get-update-function node) node)
                  (tr-node-put-expended nil node)))
            (if (tr-node-get-expended node)
                (tr-mode-contract-node-internal node)
              (tr-mode-expand-node-internal node))))))))
(defun tr-get-line-beginning-position (pos)
  (save-excursion
    (goto-char pos)
    (line-beginning-position)))
(defun tr-get-line-end-position (pos)
  (save-excursion
    (goto-char pos)
    (line-end-position)))
;; get the region for displaying the node and all his children.
(defun tr-node-get-region (node)
  (let ((parent (tr-node-get-parent node))
        (brother (tr-node-get-next-brother node)))
    (if (or (not parent) (not brother))
        (let ((last-child (car (last (tr-get-tree-node-list node)))))
          (cons 
           (max 1 (1- (tr-get-line-beginning-position (tr-overlay-start (tr-node-get-property
                                                                         :overlay node)))))
           (tr-get-line-end-position (tr-overlay-start (tr-node-get-property
                                                        :overlay (or last-child node))))))
      (cons 
       (1- (tr-get-line-beginning-position (tr-overlay-start (tr-node-get-property
                                                              :overlay node))))
       (1- (tr-get-line-beginning-position (tr-overlay-start (tr-node-get-property
                                                              :overlay brother))))))))



;; graphics supply

(defconst tree-buffer-images-can-be-used
  (and (or (fboundp 'defimage)
           (fboundp 'make-image-specifier))
       (if (fboundp 'display-images-p)
           (display-images-p)
         window-system)))

(provide 'tree)
;;; tree.el ends here
