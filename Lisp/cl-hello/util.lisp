;;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;;
;;;; Copyright (c) 2008-2009, YOUR NAME.  All rights reserved.
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;;
;;;;   * Redistributions of source code must retain the above copyright
;;;;     notice, this list of conditions and the following disclaimer.
;;;;
;;;;   * Redistributions in binary form must reproduce the above
;;;;     copyright notice, this list of conditions and the following
;;;;     disclaimer in the documentation and/or other materials
;;;;     provided with the distribution.
;;;;
;;;; THIS  SOFTWARE   IS  PROVIDED   BY  THE  COPYRIGHT   HOLDERS  AND
;;;; CONTRIBUTORS  "AS  IS" AND  ANY  EXPRESS  OR IMPLIED  WARRANTIES,
;;;; INCLUDING,  BUT  NOT  LIMITED   TO,  THE  IMPLIED  WARRANTIES  OF
;;;; MERCHANTABILITY  AND   FITNESS  FOR  A   PARTICULAR  PURPOSE  ARE
;;;; DISCLAIMED.   IN   NO  EVENT  SHALL  THE   COPYRIGHT  HOLDERS  OR
;;;; CONTRIBUTORS  BE  LIABLE FOR  ANY  DIRECT, INDIRECT,  INCIDENTAL,
;;;; SPECIAL, EXEMPLARY, OR  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;;; LIMITED TO, PROCUREMENT OF  SUBSTITUTE GOODS OR SERVICES; LOSS OF
;;;; USE, DATA,  OR PROFITS; OR BUSINESS  INTERRUPTION) HOWEVER CAUSED
;;;; AND  ON ANY  THEORY  OF LIABILITY,  WHETHER  IN CONTRACT,  STRICT
;;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
;;;; ANY WAY OUT  OF THE USE OF THIS SOFTWARE, EVEN  IF ADVISED OF THE
;;;; POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-user)

(defun version-string-to-list (ver)
  (let ((p0 0)
        (p 0)
        (len (length ver))
        (lst nil))

    (loop
      (if (and (< p len) (setf p (position #\. ver :start p)))
        (progn
          (push (subseq ver p0 p) lst)
          (setf p0 (incf p)))
        (return)))

    (if (< p0 len)
      (push (subseq ver p0) lst))

    (mapcar #'(lambda (str) (with-input-from-string (s str) (read s)))
            (nreverse lst))))


(defun version-compare (ver1 ver2)
  (let ((v1 (car ver1))
        (v2 (car ver2)))
    (if (null v1)
      (if v2 -1 0)
      (if (null v2)
        1
        (cond ((< v1 v2) -1)
              ((= v1 v2) (version-compare (cdr ver1) (cdr ver2)))
              (t 1))))))


;;;; vi: ft=lisp

