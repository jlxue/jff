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

(load "load.lisp")
(load "util.lisp")

(defun main0 ()
  (hello:main ext:*args*)
  (quit))

(if (< 0 (version-compare (version-string-to-list (lisp-implementation-version))
                           (version-string-to-list "2.39")))
  (ext:saveinitmem "cl-hello"
                   :quiet t
                   :init-function 'main0
                   :script nil
                   :executable t)
  (ext:saveinitmem "cl-hello"
                   :quiet t
                   :init-function 'main0
                   :executable t))


(format t "~%~%~{~a~%~}~%"
        '("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
          "You can run cl-hello with command './cl-hello Joe John',"
          "for CLisp lower than 2.39 use './cl-hello -- Joe John'."
          "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"))

(ext:exit)


;;;; vi: ft=lisp

