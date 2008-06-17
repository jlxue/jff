;;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: MD5-CFFI; Base: 10 -*-
;;;;
;;;; Copyright (c) 2008, Liu Yubao.  All rights reserved.
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

(in-package :md5-cffi)


(define-foreign-library libeay32 (t (:default "libeay32")))
(use-foreign-library libeay32)


(defcstruct md5-state
  (A :unsigned-int)
  (B :unsigned-int)
  (C :unsigned-int)
  (D :unsigned-int)
  (Nl :unsigned-int)
  (Nh :unsigned-int)
  (data :unsigned-int :count 16)
  (num :unsigned-int))


(defcfun ("MD5_Init" md5-init) :int
  (ctx :pointer))


(defcfun ("MD5_Update" md5-update) :int
  (ctx :pointer)
  (data :pointer)
  (len :unsigned-int))


(defcfun ("MD5_Final" md5-final) :int
  (md :pointer)
  (ctx :pointer))


(defcfun ("MD5" md5) :pointer
  (data :pointer)
  (n :unsigned-int)
  (md :pointer))


(defcfun ("MD5-Transform" md5-transform) :void
  (ctx :pointer)
  (b :pointer))


(defun make-md5-state ()
  (let ((state (foreign-alloc 'md5-state)))
    (md5-init state)
    state))


(defun update-md5-state (state sequence &key (start 0) (end (length sequence)))
  "Update the given md5-state from sequence, which is either a
simple-string or a simple-array with element-type (unsigned-byte 8),
bounded by start and end, which must be numeric bounding-indices."
  (let ((buffer (subseq sequence start end)))
    (with-foreign-string (pbuffer buffer)
      (md5-update state pbuffer (length buffer)))))


(defun finalize-md5-state (state)
  "If the given md5-state has not already been finalized, finalize it,
by processing any remaining input in its buffer, with suitable padding
and appended bit-length, as specified by the MD5 standard.

The resulting MD5 message-digest is returned as an array of sixteen
(unsigned-byte 8) values.  Calling `update-md5-state' after a call to
`finalize-md5-state' results in unspecified behaviour."
  (let ((digest (make-shareable-byte-vector 16)))
    (with-pointer-to-vector-data (pdigest digest)
      (md5-final pdigest state)
      (foreign-free state)
      digest)))


(defun md5sum-sequence (sequence &key (start 0) (end (length sequence)))
  "Calculate the MD5 message-digest of data in sequence.  On CMU CL
this works for all sequences whose element-type is supported by the
underlying MD5 routines, on other implementations it only works for 1d
simple-arrays with such element types."
  (let ((buffer (subseq sequence start end))
        (digest (make-shareable-byte-vector 16)))
    (with-foreign-object (state 'md5-state)
      (with-foreign-string (pbuffer buffer)
        (md5-init state)
        (md5-update state pbuffer (length buffer))
        (with-pointer-to-vector-data (pdigest digest)
          (md5-final pdigest state)
          digest)))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +buffer-size+ (* 128 1024)
    "Size of internal buffer to use for md5sum-stream and md5sum-file
operations.  This should be a multiple of 64, the MD5 block size."))

(deftype buffer-index () `(integer 0 ,+buffer-size+))


(defun md5sum-stream (stream)
  "Calculate an MD5 message-digest of the contents of stream.  Its
element-type has to be either (unsigned-byte 8) or character."
  (if (not (or (equal (stream-element-type stream) '(unsigned-byte 8))
               (equal (stream-element-type stream) '(character))))
    (error "Unsupported stream element-type ~S for stream ~S."
           (stream-element-type stream) stream))
  (let ((buffer (make-shareable-byte-vector +buffer-size+))
        (digest (make-shareable-byte-vector 16)))
    (with-foreign-object (state 'md5-state)
      (md5-init state)
      (with-pointer-to-vector-data (pbuffer buffer)
        (loop for bytes of-type buffer-index = (read-sequence buffer stream)
          do (md5-update state pbuffer bytes)
          until (< bytes +buffer-size+)
          finally
          (with-pointer-to-vector-data (pdigest digest)
            (md5-final pdigest state)
            (return digest)))))))


(defun md5sum-file (pathname)
  "Calculate the MD5 message-digest of the file specified by pathname."
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (md5sum-stream stream)))


;;;; vi: ft=lisp

