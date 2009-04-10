(cffi:define-foreign-library libeay32 (t (:default "libeay32")))
(cffi:use-foreign-library libeay32)

(defun md5sum-file (path)
  (with-open-file (in path :element-type 'unsigned-byte)
    (when in
      (let ((buffer (make-shareable-byte-vector (* 128 1024)))
            (digest (make-shareable-byte-vector 16)))
        (with-foreign-object (ctx 'md5_ctx)
          (md5_init ctx)
          (with-pointer-to-vector-data (pbuffer buffer)
            (loop for n = (read-sequence buffer in)
                  while (> n 0) do
                  (md5_update ctx pbuffer n)))
          (with-pointer-to-vector-data (pdigest digest)
            (md5_final pdigest ctx)
              digest))))))
