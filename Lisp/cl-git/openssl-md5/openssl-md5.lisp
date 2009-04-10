(cffi:defcstruct MD5_CTX
	(A :unsigned-int)
	(B :unsigned-int)
	(C :unsigned-int)
	(D :unsigned-int)
	(Nl :unsigned-int)
	(Nh :unsigned-int)
	(data :unsigned-int :count 16)
	(num :unsigned-int))

(cffi:defcfun ("MD5_Init" MD5_Init) :int
  (c :pointer))

(cffi:defcfun ("MD5_Update" MD5_Update) :int
  (c :pointer)
  (data :pointer)
  (len :unsigned-int))

(cffi:defcfun ("MD5_Final" MD5_Final) :int
  (md :pointer)
  (c :pointer))

(cffi:defcfun ("MD5" MD5) :pointer
  (d :pointer)
  (n :pointer)
  (md :pointer))

(cffi:defcfun ("MD5_Transform" MD5_Transform) :void
  (c :pointer)
  (b :pointer))

