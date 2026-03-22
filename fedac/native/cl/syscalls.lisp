;;; Linux syscall bindings via CFFI

(in-package :ac-native.syscalls)

;;; File operations
(cffi:defcfun ("open" sys-open) :int
  (pathname :string) (flags :int) (mode :int))

(cffi:defcfun ("close" sys-close) :int (fd :int))

(cffi:defcfun ("read" sys-read) :long
  (fd :int) (buf :pointer) (count :unsigned-long))

(cffi:defcfun ("write" sys-write) :long
  (fd :int) (buf :pointer) (count :unsigned-long))

(cffi:defcfun ("ioctl" sys-ioctl) :int
  (fd :int) (request :unsigned-long) &rest)

;;; Memory mapping
(cffi:defcfun ("mmap" sys-mmap) :pointer
  (addr :pointer) (length :unsigned-long) (prot :int)
  (flags :int) (fd :int) (offset :long))

(cffi:defcfun ("munmap" sys-munmap) :int
  (addr :pointer) (length :unsigned-long))

;;; Filesystem
(cffi:defcfun ("mount" %mount) :int
  (source :string) (target :string) (fstype :string)
  (flags :unsigned-long) (data :pointer))

(defun sys-mount (source target fstype &optional (flags 0))
  (%mount source target fstype flags (cffi:null-pointer)))

(cffi:defcfun ("umount" sys-umount) :int (target :string))

;;; System control
(cffi:defcfun ("reboot" %reboot) :int (cmd :int))

(defconstant +reboot-power-off+ #x4321FEDC)
(defconstant +reboot-restart+ #x01234567)

(defun sys-poweroff ()
  ;; sync first
  (cffi:foreign-funcall "sync" :void)
  (%reboot +reboot-power-off+))

(defun sys-reboot ()
  (cffi:foreign-funcall "sync" :void)
  (%reboot +reboot-restart+))

;;; Process
(cffi:defcfun ("getpid" sys-getpid) :int)
(cffi:defcfun ("fork" sys-fork) :int)
(cffi:defcfun ("execvp" sys-execvp) :int
  (file :string) (argv :pointer))
(cffi:defcfun ("waitpid" sys-waitpid) :int
  (pid :int) (status :pointer) (options :int))

;;; Constants
(defconstant +o-rdonly+ 0)
(defconstant +o-wronly+ 1)
(defconstant +o-rdwr+ 2)
(defconstant +o-nonblock+ #o4000)
(defconstant +o-cloexec+ #o2000000)

(defconstant +prot-read+ 1)
(defconstant +prot-write+ 2)
(defconstant +map-shared+ 1)

(defconstant +clock-monotonic+ 1)
