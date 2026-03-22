;;; DRM display — Linux DRM/KMS for direct framebuffer access

(in-package :ac-native.drm)

(defstruct display
  "DRM display state."
  (fd -1 :type fixnum)
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (crtc-id 0 :type (unsigned-byte 32))
  (connector-id 0 :type (unsigned-byte 32))
  ;; Double-buffered dumb buffers
  (fb-ids (make-array 2 :element-type '(unsigned-byte 32) :initial-element 0))
  (bo-handles (make-array 2 :element-type '(unsigned-byte 32) :initial-element 0))
  (maps (make-array 2 :initial-element (cffi:null-pointer)))
  (buf-sizes (make-array 2 :element-type 'fixnum :initial-element 0))
  (pitches (make-array 2 :element-type 'fixnum :initial-element 0))
  (back 0 :type fixnum)  ; which buffer is back (0 or 1)
  (mode-ptr (cffi:null-pointer) :type cffi:foreign-pointer))

(defun try-open-drm ()
  "Try /dev/dri/card0, card1. Return fd or -1."
  (loop for card in '("/dev/dri/card0" "/dev/dri/card1" "/dev/dri/card2")
        for fd = (ac-native.syscalls:sys-open card
                   (logior ac-native.syscalls:+o-rdwr+
                           ac-native.syscalls:+o-cloexec+)
                   0)
        when (>= fd 0) do
          (ac-native.util:ac-log "[drm] opened ~A (fd ~D)~%" card fd)
          (return fd)
        finally (return -1)))

(defun create-dumb-buffer (fd width height)
  "Create a DRM dumb buffer. Returns (values handle pitch size) or nil."
  (cffi:with-foreign-object (req '(:struct drm-mode-create-dumb))
    (cffi:foreign-funcall "memset" :pointer req :int 0
                          :unsigned-long (cffi:foreign-type-size '(:struct drm-mode-create-dumb))
                          :pointer)
    (setf (cffi:foreign-slot-value req '(:struct drm-mode-create-dumb) 'height) height)
    (setf (cffi:foreign-slot-value req '(:struct drm-mode-create-dumb) 'width) width)
    (setf (cffi:foreign-slot-value req '(:struct drm-mode-create-dumb) 'bpp) 32)
    (let ((ret (ac-native.syscalls:sys-ioctl fd +drm-ioctl-mode-create-dumb+ :pointer req)))
      (if (zerop ret)
          (values
           (cffi:foreign-slot-value req '(:struct drm-mode-create-dumb) 'handle)
           (cffi:foreign-slot-value req '(:struct drm-mode-create-dumb) 'pitch)
           (cffi:foreign-slot-value req '(:struct drm-mode-create-dumb) 'size))
          nil))))

(defun map-dumb-buffer (fd handle size)
  "Mmap a dumb buffer. Returns foreign pointer or null."
  (cffi:with-foreign-object (req '(:struct drm-mode-map-dumb))
    (cffi:foreign-funcall "memset" :pointer req :int 0
                          :unsigned-long (cffi:foreign-type-size '(:struct drm-mode-map-dumb))
                          :pointer)
    (setf (cffi:foreign-slot-value req '(:struct drm-mode-map-dumb) 'handle) handle)
    (let ((ret (ac-native.syscalls:sys-ioctl fd +drm-ioctl-mode-map-dumb+ :pointer req)))
      (if (zerop ret)
          (let ((offset (cffi:foreign-slot-value req '(:struct drm-mode-map-dumb) 'offset)))
            (ac-native.syscalls:sys-mmap
             (cffi:null-pointer) size
             (logior ac-native.syscalls:+prot-read+ ac-native.syscalls:+prot-write+)
             ac-native.syscalls:+map-shared+
             fd offset))
          (cffi:null-pointer)))))

(defun drm-init ()
  "Initialize DRM display. Returns a display struct or nil."
  (let ((fd (try-open-drm)))
    (when (< fd 0)
      (ac-native.util:ac-log "[drm] no DRM device found~%")
      (return-from drm-init nil))

    ;; Get resources
    (let ((res (drm-mode-get-resources fd)))
      (when (cffi:null-pointer-p res)
        (ac-native.util:ac-log "[drm] drmModeGetResources failed~%")
        (ac-native.syscalls:sys-close fd)
        (return-from drm-init nil))

      ;; Find first connected connector
      ;; TODO: iterate connectors, find encoder, find CRTC
      ;; For now, return a placeholder
      (ac-native.util:ac-log "[drm] resources obtained, setting up display...~%")

      ;; Placeholder — real implementation needs connector/encoder/CRTC setup
      ;; This is the most hardware-specific part
      (drm-mode-free-resources res)

      (let ((disp (make-display :fd fd :width 1366 :height 768)))
        ;; TODO: actually detect resolution from connector modes
        disp))))

(defun drm-present (display screen scale)
  "Copy the render framebuffer to the DRM back buffer and flip."
  (declare (ignore display screen scale))
  ;; TODO: implement fb-copy-scaled to mmap'd buffer + page flip
  )

(defun drm-destroy (display)
  "Clean up DRM resources."
  (when display
    (when (>= (display-fd display) 0)
      (ac-native.syscalls:sys-close (display-fd display))
      (ac-native.util:ac-log "[drm] closed~%"))))
