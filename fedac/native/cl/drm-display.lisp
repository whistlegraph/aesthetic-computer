;;; DRM display — Linux DRM/KMS for direct framebuffer access
;;; Port of fedac/native/src/drm-display.c

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
  (maps (make-array 2 :initial-element nil))
  (buf-sizes (make-array 2 :element-type 'fixnum :initial-element 0))
  (pitches (make-array 2 :element-type 'fixnum :initial-element 0))
  (back 0 :type fixnum)
  ;; Mode info — kept as foreign pointer for drmModeSetCrtc
  (mode-info (cffi:null-pointer) :type cffi:foreign-pointer)
  (is-fbdev nil :type boolean))

;;; ── Helper: read struct fields from libdrm opaque pointers ──
;;; drmModeRes fields (offsets determined by libdrm ABI)

(defun res-count-connectors (res)
  (cffi:mem-ref (cffi:inc-pointer res 24) :int32))

(defun res-connectors (res)
  (cffi:mem-ref (cffi:inc-pointer res 16) :pointer))

(defun res-count-crtcs (res)
  (cffi:mem-ref (cffi:inc-pointer res 40) :int32))

(defun res-crtcs (res)
  (cffi:mem-ref (cffi:inc-pointer res 32) :pointer))

;;; drmModeConnector fields
(defun conn-status (conn)
  (cffi:mem-ref (cffi:inc-pointer conn 4) :uint32))

(defun conn-connector-id (conn)
  (cffi:mem-ref conn :uint32))

(defun conn-encoder-id (conn)
  (cffi:mem-ref (cffi:inc-pointer conn 8) :uint32))

(defun conn-count-modes (conn)
  (cffi:mem-ref (cffi:inc-pointer conn 36) :int32))

(defun conn-modes (conn)
  "Pointer to array of drmModeModeInfo structs."
  (cffi:mem-ref (cffi:inc-pointer conn 28) :pointer))

;;; drmModeEncoder fields
(defun enc-crtc-id (enc)
  (cffi:mem-ref (cffi:inc-pointer enc 4) :uint32))

;;; drmModeModeInfo: hdisplay at offset 8, vdisplay at offset 10
(defun mode-hdisplay (mode-ptr)
  (cffi:mem-ref (cffi:inc-pointer mode-ptr 8) :uint16))

(defun mode-vdisplay (mode-ptr)
  (cffi:mem-ref (cffi:inc-pointer mode-ptr 10) :uint16))

(defconstant +mode-info-size+ 68 "sizeof(drmModeModeInfo)")

;;; ── fbdev fallback ──

(defun try-fbdev ()
  "Try /dev/fb0 as fallback. Returns display or nil."
  (let ((fd (ac-native.syscalls:sys-open "/dev/fb0"
              (logior ac-native.syscalls:+o-rdwr+) 0)))
    (when (>= fd 0)
      (ac-native.util:ac-log "[drm] fbdev fallback: /dev/fb0~%")
      ;; Read screen info via ioctl FBIOGET_VSCREENINFO (0x4600)
      (cffi:with-foreign-object (vinfo :uint8 160)
        (cffi:foreign-funcall "memset" :pointer vinfo :int 0 :unsigned-long 160 :pointer)
        (let ((ret (ac-native.syscalls:sys-ioctl fd #x4600 :pointer vinfo)))
          (if (zerop ret)
              (let ((w (cffi:mem-ref vinfo :uint32))        ; xres at offset 0
                    (h (cffi:mem-ref (cffi:inc-pointer vinfo 4) :uint32))) ; yres at offset 4
                (ac-native.util:ac-log "[drm] fbdev: ~Dx~D~%" w h)
                ;; mmap the framebuffer
                (cffi:with-foreign-object (finfo :uint8 88)
                  (cffi:foreign-funcall "memset" :pointer finfo :int 0 :unsigned-long 88 :pointer)
                  (ac-native.syscalls:sys-ioctl fd #x4602 :pointer finfo) ; FBIOGET_FSCREENINFO
                  (let* ((smem-len (cffi:mem-ref (cffi:inc-pointer finfo 32) :uint32))
                         (line-len (cffi:mem-ref (cffi:inc-pointer finfo 48) :uint32))
                         (map (ac-native.syscalls:sys-mmap
                               (cffi:null-pointer) smem-len
                               (logior ac-native.syscalls:+prot-read+
                                       ac-native.syscalls:+prot-write+)
                               ac-native.syscalls:+map-shared+ fd 0)))
                    (let ((disp (make-display :fd fd :width w :height h :is-fbdev t)))
                      (setf (aref (display-maps disp) 0) map)
                      (setf (aref (display-pitches disp) 0) (floor line-len 4))
                      (setf (aref (display-buf-sizes disp) 0) smem-len)
                      disp))))
              (progn
                (ac-native.syscalls:sys-close fd)
                nil)))))))

;;; ── DRM init ──

(defun try-open-drm ()
  "Try /dev/dri/card0..2. Return fd or -1."
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
      (when (zerop ret)
        (values
         (cffi:foreign-slot-value req '(:struct drm-mode-create-dumb) 'handle)
         (cffi:foreign-slot-value req '(:struct drm-mode-create-dumb) 'pitch)
         (cffi:foreign-slot-value req '(:struct drm-mode-create-dumb) 'size))))))

(defun map-dumb-buffer (fd handle size)
  "Mmap a dumb buffer. Returns foreign pointer or null."
  (cffi:with-foreign-object (req '(:struct drm-mode-map-dumb))
    (cffi:foreign-funcall "memset" :pointer req :int 0
                          :unsigned-long (cffi:foreign-type-size '(:struct drm-mode-map-dumb))
                          :pointer)
    (setf (cffi:foreign-slot-value req '(:struct drm-mode-map-dumb) 'handle) handle)
    (let ((ret (ac-native.syscalls:sys-ioctl fd +drm-ioctl-mode-map-dumb+ :pointer req)))
      (when (zerop ret)
        (let ((offset (cffi:foreign-slot-value req '(:struct drm-mode-map-dumb) 'offset)))
          (ac-native.syscalls:sys-mmap
           (cffi:null-pointer) size
           (logior ac-native.syscalls:+prot-read+ ac-native.syscalls:+prot-write+)
           ac-native.syscalls:+map-shared+
           fd offset))))))

(defun drm-init ()
  "Initialize DRM display. Returns a display struct or nil."
  (let ((fd (try-open-drm)))
    (when (< fd 0)
      (ac-native.util:ac-log "[drm] no card — trying fbdev~%")
      (return-from drm-init (try-fbdev)))

    (let ((res (drm-mode-get-resources fd)))
      (when (cffi:null-pointer-p res)
        (ac-native.util:ac-log "[drm] getResources failed — trying fbdev~%")
        (ac-native.syscalls:sys-close fd)
        (return-from drm-init (try-fbdev)))

      ;; Find first connected connector with modes
      (let ((n-conn (res-count-connectors res))
            (conn-ids (res-connectors res))
            (found-conn nil)
            (found-mode nil)
            (found-crtc 0))

        (loop for i below n-conn
              for cid = (cffi:mem-aref conn-ids :uint32 i)
              for conn = (drm-mode-get-connector fd cid)
              unless (cffi:null-pointer-p conn) do
                (when (and (= (conn-status conn) +drm-mode-connected+)
                           (> (conn-count-modes conn) 0))
                  ;; Use first mode (preferred/native resolution)
                  (setf found-conn conn)
                  (setf found-mode (conn-modes conn))
                  ;; Get CRTC from encoder
                  (let ((enc-id (conn-encoder-id conn)))
                    (when (> enc-id 0)
                      (let ((enc (drm-mode-get-encoder fd enc-id)))
                        (unless (cffi:null-pointer-p enc)
                          (setf found-crtc (enc-crtc-id enc))
                          (drm-mode-free-encoder enc)))))
                  ;; Fallback: use first CRTC
                  (when (zerop found-crtc)
                    (let ((n-crtcs (res-count-crtcs res))
                          (crtc-ids (res-crtcs res)))
                      (when (> n-crtcs 0)
                        (setf found-crtc (cffi:mem-aref crtc-ids :uint32 0)))))
                  (return))
              unless (cffi:null-pointer-p conn) do
                (drm-mode-free-connector conn))

        (drm-mode-free-resources res)

        (unless found-conn
          (ac-native.util:ac-log "[drm] no connected display — trying fbdev~%")
          (ac-native.syscalls:sys-close fd)
          (return-from drm-init (try-fbdev)))

        (let* ((w (mode-hdisplay found-mode))
               (h (mode-vdisplay found-mode))
               (conn-id (conn-connector-id found-conn)))

          (ac-native.util:ac-log "[drm] display: ~Dx~D crtc=~D conn=~D~%"
                                  w h found-crtc conn-id)

          ;; Save mode info for SetCrtc
          (let ((mode-copy (cffi:foreign-alloc :uint8 :count +mode-info-size+)))
            (cffi:foreign-funcall "memcpy" :pointer mode-copy :pointer found-mode
                                  :unsigned-long +mode-info-size+ :pointer)

            ;; Create double-buffered dumb buffers
            (let ((disp (make-display :fd fd :width w :height h
                                      :crtc-id found-crtc
                                      :connector-id conn-id
                                      :mode-info mode-copy)))

              (dotimes (i 2)
                (multiple-value-bind (handle pitch size) (create-dumb-buffer fd w h)
                  (unless handle
                    (ac-native.util:ac-log "[drm] create dumb buffer ~D failed~%" i)
                    (ac-native.syscalls:sys-close fd)
                    (return-from drm-init nil))
                  (setf (aref (display-bo-handles disp) i) handle)
                  (setf (aref (display-pitches disp) i) (floor pitch 4))
                  (setf (aref (display-buf-sizes disp) i) size)
                  ;; Add framebuffer
                  (cffi:with-foreign-object (fb-id :uint32)
                    (drm-mode-add-fb fd w h 24 32 pitch handle fb-id)
                    (setf (aref (display-fb-ids disp) i) (cffi:mem-ref fb-id :uint32)))
                  ;; Map buffer
                  (let ((map (map-dumb-buffer fd handle size)))
                    (setf (aref (display-maps disp) i) map))))

              ;; Set CRTC to first buffer
              (cffi:with-foreign-object (conn-ptr :uint32)
                (setf (cffi:mem-ref conn-ptr :uint32) conn-id)
                (drm-mode-set-crtc fd found-crtc
                                   (aref (display-fb-ids disp) 0)
                                   0 0 conn-ptr 1 mode-copy))

              (drm-mode-free-connector found-conn)
              (ac-native.util:ac-log "[drm] initialized OK~%")
              disp)))))))

(defun display-width (d) (display-width d))
(defun display-height (d) (display-height d))

(defun drm-present (display screen scale)
  "Copy the render framebuffer to the DRM back buffer and flip."
  (when (and display screen)
    (let* ((back (display-back display))
           (dst (aref (display-maps display) back))
           (dst-w (display-width display))
           (dst-h (display-height display))
           (dst-stride (aref (display-pitches display) back)))
      (when dst
        ;; Scale render buffer to display
        (ac-native.framebuffer:fb-copy-scaled screen dst dst-w dst-h dst-stride scale)
        (if (display-is-fbdev display)
            ;; fbdev: just wrote to mmap'd buffer, it's live
            nil
            ;; DRM: page flip
            (progn
              (drm-mode-page-flip (display-fd display)
                                  (display-crtc-id display)
                                  (aref (display-fb-ids display) back)
                                  0 (cffi:null-pointer))
              (setf (display-back display) (- 1 back))))))))

(defun drm-destroy (display)
  "Clean up DRM resources."
  (when display
    ;; Unmap buffers
    (dotimes (i 2)
      (let ((map (aref (display-maps display) i))
            (size (aref (display-buf-sizes display) i)))
        (when (and map (not (cffi:null-pointer-p map)) (> size 0))
          (ac-native.syscalls:sys-munmap map size))))
    ;; Free mode info
    (unless (cffi:null-pointer-p (display-mode-info display))
      (cffi:foreign-free (display-mode-info display)))
    ;; Close fd
    (when (>= (display-fd display) 0)
      (ac-native.syscalls:sys-close (display-fd display)))
    (ac-native.util:ac-log "[drm] closed~%")))
