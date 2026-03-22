;;; Framebuffer — foreign-memory pixel buffer for DRM/fbdev

(in-package :ac-native.framebuffer)

(defstruct framebuffer
  "A 2D pixel buffer backed by CFFI foreign memory (for DRM mmap compat)."
  (pixels (cffi:null-pointer) :type cffi:foreign-pointer)
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (stride 0 :type fixnum)  ; in pixels (= width for our buffers)
  (owned t :type boolean))  ; if t, we allocated pixels and must free

(defun fb-create (width height)
  "Allocate a new framebuffer with WIDTH x HEIGHT pixels (ARGB32)."
  (let* ((stride width)
         (nbytes (* stride height 4))
         (ptr (cffi:foreign-alloc :uint32 :count (* stride height) :initial-element 0)))
    (make-framebuffer :pixels ptr :width width :height height
                      :stride stride :owned t)))

(defun fb-destroy (fb)
  "Free the framebuffer's pixel memory (if we own it)."
  (when (and fb (framebuffer-owned fb)
               (not (cffi:null-pointer-p (framebuffer-pixels fb))))
    (cffi:foreign-free (framebuffer-pixels fb))
    (setf (framebuffer-pixels fb) (cffi:null-pointer))))

(declaim (inline fb-pixel-offset))
(defun fb-pixel-offset (fb x y)
  (declare (optimize (speed 3) (safety 0))
           (type fixnum x y)
           (type framebuffer fb))
  (the fixnum (+ x (* y (framebuffer-stride fb)))))

(defun fb-clear (fb color-u32)
  "Fill the entire framebuffer with a packed ARGB32 color."
  (declare (optimize (speed 3) (safety 0))
           (type (unsigned-byte 32) color-u32))
  (let* ((n (* (framebuffer-stride fb) (framebuffer-height fb)))
         (ptr (framebuffer-pixels fb)))
    (dotimes (i n)
      (setf (cffi:mem-aref ptr :uint32 i) color-u32))))

(declaim (inline fb-put-pixel))
(defun fb-put-pixel (fb x y color-u32)
  "Write a pixel (no blending, no bounds check in optimized mode)."
  (declare (optimize (speed 3) (safety 0))
           (type fixnum x y)
           (type (unsigned-byte 32) color-u32)
           (type framebuffer fb))
  (when (and (>= x 0) (< x (framebuffer-width fb))
             (>= y 0) (< y (framebuffer-height fb)))
    (setf (cffi:mem-aref (framebuffer-pixels fb) :uint32
                         (fb-pixel-offset fb x y))
          color-u32)))

(defun fb-blend-pixel (fb x y src-color)
  "Alpha-blend a color onto the framebuffer at (x, y)."
  (declare (optimize (speed 3) (safety 0))
           (type fixnum x y))
  (when (and (>= x 0) (< x (framebuffer-width fb))
             (>= y 0) (< y (framebuffer-height fb)))
    (let* ((off (fb-pixel-offset fb x y))
           (dst-u32 (cffi:mem-aref (framebuffer-pixels fb) :uint32 off))
           (dst (make-color :r (ldb (byte 8 16) dst-u32)
                            :g (ldb (byte 8 8) dst-u32)
                            :b (ldb (byte 8 0) dst-u32)
                            :a 255)))
      (setf (cffi:mem-aref (framebuffer-pixels fb) :uint32 off)
            (color-blend src-color dst)))))

(defun fb-copy-scaled (src dst-ptr dst-w dst-h dst-stride scale)
  "Copy SRC framebuffer to a foreign memory region at DST-PTR, scaling up by SCALE.
   Used by DRM present to blit the small render buffer to the display."
  (declare (optimize (speed 3) (safety 0))
           (type fixnum dst-w dst-h dst-stride scale)
           (type framebuffer src))
  (let ((sw (framebuffer-width src))
        (sh (framebuffer-height src))
        (sp (framebuffer-pixels src)))
    (dotimes (sy sh)
      (dotimes (sx sw)
        (let ((pixel (cffi:mem-aref sp :uint32 (+ sx (* sy sw)))))
          ;; Replicate pixel into scale x scale block
          (dotimes (dy scale)
            (let ((oy (* (+ (* (+ (* sy scale) dy) 1) 0) 1))) ; row offset placeholder
              (declare (ignore oy))
              (let ((row (+ (* sy scale) dy)))
                (when (< row dst-h)
                  (dotimes (dx scale)
                    (let ((col (+ (* sx scale) dx)))
                      (when (< col dst-w)
                        (setf (cffi:mem-aref dst-ptr :uint32
                                             (+ col (* row dst-stride)))
                              pixel)))))))))))))
