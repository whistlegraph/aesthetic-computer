;;; DRM/KMS constants and struct definitions

(in-package :ac-native.drm)

;;; DRM ioctl numbers (from <drm.h> and <drm_mode.h>)
(defconstant +drm-ioctl-base+ #x64) ; 'd'

;;; libdrm function bindings (higher-level than raw ioctls)
(cffi:define-foreign-library libdrm
  (:unix "libdrm.so.2"))

(cffi:use-foreign-library libdrm)

;; drmModeRes *drmModeGetResources(int fd)
(cffi:defcfun ("drmModeGetResources" drm-mode-get-resources) :pointer
  (fd :int))

;; void drmModeFreeResources(drmModeRes *ptr)
(cffi:defcfun ("drmModeFreeResources" drm-mode-free-resources) :void
  (ptr :pointer))

;; drmModeConnector *drmModeGetConnector(int fd, uint32_t id)
(cffi:defcfun ("drmModeGetConnector" drm-mode-get-connector) :pointer
  (fd :int) (id :uint32))

;; void drmModeFreeConnector(drmModeConnector *ptr)
(cffi:defcfun ("drmModeFreeConnector" drm-mode-free-connector) :void
  (ptr :pointer))

;; drmModeEncoder *drmModeGetEncoder(int fd, uint32_t id)
(cffi:defcfun ("drmModeGetEncoder" drm-mode-get-encoder) :pointer
  (fd :int) (id :uint32))

;; void drmModeFreeEncoder(drmModeEncoder *ptr)
(cffi:defcfun ("drmModeFreeEncoder" drm-mode-free-encoder) :void
  (ptr :pointer))

;; int drmModeSetCrtc(int fd, uint32_t crtc_id, uint32_t fb_id,
;;                    uint32_t x, uint32_t y, uint32_t *connectors,
;;                    int count, drmModeModeInfo *mode)
(cffi:defcfun ("drmModeSetCrtc" drm-mode-set-crtc) :int
  (fd :int) (crtc-id :uint32) (fb-id :uint32)
  (x :uint32) (y :uint32) (connectors :pointer)
  (count :int) (mode :pointer))

;; int drmModeAddFB(int fd, uint32_t width, uint32_t height, uint8_t depth,
;;                  uint8_t bpp, uint32_t pitch, uint32_t bo_handle, uint32_t *buf_id)
(cffi:defcfun ("drmModeAddFB" drm-mode-add-fb) :int
  (fd :int) (width :uint32) (height :uint32) (depth :uint8)
  (bpp :uint8) (pitch :uint32) (bo-handle :uint32) (buf-id :pointer))

;; int drmModeRmFB(int fd, uint32_t fb_id)
(cffi:defcfun ("drmModeRmFB" drm-mode-rm-fb) :int
  (fd :int) (fb-id :uint32))

;; int drmModePageFlip(int fd, uint32_t crtc_id, uint32_t fb_id,
;;                     uint32_t flags, void *user_data)
(cffi:defcfun ("drmModePageFlip" drm-mode-page-flip) :int
  (fd :int) (crtc-id :uint32) (fb-id :uint32)
  (flags :uint32) (user-data :pointer))

(defconstant +drm-mode-page-flip-event+ 1)

;;; DRM dumb buffer ioctls
(cffi:defcstruct drm-mode-create-dumb
  (height :uint32)
  (width :uint32)
  (bpp :uint32)
  (flags :uint32)
  ;; output
  (handle :uint32)
  (pitch :uint32)
  (size :uint64))

(cffi:defcstruct drm-mode-map-dumb
  (handle :uint32)
  (pad :uint32)
  (offset :uint64))

(cffi:defcstruct drm-mode-destroy-dumb
  (handle :uint32))

;;; DRM_IOCTL_MODE_CREATE_DUMB = DRM_IOWR(0xB2, struct drm_mode_create_dumb)
;;; These are computed from the ioctl macros
(defconstant +drm-ioctl-mode-create-dumb+ #xC02064B2)
(defconstant +drm-ioctl-mode-map-dumb+    #xC01064B3)
(defconstant +drm-ioctl-mode-destroy-dumb+ #xC00464B4)

;;; Connector status
(defconstant +drm-mode-connected+ 1)
(defconstant +drm-mode-disconnected+ 2)
