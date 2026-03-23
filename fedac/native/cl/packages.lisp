;;; Package definitions for AC Native OS

(defpackage :ac-native.util
  (:use :cl)
  (:export #:ac-log #:monotonic-time-ms #:frame-sync-60fps))

(defpackage :ac-native.syscalls
  (:use :cl :cffi)
  (:export #:sys-open #:sys-close #:sys-read #:sys-write
           #:sys-ioctl #:sys-mmap #:sys-munmap
           #:sys-mount #:sys-umount
           #:sys-reboot #:sys-poweroff #:sys-getpid
           #:sys-fork #:sys-execvp #:sys-waitpid
           #:+o-rdonly+ #:+o-wronly+ #:+o-rdwr+ #:+o-nonblock+ #:+o-cloexec+
           #:+prot-read+ #:+prot-write+ #:+map-shared+
           #:+clock-monotonic+))

(defpackage :ac-native.color
  (:use :cl)
  (:export #:make-color #:color-r #:color-g #:color-b #:color-a
           #:color-pack-argb32 #:color-blend))

(defpackage :ac-native.framebuffer
  (:use :cl :cffi :ac-native.color)
  (:export #:make-framebuffer #:fb-create #:fb-destroy
           #:fb-width #:fb-height #:fb-stride #:fb-pixels
           #:fb-clear #:fb-put-pixel #:fb-blend-pixel
           #:fb-copy-scaled))

(defpackage :ac-native.drm
  (:use :cl :cffi :ac-native.syscalls :ac-native.framebuffer)
  (:export #:drm-init #:drm-destroy #:drm-present #:drm-flip
           #:display-width #:display-height))

(defpackage :ac-native.graph
  (:use :cl :ac-native.color :ac-native.framebuffer)
  (:export #:make-graph #:graph-wipe #:graph-ink
           #:graph-plot #:graph-line #:graph-box #:graph-circle))

(defpackage :ac-native.font
  (:use :cl :ac-native.color :ac-native.graph :ac-native.framebuffer)
  (:export #:font-init #:font-draw #:font-measure))

(defpackage :ac-native.input
  (:use :cl :cffi :ac-native.syscalls)
  (:export #:input-init #:input-destroy #:input-poll
           #:make-event #:event-type #:event-key #:event-code #:event-x #:event-y
           ;; Special keys
           #:+key-esc+ #:+key-backspace+ #:+key-tab+ #:+key-enter+
           #:+key-leftctrl+ #:+key-space+ #:+key-f1+
           #:+key-up+ #:+key-down+ #:+key-left+ #:+key-right+
           #:+key-home+ #:+key-end+ #:+key-power+
           #:+key-minus+ #:+key-equal+
           ;; Number row
           #:+key-1+ #:+key-2+ #:+key-3+ #:+key-4+ #:+key-5+
           #:+key-6+ #:+key-7+ #:+key-8+ #:+key-9+ #:+key-0+
           ;; QWERTY rows
           #:+key-q+ #:+key-w+ #:+key-e+ #:+key-r+ #:+key-t+
           #:+key-y+ #:+key-u+ #:+key-i+ #:+key-o+ #:+key-p+
           #:+key-leftbrace+ #:+key-rightbrace+
           #:+key-a+ #:+key-s+ #:+key-d+ #:+key-f+ #:+key-g+
           #:+key-h+ #:+key-j+ #:+key-k+ #:+key-l+
           #:+key-semicolon+ #:+key-apostrophe+
           #:+key-z+ #:+key-x+ #:+key-c+ #:+key-v+ #:+key-b+
           #:+key-n+ #:+key-m+))

(defpackage :ac-native.alsa
  (:use :cl :cffi)
  (:export #:pcm-open #:pcm-close #:pcm-writei #:pcm-prepare #:pcm-recover
           #:pcm-set-params #:mixer-set-capture-volume
           #:snd-strerror
           #:+snd-pcm-stream-playback+ #:+snd-pcm-stream-capture+
           #:+snd-pcm-format-s16-le+ #:+snd-pcm-format-float-le+
           #:+snd-pcm-access-rw-interleaved+))

(defpackage :ac-native.audio
  (:use :cl :cffi :bordeaux-threads
        :ac-native.alsa :ac-native.util)
  (:export #:audio-init #:audio-destroy
           #:audio-synth #:audio-synth-kill
           #:audio-sample-play #:audio-sample-load-data))

(defpackage :ac-native.config
  (:use :cl)
  (:export #:load-config #:config-handle #:config-piece
           #:config-claude-token #:config-github-pat #:config-wifi
           #:write-device-tokens))

(defpackage :ac-native
  (:use :cl :ac-native.util :ac-native.color :ac-native.framebuffer
        :ac-native.drm :ac-native.graph :ac-native.font
        :ac-native.input :ac-native.audio :ac-native.config)
  (:export #:main))

(defpackage :ac-native.build
  (:use :cl)
  (:export #:build))
