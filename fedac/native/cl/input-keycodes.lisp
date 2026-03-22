;;; Linux input keycodes

(in-package :ac-native.input)

(defconstant +key-esc+ 1)
(defconstant +key-1+ 2)
(defconstant +key-backspace+ 14)
(defconstant +key-tab+ 15)
(defconstant +key-enter+ 28)
(defconstant +key-leftctrl+ 29)
(defconstant +key-a+ 30)
(defconstant +key-space+ 57)
(defconstant +key-f1+ 59)
(defconstant +key-up+ 103)
(defconstant +key-left+ 105)
(defconstant +key-right+ 106)
(defconstant +key-down+ 108)
(defconstant +key-home+ 102)
(defconstant +key-end+ 107)
(defconstant +key-power+ 116)

;;; Event types
(defconstant +ev-key+ 1)
(defconstant +ev-rel+ 2)
(defconstant +ev-abs+ 3)

;;; struct input_event (24 bytes on x86-64)
(cffi:defcstruct input-event
  (tv-sec :long)
  (tv-usec :long)
  (type :uint16)
  (code :uint16)
  (value :int32))
