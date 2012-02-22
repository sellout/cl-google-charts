(in-package #:google-charts)

(defgeneric symbol->string (value)
  (:method ((value symbol))
    (substitute #\_ #\- (string-downcase value)))
  (:method (value)
    (string value)))

(defclass icon ()
  ((text-color :initarg :text-color :accessor text-color))
  (:documentation "Icons that can be embedded within another chart."))

(defclass bubble/pin-mixin (icon)
  ((shadowp :initform nil :initarg :shadowp :accessor shadowp)
   (fill-color :initarg :fill-color :accessor fill-color)))

(defclass bubble (bubble/pin-mixin)
  ((icon :initform nil :initarg :icon :accessor icon)
   (text :initarg :text :accessor text) ; string for text, list for texts
   (bigp :initform nil :initarg :bigp :accessor bigp)
   (frame-style :initarg :frame-style :accessor frame-style))
  (:documentation "Choose small or large text bubbles, with or without icons."))

(defmethod get-parameters append ((chart bubble))
  `(("chst" . ,(format nil
                       "d_bubble~:[~;_icon~]_text~:[~;s~]_~:[small~;big~]~:[~;_withshadow~]"
                       (icon chart)
                       (listp (text chart))
                       (or (listp (text chart))
                           (and (icon chart) (bigp chart)))
                       (shadowp chart)))
    ("chld" . ,(format nil "~@[~a|~]~a|~@[~a|~]~a|~a~{|~a~}"
                       (when (icon chart) (symbol->string (icon chart)))
                       (frame-style chart)
                       (when (stringp (text chart)) (text chart))
                       (fill-color chart)
                       (text-color chart)
                       (when (listp (text chart)) (text chart))))))

(defclass fun-note (icon)
  ((title :initform nil :initarg :title :accessor title)
   (text :initform nil :initarg :text :accessor text)
   (shape :initarg :shape :accessor shape)
   (largep :initform nil :initarg :largep :accessor largep)
   (text-color :initarg :text-color :accessor text-color)
   (text-alignment :initarg :text-alignment :accessor text-alignment))
  (:documentation "You can create a variety of text notes in novelty templates,
                   such as a sticky note or a thought bubble. You can optionally
                   include a title line in the note."))

(defmethod get-parameters append ((chart fun-note))
  `(("chst" . ,(format nil "d_fnote~:[~;_title~]" (title chart)))
    ("chld" . ,(format nil "~a|~d|~a|~a~@[|~a~]~{|~a~}"
                       (symbol->string (shape chart))
                       (if (largep chart) 1 2)
                       (text-color chart)
                       (text-alignment chart)
                       (title chart)
                       (text chart)))))

(defclass pin (bubble/pin-mixin)
  ;; character for text, symbol for icon
  ((content :initarg :content :accessor content)
   (style :initform nil :initarg :style :accessor style)
   (star-fill-color :initform nil :initarg :star-fill-color
                    :accessor star-fill-color))
  (:documentation "Pin types can be plain, starred, or slanted, and can have an
                   icon, a single letter, or longer text strings."))

(defmethod get-parameters append ((chart pin))
  `(("chst" . ,(format nil
                       "d_map_~:[~;x~]pin_~:[letter~;icon~]~:[~;_withshadow~]"
                       (style chart)
                       (symbolp (content chart))
                       (shadowp chart)))
    ("chld" . ,(format nil "~@[pin_~a|~]~a|~a~@[|~a~]~@[|~a~]"
                       (when (style chart) (symbol->string (style chart)))
                       (symbol->string (content chart))
                       (fill-color chart)
                       (when (characterp (content chart)) (text-color chart))
                       (when (eq (style chart) :star)
                         (star-fill-color chart))))))
