(in-package #:google-charts)

(defun symbol->string (symbol)
  (substitute #\_ #\- (string-downcase symbol)))

(defclass icon ()
  (icon-type
   data))

(defclass bubble (icon)
  ((icon :initform nil :initarg :icon :accessor icon)
   (text :initarg :text :accessor text) ;; string for text, list for texts
   (bigp :initform nil :initarg :bigp :accessor bigp)
   (shadowp :initform nil :initarg :shadowp :accessor shadowp)
   (frame-style :initarg :frame-style :accessor frame-style)
   (fill-color :initarg :fill-color :accessor fill-color)
   (text-color :initarg :text-color :accessor text-color)))

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
