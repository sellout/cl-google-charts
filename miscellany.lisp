(in-package #:google-charts)

(defclass formula (sizing-mixin)
  ((color :initform nil :initarg :color :accessor color)
   (tex-input :initarg :tex-input :accessor tex-input)))

(defmethod get-parameters append ((chart formula))
  (let ((params `(("cht" . "tx")
                  ("chl" . ,(tex-input chart)))))
    (when (color chart)
      (push `("chco" . ,(symbol-name (color chart))) params))
    params))

(defclass map (title/legend-mixin)
  ((colors :initform nil :initarg :colors :accessor colors)
   (regions :initarg :regions :accessor regions)))

(defmethod get-parameters append ((chart map))
  (let ((params `(("cht" . "map")
                  ("chld" . ,(format nil "~{~a~^|~}" (regions chart))))))
    (when (colors chart)
      (push `("chco" . ,(format nil "~{~a~^|~}" (colors chart))) params))
    params))

(defclass qr-code (sizing-mixin)
  ((data :initarg :data :accessor data)
   (encoding :initform nil :initarg :encoding :accessor encoding
             :type (member nil :utf-8 :shift-jis :iso-8859-1))
   (error-correction-level :initform nil :initarg :error-correction-level
                           :accessor error-correction-level)
   (margin :initform nil :initarg :margin :accessor margin)))

(defmethod get-parameters append ((chart qr-code))
  (let ((params `(("cht" . "qr"))))
    (when (data chart) (push `("chl" . ,(data chart)) params))
    (when (encoding chart) (push `("choe" . ,(encoding chart)) params))
    (when (or (error-correction-level chart) (margin chart))
      (push `("chld" . ,(format nil "~@[~a~]~@[|~d~]"
                                (error-correction-level chart)
                                (margin chart)))
            params))
    params))
