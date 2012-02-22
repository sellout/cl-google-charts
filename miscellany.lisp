(in-package #:google-charts)

;;; Documentation at http://code.google.com/apis/chart/infographics/

(defclass formula (sizing-mixin)
  ((color :initform nil :initarg :color :accessor color)
   (tex-input :initarg :tex-input :accessor tex-input))
  (:documentation "You can generate an image showing a mathematical formula,
                   using the TeX language."))

(defmethod get-parameters append ((chart formula))
  (let ((params `(("cht" . "tx")
                  ("chl" . ,(tex-input chart)))))
    (when (color chart)
      (push `("chco" . ,(symbol-name (color chart))) params))
    params))

(defclass graph (sizing-mixin)
  ((engine :initform nil :initarg :engine :accessor engine)
   (dot-input :initarg :dot-input :accessor dot-input))
  (:documentation "Charts used to visualize connectivity graphs."))

(defmethod get-parameters append ((chart graph))
  `(("cht" . ,(format nil "gv~:[~;:~a~]"
                      (engine chart) (string-downcase (engine chart))))
    ("chl" . ,(dot-input chart))))

(defclass map (title/legend-mixin)
  ((colors :initform nil :initarg :colors :accessor colors)
   (regions :initarg :regions :accessor regions))
  (:documentation
   "Maps highlighting specified regions, with optional marker flags."))

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
   (margin :initform nil :initarg :margin :accessor margin))
  (:documentation
   "QR codes are a popular type of two-dimensional barcode. They are also known
    as hardlinks or physical world hyperlinks. QR Codes store up to 4,296
    alphanumeric characters of arbitrary text. This text can be anything, for
    example URL, contact information, a telephone number, even a poem! QR codes
    can be read by an optical device with the appropriate software. Such devices
    range from dedicated QR code readers to mobile phones."))

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
