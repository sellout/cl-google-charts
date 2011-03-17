(in-package #:google-charts)

;;; Documentation at http://code.google.com/apis/chart/

(defvar +base-uri+ (puri:uri "https://chart.googleapis.com/chart"))

;; NOTE: it seems like color and font-size are used always the same, and used
;; with the parameter name + "s"

(defclass sizing-mixin ()
  ((size :initarg :size :accessor size :documentation "chs")))

(defclass series ()
  ((data :initarg :data :accessor data)
   (scaling :initform nil :initarg :scaling :accessor scaling)))

(defclass legend ()
  ((labels)
   (position :documentation "chdlp"
             :type (member :bottom-horizontal
                           :bottom-vertical
                           :top-horizontal
                           :top-vertical
                           :right-vertical
                           :left-vertical))
   label-order
   style))

(defclass chart (sizing-mixin)
  ((data :initarg :data :accessor data)
   data-functions
   (title :documentation "chtt")
   (title-color :documentation "chts")
   (title-font-size :documentation "chts")
   (legend :initarg :legend :accessor legend)
   (margins :documentation "chma")
   (legend-margins :documentation "chma")
   background-fills))

(defclass axis-label ()
  (value
   position))

(defclass axis ()
  ((location :initarg :location :accessor location
             :type (member :x :y :top :right))
   (range :initform nil :initarg :range :accessor range)
   (labels :initform nil :initarg :labels :accessor axis-labels)
   (label-style :initform nil :initarg :label-style :accessor label-style)
   (tick-mark-lengths :initform nil :initarg :tick-mark-lengths
                      :accessor tick-mark-lengths)))

(defun print-range (range)
  (format nil ",~d,~d~@[,~d~]" (start range) (end range) (axis-step range)))

(defclass axis-chart (chart)
  ((axes :initform nil :initarg :axes :accessor axes :type list)))

(defclass compound-chart (axis-chart)
  (icon-markers
   (grid-lines)
   line-markers
   range-markers
   shape-markers
   value-markers))

(defclass candlestick-chart (compound-chart)
  (candlestick-markers))

(defclass bar-chart (candlestick-chart)
  ((arrangement :initarg :arrangement :accessor arrangement
                :documentation "cht"
                :type (member :stacked :overlapped :grouped))
   (direction :initarg :direction :accessor direction
              :documentation "cht" :type (member :horizontal :vertical))))

(defclass icon ()
  (icon-type
   data))

(defclass line-chart (candlestick-chart)
  ((default-axes-p :initform t :initarg :default-axes-p
                   :accessor default-axes-p)))

(defclass pie-chart (chart)
  (3dp
   concentricp))

(defclass qr-code (sizing-mixin)
  ((data :initarg :data :accessor data)
   (encoding :initform nil :initarg :encoding :accessor encoding
             :type (member nil :utf-8 :shift-jis :iso-8859-1))
   (error-correction-level :initform nil :initarg :error-correction-level
                           :accessor error-correction-level)
   (margin :initform nil :initarg :margin :accessor margin)))

(defclass venn-diagram (chart)
  ())

(defgeneric get-parameters (chart)
  (:method-combination nconc)
  (:method nconc ((chart sizing-mixin))
    `(("chs" . ,(format nil "~{~a~^x~}" (size chart)))))
  (:method nconc ((chart chart))
    (let* ((series (mapcar #'data (data chart)))
           (scaling (remove nil (mapcar #'scaling (data chart))))
           (params `(("chd" . ,(format nil "t:~{~{~a~^,~}~^|~}" series)))))
      (when scaling
        (push `("chds" . ,(format nil "~{~{~a~^,~}~}" scaling)) params))
      params))
  (:method nconc ((chart axis-chart))
    (loop for axis in (axes chart)
       for index from 0
       collecting (string-downcase (subseq (symbol-name (location axis)) 0 1))
                  into names
       collecting (when (range axis) (cons index (range axis)))
                  into ranges
       collecting (when (axis-labels axis) (list index (axis-labels axis)))
                  into labels
       collecting (when (label-style axis) (list index (label-style axis)))
                  into styles
       finally (let ((real-ranges (remove nil ranges))
                     (real-labels (remove nil labels))
                     (real-styles (remove nil styles))
                     (params ()))
                 (when names
                   (push `("chxt" . ,(format nil "~{~a~^,~}" names)) params))
                 (when real-ranges
                   (push `("chxr" . ,(format nil "~{~{~d~^,~}~^|~}" real-ranges))
                         params))
                 (when real-labels
                   (push `("chxl" . ,(format nil "~:{~d:~{|~a~}~}" real-labels))
                         params))
                 (when real-styles
                   (push `("chxs" . ,(format nil "~:{~d~a~:^|~}" real-styles))
                         params))
                 (return params))))
  (:method nconc ((chart compound-chart))
    (let ((params ()))
      params))
  (:method nconc ((chart bar-chart))
    (assert (not (and (eq (arrangement chart) :overlapped)
                      (eq (direction chart) :horizontal)))
            ((arrangement chart) (direction chart))
            "Can not specify a horizontal overlapped bar chart.")
    `(("cht" . ,(format nil "b~a~a"
                        (ecase (direction chart)
                          (:horizontal "h")
                          (:vertical "v"))
                        (ecase (arrangement chart)
                          (:stacked "s")
                          (:overlapped "o")
                          (:grouped "g"))))))
  (:method nconc ((chart line-chart))
    `(("cht" . ,(if (default-axes-p chart) "lc" "lc:nda"))))
  (:method nconc ((chart pie-chart))
    `(("cht" . "p")))
  (:method nconc ((chart venn-diagram))
    `(("cht" . "v"))))

(defmethod get-parameters nconc ((chart qr-code))
  (let ((params `(("cht" . "qr"))))
    (when (data chart) (push `("chl" . ,(data chart)) params))
    (when (encoding chart) (push `("choe" . ,(encoding chart)) params))
    (when (or (error-correction-level chart) (margin chart))
      (push `("chld" . ,(format nil "~@[~a~]~@[|~d~]"
                                (error-correction-level chart)
                                (margin chart)))
            params))
    params))

(defun parameters-with-output-format (chart output-format)
  (if output-format
      (cons `("chof" . ,(if (typep output-format 'symbol)
                            (string-downcase (symbol-name output-format))
                            output-format))
            (get-parameters chart))
      (get-parameters chart)))

(defun uri (chart &optional output-format)
  "Returns a GET-style URI that can be used in an IMG tag. If you want to get
   the image data, call IMAGE-DATA instead, as that uses a POST request and
   doesn’t have the length limitations of GET requests."
  (assert (member output-format '(nil :png :gif))
          (output-format)
          "PNG and GIF are the only supported output formats. If you want an ~
           image map or to validate the chart, there are other functions for ~
           that.")
  (puri:merge-uris (format nil "?~:{~a=~a~:^&~}"
                           (mapcar (lambda (param)
                                     (list (car param)
                                           (puri::encode-escaped-encoding
                                            (cdr param)
                                            puri::*reserved-characters*
                                            t)))
                                   (parameters-with-output-format
                                    chart output-format)))
                   +base-uri+))

(defun request (chart output-format)
  (multiple-value-bind (body status)
      (drakma:http-request +base-uri+
                           :parameters (parameters-with-output-format
                                        chart output-format)
                           :method :post)
    (if (= status 200)
        body
        (error "Request failed: ~d" status))))

(defun image-data (chart &optional output-format)
  (assert (member output-format '(:png :gif))
          (output-format)
          "PNG and GIF are the only supported output formats. If you want an ~
           image map or to validate the chart, there are other functions for ~
           that.")
  (request chart output-format))

(defun image-map (chart)
  "Returns image map data for the chart, as a JSON string. This can be used to
   generate an image map for the chart to make various regions clickable.
   http://code.google.com/apis/chart/docs/json_format.html"
  ;; TODO: Offer to return either the JSON or an HTML map structure.
  (request chart "json"))

(defun validate (chart)
  "Returns an HTML page listing any errors in the chart URL.
   http://code.google.com/apis/chart/docs/debugging.html"
  ;; TODO: Parse this and just grab the useful information.
  (request chart "validate"))
