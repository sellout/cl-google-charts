(defpackage google-charts
  (:use #:cl)
  (:export #:chart #:bar-chart #:line-chart
           #:uri #:image-data))

(in-package #:google-charts)

(defvar +base-uri+ (puri:uri "https://chart.googleapis.com/chart"))

;; NOTE: it seems like color and font-size are used always the same, and used
;; with the parameter name + "s"

(defclass chart ()
  ((size :initarg :size :accessor size :documentation "chs")
   (title :documentation "chtt")
   (title-color :documentation "chts")
   (title-font-size :documentation "chts")
   
   (legend :documentation "chdl")
   (legend-position :documentation "chdlp"
                    :type (member :bottom-horizontal
                                  :bottam-vertical
                                  :top-horizontal
                                  :top-vertical
                                  :right-vertical
                                  :left-vertical))
   (legend-label-ordering :documentation "chdlp"
                          :type (or (member :same :reverse :automatic) list))
   (legend-color :documentation "chdls")
   (legend-font-size :documentation "chdls")

   (margins :documentation "chma")
   (legend-margins :documentation "chma")
   background-fills))

(defclass line-bar-gom-radar-scatter-mixin ()
  (visible-axes
   axis-range
   (custom-axis-labels :initform nil :initarg :custom-axis-labels
                       :accessor custom-axis-labels)
   axis-label-positions
   axis-label-styles
   axis-tick-marks))

(defclass bar-candlestick-line-radar-scatter-mixin ()
  (range-markers
   line-markers))

(defclass bar-line-radar-scatter-mixin
    (line-bar-gom-radar-scatter-mixin bar-candlestick-line-radar-scatter-mixin)
  (grid-lines
   dynamic-icon-markers
   shape-markers
   text-and-data-value-markers))

(defclass bar-line-mixin (bar-line-radar-scatter-mixin)
  (candlestick-markers))

(defclass chd-chart (chart)
  ((data :initarg :data :accessor data)
   data-functions))

(defclass bar-chart (bar-line-mixin chd-chart)
  ((arrangement :initarg :arrangement :accessor arrangement
                :documentation "cht"
                :type (member :stacked :overlapped :grouped))
   (direction :initarg :direction :accessor direction
              :documentation "cht" :type (member :horizontal :vertical))))

(defclass dynamic-icon ()
  (icon-type
   data))

(defclass line-chart (bar-line-mixin chd-chart)
  ((default-axes-p :initarg :default-axes-p :accessor default-axes-p)))

(defclass pie-chart (chd-chart)
  (3dp
   concentricp))

(defclass venn-diagram (chd-chart)
  ())

(defgeneric get-parameters (chart)
  (:method-combination nconc)
  (:method nconc ((chart chart))
    (let ((parameters `(("chs" . ,(format nil "~{~a~^x~}" (size chart))))))
      parameters))
  (:method nconc ((chart chd-chart))
    `(("chd" . ,(format nil "t:~{~a~^,~}" (data chart)))))
  (:method nconc ((chart line-bar-gom-radar-scatter-mixin))
    `(("chxl" . ,(if (custom-axis-labels chart)
                     (format nil "~{0:~{|~a~}~^|~}" (custom-axis-labels chart))
                     ""))))
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

(defun uri (chart)
  "Returns a GET-style URI that can be used in an IMG tag. If you want to get
   the image data, call IMAGE-DATA instead, as that uses a POST request and
   doesn’t have the length limitations of GET requests."
  (puri:merge-uris (format nil "?~:{~a=~a~:^&~}"
                           (mapcar (lambda (param)
                                     (list (car param)
                                           (puri::encode-escaped-encoding
                                            (cdr param)
                                            puri::*reserved-characters*
                                            t)))
                                   (get-parameters chart)))
                   +base-uri+))

(defun image-data (chart)
  (multiple-value-bind (body status)
      (drakma:http-request +base-uri+
                           :parameters (get-parameters chart)
                           :method :post)
    (if (= status 200)
        body
        (error "Request failed: ~d" status))))