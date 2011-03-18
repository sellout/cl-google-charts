(in-package #:google-charts)

;;; Documentation at http://code.google.com/apis/chart/

(defvar +base-uri+ (puri:uri "https://chart.googleapis.com/chart"))

;; NOTE: it seems like color and font-size are used always the same, and used
;; with the parameter name + "s"

(defclass sizing-mixin ()
  ((size :initform nil :initarg :size :accessor size)))

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

(defclass title/legend-mixin (sizing-mixin)
  ((legend :initform nil :initarg :legend :accessor legend)
   (margins :initform nil :initarg :margins :accessor margins)
   (legend-margins :documentation "chma")
   (title :initform nil :initarg :title :accessor title)
   (title-color :documentation "chts")
   (title-font-size :documentation "chts")))

(defclass chart (title/legend-mixin)
  ((data :initarg :data :accessor data)
   data-functions
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

(defclass meter (axis-chart)
  ((label :initform nil :initarg :label :accessor label)))

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

(defclass line-chart (candlestick-chart)
  ((default-axes-p :initform t :initarg :default-axes-p
                   :accessor default-axes-p)))

(defclass pie-chart (chart)
  (3dp
   concentricp))

(defclass radar-chart (compound-chart)
  ((curvedp :initform nil :initarg :curvedp :accessor curvedp)))

(defclass scatter-plot (chart)
  ())

(defclass venn-diagram (chart)
  ())

(defgeneric get-parameters (chart)
  (:method-combination append)
  (:method append ((chart sizing-mixin))
    (when (size chart)
      `(("chs" . ,(format nil "~{~a~^x~}" (size chart))))))
  (:method append ((chart title/legend-mixin))
    (let ((params ()))
      (when (title chart) (push `("chtt" . ,(title chart)) params))
      (when (margins chart)
        (push `("chma" . ,(format nil "~{~d~^,~}" (margins chart))) params))
      params))
  (:method append ((chart chart))
    (let* ((series (mapcar #'data (data chart)))
           (scaling (remove nil (mapcar #'scaling (data chart))))
           (params `(("chd" . ,(format nil "t:~{~{~a~^,~}~^|~}" series)))))
      (when scaling
        (push `("chds" . ,(format nil "~{~{~a~^,~}~^,~}" scaling)) params))
      params))
  (:method append ((chart axis-chart))
    (loop for axis in (axes chart)
       for index from 0
       collecting (subseq (string-downcase (location axis)) 0 1)
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
  (:method append ((chart meter))
    `(("cht" . "gom")))
  (:method append ((chart compound-chart))
    (let ((params ()))
      params))
  (:method append ((chart bar-chart))
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
  (:method append ((chart line-chart))
    `(("cht" . ,(if (default-axes-p chart) "lc" "lc:nda"))))
  (:method append ((chart pie-chart))
    `(("cht" . "p")))
  (:method append ((chart radar-chart))
    `(("cht" . ,(if (curvedp chart) "rs" "r"))))
  (:method append ((chart scatter-plot))
    `(("cht" . "s")))
  (:method append ((chart venn-diagram))
    `(("cht" . "v"))))
