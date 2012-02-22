(in-package #:google-charts)

;;; Documentation at http://code.google.com/apis/chart/image/

(defvar +base-uri+ (puri:uri "https://chart.googleapis.com/chart"))

;; NOTE: it seems like color and font-size are used always the same, and used
;; with the parameter name + "s"

(defclass sizing-mixin ()
  ((size :initform nil :initarg :size :accessor size)))

(defclass series ()
  ((data :initarg :data :accessor data)
   (scaling :initform nil :initarg :scaling :accessor scaling))
  (:documentation "Each series is an independent set of data, a list of series
                   can be used in the DATA slot of a CHART."))

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
   style)
  (:documentation
   "The legend is a side section of the chart that gives a small text
    description of each series. You can specify the text associated with each
    series in this legend, and specify where on the chart it should appear."))

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
   background-fills)
  (:documentation "This is the superclass of all the charts that are actually
                   chart-like."))

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
                      :accessor tick-mark-lengths))
  (:documentation "You can specify which axes to display on the chart, and give
                   them custom labels and positions, ranges, and styles."))

(defclass axis-chart (chart)
  ((axes :initform nil :initarg :axes :accessor axes :type list)))

(defclass meter (axis-chart)
  ((label :initform nil :initarg :label :accessor label))
  (:documentation
   "A meter is a gauge that points toward a single value on a range. The range
    has a color gradation that you can optionally specify. You can also specify
    custom text above the pointer. The dial range is from the data format
    minimum value to the maximum value. Specify one data value in your DATA slot
    for the arrow to point to.

    You can specify multiple arrows on the same chart, and style the arrow body
    and point size differently on each. If you want the same arrow style for all
    arrows, specify the data points as values in the same series. If you want to
    apply different styles to different arrows, group all arrows with the same
    style into the same series."))

(defclass compound-chart (axis-chart)
  (icon-markers
   (grid-lines)
   line-markers
   range-markers
   shape-markers
   value-markers)
  (:documentation "Combinations of two or more charts in a single image."))

(defclass candlestick-chart (compound-chart)
  (candlestick-markers)
  (:documentation "A type of chart that shows a range, max, and min values for a
                   set of data."))

(defclass bar-chart (candlestick-chart)
  ((arrangement :initarg :arrangement :accessor arrangement
                :documentation "cht"
                :type (member :stacked :overlapped :grouped))
   (direction :initarg :direction :accessor direction
              :documentation "cht" :type (member :horizontal :vertical)))
  (:documentation
   "Horizontal and vertical bar charts, with grouped or stacked series."))

(defclass line-chart (candlestick-chart)
  ((default-axes-p :initform t :initarg :default-axes-p
                   :accessor default-axes-p))
  (:documentation "Line charts and sparklines."))

(defclass pie-chart (chart)
  (3dp
   concentricp)
  (:documentation "Two- and three-dimensional pie charts, with optional
                   concentric rings of data."))

(defclass radar-chart (compound-chart)
  ((curvedp :initform nil :initarg :curvedp :accessor curvedp))
  (:documentation
   "A circular chart, similar to a line chart wrapped around a center point."))

(defclass scatter-plot (chart)
  ()
  (:documentation "Chart with discrete data points, where you can specify both X
                   and Y values."))

(defclass venn-diagram (chart)
  ()
  (:documentation "Two or three circles depicting similarity or differences
                   between data sets."))

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
