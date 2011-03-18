(in-package #:google-charts)

(defun parameters-with-output-format (chart output-format)
  (if output-format
      (cons `("chof" . ,(string-downcase output-format))
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
