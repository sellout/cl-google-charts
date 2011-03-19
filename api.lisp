(in-package #:google-charts)

(defun parameters-with-output-format (chart output-format)
  (if output-format
      (cons `("chof" . ,(string-downcase output-format))
            (get-parameters chart))
      (get-parameters chart)))

(defun url (chart paramaters)
  (puri:merge-uris (format nil "?~a"
                           (drakma::alist-to-url-encoded-string parameters
                                                                :utf-8))
                   +base-uri+))

(defun image-url (chart &optional output-format)
  "Returns a GET-style URI that can be used in an IMG tag. If you want to get
   the image data, call IMAGE-DATA instead, as that uses a POST request and
   doesn’t have the length limitations of GET requests."
  (assert (member output-format '(nil :png :gif))
          (output-format)
          "PNG and GIF are the only supported output formats. If you want an ~
           image map or to validate the chart, there are other functions for ~
           that.")
  (url chart (parameters-with-output-format chart output-format)))

(defun request (chart output-format)
  (multiple-value-bind (body status)
      (drakma:http-request +base-uri+
                           :parameters (parameters-with-output-format
                                        chart output-format)
                           :method :post)
    (if (= status 200)
        body
        (error "Request failed: ~d" status))))

(defun image (chart &optional output-format)
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

(defun image-map-url (chart js-callback)
  "If you want to process the profile data using client-side JavaScript, this
   generates a URL that you can use as a `src` value to the HTML `script`
   element. And the JavaScript function named by _js-callback_ will be called on
   the resulting JSON data."
  (url chart (cons `("callback" . ,js-callback)
                   (parameters-with-output-format chart "json"))))

(defun validate (chart)
  "Returns an HTML page listing any errors in the chart URL.
   http://code.google.com/apis/chart/docs/debugging.html"
  ;; TODO: Parse this and just grab the useful information.
  (request chart "validate"))

(defun validation-url (chart)
  "Returns a URL for the HTML page containing the validation information. This
   is suitable for use in an A or IFRAME element."
  (url chart (parameters-with-output-format chart "validate")))
