(defpackage google-charts
  (:use #:cl)
  (:documentation "The -URL functions are built using HTTP GET-style query
                   parameters so they can be used in HTML elements like `img`,
                  `script`, and `iframe`, but this also means that they are
                   limited to 2048 characters. The non-URL functions use POST
                   requests and do not have this limitation.")
  (:export #:image-url #:image
           #:image-map #:image-map-url
           #:validate #:validation-url
           ;; charts
           #:bar-chart #:line-chart #:meter #:pie-chart #:radar-chart
           #:scatter-plot #:venn-diagram
           ;; icons
           #:bubble #:fun-note #:pin
           ;; other image types
           #:formula #:graph #:map #:qr-code
           ;; elements
           #:axis #:legend #:series
           ;; abstract classes
           #:chart #:icon))
