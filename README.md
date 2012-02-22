cl-google-charts is a Common Lisp wrapper for Google's chart API (at least the image charts and infographics bits). It is pretty basic, but gives better abstractions than Google's HTTP request parameters, IMO.

Every aspect of the APIs should be available. The primary way to use them is to do something like:

```
(image-url (make-instance 'line-chart
                          :data (list (make-instance 'series :data '(10 30 50))
                                      (make-instance 'series :data '(25 35 10)))
                          :size '(200 100)))

=> #<URI https://chart.googleapis.com/chart?cht=lc&chd=t%3A10,30,50%7C25,35,10&chs=200x100>
```
