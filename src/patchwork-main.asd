(asdf:defsystem :patchwork-main
  :name "Patch Work Application"
  :description "Patch Work: IRCAM Computer Assisted Composition"
  :author "IRCAM"
  :version "10.0.3"
  :license "GPL3"
  :properties ((#:author-email                   . "pjb@informatimago.com")
               (#:date                           . "Summer 2021a")
               ((#:albert #:output-dir)          . "../documentation/patchwork/")
               ((#:albert #:formats)             . ("docbook"))
               ((#:albert #:docbook #:template)  . "book")
               ((#:albert #:docbook #:bgcolor)   . "white")
               ((#:albert #:docbook #:textcolor) . "black"))
  :depends-on ("mclgui" "patchwork")
  :components ((:file "main")))
