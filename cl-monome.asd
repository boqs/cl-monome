;;;; cl-monome.asd

(asdf:defsystem #:cl-monome
  :description "CL bindings for monome grid"
  :author "rick venn <sasquatch@rickvenn.com"
  :license "errr whut?"
  :serial t
  :components ((:file "package")
	       (:file "monome-serialraw")
               (:file "monome-serialosc"))
  :depends-on (:cffi :external-program :optima))

