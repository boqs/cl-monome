;;;; cl-monome.asd

(asdf:defsystem #:cl-monome
  :description "CL bindings for monome grid"
  :author "rick venn <sasquatch@rickvenn.com"
  :license "chuffs & guffs"
  :serial t
  :components ((:file "package")
               (:file "cl-monome"))
  :depends-on (:cffi))
