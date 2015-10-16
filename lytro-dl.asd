(defsystem lytro-dl
  :description "Quick hack to download pictures from lytro camera"
  :author "Bart Botta <00003b@gmail.com>"
  :license "MIT"
  :depends-on (3b-libusb1 babel sb-concurrency ieee-floats nibbles)
  :serial t
  :components ((:file "package")
               (:file "protocol-usb")))
