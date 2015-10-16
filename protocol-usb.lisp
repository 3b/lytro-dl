#++
(asdf:load-systems '3b-libusb1 'babel 'sb-concurrency 'ieee-floats 'nibbles)

(in-package #:lytro-dl)

(defconstant +endpoint+ 2)

(defun send-command-usb (device command subcommand
                         &key (direction :in) payload parameters
                           (response-length 0))
  (let* ((data (make-array 12 :initial-element 0
                              :element-type '(unsigned-byte 8)))
         (in (logior u::+endpoint-in+ +endpoint+))
         (ret nil))
    (setf (nibbles:ub16ref/le data 0) command)
    (setf (aref data 2) subcommand)
    (assert (not (and payload response-length (plusp response-length))))
    (if (integerp parameters)
        (setf (nibbles:sb32ref/le data 3) parameters)
        (replace data parameters :start1 3))
    (u:bulk-transfer-out device +endpoint+
                         (u:command-block-wrapper
                          (if payload
                              (length payload)
                              response-length)
                          data
                          :direction direction)
                         :timeout 2)
    (when payload
      #++(format t "sending payload ~s~%" payload)
      (assert (= (length payload)
                 (u:bulk-transfer-out device +endpoint+ payload :timeout 2))))
    ;; check response if any
    (when (and response-length (plusp response-length))
      (setf ret (u:bulk-transfer-in device in response-length
                                  :timeout 2)))

    ;; check status
    (let ((csw (u:bulk-transfer-in device in 13 :timeout 2)))
      (assert (= (length csw) 13))
      ;; not sure which of these is more important or if both are
      ;; usually needed? possibly should be (values ret csw) instead?
      (list (u::decode-csw csw) ret))))

(defun is-ready (dev)
  "see if camera device is responding"
  (let ((ret (send-command-usb dev 0 0 :direction :out)))
    (format t "is-ready status = ~s (~s)~%" (getf (car ret) :status)
            ret)
    (zerop (getf (car ret) :status))))

(defun download-data (dev)
  ;; get size
  (let ((ret (send-command-usb dev #xc6 0
                               :response-length 65536
                               :direction :in)))
    #++(format t "download-data ret ~s~%" ret)
    (let* ((size (nibbles:ub32ref/le (cadr ret) 0))
           (buf (make-array size :element-type '(unsigned-byte 8)
                                 :initial-element 0))
           (chunk-size 65536))
      #++(format t "dl size = ~s~%" size)
      (loop with i = 0
            for chunk = (min chunk-size (- size i))
            while (< i size)
            do (destructuring-bind (csw part)
                   (send-command-usb dev #xc4 1
                                     :response-length chunk
                                     :direction :in
                                     :parameters i)
                 #++(Format t "read chunk @ ~s ~s/~s =~%  ~s~%"
                         i chunk size csw)
                 ;; todo: handle partial read
                 (assert (zerop (getf csw :residue)))
                 (replace buf part :start1 i)
                 (incf i (length part))))
      buf)))

(defun get-picture-list (dev)
  "download picture list from specified device"
  (let ((ret (send-command-usb dev #xc2 2 :direction :out)))
    (format t "picture-list ret ~s~%" ret)
    (let ((pl (download-data dev))
          (header nil)
          (files nil))
      (decode-picture-list pl 0 (length pl)
                           (lambda (a)
                             (push a files))
                           (lambda (a)
                             (setf header a)))
      (list* header files))))

(defun %reset-camera ()
  "clear-halt + reset first camera device, should only be needed when
experimenting with protocol crashes the camera "
  (u:with-context (:debug 3)
    (let* ((devs (u:map-device-list (lambda (dev desc)
                                      (format t "~&dev ~s =~%   ~x~%" dev desc)
                                      (when (= #x24cf (getf desc :vendor-id))
                                        (u:usb-open dev)))
                                    :vendor #x24cf :product #x00a1))
           (dev (car devs)))
      (when dev
        (unwind-protect
             (progn
               (format t "kernel driver active = ~s~%"
                       (%u:kernel-driver-active dev 0))
               (when (eql 1 (%u:kernel-driver-active dev 0))
                 (format t "detach kernel driver...~%")
                 (%u:detach-kernel-driver dev 0))
               (progn
                 (format t "halt-reset~%")
                 (%u:clear-halt dev +endpoint+)
                 (%u:clear-halt dev (logior +endpoint+ u::+endpoint-in+))
                 (%u:reset-device dev)))
          (mapcar 'u:usb-close devs)))))
  (format t "reset done~%")
  (finish-output))

(defun download-log (dev)
  "download debug log from DEV"
  (let ((ret (send-command-usb dev #xc2 8 :direction :out)))
    (format t "dl log status = ~s~%" ret)
    (babel:octets-to-string (download-data dev)
                            :encoding :iso-8859-1)))

(defun camera-info (dev)
  (let* ((ret (send-command-usb dev #xc2 0 :direction :out))
         (data (download-data dev)))
    (format t "get camera info, status=~s~%" ret)
    (flet ((d (s size)
             (stringz data s size)))
      (let ((manufacturer (d 0 #x100))
            (serial (d #x100 #x80))
            (build (d #x180 #x80))
            (software-version (d #x200 #x80))
            (?? (subseq data #x280 #x284)))
        (format t "got info packet: ~s, ~s, ~s, ~s, ~s~%"
                manufacturer serial build software-version
                ??)
        (list :manufacturer manufacturer
              :serial serial
              :build build
              :software-version software-version)))))

(defun download-file (dev filename)
  #++(format t "dl file ~s...~%" filename)
  (let ((ret (send-command-usb dev #xc2 1 :direction :out
                                          :payload (cstring filename))))
    (declare (ignorable ret))
    #++(format t "dl file ~s status = ~s~%" filename ret)
    (download-data dev)))

#++
(%reset-camera)

(defparameter *foo* nil)
(defparameter *foo2* nil)
#++
(u:with-context (:debug 3)
  (format t "context = ~s~%" u:*context*)
  (format t "version = ~s~%" (u:get-version))
  (let* ((devs (u:map-device-list (lambda (dev desc)
                                    (format t "~&dev ~s =~%   ~x~%" dev desc)
                                    (when (= #x24cf (getf desc :vendor-id))
                                      (u:usb-open dev)))
                                  :vendor #x24cf :product #x00a1))
         (dev (car devs))
         (claimed nil))
    (when dev
      (unwind-protect
           (progn
             (format t "kernel driver active = ~s~%"
                     (%u:kernel-driver-active dev 0))
             (when (eql 1 (%u:kernel-driver-active dev 0))
               (format t "detach kernel driver...~%")
               (%u:detach-kernel-driver dev 0))
             (format t "claim interface 0...~%")
             (u:claim-interface dev 0)
             (setf claimed t)
             (format t "is-ready...") (finish-output)
             (format t "=~s~%" (is-ready dev))
             (format t "info=~s~%" (camera-info dev))
             (format t "picture-list~%")
             (setf *foo* (get-picture-list dev))
             #++(format t "log = ~%~s~%" (download-log dev))
             #++(setf *foo2* (download-file dev "I:\\CRASH000.LOG"))
             (setf *foo2* (download-file dev
                                         (format nil
                                                 "i:\\DCIM\\~3,'0d~a\\~a~4,'0d.~a"
                                                 100 "PHOTO" "IMG_" 5
                                                 "jpg"))))
        (when claimed
          (u:release-interface dev 0))
        (mapcar 'u:usb-close devs))))
  nil)

#++
(babel:octets-to-string *foo2* :encoding :iso-8859-1)
#++
(with-open-file (f "/tmp/l/foo5.jpg" :direction :output
                                     :if-does-not-exist :create
                   :element-type '(unsigned-byte 8))
  (write-sequence *foo2* f)
  nil)



(defun download-pictures (&key (dir *lytro-dir*))
  (u:with-context (:debug 3)
    (format t "context = ~s~%" u:*context*)
    (format t "version = ~s~%" (u:get-version))
    (let* ((devs (u:map-device-list (lambda (dev desc)
                                      (format t "~&dev ~s =~%   ~x~%" dev desc)
                                      (when (= #x24cf (getf desc :vendor-id))
                                        (u:usb-open dev)))
                                    :vendor #x24cf :product #x00a1))
           (dev (car devs))
           (claimed nil)
           (list nil)
           (info nil))
      (when dev
        (unwind-protect
             (progn
               (format t "kernel driver active = ~s~%"
                       (%u:kernel-driver-active dev 0))
               (when (eql 1 (%u:kernel-driver-active dev 0))
                 (format t "detach kernel driver...~%")
                 (%u:detach-kernel-driver dev 0))
               (format t "claim interface 0...~%")
               (u:claim-interface dev 0)
               (setf claimed t)
               (format t "is-ready...") (finish-output)
               (format t "=~s~%" (is-ready dev))
               ;; get camera info
               (setf info (camera-info dev))
               (format t "picture-list~%")
               ;; get list of pictures on camera
               (setf list (get-picture-list dev))
               ;; make sure dest dir exists
               (setf dir (merge-pathnames (format nil "~a/" (getf info :serial))
                                          dir))
               (format t "camera dir = ~s~%" dir)
               (ensure-directories-exist dir)

               (loop for info in (cdr list)
                     for base-name = (format nil
                                             "i:\\DCIM\\~3,'0d~a\\~a~4,'0d."
                                             (getf info :folder-number)
                                             (getf info :folder-suffix)
                                             (getf info :file-prefix)
                                             (getf info :file-number))
                     for id = (getf info :picture-id)
                     for pic-dir = (merge-pathnames (format nil "~a/" id)
                                                    dir)
                     do (ensure-directories-exist pic-dir)
                        (unless (probe-file (merge-pathnames "metadata"
                                                             pic-dir))
                          (with-open-file  (md (merge-pathnames "metadata"
                                                                pic-dir)
                                               :direction :output)
                            (write info :stream md)))
                        (loop for ext in '("jpg" "raw" "txt" "128" "stk")
                              for dl-file = (merge-pathnames
                                             (format nil "~3,'0d-~4,'0d.~a"
                                                     (getf info :folder-number)
                                                     (getf info :file-number)
                                                     ext)
                                             pic-dir)
                              for cam-file = (format nil "~a~a" base-name ext)
                              unless (probe-file dl-file)
                                do (format t "downloading ~s to ~s..~%"
                                           cam-file dl-file)
                                   (let ((dl (download-file dev cam-file)))
                                     (when (plusp (length dl))
                                       (format t "  got ~s bytes~%" (length dl))
                                       (with-open-file
                                           (f dl-file
                                              :direction :output
                                              :if-does-not-exist :create
                                              :element-type '(unsigned-byte 8))
                                         (write-sequence dl f)))))))
          (when claimed
            (u:release-interface dev 0))
          (mapcar 'u:usb-close devs))))))


