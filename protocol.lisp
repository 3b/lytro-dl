#++
(asdf:load-systems 'conserv 'babel 'sb-concurrency 'ieee-floats)

(in-package #:lytro-dl)


(defparameter *partial-packet* nil)
(defparameter *current-download* nil)
(defparameter *close-stream* nil)
(defparameter *worklist* (sb-concurrency:make-mailbox :name "worklist"))
(defparameter *control* nil)
(defparameter *callback* nil)


(defclass lytro-dl ()
  ((name :accessor name :initarg :name :initform nil)
   (socket :accessor socket :initform nil)))

(defclass lytro-dl-status (lytro-dl)
  ((name :accessor name :initarg :name :initform nil)))

(defparameter *callback-port* 5677)
(defparameter *control-port* 5678)
(defparameter *download-port* 5679)

(defmethod conserv.tcp:on-tcp-client-timer ((driver lytro-dl) data)
  (format t "timer!")
  ;; check for work if *current-download* isn't set
  ;; (work should set *current-download* if it is waiting for a reply,
  ;;  so should be set before it returns and we schedule next timer
  ;;  so if there is a race on clearing it we only wait an extra timer.
  ;;  need to make sure it doesn't get cleared temporarily and reset though)
  (unless *current-download*
    (let ((work (sb-concurrency:receive-message-no-hang *worklist*)))
      (cond
        (work
         (format t "starting worklist...~%")
         (funcall work)))))
  ;; wait for work (but keep handling events)
  (conserv.tcp:tcp-client-add-timer driver 1.0))

(defmethod conserv.tcp:on-tcp-client-close ((driver lytro-dl))
  (conserv.tcp::exit-event-loop :delay 1)
  (format t "client ~s closed~%" (name driver)))

(defmethod conserv.tcp:on-tcp-client-connect ((driver lytro-dl))
  (format t "client ~s connected, add timer~%" (name driver))
  ;; wait for work (but keep handling events)
  (conserv.tcp:tcp-client-add-timer driver 0.25))

(defmethod conserv.tcp:on-tcp-client-connect ((driver lytro-dl-status))
  (format t "client ~s connected~%" (name driver)))

(defmethod conserv.tcp:on-tcp-client-end-of-file ((driver lytro-dl))
  (conserv.tcp::exit-event-loop :delay 1)
  (format t "client ~s got eof~%" (name driver)))

(defmethod conserv.tcp:on-tcp-client-error ((driver lytro-dl) error)
  (format t "client ~s got error ~s~%" (name driver) error))

(defmethod conserv.tcp:on-tcp-client-output-empty ((driver lytro-dl))
  (format t "client ~s got output empty~%" (name driver)))

(defmethod conserv.tcp:on-tcp-client-connect ((driver lytro-dl))
  (format t "client ~s connected~%" (name driver)))


(defun u32 (buffer offset)
  (loop for i below 4
        for octet = (aref buffer (+ offset i))
        sum (ash octet (* i 8))))

(defun u16 (buffer offset)
  (loop for i below 2
        for octet = (aref buffer (+ offset i))
        sum (ash octet (* i 8))))

(defun stringz (buffer offset size)
  (babel:octets-to-string buffer
                          :start offset
                          :end (min (+ offset size)
                                    (position 0 buffer
                                              :start offset))))



(defmethod download-chunk ((stream stream) sequence offset length)
  (write-sequence sequence stream :start offset :end (+ offset length))
  (when (eq *close-stream* stream)
    (close stream)
    (setf *close-stream* nil))
  t)

(defmethod download-chunk ((stream (eql :hex)) sequence offset length)
  (loop for i from offset by 16
        repeat (max (ceiling length 16) 16)
        do (loop for j from i below (+ offset length) repeat 16
                 do (format t "~2,'0x " (aref sequence j)))
           (loop for j from i below (+ offset length) repeat 16
                 for c = (code-char (aref sequence j))
                 do (format t "!~c" (if (and (< (aref sequence j) 128)
                                            (graphic-char-p c))
                                       c #\.)))
           (format t "~%"))
  t)

(defmethod download-chunk ((stream (eql :text)) sequence offset length)
  (format t "~s~%" (babel:octets-to-string sequence :start offset :end (+ offset length)
                                           :encoding :iso-8859-1))
  t)

(defmethod download-chunk ((stream (eql :#xc20000)) sequence offset length)
  (flet ((d (s size)
           (stringz sequence (+ s offset) size)))
   (format t "got info packet: ~s, ~s, ~s, ~s, ~s~%"
           (d 0 256)
           (d #x100 128)
           (d #x180 128)
           (d #x200 128)
           (subseq sequence #x280 #x284)))
  t)

(defmethod process-command-data ((command (eql #x00c4))
                                 subcommand parameters length flags
                                 data offset)
  (unless *current-download*
    (format t "got download data with no current download ?")
    (format t "  command ~4,'0x (~2,'0x) length #x~8,'0x, flags #x~8,'0x~%  parameters ~s, payload ~s bytes~%"
            command subcommand length flags parameters
            (- (length data) offset))
    (return-from process-command-data nil))
  (format t "  got download data ~4,'0x (~2,'0x) length #x~8,'0x, flags #x~8,'0x~%  parameters ~s, payload ~s bytes~%"
          command subcommand length flags parameters
          (- (length data) offset))
  ;(break "data" data)
  (when (if (functionp *current-download*)
            (funcall *current-download* data offset length)
            (download-chunk *current-download* data offset length))
    (setf *current-download* nil)))

(defmethod process-command-data (command subcommand parameters length flags
                                 data offset)
  (format t "got unknown command ~4,'0x (~2,'0x) length #x~8,'0x, flags #x~8,'0x~%  parameters ~s, payload ~s bytes~%"
          command subcommand length flags parameters  (- (length data) offset))
  (with-open-file (s "/tmp/unknown-command":direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
    (write-sequence data s :start offset)))

(defun process-data (data)
  ;; u32le magic = #xfaaa55af
  ;; u32le length
  ;; u32le flags
  ;; u16le command
  ;; 16x octet parameters
  ;; optional length x octet payload

  ;; fixme: figure out how to get binary data from conserv
  (when (and (not (typep data '(vector (unsigned-byte 8))))
             (characterp (aref data 0)))
    (setf data (map 'vector 'char-code data)))
  (unless *partial-packet*
    (let ((offset 0))
      (symbol-macrolet ((u32 (prog1 (u32 data offset) (incf offset 4)))
                        (u16 (prog1 (u16 data offset) (incf offset 2)))
                        (u8 (prog1 (aref data offset) (incf offset 1))))
        (assert (= u32 #xfaaa55af))
        (let* ((length u32)
               (flags u32)
               (command u16)
               (subcommand u8)
               (parameters (subseq data offset (incf offset 13))))
          (format t "got start of data: command ~4,'0x (~2,'0x) length #x~8,'0x, flags #x~8,'0x~%  parameters ~s, payload ~s bytes~%"
          command subcommand length flags parameters (- (length data) offset))
          (setf *partial-packet*
                (list (make-array (+ 28 length)
                                  :fill-pointer 0 :element-type '(unsigned-byte 8))
                      command subcommand parameters length flags offset))))))
  (let ((leftover nil)
        (data-length (length data)))
    (destructuring-bind (buf command subcommand parameters length flags offset)
        *partial-packet*
      (format t "add ~s bytes to buffer @ ~s -> ~s/~s~%"
              data-length (fill-pointer buf)
              (+ data-length (fill-pointer buf))
              (array-total-size buf))
      ;; we might have more than 1 packet, so handle extra separately
      (when (> (+ data-length (fill-pointer buf)) (array-total-size buf))
        (setf leftover (- data-length
                          (- (array-total-size buf) (fill-pointer buf)))))
      (let ((s (fill-pointer buf)))
        (incf (fill-pointer buf) (min (- (array-total-size buf)
                                         (fill-pointer buf))
                                      (length data)))
        (replace buf data :start1 s))
      (when (= (fill-pointer buf) (array-total-size buf))
        (setf *partial-packet* nil)
        (process-command-data command subcommand parameters length flags
                              buf offset)))
    (when (and leftover (< leftover (length data)))
      (process-data (subseq data leftover)))))

(defmethod conserv.tcp:on-tcp-client-data ((driver lytro-dl) data)
  (process-data data))

(defparameter *thunk* nil)

(defmethod conserv.tcp:on-tcp-client-data ((driver lytro-dl-status) data)
  (if (string= "[HeartbeatTick" data :end1 8 :end2 8)
      (format t "H")
      (progn
        (format t "client ~s got data (~s) :~%~s~%" (name driver)
                (length data)
               data)
        (loop for c across data
              do (format t "~2,'0x " (char-code c)))))
  (when *thunk*
    (funcall (pop *thunk*))))


(defun send-command (stream command subcommand &key (flags 0) payload parameters length)
  (let ((s (vector #xaf #x55 #xaa #xfa
                   #x00 #x00 00 #x00
                   00 00 #x00 #x00
                   0 0
                   00 00 00 00 00 00 00 00 00 00 00 00 00 00)))
    (setf (aref s 12) (ldb (byte 8 0) command)
          (aref s 13) (ldb (byte 8 8) command)
          (aref s 14) subcommand)
    (when (and payload (not length))
      (setf length (length payload)))
    (when payload
      (assert (<= length (length payload))))
    (when length
      (loop for i from 4 below 8
            for j from 0 by 8
            do (setf (aref s i) (ldb (byte 8 j) length))))
    (when flags
      (loop for i from 8 below 12
            for j from 0 by 8
            do (setf (aref s i) (ldb (byte 8 j) flags))))
    (loop for i from 15 below 28
          for p in parameters
          do (setf (aref s i) p))
    (format t "send command ~4,'0x (~2,'0x) flags ~8,'0x parameters ~s length ~s~%" command subcommand flags parameters length)
    (write-sequence (print (coerce s '(vector (unsigned-byte 8)))) stream)
    (when payload
      (write-sequence (coerce payload '(vector (unsigned-byte 8))) stream :end length))))

(defun gather (thunk count)
  (let ((chunks nil))
    (lambda (data offset length)
      ;; fixme: if we are making copies and combining result at end,
      ;; just preallocate and copy into final buffer...
      (push (subseq data offset (+ offset length))
            chunks)
      (when (zerop (decf count))
        (let ((b (apply 'concatenate '(vector (unsigned-byte 8))
                        (reverse chunks))))
          (typecase thunk
            (function
             (funcall thunk b 0 (length b)))
            (stream
             (write-sequence b thunk))
            (t (download-chunk thunk b 0 (length b)))))))))


(defmethod process-command-data ((command (eql #x00c6)) (subcommand (eql 0))
                                 ;; 'c6 00 00 = query content length'
                                 parameters length flags
                                 data offset)
  (format t "query content length, payload length = ~s, length = ~s~%"
          length (when (plusp length) (u32 data offset)))
  (unless (plusp length)
    (setf *current-download* nil)
    (return-from process-command-data nil))
  (let ((dl-size (u32 data offset))
        (chunk-size (expt 2 20)));; 1MB/chunk for now
    (when (zerop dl-size)
      (format t "~& no data to download...~%")
      (when *close-stream*
        (close *close-stream*)
        (setf *close-stream* nil))
      (setf *current-download* nil)
      (return-from process-command-data nil))
    (when (> dl-size chunk-size)
      (setf *current-download*
            (gather *current-download*
                    (ceiling dl-size chunk-size))))
    (loop for i below dl-size by chunk-size
          do (send-command conserv.tcp:*tcp-client*
                           #xc4 0 :flags 1 :length chunk-size
                                  :parameters (list (ldb (byte 8 0) i)
                                                    (ldb (byte 8 8) i)
                                                    (ldb (byte 8 16) i)
                                                    (ldb (byte 8 24) i))))))

(defun download-response (stream)
  ;; send command
  (send-command stream #xc6 00 :flags 1 :length 4))


(defmethod process-command-data ((command (eql #xc2)) (subcommand (eql 1))
                                 ;; 'c2 00 01 = load file
                                 parameters length flags
                                 data offset)
  (format t "got response to load file command:~%")
  (when (zerop length)
    (format t "  file not found?")
    (when *close-stream*
      (close *close-stream*)
      (setf *close-stream* nil))
    (setf *current-download* nil)
    (return-from process-command-data nil))
  (when (plusp length)
    (format t "  found file ~s~%" (stringz data offset length)))
  (download-response *control*))


(defun run-event-loop ()
  (setf *thunk* nil)
  (sb-concurrency:receive-pending-messages *worklist*)

  (conserv:with-event-loop ()
    (let* ((control-client (make-instance 'lytro-dl :name "control"))
           (callback-client (make-instance 'lytro-dl-status :name "callback")))
      (setf (socket control-client)
            (conserv.tcp::tcp-connect control-client
                                      (iolib:make-address #(10 100 1 1))
                                      ;;(iolib:make-address #(192 168 1 1))
                                      :port *control-port*
                                      :external-format-out :iso-8859-1
                                      :external-format-in :iso-8859-1))
      (setf (socket callback-client)
            (conserv.tcp::tcp-connect callback-client
                                      (iolib:make-address #(10 100 1 1))
                                      :port *callback-port*))
      (setf *control* (socket control-client))
      (setf *callback* (socket callback-client))))
  (format t "exited~%"))


#++
(defmethod process-command-data ((command (eql #x00c6)) (subcommand (eql 0))
                                 ;; 'c6 00 00 = query content length'
                                 parameters length flags
                                 data offset)
  (format t "query content length, payload length = ~s, length = ~s~%"
          length (when (plusp length) (u32 data offset)))
  ;;(assert (plusp length))
  (when (plusp length)
    (let ((dl-size (u32 data offset)))
      (loop with chunk-size = (expt 2 20) ;; 1MB/chunk for noe
            for i below dl-size by chunk-size
            do (send-command conserv.tcp:*tcp-client*
                             #xc4 0 :flags 1 :length dl-size))))
  (when (zerop length)
    (when *close-stream* (close *close-stream*))
    (setf *current-download* nil)))


(defun decode-picture-list (data offset length thunk &optional header-thunk)
  (let ((end (+ offset length)))
    (symbol-macrolet ((u32 (prog1 (u32 data offset) (incf offset 4)))
                      (f32 (prog1 (ieee-floats:decode-float32 (u32 data offset))
                             (incf offset 4)))
                      (u16 (prog1 (u16 data offset) (incf offset 2)))
                      (u8 (prog1 (aref data offset) (incf offset 1)))
                      (s8 (prog1 (stringz data offset 8) (incf offset 8)))
                      (s24 (prog1 (stringz data offset 24) (incf offset 24)))
                      (s48 (prog1 (stringz data offset 48) (incf offset 48))))
      (funcall (or header-thunk #'identity)
               (loop repeat 23 collect u32))
      (loop while (< offset end)
            do (funcall thunk
                        (list :folder-suffix s8 :file-prefix s8
                              :folder-number u32 :file-number u32
                              :?1 u32 :?2 u32 :?3 u32 :?4 u32
                              :liked u32 :last-lambda f32
                              :picture-id s48 :picture-date s24
                              :?5 u32 :rotation u32))))))

(defmethod download-chunk ((stream (eql :#xc20002)) data offset length)
  ;; 'list pictures' download data
  (format t "got picture list packet: ~%")
  (decode-picture-list data offset length
                       (lambda (a)
                         (format t "  ~s~%" a))
                       (lambda (a)
                         (format t "  ~{(~2,'0x)~^ ~}" a))
)
  t)

#++
(run-event-loop)


#++
(push
      *thunk*)

#++
(push (lambda ()
        (setf *partial-packet* nil)
        (push :hex *current-download*)
        (send-command *control* #xc2 6)
        (download-response *control*))
      *thunk*)

(setf *current-download* nil)

(defun cstring (string)
  (let ((o (babel:string-to-octets string)))
    (concatenate '(vector (unsigned-byte 8)) o (list 0))))

#++
(defparameter *dump* (open "/tmp/dump.jpg" :direction :output :element-type '(unsigned-byte 8)))
#++
(close *dump*)
#++
(finish-output *dump*)

#++
(sb-concurrency:send-message *worklist*
                             (lambda ()
                               (setf *current-download* :#xc20002)
                               ;(setf *current-download* *dump*)
                               (send-command *control* #xc2 2)
                               (download-response *control*)
                               ))

#++
(progn
  ;; get current camera time
  #++
  (sb-concurrency:send-message *worklist*
                               (lambda ()
                                 (send-command *control* #xc6 3 :flags 1
                                                                :length #x10)))
  ;; download camera info
  #++
  (sb-concurrency:send-message *worklist*
                               (lambda ()
                                 (setf *current-download* :#xc20000)
                                 (send-command *control* #xc2 0
                                               ;; returns a bunch of
                                               ;; junk if receive
                                               ;; buffer size is set?
                                               ;; :flags 1 :length #x200000
                                               )
                                 (download-response *control*)))
  ;; load file
  #++
  (sb-concurrency:send-message *worklist*
                               (lambda ()
                                 (setf *current-download* :text)
                                 (send-command *control* #xc2 1
                                               :payload
                                               #++(cstring "c:\\t1calib\\mlacalibration.txt")
                                               (cstring "i:/3b.3b"))
                                 (download-response *control*)
                                 ))

  ;; download logs?
  #++
  (sb-concurrency:send-message *worklist*
                               (lambda ()
                                 (setf *current-download* *dump*)
                                 (send-command *control* #xc2 8)
                                 (download-response *control*)
                                 ))
  #++
  (sb-concurrency:send-message *worklist*
                               (lambda ()
                                 (setf *current-download* :text)
                                 (send-command *control* #xc2 1
                                               :parameters '(1)
                                               :payload
                                               (cstring "i:\\3b.3b"))
                                 (download-response *control*)
                                 ))

  (sb-concurrency:send-message *worklist*
                               (lambda ()
                                 (setf *current-download* *dump*)
                                 (send-command *control* #xc2 1
                                               ;:parameters '()
                                               :payload
                                               #++(cstring "c:/t1calib/mlacalibration.txt")
                                               (cstring
                                                ;"i:/dcim/100photo/img_0003.raw"
                                                "i:/dcim/100photo/img_0027.jpg"
))
                                 ;(download-response *control*)
                                 ))
  ) 

#++
(sb-concurrency:receive-pending-messages *worklist*)

#++
(progn
  (sb-concurrency:send-message *worklist* (lambda ()
         (conserv.tcp::exit-event-loop :delay 1))
       )
  (push (lambda ()
          (conserv.tcp::exit-event-loop :delay 1))
       *thunk*))

(defparameter *lytro-dir* #P"~/lytro/")
(defparameter *current-camera* nil)
(defparameter *camera-dir* nil)
(defparameter *new-pictures* nil)

(defun run-download (&optional (*lytro-dir* *lytro-dir*))
  (sb-concurrency:receive-pending-messages *worklist*)
  (when *close-stream*
    (close *close-stream*))
  (setf *current-download* nil)
  (setf *partial-packet* nil)

  ;; get camera data, make sure camera dir exists
  (send-message *worklist*
                (lambda ()
                  (format t "info?~%")
                  (setf *current-download*
                        (lambda (data offset length)
                          (assert (>= length #x284))
                          (flet ((d (s size)
                                   (stringz data (+ s offset) size)))
                            (let ((manufacturer (d 0 #x100))
                                  (serial (d #x100 #x80))
                                  (build (d #x180 #x80))
                                  (software-version (d #x200 #x80))
                                  (?? (subseq data (+ offset #x280)
                                              (+ offset #x284))))
                              (format t "got info packet: ~s, ~s, ~s, ~s, ~s~%"
                                      manufacturer serial build software-version
                                      ??)
                              (setf *current-camera* serial)
                              (setf *camera-dir*
                                    (merge-pathnames
                                     (format nil "~a/" *current-camera*)
                                     *lytro-dir*))
                              (format t "camera dir = ~s~%" *camera-dir*)
                              (ensure-directories-exist *camera-dir*)
                              t))))
                  (send-command *control* #xc2 0)
                  (download-response *control*)))
  ;; get list of pictures, schedule download for any that aren't
  ;; downloaded yet
  (labels ((dl-file (camera-path fs-path)
             (when (probe-file fs-path)
               (format t "already downloaded ~s @ ~s~%" camera-path fs-path)
               (return-from dl-file t))
             (format t "scheduling download of ~s to ~s~%" camera-path fs-path)
             (send-message *worklist*
                           (lambda ()
                             (format t "starting download of ~s to ~s~%" camera-path fs-path)
                             (unless (probe-file fs-path)
                               (setf *current-download*
                                     (open fs-path
                                           :element-type '(unsigned-byte 8)
                                           :direction :output))
                               (setf *close-stream* *current-download*)
                               (send-command *control* #xc2 1
                                             :payload (cstring camera-path))
                               #++(download-response *control*)))))
           (dl-picture (info)
             (format t " got pic ~s~%" info)
             (let* ((id (getf info :picture-id))
                    (pic-dir (merge-pathnames (format nil "~a/" id)
                                              *camera-dir*))
                    (base-name (format nil "i:\\DCIM\\~3,'0d~a\\~a~4,'0d."
                                       (getf info :folder-number)
                                       (getf info :folder-suffix)
                                       (getf info :file-prefix)
                                       (getf info :file-number))))
               (format t "picture ~s in ~s: = ~s~%"
                       id pic-dir (probe-file pic-dir))
               (ensure-directories-exist pic-dir)
               (when (probe-file pic-dir)
                 (format t " downloading ~s to ~s~%" base-name pic-dir)
                 (loop for filetype in '("jpg" "raw" "txt" "128" "stk")
                       for file = (format nil "~a~a" base-name filetype)
                       do (dl-file file
                                   (merge-pathnames
                                    (format nil "~3,'0d-~4,'0d.~a"
                                            (getf info :folder-number)
                                            (getf info :file-number)
                                            filetype)
                                    pic-dir)))
                 (unless (probe-file (merge-pathnames "metadata" pic-dir))
                   (with-open-file  (md (merge-pathnames "metadata" pic-dir)
                                        :direction :output)
                     (write info :stream md))
                   )))))

    (send-message *worklist*
                 (lambda ()
                   (setf *current-download*
                         (lambda (data offset length)
                           (format t "got picture list packet: ~%")
                           (decode-picture-list data offset length
                                                #'dl-picture
                                                (lambda (a)
                                                  (format t "  ~{(~2,'0x)~^ ~}" a)))
                           t))

                   (send-command *control* #xc2 2)
                   (download-response *control*))))

  )

#++
(close *close-stream*)
#++
(setf *current-download* nil)

#++
(run-event-loop)
#++
(run-download)
#++
(progn
  (sb-concurrency:send-message *worklist* (lambda ()
         (conserv.tcp::exit-event-loop :delay 1))
       )
  (push (lambda ()
          (conserv.tcp::exit-event-loop :delay 1))
       *thunk*))
