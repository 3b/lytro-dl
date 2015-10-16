(in-package #:lytro-dl)

(defparameter *lytro-dir* #P"~/lytro/")
(defparameter *current-camera* nil)
(defparameter *camera-dir* nil)
(defparameter *new-pictures* nil)


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

(defun cstring (string)
  (let ((o (babel:string-to-octets string)))
    (concatenate '(vector (unsigned-byte 8)) o (list 0))))



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
