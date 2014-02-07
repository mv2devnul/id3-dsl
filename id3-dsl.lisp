(in-package #:id3-dsl)

(defparameter *dbg* nil)
(defparameter *current-file* nil)

;;;; Generic methods/functions/etc used by all kinds of ID3 tags/frames/etc

;;; Signaled when we read a 0 when expecting a character
;;; Means that we've hit padding in the tag
(define-condition in-padding () ())

;;;; ID3 Strings
;;; Note: I've thrown away much of the original PCL code here because I wanted
;;; 1) provide round-trip consistency on reads/writes (hence, the introduction
;;; of ID3-STRINGS), and 2) use the most excellent FLEXI-STREAMS so as to minimize
;;; the work on my part to implement the two ID3 strings PCL did not provide: UCS-2BE,
;;; and UTF-8.

;;; ID3 strings are strange and wonderful (not really) beasties.
;;; They come in 4 major flavors, with 2 minor types:
;;; terminated (via #x0) and unterminated (a length
;;; is supplied in some other way:
;;;
;;; 0: ISO-8859-1 string -- An "ASCII" string
;;; 1: UCS-encoded string -- 16-bit code-point, prepended with a byte-order mark (BOM)
;;; 2: UCS, big-endian string -- 16-bit code-point, no BOM
;;; 3: UTF-8 encoded string -- variable-byte length encoding
;;;
;;; To preserve round-trip transparency, we define a structure that captures
;;; how a string is read in from a file:
;;; KIND: one of the above
;;; BOM: if string was a UCS-2, the read-in BOM
;;; STRING : the actual string
;;;
;;; ID3-STRINGS are immutable in the sense that none of the slots
;;; have (setf) methods: if you want to change and ID3-STRING, you should
;;; create a new one via CHANGE-ID3-STRING-TO below.  This is done so we can
;;; "catch" changing of an ID3-STRING in a frame and update the frame's size
;;; accordingly.
(defclass id3-string ()
  ((kind      :reader id3-string-kind      :initarg :kind      :initform nil)
   (bom       :reader id3-string-bom       :initarg :bom       :initform nil)
   (string    :reader id3-string-string    :initarg :string    :initform nil)
   (terminate :reader id3-string-terminate :initarg :terminate :initform nil)))

(defmethod print-object ((s id3-string) stream)
  (with-slots (kind bom string terminate) s
    (format stream "~a/~a:<~a>:~a" kind bom string terminate)))

;;; Convenience function
(defun make-id3-string (&key kind bom string terminate)
  (make-instance 'id3-string
                 :kind kind
                 :bom bom
                 :string string
                 :terminate terminate))

;;; In general, you shouldn't be changing anything about an ID3-STRING
;;; except it's STRING slot. In addition, we treat ID3-STRINGs as immutable,
;;; not providing any writer access methods.  The rationale for this is that
;;; we want to be able to "automatically" update the size of a frame when
;;; one of it's variable length slots are changed.  We catch this change
;;; by supplying an auto-generated set of :AFTER methods on the (SETF)
;;; methods for the variable slots.
;;;
;;; The upshot is that if you want to change the value of an ID3-STRING
;;; in a frame, you follow this protocol (Assume foo is a COMM-FRAME-V2.3,
;;; and we want to change the TEXT slot):
;;;
;;; (setf (text foo) (change-string-to (text-foo) "The New Description"))
;;;
;;; Klunky, but workable.
(defmethod change-id3-string-to ((orig id3-string) new)
  (with-slots (kind bom terminate) orig
    (make-id3-string :kind kind
                     :bom bom
                     :string new
                     :terminate terminate)))

;;; Convenience string/ID3-STRING compare methods that allow us to
;;; intermix on compares.
(defmethod str= ((s1 string) (s2 id3-string))
  (string= s1 (id3-string-string s2)))

(defmethod str= ((s1 id3-string) (s2 id3-string))
  (string= (id3-string-string s1) (id3-string-string s2)))

(defmethod str= ((s1 id3-string) (s2 string))
  (string= (id3-string-string s1) s2))

;;; Function to read in a null (#x0) terminated "single-byte",
;;; where single byte is ISO-8859-1 or UTF-8
(defun read-single-byte-loop (instream)
  (flex:with-output-to-sequence (out :element-type 'octet)
    (do ((b (read-value 'u1 instream)
            (read-value 'u1 instream)))
        (nil)
      (when (zerop b)
        (return))            ; leave loop w/o writing
      (write-byte b out))))

;;; Function to read in UCS-16 string (terminated by #x00 #x00)
(defun read-double-byte-loop (instream)
  (flex:with-output-to-sequence (out :element-type 'octet)
    (do* ((b0 (read-value 'u1 instream)
              (read-value 'u1 instream))
          (b1 (read-value 'u1 instream)
              (read-value 'u1 instream)))
         (nil)
      (when (and (zerop b0) (zerop b1))
        (return))
      (write-byte b0 out)
      (write-byte b1 out))))

(defun read-id3-string (kind length instream)
  "Read in an ID3 string based on it's KIND
If KIND is UCS-2, we store the read-in BOM so we can write it out properly.
If LENGTH is 0, then we read in a terminated string, else we read in LENGTH
characters"
  (let* ((octets)
         (term)
         (bom)
         (ns))

    (dbg *dbg* 'read-id3-string-entry kind length (file-position instream))

    (when (eq kind 'ucs-2)
      (if length
          (decf length 2))              ; account for BOM read-in below

      (setf bom (read-value 'u2 instream))
      (setf kind (ecase bom
                   (#xfeff 'ucs-2be)
                   (#xfffe 'ucs-2le))))

    (cond
      ((null length)                    ; reading a terminated-string
       (setf term t)                    ; remember to terminate on write
       (setf octets (cond
                      ((or (eq 'ucs-2le kind)
                           (eq 'ucs-2be kind))
                       (read-double-byte-loop instream))
                      ((or (eq 'iso-8859-1 kind)
                           (eq 'utf-8 kind))
                       (read-single-byte-loop instream))
                      (t
                       (error "unknown kind: ~a" kind)))))
      (t                                ; reading a non-terminated string
       (setf octets (make-octets length t))
       (read-sequence octets instream)))

    ;; NB: in both cases from above, OCTETS is adjustable.
    (let ((end (length octets)))
      (when (and (or (eq 'ucs-2le kind)
                     (eq 'ucs-2be kind))
                 (oddp end))
        ;; I have found some MP3's UCS-2 encoded strings that throw in an extra #x0
        ;; for a non-terminated string, or conversely, only use one #x0 to
        ;; terminate a UCS-2 string.
        ;; Propagate up to adjust frame's size and add another #x0
        (warn-user "File:~a, pos = ~:d~%Odd length for UCS string (~a), terminated = ~a~%"
                   *current-file*
                   (file-position instream)
                   octets
                   term)
        ;; if terminated, add another #x0 to the end, else drop the extraneous byte.
        ;; NB: we will fix up the enclosing frame's side on write via calc-frame-size
        (if term
            (progn
              (vector-push-extend (1+ end) octets)
              (setf (aref octets end) #x0)
              (incf end))
            (decf end)))

      (dbg *dbg* 'read-id3-string-making-ns bom term kind octets)
      (setf ns (make-id3-string
                :bom bom :terminate term :kind kind
                :string (flex:octets-to-string octets :external-format (make-keyword kind) :end end)))
      (dbg *dbg* 'read-id3-string-returning ns)
      ns)))

;;; And now, the 4 ID3-STRING types
(define-binary-type iso-8859-1 (length)
  (:reader (instream)
           (read-id3-string 'iso-8859-1 length instream))
  (:writer (out string)
    (let ((octets (flex:string-to-octets (id3-string-string string) :external-format :iso-8859-1)))
      (write-sequence octets out)
      (if (id3-string-terminate string)
          (write-value 'u1 out 0)))))

(define-binary-type ucs-2 (length)
  (:reader (instream)
   (read-id3-string 'ucs-2 length instream))
  (:writer (out string)
    (let ((octets (flex:string-to-octets
                   (id3-string-string string)
                   :external-format (ecase (id3-string-bom string)
                                      (#xfeff :ucs-2be)
                                      (#xfffe :ucs-2le)))))
      (write-value 'u2 out (id3-string-bom string))
      (write-sequence octets out)
      (if (id3-string-terminate string)
          (write-value 'u2 out 0)))))

(define-binary-type ucs-2be (length)
  (:reader (instream)
           (read-id3-string 'ucs-2be length instream))
  (:writer (out string)
           (let ((octets (flex:string-to-octets
                          (id3-string-string string)
                          :external-format :ucs-2be)))
             (write-sequence octets out)
             (if (id3-string-terminate string)
                 (write-value 'u2 out 0)))))

(define-binary-type utf-8 (length)
  (:reader (instream)
           (read-id3-string 'utf-8 length instream))
  (:writer (out string)
           (let ((octets (flex:string-to-octets
                          (id3-string-string string)
                          :external-format :utf-8)))
             (write-sequence octets out)
             (if (id3-string-terminate string)
                 (write-value 'u1 out 0)))))

;;; Convert ID3 symbolic encoding from number to symbol
(defun get-id3-encoding (encoding)
  (ecase encoding
    (0 'iso-8859-1)
    (1 'ucs-2)
    (2 'ucs-2be)
    (3 'utf-8)))

;;; Get the length of an encoded string, where string is an ID3-STRING structure
(defun encoded-string-length (string)
  (let ((characters (+
                     (length (id3-string-string string))
                     (if (id3-string-terminate string) 1 0)
                     (if (id3-string-bom string) 1 0))))
    (* characters (ecase (id3-string-kind string)
                    (iso-8859-1 1)
                    (ucs-2 2)
                    (ucs-2le 2)
                    (ucs-2be 2)
                    (utf-8 1)))))

;;; Get what type string we are to read based on the ENCODING. If LENGTH
;;; is 0, we are reading a terminated string.
(defun string-args (encoding length)
  (values (get-id3-encoding encoding) :length length))

;;; The actual ID3-encoded string type.
;;; ENCODING: see above for valid values (0, 1, 2, 3)
;;; LENGTH: if set, the length of the non-terminated string
(define-binary-type id3-encoded-string (encoding length)
  (:reader (in)
           (multiple-value-bind (type kw len)
               (string-args encoding length)
             (read-value type in kw len)))
  (:writer (out string)
           (multiple-value-bind (type kw len)
               (string-args encoding length)
             (write-value type out string kw len))))

;;; calculate bytes left to read in the tag metadata
(defun bytes-left (bytes-read)
  (let ((ret (- (size (current-binary-object)) bytes-read)))
    ;(dbg *dbg* 'bytes-left ret)
    ret))

;;; Generic wrapper to read a frame: catches an IN-PADDING condition, terminates
;;; and returns nil to signal we are done reading.
(defun read-frame (frame-type in)
  (handler-case
      (read-value frame-type in)
    (in-padding () nil)))

;;; Read an expected frame ID of LENGTH (either 3 for V2.2 or
;;; 4 for V2.3/4.
(define-binary-type frame-id (length)
  (:reader (in)
           (let ((first-byte (read-byte in)))
             (when (= first-byte 0)
               (signal 'in-padding))
             (let ((ns (make-id3-string
                        :terminate nil
                        :kind 'iso-8859-1
                        :string (concatenate
                                 'string
                                 (string (code-char first-byte))
                                 (id3-string-string (read-value 'iso-8859-1 in :length (1- length)))))))
               (dbg *dbg* 'reading-frame-id ns)
               ns)))
  (:writer (out id)
           (write-value 'iso-8859-1 out id :length length)))

;;;  When IF is non-nil, do a READ-VALUE for TYPE
(define-binary-type optional (type if)
  (:reader (in)
           (when if (read-value type in)))
  (:writer (out value)
           (when if (write-value type out value))))

;;; A 32-bit, sync-safe integer. Used primarily for tag/frame sizes
(define-binary-type id3-sync-safe-u32 () (unsigned-integer :bytes 4 :bits-per-byte 7))

;;; ID3 information is stored as (at position 0 of a file):
;;; identifier: 3 bytes == "ID3"
;;; major-version: 1 byte and one of 2, 3, or 4
;;; revision: 1 byte, usually (always?) == 0
;;; flags: 1 byte that indicates additional info like compression, etc
;;; size: a sync-safe, 32-bit integer
;;; All of this is then followed by the actually frames themselves
(define-tagged-binary-class generic-id3-tag ()
  ((identifier     (iso-8859-1 :length 3))
   (major-version  u1)
   (revision       u1)
   (flags          u1)
   (size           id3-sync-safe-u32))
  (:dispatch
   (ecase major-version
     (2 'id3v2.2-tag)
     (3 'id3v2.3-tag)
     ;;(4 'id3v2.4-tag) ; XXX - todo
     )))

(defgeneric data-bytes (frame))
(defgeneric frame-header-size (frame))

;;; Class to read in a sequence of raw bytes (data in PIC/APIC frames,
;;; unknown frames, etc.
(define-binary-type raw-bytes (size)
  (:reader (in)
           (let ((buf (make-octets size)))
             (read-sequence buf in)
             buf))
  (:writer (out buf)
           (write-sequence buf out)))

;;;; The "Generics": abstract classes used for describing real frame classes

;;; XXX General question: for counters, like POP-FRAMES, do we want to define
;;; increment/decrement methods to hide counter expansion/contraction?

;;; method to get length of a variable-length field
(defgeneric get-length (obj))
(defmethod get-length ((str id3-string)) (encoded-string-length str))
(defmethod get-length ((o sequence)) (length o))

;;; Since ID3 frames have variable length fields, we need to update
;;; their size when those variable-length fields are changed. This macro
;;; automates that, generating an :AFTER method, taking the CLASS-NAME we
;;; specialize on for the method, the total FIXED-LEN (non-variable length),
;;; and finally the variable-length fields in the frame.
;;;
;;; Also generated is the CALC-FRAME-SIZE method that can be
;;; used to ensure the frame size corresponds to the actual contents of the frame.
;;;
;;; Use this macro after every frame class.
(defgeneric calc-frame-size (frame))

(defmacro generate-after-methods (class-name fixed-len &rest slots)
  (let ((methods))
    (dolist (field slots)
      (push `(defmethod (setf ,field) :after (new-val (frame ,class-name))
               (declare (ignore new-val))
               (with-slots (size) frame
                 (setf size (calc-frame-size frame))))
            methods))
    (push `(defmethod calc-frame-size ((frame ,class-name))
             (with-slots (size ,@slots) frame
               (+ ,fixed-len (loop for s in (list ,@slots)
                                   summing (get-length s) into total
                                   finally (return total)))))
          methods)
    (push 'progn methods)
    methods))

;;; Don't try to grok the frame, just read in its payload.
(define-binary-class raw-frame ()
  ((data (raw-bytes :size (data-bytes (current-binary-object))))))
(generate-after-methods raw-frame 0 raw-bytes)

(define-binary-class generic-comment-frame ()
  ((encoding    u1)
   (language    (iso-8859-1 :length 3))
   (description (id3-encoded-string :encoding encoding))
   (text        (id3-encoded-string
                 :encoding encoding
                 :length (bytes-left
                          (+ 1          ; encoding
                             3          ; language
                             (encoded-string-length description)))))))
(generate-after-methods generic-comment-frame 4 description text)

(define-binary-class generic-text-info-frame ()
  ((encoding    u1)
   (information (id3-encoded-string
                 :encoding encoding
                 :length (bytes-left 1)))))
(generate-after-methods generic-text-info-frame 1 information)

(define-binary-class generic-desc-value-frame ()
  ((encoding    u1)
   (description (id3-encoded-string :encoding encoding))
   (value       (id3-encoded-string
                 :encoding encoding
                 :length (bytes-left (+ 1 ; encoding
                                        (encoded-string-length description)))))))
(generate-after-methods generic-desc-value-frame 1 description value)

(define-binary-class generic-url-link-frame ()
  ((url (iso-8859-1 :length (bytes-left 0)))))
(generate-after-methods generic-url-link-frame 0 url)


(define-binary-class generic-ufid-frame ()
  ((owner-id   (iso-8859-1))
   (identifier (raw-bytes
                :size (bytes-left (encoded-string-length owner-id))))))
(generate-after-methods generic-ufid-frame 0 owner-id identifier)

(define-binary-class generic-time-stamp-event-frame ()
  ((time-stamp-format u1)
   (type-of-event     u1)
   (time-stamp        (raw-bytes :size (bytes-left (+ 1 1))))))
(generate-after-methods generic-time-stamp-event-frame 2 time-stamp)

;; (define-binary-class generic-mpeg-lookup-table-frame () ; XXX WRONG
;;   ((frames-between-reference u2)
;;    (bytes-between-reference  u3)
;;    (bits-for-bytes-deviation u1)
;;    (bits-for-ms-deviation    u1)))


(define-binary-class generic-synced-tempo-code-frame ()
  ((time-stamp-format u1)
   (tempo-data        (raw-bytes :size (bytes-left 1)))))
(generate-after-methods generic-synced-tempo-code-frame 1 tempo-data)

(define-binary-class generic-unsynced-lyrics-frame ()
 ((encoding           u1)
  (language           (iso-8859-1 :length 3))
  (content-descriptor (id3-encoded-string :encoding encoding))
  (lyrics             (id3-encoded-string
                       :encoding encoding
                       :length (bytes-left (+ 1
                                              3
                                              (encoded-string-length content-descriptor)))))))
(generate-after-methods generic-unsynced-lyrics-frame 4 content-description lyrics)

(define-binary-class generic-synced-lyrics-frame ()
  ((encoding           u1)
   (language           (iso-8859-1 :length 3))
   (time-stamp-format  u1)
   (content-type       u1)
   (content-descriptor (id3-encoded-string
                        :encoding encoding
                        :length (bytes-left (+ 1
                                               3
                                               1
                                               1))))))
(generate-after-methods generic-synced-lyrics-frame 6 content-descriptor)

(define-binary-class generic-pop-frame ()
  ((user-email (iso-8859-1))
   (rating     u1)
   (counter    (raw-bytes :size
                          (bytes-left (+ (encoded-string-length user-email)
                                         1))))))
(generate-after-methods generic-pop-frame 1 user-email counter)

(define-binary-class generic-buf-frame ()
  ((buffer-size         u3)
   (embedded-info-flags u1)
   (offset-to-next-tag  u4)))
(generate-after-methods generic-buf-frame 8)

(define-binary-class generic-link-frame ()
  ((link-url           (iso-8859-1))
   (additional-id-data (iso-8859-1))))
(generate-after-methods generic-link-frame 0 link-url additional-id-data)

;;;; ID3 version 2.1 tag

;;; If present, it is found 128 bytes from the end of a file.
;;; It starts with "TAG", the contains 4 30-byte fields denoting the
;;; title of the file, the performing artist, the album of the file,
;;; and a comment.  In addition, the V21 tag contains a 4-byte string
;;; denoting the year the title was recorded, and a 1-byte field denoting
;;; the genre of the song.
;;;
;;; V21 tags are obsolete and, in general, should not be used or updated;
;;; however, you will still run across these in the wild, and it's very
;;; easy to implement.
(define-binary-class id3-v2.1-tag ()
  ((tag     (iso-8859-1 :length 3))
   (title   (iso-8859-1 :length 30))
   (artist  (iso-8859-1 :length 30))
   (album   (iso-8859-1 :length 30))
   (year    (iso-8859-1 :length 4))
   (comment (iso-8859-1 :length 30)) ; the last two bytes of COMMENT can be used as a track number
   (genre   u1)))

;;;; NB: in listing the super classes of a frame class, ORDER IS IMPORTANT!
;;;; You MUST list the "generic" class first, then the id3v2.x class second.
;;;; If you don't, then on write, the data in the frame will be written out before
;;;; the frame header.  This is true on SBCL/ABCL/CCL/CLISP.  I have NOT tested it on
;;;; other Lisps.

;;;; "RAW" frames: just slurp in the frame contents as octets
(define-binary-class raw-frame-v2.2 (raw-frame id3v2.2-frame) ())
(define-binary-class raw-frame-v2.3 (raw-frame id3v2.3-frame) ())

;;;; Concrete frames, listed by the number found in the ID3 "specs"
;;; 4.1
(define-binary-class ufi-frame-v2.2 (generic-ufid-frame id3v2.2-frame) ())
(define-binary-class ufid-frame-v2.3 (generic-ufid-frame id3v2.3-frame) ())

;;; 4.2
(define-binary-class text-info-frame-v2.2 (generic-text-info-frame id3v2.2-frame) ())
(define-binary-class text-info-frame-v2.3 (generic-text-info-frame id3v2.3-frame) ())

;;; Known text-info frames
(define-binary-class tal-frame-v2.2 (text-info-frame-v2.2) ())
(define-binary-class tbp-frame-v2.2 (text-info-frame-v2.2) ())
(define-binary-class tcm-frame-v2.2 (text-info-frame-v2.2) ())
(define-binary-class tco-frame-v2.2 (text-info-frame-v2.2) ())
(define-binary-class tcr-frame-v2.2 (text-info-frame-v2.2) ())
(define-binary-class tda-frame-v2.2 (text-info-frame-v2.2) ())
(define-binary-class tdy-frame-v2.2 (text-info-frame-v2.2) ())
(define-binary-class ten-frame-v2.2 (text-info-frame-v2.2) ())
(define-binary-class tft-frame-v2.2 (text-info-frame-v2.2) ())
(define-binary-class tim-frame-v2.2 (text-info-frame-v2.2) ())
(define-binary-class tke-frame-v2.2 (text-info-frame-v2.2) ())
(define-binary-class tla-frame-v2.2 (text-info-frame-v2.2) ())
(define-binary-class tle-frame-v2.2 (text-info-frame-v2.2) ())
(define-binary-class tmt-frame-v2.2 (text-info-frame-v2.2) ())
(define-binary-class toa-frame-v2.2 (text-info-frame-v2.2) ())
(define-binary-class tof-frame-v2.2 (text-info-frame-v2.2) ())
(define-binary-class tol-frame-v2.2 (text-info-frame-v2.2) ())
(define-binary-class tor-frame-v2.2 (text-info-frame-v2.2) ())
(define-binary-class tot-frame-v2.2 (text-info-frame-v2.2) ())
(define-binary-class tp1-frame-v2.2 (text-info-frame-v2.2) ())
(define-binary-class tp2-frame-v2.2 (text-info-frame-v2.2) ())
(define-binary-class tp3-frame-v2.2 (text-info-frame-v2.2) ())
(define-binary-class tp4-frame-v2.2 (text-info-frame-v2.2) ())
(define-binary-class tpa-frame-v2.2 (text-info-frame-v2.2) ())
(define-binary-class tpb-frame-v2.2 (text-info-frame-v2.2) ())
(define-binary-class trc-frame-v2.2 (text-info-frame-v2.2) ())
(define-binary-class trd-frame-v2.2 (text-info-frame-v2.2) ())
(define-binary-class trk-frame-v2.2 (text-info-frame-v2.2) ())
(define-binary-class tsi-frame-v2.2 (text-info-frame-v2.2) ())
(define-binary-class tss-frame-v2.2 (text-info-frame-v2.2) ())
(define-binary-class tt1-frame-v2.2 (text-info-frame-v2.2) ())
(define-binary-class tt2-frame-v2.2 (text-info-frame-v2.2) ())
(define-binary-class tt3-frame-v2.2 (text-info-frame-v2.2) ())
(define-binary-class txt-frame-v2.2 (text-info-frame-v2.2) ())
(define-binary-class tye-frame-v2.2 (text-info-frame-v2.2) ())

(define-binary-class talb-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tbpm-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tcom-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tcon-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tcop-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tdat-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tden-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tdly-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tdor-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tdrc-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tdrl-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tdtg-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tenc-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class text-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tflt-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class time-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tipl-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tit1-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tit2-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tit3-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tkey-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tlan-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tlen-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tmcl-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tmed-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tmoo-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class toal-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tofn-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class toly-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tope-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tory-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class town-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tpe1-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tpe2-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tpe3-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tpe4-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tpos-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tpro-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tpub-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class trck-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class trda-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class trsn-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class trso-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tsiz-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tsoa-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tsop-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tsot-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tsrc-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tsse-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tsst-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tyer-frame-v2.3 (text-info-frame-v2.3) ())

;;; 4.2.2
(define-binary-class txx-frame-v2.2 (generic-desc-value-frame id3v2.2-frame) ())
(define-binary-class txxx-frame-v2.3 (generic-desc-value-frame id3v2.3-frame) ())

;;; 4.3
(define-binary-class url-link-frame-v2.2 (generic-url-link-frame id3v2.2-frame) ())
(define-binary-class url-link-frame-v2.3 (generic-url-link-frame id3v2.3-frame) ())

(define-binary-class wxx-frame-v2.2 (generic-desc-value-frame id3v2.2-frame) ())
(define-binary-class wxxx-frame-v2.3 (generic-desc-value-frame id3v2.3-frame) ())

(define-binary-class waf-frame-v2.2 (url-link-frame-v2.2) ())
(define-binary-class war-frame-v2.2 (url-link-frame-v2.2) ())
(define-binary-class was-frame-v2.2 (url-link-frame-v2.2) ())
(define-binary-class wcm-frame-v2.2 (url-link-frame-v2.2) ())
(define-binary-class wcp-frame-v2.2 (url-link-frame-v2.2) ())
(define-binary-class wpb-frame-v2.2 (url-link-frame-v2.2) ())

(define-binary-class wcom-frame-v2.3 (url-link-frame-v2.3) ())
(define-binary-class wcop-frame-v2.3 (url-link-frame-v2.3) ())
(define-binary-class woaf-frame-v2.3 (url-link-frame-v2.3) ())
(define-binary-class woar-frame-v2.3 (url-link-frame-v2.3) ())
(define-binary-class woas-frame-v2.3 (url-link-frame-v2.3) ())
(define-binary-class wors-frame-v2.3 (url-link-frame-v2.3) ())
(define-binary-class wpay-frame-v2.3 (url-link-frame-v2.3) ())
(define-binary-class wpub-frame-v2.3 (url-link-frame-v2.3) ())

;;; 4.4
(define-binary-class ipl-frame-v2.2 (generic-desc-value-frame id3v2.2-frame) ())
(define-binary-class ipls-frame-v2.3 (generic-desc-value-frame id3v2.3-frame) ())

;;; 4.5
(define-binary-class mci-frame-v2.2 (raw-frame-v2.2) ())
(define-binary-class mcdi-frame-v2.3 (raw-frame-v2.3) ())

;;; 4.6
(define-binary-class etc-frame-v2.2 (generic-time-stamp-event-frame id3v2.2-frame) ())
(define-binary-class etco-frame-v2.3 (generic-time-stamp-event-frame id3v2.3-frame) ())

;;; 4.7
(define-binary-class mll-frame-v2.2 (raw-frame-v2.2) ())
(define-binary-class mllt-frame-v2.3 (raw-frame-v2.3) ())
;; (define-binary-class mll-frame-v2.2 (generic-mpeg-lookup-table-frame id3v2.2-frame) ())
;; (define-binary-class mllt-frame-v2.3 (generic-mpeg-lookup-table-frame id3v2.3-frame) ())

;;; 4.8
(define-binary-class stc-frame-v2.2 (generic-synced-tempo-code-frame id3v2.2-frame) ())
(define-binary-class sytc-frame-v2.3 (generic-synced-tempo-code-frame id3v2.3-frame) ())

;;; 4.9
(define-binary-class ult-frame-v2.2 (generic-unsynced-lyrics-frame id3v2.2-frame) ())
(define-binary-class uslt-frame-v2.3 (generic-unsynced-lyrics-frame id3v2.3-frame) ())

;;; 4.10
(define-binary-class slt-frame-v2.2 (generic-synced-lyrics-frame id3v2.2-frame) ())
(define-binary-class sylt-frame-v2.3 (generic-synced-lyrics-frame id3v2.3-frame) ())

;;; 4.11
(define-binary-class com-frame-v2.2 (generic-comment-frame id3v2.2-frame) ())
(define-binary-class comm-frame-v2.3 (generic-comment-frame id3v2.3-frame) ())

;;; 4.12
(define-binary-class rva-frame-v2.2 (raw-frame-v2.2) ())
(define-binary-class rvad-frame-v2.3 (raw-frame-v2.3) ())

;;; 4.13
(define-binary-class equ-frame-v2.2 (raw-frame-v2.2) ())
(define-binary-class equa-frame-v2.3 (raw-frame-v2.3) ())

;;; 4.14
(define-binary-class rev-frame-v2.2 (raw-frame-v2.2) ())
(define-binary-class rvrb-frame-v2.3 (raw-frame-v2.3) ())

;;; 4.15
(define-binary-class pic-frame-v2.2 (id3v2.2-frame)
  ((encoding     u1)
   (image-format u3)
   (picture-type u1)
   (description  (id3-encoded-string :encoding encoding))
   (picture-data (raw-bytes :size
                            (bytes-left (+ 1 ; encoding
                                           3 ; image-format
                                           1 ; picture-type
                                           (encoded-string-length description)))))))
(generate-after-methods pic-frame-v2.2 5 description picture-data)

(define-binary-class apic-frame-v2.3 (id3v2.3-frame)
  ((encoding     u1)
   (mime-type    (iso-8859-1))
   (picture-type u1)
   (description  (id3-encoded-string :encoding encoding))
   (picture-data (raw-bytes :size
                            (bytes-left (+ 1 ; encoding
                                           (encoded-string-length mime-type)
                                           1 ; picture-type
                                           (encoded-string-length description)))))))
(generate-after-methods apic-frame-v2.3 2 mime-type description picture-data)

;;; 4.16
(define-binary-class geo-frame-v2.2 (id3v2.2-frame)
  ((encoding            u1)
   (mime-type           (iso-8859-1))
   (filename            (iso-8859-1))             ; NB: 2.2 spec says this is ISO...
   (content-description (id3-encoded-string :encoding encoding))
   (encapsulated-object (raw-bytes
                         :size (bytes-left (+ 1
                                              (encoded-string-length mime-type)
                                              (encoded-string-length filename)
                                              (encoded-string-length content-description)))))))
(generate-after-methods geo-frame-v2.2 1 mime-type filename content-description encapsulated-object)

(define-binary-class geob-frame-v2.3 (id3v2.3-frame)
  ((encoding            u1)
   (mime-type           (iso-8859-1))
   (filename            (id3-encoded-string :encoding encoding))
   (content-description (id3-encoded-string :encoding encoding))
   (encapsulated-object (raw-bytes
                         :size (bytes-left (+ 1
                                              (encoded-string-length mime-type)
                                              (encoded-string-length filename)
                                              (encoded-string-length content-description)))))))
(generate-after-methods geob-frame-v2.3 1 mime-type filename content-description encapsulated-object)


;;; 4.17
(define-binary-class cnt-frame-v2.2 (raw-frame-v2.2) ())
(define-binary-class pcnt-frame-v2.3 (raw-frame-v2.3) ())

;;; 4.18
(define-binary-class pop-frame-v2.2 (generic-pop-frame id3v2.2-frame) ())
(define-binary-class popm-frame-v2.3 (generic-pop-frame id3v2.3-frame) ())

;;; 4.19
(define-binary-class buf-frame-v2.2 (generic-buf-frame id3v2.2-frame) ())
(define-binary-class rbuf-frame-v2.3 (generic-buf-frame id3v2.3-frame) ())

;;; 4.20
(define-binary-class crm-frame-v2.2 (raw-frame-v2.2) ())
(define-binary-class aenc-frame-v2.3 (raw-frame-v2.3) ())

;;; 4.21 NB v2.2/v2.3 diverge here
(define-binary-class cra-frame-v2.2 (raw-frame-v2.2) ())
(define-binary-class link-frame-v2.3 (generic-link-frame id3v2.3-frame) ())

;;; 4.22
(define-binary-class lnk-frame-v2.2 (generic-link-frame id3v2.2-frame) ())

;;; 4.22 (from here on out only 2.3/2.4 frames)
(define-binary-class poss-frame-v2.3 (id3v.2.3-frame)
  ((time-stamp-format u1)
   (ts-position       (raw-bytes :size (bytes-left 1)))))
(generate-after-methods poss-frame-v2.3 1 ts-position)

;;; 4.23
(define-binary-class user-frame-v2.3 (generic-comment-frame id3v2.3-frame) ())

;;; 4.24
(define-binary-class owne-frame-v2.3 (id3v2.3-frame)
 ((encoding          u1)
   (price-payed      (iso-8859-1))
   (date-of-purchase (iso-8859-1 :length 8))
   (seller           (id3-encoded-string
                      :encoding encoding
                      :length (bytes-left (+ 1
                                             (encoded-string-length price-payed)
                                             (encoded-string-length date-of-purchase)))))))
(generate-after-methods owne-frame-v2.3 9 price-payed seller)

;;; 4.25
(define-binary-class comr-frame-v2.3 (id3v2.3-frame)
  ((encoding          u1)
   (price-string      (iso-8859-1))
   (valid-until       (iso-8869-1 :length 8))
   (contact-url       (iso-8859-1))
   (received-as       u1)
   (name-of-seller    (id3-encoded-string :encoding encoding)) ; optional XXX
   (description       (id3-encoded-string :encoding encoding)) ; optional XXX
   (picture-mime-type (id3-encoding-string))
   (seller-logo       (raw-bytes :size (+ 1 ; encoding
                                          (encoded-string-length price-string)
                                          8 ; defined valid-until field
                                          (encoded-string-length contact-url)
                                          1 ; received-as
                                          (encoded-string-length name-of-seller)
                                          (encoded-string-length description)
                                          (encoded-string-length picture-mime-type))))))
(generate-after-methods comr-frame-v2.3 10 price-string contact-url name-of-seller description picture-mime-type seller-logo)

(define-binary-class generic-registration-data-frame ()
  ((owner-identifier (iso-8859-1))
   (the-symbol       u1)
   (the-data         (raw-bytes
                      :size (bytes-left (+ (encoded-string-length owner-identifier)
                                           1))))))
(generate-after-methods generic-registration-data-frame 1 owner-identifier the-data)

;;; 4.26
(define-binary-class encr-frame-v2.3 (generic-registration-data-frame id3v2.3-frame) ())

;;; 4.27
(define-binary-class grid-frame-v2.3 (generic-registration-data-frame id3v2.3-frame) ())

;;; 4.28
(define-binary-class priv-frame-v2.3 (generic-ufid-frame id3v2.3-frame) ())

;;;; Non-standard frames

;;;; Apple stuff

;;; Apple, in its infinite wisdom, signifies that a file is part of a
;;; compilation with the 'TCM|TCMP' frame.  Ideally, this would be a simple
;;; case of a Text Info frame, but as near as I can tell, Apple puts an
;;; invalid UCS-2 string (ie, they use 01 ff fe fe, where 1 is the
;;; encoding, fffe is the BOM). Upshot is that we have to define a special
;;; class.  Sigh.
(define-binary-class tcp-frame-v2.2 (raw-frame-v2.2) ())
(define-binary-class tcmp-frame-v2.3 (raw-frame-v2.3) ())

;;; NB
;;; TEMP: I've seen TCMP encoded in two ways:
;;; as raw-bytes or like this:
;;; ((encoding u1)
;;;  (bom u2)
;;;  (payload u1)))

;;; Apple's sort "enhancements" for v2.2/v2.3
;;; Note: v2.4 adopted these some of these, so we don't redefine them here.
;;; Description        v2.3/2.4    v2.2
;;; Title Sort         TSOT        TST
;;; Artist Sort        TSOP        TSP
;;; Album Sort         TSOA        TSA
;;; Album Artist Sort  TSO2        TS2
;;; Composer Sort      TSOC        TSC
(define-binary-class ts2-frame-v2.2 (text-info-frame-v2.2) ())
(define-binary-class tsa-frame-v2.2 (text-info-frame-v2.2) ())
(define-binary-class tsc-frame-v2.2 (text-info-frame-v2.2) ())
(define-binary-class tsp-frame-v2.2 (text-info-frame-v2.2) ())
(define-binary-class tst-frame-v2.2 (text-info-frame-v2.2) ())

(define-binary-class tso2-frame-v2.3 (text-info-frame-v2.3) ())
(define-binary-class tsoc-frame-v2.3 (text-info-frame-v2.3) ())

;;;; Non-Apple
;;; Non-standard, replay gain
(define-binary-class rgad-frame-v2.3 (id3v2.3-frame)
  ((peak-amplitude         u4)
   (radio-replay-gain      u2)
   (audiophile-replay-gain u2)))

;;; Non-standard, Music Match fluff
(define-binary-class ncon-frame-v2.3 (raw-frame-v2.3) ())

;;; Non-standard
(define-binary-class xdor-frame-v2.3 (generic-text-info-frame id3v2.3-frame) ())

;;;; Finding frame classes
(defun possibly-valid-frame-id? (frame-id)
  "Test to see if a string is a potentially valid frame id"

  ;; test each octet to see if it is alphanumeric. NB: this should probably
  ;; be rewritten to be "if FRAME-ID starts with a T, W, X, Y, Z, and is followed
  ;; only by (upper-case) alphanumerics...
  (loop for c across frame-id do
    (when (not (and (alphanumericp c)
                    (if (alpha-char-p c)
                        (upper-case-p c)
                        t)))
      (return-from possibly-valid-frame-id? nil)))
  t)

;;; Create a frame-class name string.
(defun mk-frame-class-name (id)
  (string-upcase (concatenate 'string id (ecase (length id)
                                           (3 "-frame-v2.2")
                                           (4 "-frame-v2.3"))))) ; XXX 2.4???

;;; To instantiate frames, we take in STR, which is the frame id, create
;;; an appropriate frame-class name, and try to find the so-named class
;;; using FOUND-CLASS-SYMBOL.  If we don't find the class, we try the
;;; assume that any class starting with T or W are TEXT-INFO/URL-LINK
;;; frames, and failing that, if the class name is possibly valid,
;;; we just return a RAW class.
(defun find-frame-class (str)
  (let* ((name (id3-string-string str))
         (found-class-symbol (find-symbol (mk-frame-class-name name)))
         (found-class))

    (dbg *dbg* 'find-frame-class name (mk-frame-class-name name) found-class-symbol)

    ;; if we found the class name, return the class (to be used for MAKE-INSTANCE)
    (when found-class-symbol
      (setf found-class (find-class found-class-symbol))
      (return-from find-frame-class found-class))

    ;; if not a "normal" frame-id, look at general cases of
    ;; starting with a 'T' or a 'W'.  Note that TXXX/WXXX have
    ;; been accounted for above, so we can just look at first character
    (setf found-class
          (case (aref name 0)
            (#\T
             (format t "Warning: encountered an unknown text-info frame, ~a~%" name)
             (ecase (length name)
               (3 'text-info-frame-v2.2)
               (4 'text-info-frame-v2.3))) ; XXX 2.4?
            (#\W
             (format t "Warning: encountered an unknown url-link-info frame, ~a~%" name)
             (ecase (length name)
               (3 'url-link-frame-v2.2)
               (4 'url-link-frame-v2.3))) ; XXX 2.4?
            (t
             ;; we don't recognize the frame name.  if it could
             ;; possibly be a real frame name, then just read
             ;; it raw
             (format t "file ~a~%Unknown frame type <~a> encountered~%"
                     *current-file* name)
             (if (not (possibly-valid-frame-id? name))
                 (progn
                   ;; written this was so I can debug...
                   (error "Bad frame name")
                   nil)
                 ;; possibly a frame name, so just read it in raw
                 (ecase (length name)
                   (3 'raw-frame-v2.2)
                   (4 'raw-frame-v2.3)))))) ; XXX 2.4?
    found-class))

;;; id3-frames
(define-binary-type id3-frames (tag-size flags frame-type)
  (:reader (in)
    (let ((octets)
          (in-stream)
          (size 0))
      (if (not (has-unsync-scheme-applied flags))
          (progn                    ; simply read in the octets as is
            (setf octets (make-octets tag-size))
            (read-sequence octets in))
          (let* ((last-byte-was-FF) ; else, need to remove unsync scheme
                 (byte))
            (setf octets (flex:with-output-to-sequence (out :element-type 'octet)
                           (dotimes (i tag-size)
                             (setf byte (read-byte in))
                             (if last-byte-was-FF
                                 (if (not (zerop byte)) ; drop any #x00
                                     (write-byte byte out))
                                 (write-byte byte out))
                             (setf last-byte-was-FF (= byte #xFF)))))))

      ;; create a FLEX in-memory stream of the frame area and read frames from that.
      ;; this handles the unsync cleanly, plus makes is impossible to have "run-away"
      ;; read from badly formed ID3 tags.
      (setf in-stream (flex:make-in-memory-input-stream octets)
            size (length octets))

        (loop with to-read = size
              while (plusp to-read)
              for frame = (read-frame frame-type in-stream)
              while frame do
                (decf to-read (+ (frame-header-size frame) (size frame)))
              collect frame
              finally (loop repeat (1- to-read) do (read-byte in-stream)))))

  (:writer (out frames)
    ;; Write frames to FLEXI sequence.
    (let* ((last-byte-was-ff)
           (buf (flex:with-output-to-sequence (tmp)
                  (loop with to-write = tag-size
                        for frame in frames do
                         (write-value frame-type tmp frame)
                         (decf to-write (+ (frame-header-size frame) (size frame)))))))

      (if (not (has-unsync-scheme-applied flags))
          (progn
            (dbg *dbg* 'no-sync-write)
            (write-sequence buf out)
            ;; pad with #x00
            (loop for count from (length buf) upto (1- tag-size) do
              (write-byte #x00 out)))

          ;; else, we have to apply unsync scheme before wrting out to file
          (let ((bytes-written 0))
            (loop for b across buf do
              (when (and last-byte-was-ff (or (zerop b)
                                              (= (logand b #xf0) #xf0)
                                              (= (logand b #xf0) #xe0)))
                (incf bytes-written)
                (write-byte #x0 out))
              (incf bytes-written)
              (write-byte b out)
              (setf last-byte-was-ff (= b #xff)))

            ;; write out padding, if any
            (loop for n from bytes-written  upto (1- tag-size) do
              (write-byte #x00 out)))))))

(defmethod frame-header-size ((frame id3v2.2-frame)) 6)
(defmethod frame-header-size ((frame id3v2.3-frame)) 10)

(defmethod data-bytes ((frame id3v2.2-frame))
  (size frame))

(defmethod data-bytes ((frame id3v2.3-frame))
  (let ((flags (flags frame)))
    (- (size frame)
       (if (frame-compressed-p flags) 4 0)
       (if (frame-encrypted-p flags) 1 0)
       (if (frame-grouped-p flags) 1 0))))

;;;; ID3V2.3 header
(defun has-unsync-scheme-applied (flags)             (logbitp 7 flags))
(defun extended-p                (flags)             (logbitp 6 flags))
(defun crc-p                     (flags extra-flags) (and (extended-p flags) (logbitp 15 extra-flags)))
(defun frame-compressed-p        (flags)             (logbitp 7 flags))
(defun frame-encrypted-p         (flags)             (logbitp 6 flags))
(defun frame-grouped-p           (flags)             (logbitp 5 flags))

;;;; ID3V2.2 header
(define-binary-class id3v2.2-tag (generic-id3-tag)
  ((frames (id3-frames :tag-size size :flags flags :frame-type 'id3v2.2-frame))))

(define-tagged-binary-class id3v2.2-frame ()
  ((id   (frame-id :length 3))
   (size u3))
  (:dispatch (find-frame-class id)))

;;; ID3V2.3 header
;;; XXX BUG: extended header must be read AFTER undoing any unsync scheme
(define-binary-class id3v2.3-tag (generic-id3-tag)
  ((extended-header-size (optional :type 'u4 :if (extended-p flags)))
   (extra-flags          (optional :type 'u2 :if (extended-p flags)))
   (padding-size         (optional :type 'u4 :if (extended-p flags)))
   (crc                  (optional :type 'u4 :if (crc-p flags extra-flags)))
   (frames               (id3-frames :tag-size size :flags flags :frame-type 'id3v2.3-frame))))

(define-tagged-binary-class id3v2.3-frame ()
  ((id                (frame-id :length 4))
   (size              u4)
   (flags             u2)
   (decompressed-size (optional :type 'u4 :if (frame-compressed-p flags)))
   (encryption-scheme (optional :type 'u1 :if (frame-encrypted-p flags)))
   (grouping-identity (optional :type 'u1 :if (frame-grouped-p flags))))
  (:dispatch (find-frame-class id)))

;;; XXX v2.4 todo


;;; Check to see if file starts with ID3 or if the old V2.1 tag is at
;;; the end of the file.
(defun id3-p (file)
  (with-open-file (in file :element-type 'octet)
    (or (str= "ID3" (read-value 'iso-8859-1 in :length 3))
        (let ((len (file-length in)))
          (if (> len 128)
              (progn
                (file-position in (- (file-length in) 128))
                (str= "TAG" (read-value 'iso-8859-1 in :length 3)))
              nil)))))

;;; Check to see if FILE is an MP3 file (or more precisely,
;;; if it already has any ID3 tags in it). If so, return
;;; both the newer (v2.[234]) tag and any older (v2.1) tag
(defun read-id3 (file)
  (let ((*current-file* file)
        (tag1)
        (tag2))

    (if (id3-p file)
        (with-open-file (in file :element-type 'octet)
          (setf tag1 (read-value 'generic-id3-tag in))
          (let ((len (file-length in))
                (tst))
            (when (> len 128)
              (file-position in (- len 128))
              (setf tst (read-value 'id3-v2.1-tag in))
              (if (str= (tag tst) "TAG")
                  (setf tag2 tst))))))
    (values tag1 tag2)))

;;; Write out newer (v2.[234]) tag and v2.1 tag
;;; Remember: we might have read in "broken" frames, so we need to recalculate
;;; the each frame's size on write (see READ-ID3-STRING)
(defun write-id3 (file id3 &optional v2.1-tag)
  (let ((new-tag-size 10)) ; the ID3 tag header size
    (loop for frame in (frames id3) do
      (setf (size frame) (calc-frame-size frame)) ; make sure we have correct frames sizes
      (incf new-tag-size (+ (size frame) (frame-header-size frame))))

    (dbg *dbg* new-tag-size (size id3))
    (when (> new-tag-size (size id3))
        (error "Need to implement growing tag logic"))

    (with-open-file (out file :element-type 'octet :direction :output :if-does-not-exist :create :if-exists :overwrite)
      (write-value (type-of id3) out id3)

      (when v2.1-tag ; XXX we need to handle ADDING a v2.1 (we're just assuming replacement here)
        (file-position out (- (file-position out 128)))
        (write-value 'id3-v2.1-tag v2.1-tag out)))))

(defun read-dir-id3s (&optional (dir "/home/markv/Music"))
  (let ((count 0))
    (cl-fad:walk-directory
     dir
     (lambda (f)
       (handler-case
           (multiple-value-bind (id3 id3v2.1) (read-id3 f)
             (when (or id3 id3v2.1)
               ;; do whatever you want here
               (incf count)))
         (condition (c)
           (format t "File: ~a~%Condition: ~a~%" f c)))))

    (format t "~&~&~:d MP3s examined~%" count)))

(defparameter *lots* "/smb/devnulpogo/markv/Backups/klinger/Music/iTunes/iTunes Music/Music/Boston/Third Stage/08 I Think I Like It_Can'tcha Say.mp3")

(defun ls-frames (foo)
  (let ((count 0))
    (dolist (f (frames foo))
      (format t "~3d: ~a~%" count f)
      (incf count))))

(defun remove-frames (id3 &rest frame-types)
  (dolist (ft frame-types)
    (setf (frames id3)
          (remove-if (lambda (x) (typep x ft)) (frames id3)))))

#|
(defun test-it ()
  (handler-bind ((unbound-variable
                   #'(lambda (c) ;argument is condition description
                       (format t "got condition ~a~%" c)
                       (dolist (tag '(store-value use-value))
                        (let ((restart (find-restart tag c)))
                          (when restart
                            (format t "Found restart ~a, invoking~%" restart)
                            (invoke-restart restart 17)))))))
    (+ (my-symbol-value 'this-symbol-unbound)
       (my-symbol-value 'pi))))

(defun my-symbol-value (name)
  (if (boundp name)
      (symbol-value name)
      (restart-case (error 'unbound-variable :name name)
        (use-value (value)
          :report "Specify a value to use."
          :interactive (lambda ()
                         (format t "~&Value to use: ")
                         (list (eval (read))))
          value)
        (store-value (value)
          :report "Specify a value to use and store."
          :interactive (lambda ()
                         (format t "~&Value to use and store: ")
                         (list (eval (read))))
          (setf (symbol-value name) value)
          value))))
|#
