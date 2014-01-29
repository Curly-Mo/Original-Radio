
;;;
;;; File: MGC (Music Generator for Composing)
;;;
;;; Author: David Cope <howell@ucsc.edu>, Peter Elsea (elsea@ucsc.edu)
;;; WWW Address: arts.ucsc.edu/faculty/cope
;;; Organization: UCSC
;;; Created: 2008/06/10 3:21:50 (UTC-8)
;;; Implementation: Common Lisp
;;; Target: same
;;; Purpose: simple testing software
;;; Keywords: no
;;; Requires: none
;;; Resources: none
;;; Dependencies: CL
;;; References: Help file
;;; Standards: 
;;; Unit-Test: 
;;; Limitations: 
;;; To-Do: debug and enhance
;;; Bugs: sure

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;MUSIC GENERATOR FOR COMPOSERS (MGC)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
MGC (a flexible program written in Common Lisp for generating MIDI files) is completely free:
(1) MGC is cross platform - it may be used in conjunction with any Common Lisp which is available for every machine type and operating system
  with many implementations free of charge;
(2) MGC is open source - anyone familiar with Lisp can read, alter, and follow its code;
(3) MGC can interpret plugins of almost any variety written in Lisp as long as they conform to the simple parametric constraints of the program;
(4) MGC contains as few biases toward composition as possible. By employing the aforementioned plugins, users may employ any compositional
  technique they wish as long as the code is written in Common Lisp;
(5) MGC plugins may be up- and down-loaded from the MGC website and theory.org under WACM (Workshop in Algorimic Computer Music) and thus the 
  program continues to grow and develop.
Please read the accompanying manual for using MGC to avoid problems. Bugs should be reported to the WACM list after ensuring that the problems 
  encountered are not ones created by the reporting party. 
Periodic updates will be made available at theory.org.
|#

(defvar *ontimes* '((0)) "Holds a parameter for use in next run.")
(defvar *pitches* nil "Holds a parameter for use in next run.")
(defvar *durations* nil "Holds a parameter for use in next run.")
(defvar *channels* nil "Holds a parameter for use in next run.")
(defvar *dynamics* nil "Holds a parameter for use in next run.")
(defvar *default-number* 100 "Number of elements when not otherwise determined.")
(defvar *octatonic* '(36 37 39 40 42 43 45 46 48 49 51 52 54 55 57 58 60 61 63 64 66 67 69 70 72 73 75 76 78 79 81 82 84 85 87 88 90 91 93 94 96))
(defVar *RS* (make-random-state t) "Variable for storing the current random state.")
(defvar *MGC-EVENTS* () "Storage of cope-events after composing.")
(defvar *WORK-DIR* "" "Path to where you want to store the resultant file.")

#|
top-level function of the mgc program
first arg is a string that will become the filename of the new midi file.
random output
(mgc "cope.mid")
"cope.mid"
individual parameter settings
(mgc "cope1.mid"  :ontimes '((0 1000)) :pitches (list (choose 2 '(60 62 64))) :durations '((500 1000)) :channels '((1 1)) :dynamics '((127 127)))
"cope1.mid"
(mgc "cope2.mid"  :ontimes '() :pitches '() :durations '() :channels '() :dynamics '())
"cope2.mid"
(mgc "cope3.mid" :ontimes '((0 1000)(0 1000)) :pitches '(((60 62 64)(60 62 64))((40 46 50)(40 46 50))) :durations '((1000 1000)(1000 1000)) :channels '((1 1)(2 2)) :dynamics '((127 127)(127 127))))
"cope3.mid"
collective parameter settings
(mgc "cope4.mid" :all (create-parameters))
"cope4.mid"
Using the previous material with a new instrument.
(mgc "cope5.mid" :ontimes *ontimes* :pitches *pitches* :durations *durations* :channels (list (choose 50 '(5))) :dynamics *dynamics*)
"cope5.mid"
These produce a midi files that when opened with a notation sequencer will create a readable and playable score.
|#

(defun mgc (namestring &key (all nil) (ontimes '((make-ontimes-from-durations (choose *default-number* '(250 500 1000 1500)))))(pitches '((choose *default-number* *octatonic*)))
                        (durations '((choose *default-number* '(1000 2000 3000 4000 5000 6000 7000))))(channels '((choose *default-number* '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))))(dynamics '((choose *default-number* '(127)))))
  "Produces a MIDI file containing the properties of the args given above."
  (if all (generate namestring 
                    (setq *ontimes* (first all))
                    (if (null (second all)) (setq *pitches* (make-lists (length (first all))(length (very-first all)) '(60)))
                        (setq *pitches* (second all)))
                    (if (null (third all)) (setq *durations* (make-lists (length (first all))(length (very-first all)) '(1000)))
                        (setq *durations* (third all)))
                    (if (null (fourth all)) (setq *channels* (make-lists (length (first all))(length (very-first all)) '(1)))
                        (setq *channels* (fourth all)))
                    (if (null (fifth all)) (setq *dynamics* (make-lists (length (first all))(length (very-first all)) '(127)))
                        (setq *dynamics* (fifth all))))
      (generate namestring
                (setq *ontimes* (evaluate ontimes))
                (setq *pitches* (evaluate pitches))
                (setq *durations* (evaluate durations))
                (setq *channels* (evaluate channels))
                (setq *dynamics* (evaluate dynamics)))))

(defun make-lists (n length amt)
  "Creates default lists of parameters when a plug-in does not provide for all five."
  (if (zerop n)()
      (cons (choose length amt)
            (make-lists (1- n) length amt))))

#|
function and arg
(evaluate '((car '(1 2 3))))
function and list
(evaluate '((1 2 3)))
function and qn arg of a function with multiple args
(evaluate '((append '(1 2 3) '(1 2 3))))
|#

(defun evaluate (data-or-function)
  (mapcar #'evaluate-them data-or-function))

(defun evaluate-them (data-or-function)
  (if (or (numberp (first data-or-function))
          (and (listp (first data-or-function))
               (numberp (very-first data-or-function))))
         data-or-function
        (eval data-or-function)))

(defun generate (namestring ontimes pitches durations channels dynamics)
  "Top-level function for generating MIDI files based on the combination ofd its arguments. Returns the pathname of the MIDI file created."
  (if (listp (very-first pitches)) 
    (let ((cope-events (apply #'append (loop for harmony in pitches 
                                             collect (make-harmonic-cope-events (first ontimes)
                                                                                harmony 
                                                                                (first durations) 
                                                                                (first channels) 
                                                                                (first  dynamics))
                                             do (setq ontimes (rest ontimes))
                                             do (setq durations (rest durations))
                                             do (setq channels (rest channels))
                                             do (setq dynamics (rest dynamics))))))
      (SAVE-MIDI-FILE namestring (setq *mgc-events* (remove-overlap cope-events))))
    (SAVE-MIDI-FILE namestring
                  (setq *mgc-events* (remove-overlap (apply #'append (make-cope-events ontimes pitches durations channels dynamics)))))))

(defun make-harmonic-cope-events (ontimes pitches durations channels dynamics &optional (ontime (first ontimes)))
  "Turns harmonies into simultaneous cope-events."
  (if (null ontimes)()
       (append (loop for pitch in (first pitches)
                         collect (list ontime pitch (first durations)(first  channels)(first dynamics)))
                   (make-harmonic-cope-events 
                    (rest ontimes) 
                    (rest pitches) 
                    (rest durations) 
                    (rest channels)
                    (rest dynamics) 
                    (unless (null (second ontimes))(+ ontime (- (second ontimes)
                                                                (first ontimes))))))))

(defun make-cope-events (ontimes pitches durations channels dynamics)
  (if (or (null ontimes)(null pitches)(null durations)(null channels)(null dynamics))()
      (cons (make-events (first ontimes)(first pitches)(first durations)(first channels)(first dynamics))
            (make-cope-events (rest ontimes)(rest pitches)(rest durations)(rest channels)(rest dynamics)))))

(defun make-events (ontimes pitches durations channels dynamics &optional (ontime 0))
  "Creates contiguous cope-events from its argument lists."
  (cond ((any-nullp ontimes pitches durations channels dynamics) nil)
        (t (cons (list (first ontimes) (first pitches)(first durations)(first channels)(first dynamics))
                 (make-events (rest ontimes)(rest pitches)(rest durations)(rest  channels)(rest dynamics)(+ ontime (first ontimes)))))))

;****************
;;;UTILITIES
;****************

(defun very-first (list)
  "Returns the first of the first of a list of lists."
  (first (first list)))

(defun choose (n list &optional (save-list list))
  "Pseudo-randomly chooses n number of elements of second arg."
  (cond ((zerop n)())
        ((null list)(choose n save-list))
      (t (let ((test (choose-one list)))
           (cons test
                 (choose (1- n) (remove test list :count 1) save-list))))))

(defun create-parameters ()
  "A simple and mindless random parameter producer showing the manner in which the 5 elements of the cope events can be created within a single function."
  (let ((n (random 100 *rs*)))
    (list (list (make-ontimes-from-durations (choose n '(500 1000 2000 3000))))
          (list (choose n *octatonic*))
          (list (choose n '(500 1000 2000 3000)))
          (list (choose n '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)))
          (list (choose n '(60 70 80 90 100 110 120))))))

(defun make-ontimes-from-durations (durations &optional (ontime 0))
  "Makes ontimes from a given single list of durations."
  (if (null durations)()
      (cons ontime
            (make-ontimes-from-durations (rest durations) (+ ontime (first durations))))))

(defun choose-one (list)
  "Chooses a random member of its argument."
  (nth (random (length list) *rs*)
       list))

(defun my-last (list)
  "My version of last without the list form."
  (first (last list)))

(defun my-sort (function lists)
  "(my-sort '< '(3 4 2 6 1 4))
      >> (1 2 3 4 4 6)"
  (loop for item in (sort (loop for x in lists
                                collect (list x))  function :key #'car)
        collect (first item)))

(defun firstn (number list)
  "Returns the first n number of its list arg."
  (if (< (length list) number)(firstn (1- number) list)
      (butlast list (- (length list) number))))

(defun any-nullp (&rest lists)
  "Returns nil if any one of its args is nil."
  (if (member nil lists :test #'equal) t nil))

(defun remove-overlap (cope-events &optional (channel 1))
  (if (equal channel 17)()
      (append (remove-over (get-channel channel cope-events))
              (remove-overlap cope-events (1+ channel)))))

(defun remove-over (cope-events)
  "Removes any overlap between successive cope-events."
(unless (null cope-events)
  (if (equal (first (first cope-events))(first (second cope-events)))
    (cons (first cope-events)(remove-over (rest cope-events)))
    (if (null (rest cope-events))
      cope-events
      (let ((cur-event (first cope-events))
            (next-event (second cope-events)))
        (if (> (third cur-event)
               (- (first next-event)
                  (first cur-event)))
          (cons (list (first cur-event)
                      (second cur-event)
                      (- (first next-event)
                         (first cur-event))
                      (fourth cur-event)
                      (fifth cur-event))
                (remove-over (rest cope-events)))
          (cons cur-event (remove-over (rest cope-events)))))))))

(defun get-channel (channel events)
  "Returns all of the events that have the same channel slot."
  (cond ((null events)())
        ((equal (fourth (first events)) channel)
          (cons (first events)
                (get-channel channel (rest events))))
        (t (get-channel channel (rest events)))))

;****************
;;;MIDI-SAVE
;****************

#|
Written by Paul Pelton with additions and changes by Soren Goodman and David Cope.

The input format that the program uses follows David Cope's event structure, called "Cope
Events".  Each event is a list in the format (ontime duration pitch channel
volume).  All times are in milliseconds, with pitch as 60 = middle C,
channels 1-16, and dynamics in the range 0-127.  
The input should be a list of events in the variable *events*, so for example:

((0 60 1500 1 90)(500 64 1000 2 90)(1000 67 500 3 90))

would be an arpeggiated major chord (midi pitches 60 64 67) such that the notes
enter at different times and will overlap and end at the same time.

The format:
(SAVE-MIDI-FILE (make-pathname :name "dave.mid") '((0 60 1000 1 127)))
will put a quarter-note middle C in a midi file called "dave.mid" in your top-level disk drive
(SAVE-MIDI-FILE (make-pathname :directory "books" :name "dave.mid") '((0 60 1000 1 127)))
will put the above in a midi file in a folder called "books" in your top-level disk drive 
and so on. Note that some Common Lisps will automatically place files in the same folder
as the Common Lisp application. 
|#

(defConstant KMTHDHEADERBYTES '(#x4D #x54 #x68 #x64))
(defConstant KMTHDHEADERLENBYTES '(0 0 0 6))
(defConstant KMTHDHEADERFORMATBYTES '(0 1))
(defConstant KMTRKHEADERBYTES '(#x4D #x54 #x72 #x6B))
(defConstant KMTRKENDBYTES '(#x00 #xFF #x2F #x00))
(defConstant KPPQNVALUE #x30)

(defVar *EVENTS* nil)
(defvar *CHANNEL-NO-1* 74)
(defvar *CHANNEL-NO-2* 69)
(defvar *CHANNEL-NO-3* 72)
(defvar *CHANNEL-NO-4* 71)
(defvar *CHANNEL-NO-5* 61)
(defvar *CHANNEL-NO-6* 60)
(defvar *CHANNEL-NO-7* 58)
(defvar *CHANNEL-NO-8* 47)
(defvar *CHANNEL-NO-9* 10)
(defvar *CHANNEL-NO-10* 48)
(defvar *CHANNEL-NO-11* 1)
(defvar *CHANNEL-NO-12* 41)
(defvar *CHANNEL-NO-13* 41)
(defvar *CHANNEL-NO-14* 42)
(defvar *CHANNEL-NO-15* 43)
(defvar *CHANNEL-NO-16* 49)
(defvar *overwrite-if-exists* t)

#|
Calling (SAVE-MIDI-FILE #P"mj" ((0 60 1000 1 127))) 
SAVE-MIDI-FILE returned #P"mj"
|#

(defun SAVE-MIDI-FILE (outfilename events &optional (pathname *work-dir*))
  (when pathname (setq outfilename (concatenate 'string pathname outfilename))) 
  (let ((tracks (create-midi-tracks (insert-program-changes events))))
      (with-open-stream (file (create-midi-file outfilename))
        (push (create-MThd (length tracks)) tracks)
        (write-to-midi-file file tracks))
     outfilename)) 
   
#|
Calling (INSERT-PROGRAM-CHANGES ((0 60 1000 1 127))) 
INSERT-PROGRAM-CHANGES returned ((0 1 0 1 255) (0 2 0 2 255) (0 3 0 3 255) (0 4 0 4 255) (0 5 0 5 255) 
   (0 6 0 6 255) (0 7 0 7 255) (0 8 0 8 255) (0 9 0 9 255) (0 10 0 10 255) (0 11 0 11 255) (0 12 0 12 255) 
   (0 13 0 13 255) (0 14 0 14 255) (0 15 0 15 255) (0 16 0 16 255) (0 60 1000 1 127))
|#

(defun INSERT-PROGRAM-CHANGES (events)
  (append
   (list
    (list 0 *channel-no-1* 0 1 255)
    (list 0 *channel-no-2* 0 2 255)
    (list 0 *channel-no-3* 0 3 255)
    (list 0 *channel-no-4* 0 4 255)
    (list 0 *channel-no-5* 0 5 255)
    (list 0 *channel-no-6* 0 6 255)
    (list 0 *channel-no-7* 0 7 255)
    (list 0 *channel-no-8* 0 8 255)
    (list 0 *channel-no-9* 0 9 255)
    (list 0 *channel-no-10* 0 10 255)
    (list 0 *channel-no-11* 0 11 255)
    (list 0 *channel-no-12* 0 12 255)
    (list 0 *channel-no-13* 0 13 255)
    (list 0 *channel-no-14* 0 14 255)
    (list 0 *channel-no-15* 0 15 255)
    (list 0 *channel-no-16* 0 16 255))
   events))
   
#|
Calling (CREATE-MIDI-FILE #P"mj") 
CREATE-MIDI-FILE returned #<BASIC-FILE-BINARY-OUTPUT-STREAM ("mj"/12 ISO-8859-1) #x8B5E79E>
|#

(defun CREATE-MIDI-FILE (outfilename)
  (open outfilename :direction :output :if-exists (if *overwrite-if-exists* :overwrite :error)
        :if-does-not-exist :create :element-type 'unsigned-byte))
   
#|
Calling (WRITE-TO-MIDI-FILE #<BASIC-FILE-BINARY-OUTPUT-STREAM ("mj"/12 ISO-8859-1) #x8B5E79E> ((77 84 104 100 0 0 0 6 0 1 0 17 0 48) 
    (77 84 114 107 0 0 0 4 0 255 47 0) (77 84 114 107 0 0 0 15 0 192 0 0 144 60 127 48 128 60 127 0 255 47 0) 
    (77 84 114 107 0 0 0 7 0 193 1 0 255 47 0) (77 84 114 107 0 0 0 7 0 194 2 0 255 47 0) . . . .
WRITE-TO-MIDI-FILE returned NIL
|#

(defun WRITE-TO-MIDI-FILE (file listOfChunks)
   (if (null listOfChunks) ()
      (progn
       (dolist (byte (first listOfChunks))
         (write-byte byte file))
       (write-to-midi-file file (rest listOfChunks)))))
   
#|
0> Calling (CREATE-MIDI-TRACKS ((0 1 0 1 255) (0 2 0 2 255) (0 3 0 3 255) (0 4 0 4 255) (0 5 0 5 255) (0 6 0 6 255) 
   (0 7 0 7 255) (0 8 0 8 255) (0 9 0 9 255) (0 10 0 10 255) (0 11 0 11 255) (0 12 0 12 255) (0 13 0 13 255) (0 14 0 14 255) 
   (0 15 0 15 255) (0 16 0 16 255) (0 60 1000 1 127))) 
CREATE-MIDI-TRACKS returned ((77 84 114 107 0 0 0 4 0 255 47 0) (77 84 114 107 0 0 0 15 0 192 0 0 144 60 127 48 128 60 127 0 255 47 0) 
   (77 84 114 107 0 0 0 7 0 193 1 0 255 47 0) (77 84 114 107 0 0 0 7 0 194 2 0 255 47 0) (77 84 114 107 0 0 0 7 0 195 3 0 255 47 0) . . . 
|#

(defun CREATE-MIDI-TRACKS (copeEvents)
  (let ((tracks (create-tempo-track)))
    (dotimes (channel 16 (reverse tracks))
      (let ((channelEvents (get-channel-events (1+ channel) copeEvents)))
        (if channelEvents
          (push (create-MTrk channelEvents) tracks))))))
   
#|
Calling (CREATE-TEMPO-TRACK) 
CREATE-TEMPO-TRACK returned ((77 84 114 107 0 0 0 4 0 255 47 0))
|#

(defun CREATE-TEMPO-TRACK ()
   (list (append kMTrkHeaderBytes (split-bytes (length kMTrkEndBytes) 4) kMTrkEndBytes)))
   
#|
Calling (GET-CHANNEL-EVENTS 1 ((0 1 0 1 255) (0 2 0 2 255) (0 3 0 3 255) (0 4 0 4 255) (0 5 0 5 255) (0 6 0 6 255) 
    (0 7 0 7 255) (0 8 0 8 255) (0 9 0 9 255) (0 10 0 10 255) (0 11 0 11 255) (0 12 0 12 255) (0 13 0 13 255) (0 14 0 14 255) 
    (0 15 0 15 255) (0 16 0 16 255) (0 60 1000 1 127))) 
GET-CHANNEL-EVENTS returned ((0 1 0 1 255) (0 60 1000 1 127))
|#

(defun GET-CHANNEL-EVENTS (channel events)
   (cond
    ((null events) ())
    ((= channel (fourth (first events)))
     (cons (first events) (get-channel-events channel (rest events))))
    (t (get-channel-events channel (rest events)))))
   
#|
Calling (MAKE-VAR-LEN 0) 
MAKE-VAR-LEN returned (0)

this is adapted to Lisp from the C code at
www.borg.com/~jglatt/tech/midifile.htm
|#

(defun MAKE-VAR-LEN (value)
   (let ((buffer (list (logand #x7F value))))
      (loop while (not (zerop (setq value (ash value -7))))
         do (push (logior (logior #x80 (logand #x7F value))) buffer))
      buffer))
   
#|
Calling (CREATE-MTHD 17) 
CREATE-MTHD returned (77 84 104 100 0 0 0 6 0 1 0 17 0 48)


assume that 1 <= numtracks <= 16
|#

(defun CREATE-MTHD (numtracks)
   (append
    kMThdHeaderBytes
    kMThdHeaderLenBytes
    kMThdHeaderFormatBytes
    (list 0 numtracks 0 kPPQNValue)))
   
#|
Calling (CREATE-MTRK ((0 1 0 1 255) (0 60 1000 1 127))) 
CREATE-MTRK returned (77 84 114 107 0 0 0 15 0 192 0 0 144 60 127 48 128 60 127 0 255 47 0)


assume 2 bytes for length of track data
|#

(defun CREATE-MTRK (events)
   (if (null events) ()
      (let* ((mtrkData
              (append
               (create-midi-track-data
                (fix-deltatime 0 (sort-by-deltatime (create-midi-events events))))
               kMTrkEndBytes)))
         (append kMTrkHeaderBytes (split-bytes (length mtrkData) 4) mtrkData))))
   
#| 
Calling (SPLIT-BYTES 7 4) 
SPLIT-BYTES returned (0 0 0 7)

splits a long integer into its high byte and low byte
|#

(defun SPLIT-BYTES (num count)
   (let ((bytes ()))
      (dotimes (i count bytes)
         (push (get-byte i num) bytes))))
   
#|
Calling (GET-BYTE 3 7) 
GET-BYTE returned 0

byteIndex starts at 0 (rightmost) and goes to whatever (leftmost)
|#

(defun GET-BYTE (byteIndex num)
   (ldb (byte 8 (* 8 byteIndex)) num))
   
#|
Calling (CREATE-MIDI-TRACK-DATA ((0 (193 1)))) 
CREATE-MIDI-TRACK-DATA returned (0 193 1)

midiEvents are ((deltaTime (byte3 byte2 byte1))(deltaTime (byte3 byte2 byte1))...),
are sorted in the order they should be in the file and their deltaTimes are
relative to each other (each relative to the previous).
|#

(defun CREATE-MIDI-TRACK-DATA (midiEvents)
   (if (null midiEvents) ()
      (let ((midiEvt (first midiEvents)))
        (append
         (make-var-len (first midiEvt))
         (second midiEvt)
         (create-midi-track-data (rest midiEvents))))))
   
#|
Calling (CREATE-MIDI-EVENTS ((0 3 0 3 255))) 
CREATE-MIDI-EVENTS returned ((0 (194 2)))
|#

(defun CREATE-MIDI-EVENTS (copeEvents)
   (if (null copeEvents) ()
      (let* ((event (first copeEvents))
             (ontime (first event)))
         (append
          (cond ((= (fifth event) 255)
                 (list (list (convert-ontime-to-deltatime ontime) (make-midi-pc-msg event))))
                (t
                 (list
                  (list
                   (convert-ontime-to-deltatime ontime) (make-midi-note-msg event #x90))
                  (list
                   (convert-ontime-to-deltatime (+ ontime (third event))) (make-midi-note-msg event #x80)))))
          (create-midi-events (rest copeEvents))))))
   
#|
Calling (MAKE-MIDI-NOTE-MSG (0 60 1000 1 127) 144) 
MAKE-MIDI-NOTE-MSG returned (144 60 127)

#x90 = note-on
#x80 = note-off
|#

(defun MAKE-MIDI-NOTE-MSG (copeEvent flag)
   (list (logior (1- (fourth copeEvent)) flag) (second copeEvent) (fifth copeEvent)))
   
#|
Calling (MAKE-MIDI-PC-MSG (0 16 0 16 255)) 
MAKE-MIDI-PC-MSG returned (207 15)

#xC0 = program change
|#

(defun MAKE-MIDI-PC-MSG (copeEvent)
   (list (logior (1- (fourth copeEvent)) #xC0) (1- (second copeEvent))))
                 
#|
Calling (SORT-BY-DELTATIME ((0 (207 15)))) 
SORT-BY-DELTATIME returned ((0 (207 15)))
|#
                                 
(defun SORT-BY-DELTATIME (midiEvents)
   (sort midiEvents #'< :key #'car))
   
#|
Calling (FIX-DELTATIME 0 ((0 (207 15)))) 
FIX-DELTATIME returned ((0 (207 15)))
|#

(defun FIX-DELTATIME (lasttime midiEvents)
   (if (null midiEvents) ()
      (let* ((midiEvt (first midiEvents))
            (newLastTime (first midiEvt)))
         (cons
          (list (- newLastTime lasttime) (second midiEvt))
          (fix-deltatime newLastTime (rest midiEvents))))))
   
#|
Calling (CONVERT-ONTIME-TO-DELTATIME 0) 
CONVERT-ONTIME-TO-DELTATIME returned 2 values :
      0
      0
|#

(defun CONVERT-ONTIME-TO-DELTATIME (copeOntime)
   (round (* (/ kPPQNValue 1000) copeOnTime)))

;****************
;;;MIDI-LOAD
;****************

;;;MIDI-LOAD
;; This is the simple version by Peter Elsea.
;; The lack of full documentation means that users should avoid messing with this code.
;; All it gives is a list of note events
;; Tempo events are collected and used to calculate times
;; Text Meta events are colleccted into *sequence-strings*
;; other events are discarded
;; the functiion load-midi-file requires a valid path as a string.
;; pqe 6-04-08

;; a place to put note data
;; *sequence-notes* format is (time-ms  note-number duration channel velocity)
;; This is an array to simplify setting durations when note off is detected.
(defVar *SEQUENCE-NOTES* (make-array 0 :element-type 'list :initial-element '(0 0 0 0 0) :fill-pointer t :adjustable t ))
(defVar *CHUNK-TYPE* ())             ; only two types are defined so far
(defVar *CHUNK-LENGTH* 0)            ; number of bytes in chunk
(defVar *MIDI-FILE-FORMAT* 0)        ; type 0 is single track, type 1 is multitrack, type 2 is indepentent loops
(defVar *MIDI-FILE-NTRKS* 0)         ; number of tracks in file
(defVar *MIDI-FILE-GRANULARITY* 24)  ; number of ticks per quarter note
(defVar *TRACK-TIME* 0)              ; unconverted track time, in ticks
(defVar *RUNNING-STATUS* 0)          ; running status is used
(defVar *TRACK-END* t)               ; flag for finding ends of tracks (rather than byte coounting) EOT sets this nil

(defun LOAD-MIDI-FILE (fstring)
  (with-open-file (input-stream fstring :element-type '(unsigned-byte 8) :if-does-not-exist nil)
    (setup)
    (get-header input-stream)
    (do ((track-index 0 (+ track-index 1)))
	((>= track-index *midi-file-ntrks*) ())
      (read-track input-stream))
   (setq *sequence-notes* (sort *sequence-notes* #'earlier))
   (loop for notes across *sequence-notes* collect (mapcar #'round notes))))

;; A place to put metadata -- later version can be more elegant
(defVar *SEQUENCE-STRINGS* (make-array 1 :initial-contents #("sequence-strings") :fill-pointer t :adjustable t ))

;; a place to put tempos. all tracks must refer to this when converting from ticks to time in ms
;; format of each entry is (time-in-ticks time-in-ms usec/qn)
(defVar *SEQUENCE-TEMPO-MAP* (make-array 1 :element-type 'list :initial-element '(0 0 500000) :fill-pointer t :adjustable t ))

; helper for header reading
(defun GET-TYPE (input-stream)
    (let ((type-string (make-string 4)))
      (loop for i from 0 to 3
            do (setf (char type-string i) (code-char(read-byte input-stream)))
            )
      type-string))

; general 32 bit retreiver
(defun GET-WORD (input-stream)
    (let ((value 0))
      (loop for i from 0 to 3
            do (setq value (+ (* value 256) (read-byte input-stream)))
            )
      value))

; general 16 bit retriever
(defun GET-SHORT (input-stream)
    (+ (* (read-byte input-stream) 256) (read-byte input-stream)))

; division is weird- this is a try at making sense out of it
; granularity is ticks per beat (quarter note)
(defun CONVERT-GRANULARITY (division)
  (let ((high-byte (ash division -8))(low-byte (logand #XFF)))
    (case high-byte
      (#XE2 (* 30 low-byte))
      (#XE3 (* 30 low-byte))
      (#XE7 (* 25 low-byte))
      (#XE8 (* 24 low-byte))
      (t division))))

; read the file header
(defun GET-HEADER (input-stream)
    (setq *chunk-type* (get-type input-stream))
    (setq *chunk-length* (get-word input-stream))
    (setq *midi-file-format* (get-short input-stream))
    (setq *midi-file-ntrks* (get-short input-stream))
    (setq *midi-file-granularity*(convert-granularity (get-short input-stream))))

; read a track header
(defun GET-TRACK-HEADER (input-stream)
    (setq *chunk-type* (get-type input-stream))
    (setq *chunk-length* (get-word input-stream)))

; time is listed as ticks in variable length quantities
(defun CONVERT-VLQ (arg-list &optional (accum 0))
      (if (> (first arg-list) 127)
        (convert-vlq (rest arg-list) (+ (- (first arg-list) 128) (* accum 128)))
        (+ (first arg-list) (* accum 128))))

; all events are seperated by a delta time 
(defun GET-VLQ (input-stream)
  (let ((new-byte (read-byte input-stream)))
    (if (< new-byte 128) (list new-byte)
        (cons new-byte (get-vlq input-stream)))))

; times are between events, so *track-time* must be accumulated across each track
(defun SET-TRACK-TIME (input-stream)
  (incf *track-time* (convert-vlq (get-vlq input-stream))))

; read arbitrary bytes into a list
(defun GATHER-BYTES (input-stream how-many)
  (if (zerop how-many) ()
      (cons (read-byte input-stream) (gather-bytes input-stream (1- how-many)))))

; reads a length, then gathers that many
(defun GET-METADATA (input-stream)
  (gather-bytes input-stream (read-byte input-stream)))

; test function for tempo searches
(defun FIRST>= ( data alist)
  (>= data (first alist) ))

;; Stuff the tempo map. format of each entry is (time-in-ticks time-in-ms usec/qn)
;; tempo and granualrity are need to convert ticks to ms
;; storing the time of the tempo change in both formats simplifies the calculations
(defun ADD-TEMPO (the-data)
  (let* ((us-qn (+ (ash (first the-data) 16)(ash (second the-data) 8) (third the-data)))
	(last-tempo-entry (elt *sequence-tempo-map*  (- (length *sequence-tempo-map* )1)))
        (last-tempo-time (second last-tempo-entry))
        (last-tempo (third last-tempo-entry))
        (ticks (- *track-time* (first last-tempo-entry))))
    (vector-push-extend (list *track-time* 
			      (+ last-tempo-time
                                 (/(* ticks last-tempo )(* *midi-file-granularity* 1000)))
                                 us-qn)
                        *sequence-tempo-map*)))

;; the time conversion function
;; search the tempo map from the end to find tempo in effect at the time
(defun TICKS-MS (ticks)
  (let* ((current-tempo-entry (find ticks *sequence-tempo-map* :test #'first>= :from-end t))
	 (current-tempo-time (second current-tempo-entry))
	 (current-tempo (third current-tempo-entry))
	 (delta-ticks (- ticks (first current-tempo-entry))))
    (float (+ current-tempo-time (/(* delta-ticks current-tempo)(* *midi-file-granularity* 1000))))))

;; most meta-data is text
(defun LIST-TO-STRING (ascii)
  (if (null ascii) #\. 
      (format nil "~A~A" (code-char (car ascii)) (list-to-string (cdr ascii)))))

;; meta data is mostly in the way, but tempos and end of trak are vital
(defun PARSE-METADATA (the-data)
  (case (car the-data)
    (0 ()) ; sequence number
    ((1 2 3 4 5 6 7 8 9 10)  (vector-push-extend (list-to-string (cdr the-data)) *sequence-strings* )); text 
    (#X20 ()) ; MIDI Channel prefix
    (#X2F (setq *track-end* nil)) ; End of track
    (#X51 (add-tempo (cdr the-data))) ; Set tempo  usec/qn in *sequence-tempo-map*
    (#X54 ()) ;  SMPTE offset H:M:S:F:F/100
    (#X58 ()) ;  Time Signature nnn dd  cc bb
    (#X59 ()) ;  Key Signature
    (#X7F ()) ;  Program specific
    (t ())))  ; unknown

;; Other events to parse
;; note ons are keepers
(defun HANDLE-NOTE (status nn vel)
  (vector-push-extend (list (ticks-ms *track-time*) nn 0 (+ (logand status #X0F) 1) vel ) *sequence-notes* ))

; test function for not off, which must search for matching note on
(defun MATCH-NOTE (status-nn target)
  (and (= (second status-nn) (second target))(= (first status-nn) (fourth target))(zerop (third target))))

;; search for note on this belongs to and set duration
;; this doesn't handle overlapping notes of the same pitch well but whatcha gonna do?
;; note number is &rest because we don't get a velocity with running status
;; note off velocity is discarded anyhow
(defun HANDLE-OFF (status &rest nn  )
  (let* ((channel (+ (logand status #X0F) 1)) 
	 (where (position (list channel (first nn)) *sequence-notes* :test #'match-note :from-end t))
	 (the-note)
	 (duration))
    (if (null where) () ; no matchng note on
      (progn
	(setf the-note (elt *sequence-notes* where))
	(setf duration (- (ticks-ms *track-time*) (first the-note)))
	(setf (third (elt *sequence-notes* where)) duration)))))

;; these just discard the data- ther are listed to prevent compiler warnings
;; one day I'll do something intelligent with these
(defun HANDLE-TOUCH (status nn pressure)
  (list status nn pressure))

(defun HANDLE-CONTROL (status cn value)
  (list status cn value))

(defun HANDLE-PROGRAM (status pn)
  (list status pn))

(defun HANDLE-PRESSURE (status pressure)
  (list status pressure))

(defun HANDLE-BEND (status lsb msb)
  (list status lsb msb))

(defun STRIP-SYSEX (input-stream)
  "just delete sysex for now"
  (if (= (read-byte input-stream) #XF7) ()
      (strip-sysex input-stream)))

;;; this is the grand track data handler
(defun PARSE-EVENTS (status-byte data-byte input-stream)
  (let ((vel))
   (cond 
    ((< status-byte #X90) (handle-off status-byte data-byte (read-byte input-stream)))
    ((< status-byte #XA0) (if (zerop (setq vel (read-byte input-stream)))
                             (handle-off status-byte data-byte )
                                (handle-note status-byte data-byte vel)))
    ((< status-byte #XB0) (handle-touch status-byte data-byte (read-byte input-stream)))
    ((< status-byte #XC0) (handle-control status-byte data-byte (read-byte input-stream)))
    ((< status-byte #XD0) (handle-program status-byte data-byte ))
    ((< status-byte #XE0) (handle-pressure status-byte data-byte ))
    ((< status-byte #XF0) (handle-bend status-byte data-byte (read-byte input-stream)))
    ((= status-byte #XF0) (strip-sysex input-stream))
    ((= status-byte #XFF) (parse-metadata (cons data-byte (get-metadata input-stream))))
    (t ()))))

;;; this layer deals with running status
(defun READ-AND-PARSE-EVENT (input-stream)
  (let ((first-byte (read-byte input-stream)))
    (if (>= first-byte #X80) (parse-events (setf *running-status* first-byte) (read-byte input-stream) input-stream)
      (parse-events *running-status* first-byte input-stream))))

;;;; call this once per track
(defun READ-TRACK (input-stream)
  (get-track-header input-stream)
  (if (zerop *chunk-length*) ()
    (if (not (equal *chunk-type* "MTrk")) (gather-bytes input-stream *chunk-length*) ; discard alien chunks
      (do ((*track-end* t)(*track-time* 0)(*running-status* 0))
	  ((null *track-end*)())
	(set-track-time input-stream)
	(read-and-parse-event input-stream)))))

;;;; initialize all those specials
(defun SETUP ()
  (setf *sequence-strings* (make-array 1 :initial-contents #("sequence-strings") :fill-pointer t :adjustable t ))
  (setq *sequence-tempo-map* (make-array 1 :element-type 'list :initial-element '(0 0 500000) :fill-pointer t :adjustable t ))
  (setq *sequence-notes* (make-array 0 :element-type 'list :initial-element '(0 0 0 0 0) :fill-pointer t :adjustable t ))) 

;; test function for sorting by time
(defun EARLIER (alist blist)
  (< (first alist) (first blist)))


(defun RE-TIME (to-be-added-to-events events)
  "For placing cope-event groups one after another in time sequence."
  (let* ((last-event (my-last events))
         (offset-time (if last-event (+ (first last-event)(third last-event)) 0)))
    (append events (loop for event in to-be-added-to-events
                         collect (cons (+ (first event) offset-time)(rest event))))))

;;;;;;;;;;;;;;;;;;;;;;;;

(defun delay (amt ontimes)
  "Delays the ontimes by the amt."
  (mapcar #'(lambda (x)(+ x amt)) ontimes))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;changingd instruments on channels
;;;;;;;;;;;;;;;;;;;;;;;;

#|
resetting any channel to a new number will change the timbre for that channel
the choices
 1 = ACOUSTIC GRAND PIANO
 2 = BRIGHT ACOUSTIC PIANO
 3 = ELECTRIC GRAND PIANO
 4 = HONKYTONK PIANO
 5 = RHODES PIANO
 6 = CHORUSED PIANO
 7 = HARPSICHORD
 8 = CLAVINET
 9 = CELESTA
 10 = GLOCKENSPIEL
 11 = MUSIC BOX
 12 = VIBRAPHONE
 13 = MARIMBA
 14 = XYLOPHONE
 15 = TUBULAR BELLS
 16 = DULCIMER
 17 = HAMMOND ORGAN
 18 = PERCUSSIVE ORGAN
 19 = ROCK ORGAN
 20 = CHURCH ORGAN
 21 = REED ORGAN
 22 = ACCORDIAN
 23 = HARMONICA
 24 = TANGO ACCORDIAN
 25 = ACOUSTIC NYLON GUITAR
 26 = ACOUSTIC STEEL GUITAR
 27 = ELECTRIC JAZZ GUITAR
 28 = ELECTRIC CLEAN GUITAR
 29 = ELECTRIC GUITAR MUTED
 30 = OVERDRIVEN GUITAR
 31 = DISTORTION GUITAR
 32 = GUITAR HARMONICS
 33 = ACOUSTIC FRETLESS  BASS
 34 = ELECTRIC BASS  FINGERED
 35 = ELECTRIC BASS PICKED
 36 = FRETLESS BASS
 37 = SLAP BASS 1
 38 = SLAP BASS 2
 39 = SYNTH BASS 1
 40 = SYNTH BASS 2
 41 = VIOLIN
 42 = VIOLA
 43 = CELLO
 44 = CONTRABASS
 45 = TREMOLO STRINGS
 46 = PIZZICATO STRINGS
 47 = ORCHESTRAL HARP
 48 = TIMPANI
 49 = ACOUSTIC STRING ENSEMBLE
 50 = ACOUSTIC STRING ENSEMBLE 2
 51 = SYNTH STRINGS 1
 52 = SYNTH STRINGS 2
 53 = AH CHOIR
 54 = OOH CHOIR
 55 = SYNTHVOX
 56 = ORCHESTRA HIT
 57 = TRUMPET
 58 = TROMBONE
 59 = TUBA
 60 = MUTED TRUMPET
 61 = FRENCH HORN
 62 = BRASS SECTION
 63 = SYNTHBRASS 1
 64 = SYNTHBRASS 2
 65 = SOPRANO SAX
 66 = ALTO SAX
 67 = TENOR SAX
 68 = BARITONE SAX
 69 = OBOE
 70 = ENGLISH HORN
 71 = BASSOON
 72 = CLARINET
 73 = PICCOLO
 74 = FLUTE
 75 = RECORDER
 76 = PAN FLUTE
 77 = BOTTLE BLOW
 78 = SHAKUHACHI
 79 = WHISTLE
 80 = OCARINA
 81 = SQUARE WAVE
 82 = SAW WAVE
 83 = CALLIOPE
 84 = CHIFFER
 85 = CHARANG
 86 = SOLO VOX
 87 = 5TH SAW WAVE
 88 = BASS AND LEAD
 89 = FANTASY
 90 = WARM
 91 = POLYSYNTH
 92 = CHOIR
 93 = BOWED
 94 = METAL
 95 = HALO
 96 = SWEEP
 97 = ICE RAIN
 98 = SOUND TRACKS
 99 = CRYSTAL
 100 = ATMOSPHERE
 101 = BRIGHTNESS
 102 = GOBLINS
 103 = ECHOS
 104 = SPACE
 105 = SITAR
 106 = BANJO
 107 = SHAMISEN
 108 = KOTO
 109 = KALIMBA
 110 = BAGPIPE
 111 = FIDDLE
 112 = SHANNAI
 113 = TINKLE BELL
 114 = AGOGO
 115 = STEEL DRUMS
 116 = WOODBLOCK
 117 = TAIKO DRUM
 118 = MELODIC TOM
 119 = SYNTH DRUM
 120 = REVERSE CYMBAL
 121 = GUITAR FRET NOISE
 122 = BREATHNOISE
 123 = SEASHORE
 124 = BIRD TWEET
 125 = TELEPHONE RING
 126 = HELICOPTER
 127 = APPLAUSE
 128 = GUNSHOT
|#

;;;the default settings

(setf *CHANNEL-NO-1* 74) ;;
(setf *CHANNEL-NO-2* 69) ;;
(setf *CHANNEL-NO-3* 72) ;;
(setf *CHANNEL-NO-4* 71) ;;
(setf *CHANNEL-NO-5* 61) ;;
(setf *CHANNEL-NO-6* 57) ;;
(setf *CHANNEL-NO-7* 58) ;;
(setf *CHANNEL-NO-8* 47) ;;
(setf *CHANNEL-NO-9* 10) ;;
(setf *CHANNEL-NO-10* 1) ;;
(setf *CHANNEL-NO-11* 10) ;;
(setf *CHANNEL-NO-12* 41) ;; 
(setf *CHANNEL-NO-13* 41) ;;
(setf *CHANNEL-NO-14* 42) ;;
(setf *CHANNEL-NO-15* 43) ;;
(setf *CHANNEL-NO-16* 44) ;;
