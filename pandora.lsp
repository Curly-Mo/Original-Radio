;;pandora will create radio stations represented by folders in the location /pandora/station_name
;;The pandora folder will most likely be put in your home folder.

(defvar *stm* ())
(defvar *userfolder* ())
(defvar *stationname* ())
(defvar *events* ())
(defvar *output* ())
(defvar *orders* '(2))
(defvar *count* 0)
(defvar *instruments* ())
(defvar *chosen-instruments* ())
(defvar *num-base-midis* 0)
(defvar *time* 0)
(defvar *nextstart* nil)

;;These lines were so that I only had to load one file. to start the whole program.
;;I am commenting them out since I am sure the path file will be different for you.
;;Just make sure to load mgc and pandora_markov before loading this file.
(load "/pandora/lisp/mgc.lisp")
(load "/pandora/lisp/pandora_markov.lsp")


;;Initialize the radio station in use. Load *stm*
(defun run ()
	(progn (print "Enter radio station name:")
		(setf *stationname* (prin1-to-string (read)))
		(setf *userfolder* (concatenate 'string "/pandora/" *stationname*))
		(setf *stm* nil)
		(read-stm)
		(if (null *stm*) (add-midi-to-station *orders*))
		(main-loop)
	))
	
;;Main loop.  User has option of adding a midi to station, creating next midi, changing stations, quiting program
(defun main-loop (&optional (user-input 0))
	(progn
		(cond 	((equal 1 user-input) (add-midi-to-station *orders*))
				((equal 2 user-input) (progn (create-midi) (get-opinion)))
				((equal 3 user-input) (run))
				((equal 4 user-input) (break))
				(t nil))
		(print "Would you like to:")
		(print (concatenate 'string "(1)Add another base midi to " *stationname* " station."))
		(print "(2)Create next midi.")
		(print "(3)Change radio station.")
		(print "(4)Quit.")
		(main-loop (read))))
	
;;Ask for thumbs up or thumbs down from last song created	
(defun get-opinion ()
	(progn
		(print (concatenate 'string "Did you like output" (prin1-to-string *count*) "?"))
		(print "(1) Thumbs up!")
		(print "(2) Thumbs down!")
		(print "(3) Indifferent.")
		(let ((x (read)))
			(if (equal 1 x) (thumbs-up))
			(if (equal 2 x) (thumbs-down))
			)))

;;Increase probabilities for everything similar to what was just created
;;Currently it only adjusts instrument selection, and increasing probabilities in the stm
(defun thumbs-up ()
	(progn
		(setf *instruments* (liked-instruments *instruments* *chosen-instruments*))
		(write-instruments)
		(markov-analyze-multichannel-multiorders *output* *orders*)
		(write-stm)))

;;Decrease probabilites for everything similar to what was just created		
(defun thumbs-down ()
	(progn
		(setf *instruments* (disliked-instruments *instruments* *chosen-instruments*))
		(write-instruments)
		(markov-deanalyze-multichannel-multiorders *output* *orders*)
		(write-stm)))

;;Get input and analyze the midi added
(defun add-midi-to-station (orders)
	(progn	(print "Enter path of a midi file you would like to add from:")
			(let ((x (read)))
				(setf *num-base-midis* 0)
				(read-events)
				(if 
					(ignore-errors
						(setf *events* (load-midi-file (prin1-to-string x))))
						    (progn
							(setf *events* (fix-loaded-midi-durations *events*))
							(write-num-base)
							(write-events)
							(print "Analyzing...")
							(markov-analyze-multichannel-multiorders *events* orders)
							(write-stm))
					(progn (print "Invalid File:")
							)
))))

;;Load *stm* from stm file (when it exists)			
(defun read-stm ()
	(let ((in (open (concatenate 'string *userfolder* "/stm.txt") :if-does-not-exist nil)))
		(when in
			(setf *stm* (read in))
			(close in))))

;;write current *stm* to stm file			
(defun write-stm ()
	(with-open-file (outfile (concatenate 'string *userfolder* "/stm.txt") :direction :output
                                   :if-exists :supersede :if-does-not-exist :create)
								   (prin1 *stm* outfile)))
								   
;;write *count* to file (number of midis created so far in station)
(defun write-count ()
	(with-open-file (outfile (concatenate 'string *userfolder* "/count.txt") :direction :output
                                   :if-exists :supersede :if-does-not-exist :create)
								   (prin1 *count* outfile)))

;;load *count* from file								 
(defun read-count ()
	(let ((in (open (concatenate 'string *userfolder* "/count.txt") :if-does-not-exist nil)))
		(when in
			(setf *count* (read in))
			(close in))))

;;read *events* from file (randomly picks one from the base midis)			
(defun read-events ()
	(let ((in (open (concatenate 'string *userfolder* "/events.txt") :if-does-not-exist nil)))
		(when in
			(progn (setf *num-base-midis* (read in))
				(loop for i from 0 to (random *num-base-midis* *rs*) do 
						(setf *events* (read in)))				
					(close in)))))

;;write *events* to file					
(defun write-events ()
	(with-open-file (outfile (concatenate 'string *userfolder* "/events.txt") :direction :output
                                   :if-exists :append :if-does-not-exist :create)
							(prin1 *events* outfile)))

;;write the number of base midis so far to file
(defun write-num-base ()
	(with-open-file (outfile (concatenate 'string *userfolder* "/events.txt") :direction :output
                                   :if-exists :overwrite :if-does-not-exist :create)
							(prin1 (+ 1 *num-base-midis*) outfile)))

;;*count*++. Read, add, and rewrite to file		
(defun update-count ()
	(progn
	    (setf *count* nil)
		(read-count)
		(if (null *count*) (setf *count* 1)
			(setf *count* (+ 1 *count*)))
		(write-count)))

;;write *instruments* to file
(defun write-instruments ()
	(with-open-file (outfile (concatenate 'string *userfolder* "/instruments.txt") :direction :output
                                   :if-exists :supersede :if-does-not-exist :create)
								   (prin1 *instruments* outfile)))
								 
;;read *instruments* from file
(defun read-instruments ()
	(let ((in (open (concatenate 'string *userfolder* "/instruments.txt") :if-does-not-exist nil)))
		(when in
			(setf *instruments* (read in))
			(close in))))

;;increase probability of chosen instruments
(defun liked-instruments (instruments chosen)
	(if (null instruments) nil
		(cons (cons (first chosen) (first instruments)) (liked-instruments (rest instruments) (rest chosen)))))

;;decrease probability of chosen instruments		
(defun disliked-instruments (instruments chosen)
	(if (null instruments) nil
		(cons (remove (first chosen) (first instruments) :count 1) (disliked-instruments (rest instruments) (rest chosen)))))
	
;;set the channels to "random" instruments. Randomly chosen from the running list of instrument probabilities			  
(defun set-instruments ()
	(progn (setf *instruments* nil)
		(read-instruments)
		(if (null *instruments*) (default-instruments))
		(setf *chosen-instruments*
			(loop for i from 0 to 14 collect (choose-one (nth i *instruments*))))
		(setf *CHANNEL-NO-1* (nth 0 *chosen-instruments*)) ;;
		(setf *CHANNEL-NO-2* (nth 1 *chosen-instruments*)) ;;
		(setf *CHANNEL-NO-3* (nth 2 *chosen-instruments*)) ;;
		(setf *CHANNEL-NO-4* (nth 3 *chosen-instruments*)) ;;
		(setf *CHANNEL-NO-5* (nth 4 *chosen-instruments*)) ;;
		(setf *CHANNEL-NO-6* (nth 5 *chosen-instruments*)) ;;
		(setf *CHANNEL-NO-7* (nth 6 *chosen-instruments*)) ;;
		(setf *CHANNEL-NO-8* (nth 7 *chosen-instruments*)) ;;
		(setf *CHANNEL-NO-9* (nth 8 *chosen-instruments*)) ;;
		;(setf *CHANNEL-NO-10* 1) ;;
		(setf *CHANNEL-NO-11* (nth 9 *chosen-instruments*)) ;;
		(setf *CHANNEL-NO-12* (nth 10 *chosen-instruments*)) ;; 
		(setf *CHANNEL-NO-13* (nth 11 *chosen-instruments*)) ;;
		(setf *CHANNEL-NO-14* (nth 12 *chosen-instruments*)) ;;
		(setf *CHANNEL-NO-15* (nth 13 *chosen-instruments*)) ;;
		(setf *CHANNEL-NO-16* (nth 14 *chosen-instruments*)) ))

;;all channels random probability for all instruments (except with a slight bias towards normal orchestration)
(defun default-instruments ()
	(progn 
		(setf *instruments* (loop for i from 1 to 15 collect (loop for i from 2 to 128 collect i)))
		(setf *instruments* (liked-instruments *instruments* '(74 69 72 71 61 57 58 47 10 10 41 41 42 43 44)));Set a bias towards a normal orchestration (not much of a bias)
		(setf *instruments* (liked-instruments *instruments* '(74 69 72 71 61 57 58 47 10 10 41 41 42 43 44)))
		(setf *instruments* (liked-instruments *instruments* '(74 69 72 71 61 57 58 47 10 10 41 41 42 43 44)))
		(setf *instruments* (liked-instruments *instruments* '(74 69 72 71 61 57 58 47 10 10 41 41 42 43 44)))
		(setf *instruments* (liked-instruments *instruments* '(74 69 72 71 61 57 58 47 10 10 41 41 42 43 44)))
		(setf *instruments* (liked-instruments *instruments* '(74 69 72 71 61 57 58 47 10 10 41 41 42 43 44)))))

;;create the midi
;;start witht he beginning of one of the base midis
;;use *stm* to generate rest of order: *orders*		
(defun create-midi ()
	(progn 
		(read-events)
		(setf *time* 0)
		(setf *output* (create-multichannel-events-from-*stm* (get-first-n-events-of-each-channel *events* (first *orders*))))
		(setf *output* (sort-events-by-ontimes *output*))

		(cut-off-and-recuperate);for now just do it this many times
		(cut-off-and-recuperate)
		(cut-off-and-recuperate)

		(update-count)
		(set-instruments)
		(SAVE-MIDI-FILE (concatenate 'string *userfolder* "/music/output" (prin1-to-string *count*) ".mid") *output*)))

;;this was kind of a work around for some issues, and still isnt working how I wanted it
;;for now this will cut off the events at a randomized time
;;then generate the rest of the events from (what was supposed to be a different source for variation)
(defun cut-off-and-recuperate ()	
	(progn
		(setf *time* (+ *time* (random 100000 *rs*)))
		(setf *output* (remove-events-starting-after *output* *time*))
		(setf *nextstart* (get-first-n-events-of-each-channel (sort-events-by-ontimes (remove-events-starting-before *events* *time*)) (first *orders*)))
		(setf *output* (append *output* (create-multichannel-events-from-*stm* *nextstart*)))))

		
(run)