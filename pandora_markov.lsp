(defvar *stm* ())
(defvar *debug* ())
(setf *stm* ())


(defun markov-analyze-multichannel (events order)
	(markov-analyze-events-by-channel events order))

;;analyze all 16 channels to each order in list orders
;;store everything in *stm* with a key for channel and (pitch duration ontime or dynamic)
;;this way there is no accidental overlap and misinformation
;;all channels are kept separate to retain melodic form
(defun markov-analyze-multichannel-multiorders (events orders)
	(if (null orders) nil
		(progn
			(markov-analyze-events-by-channel events (first orders))
			(markov-analyze-multichannel-multiorders events (rest orders)))))
			
(defun markov-analyze-events-by-channel (events order &optional (channel 1))
	(if (equal 17 channel) nil
	(progn (markov-analyze-this-channel events order channel)
			(markov-analyze-events-by-channel events order (+ 1 channel)))))
			
(defun markov-analyze-this-channel (events order channel)
	(let ((eventsofchannel (get-events-of-channel events channel)))
		(markov-analyze-events eventsofchannel order channel)))

;;return events of the given channel only out of events supplied
(defun get-events-of-channel (events channel)
	(if (null events) nil
		(if (equal channel (fourth (first events))) (cons (first events) (get-events-of-channel (rest events) channel))
			(get-events-of-channel (rest events) channel))))

(defun markov-analyze-events (events order channel)
    (progn (markov-analyze-for-channel (gettimesbetweenontimes (mapcar #'first events)) order 'ontime channel)
			(markov-analyze-for-channel (mapcar #'second events) order 'pitch channel)
			(markov-analyze-for-channel (mapcar #'third events) order 'duration channel)
			(markov-analyze-for-channel (mapcar #'fifth events) order 'dynamic channel)))

;;get the times between ontimes
;;this is to obatin rhythm from ontimes, not from durations
;;this way there can be staccato notes and overlapping pitches
(defun gettimesbetweenontimes (ontimes &optional (firsttime 1))
	(if (equal 1 firsttime) (cons 0 (gettimesbetweenontimes ontimes 0))
		(if (null (second ontimes)) nil
			(cons (- (second ontimes) (first ontimes)) (gettimesbetweenontimes (rest ontimes) 0)))))

;;turn rhytms back into ontimes
(defun get-ontimes-from-between-times (betweentimes &optional (ontime 0))
	(if (null betweentimes) nil
		(let ((time (+ (first betweentimes) ontime)))
			(cons time (get-ontimes-from-between-times (rest betweentimes) time)))))			
		
(defun markov-analyze-for-channel (pitches order type channel)
	(let ((key (list (loop for i from 0 to (1- order) collect (nth i pitches)) type channel)) (next (nth order pitches)))
		(if (null pitches) nil
			(progn	(if (assoc key *stm* :test #'equal)	(add-to-key-in-stm key next)
														(add-entry-to-stm key next))
					(markov-analyze-for-channel (rest pitches) order type channel)))))
		

;;Replaced with a modified version that allows for decreasing probabilities of specific markov chains				
;(defun add-entry-to-stm (key next)
;	(setf *stm*
;		(cons  (list key (list next))
;				*stm*)
;	)
;)
;
;(defun add-to-key-in-stm (key next)
;	(let ((keymap (assoc key *stm* :test #'equal))
;			(newkeymap (cons key (list (cons next (second (assoc key *stm* :test #'equal)))))))
;		(setf *stm* 
;				(cons newkeymap
;					  (remove keymap *stm* :test #'equal)))))
		
(defun add-entry-to-stm (key next)
	(setf *stm*
		(cons  (list key (list next next next next));;For now, a hardcoded starting point of 4 for each next note. This way they can be removed to adjust probability without removing the note entirely.
				*stm*)
	)
)

(defun add-to-key-in-stm (key next)
	(let ((keymap (assoc key *stm* :test #'equal))
			(newkeymap (cons key (list (append (list next next next next) (second (assoc key *stm* :test #'equal)))))))
		(setf *stm* 
				(cons newkeymap
					  (remove keymap *stm* :test #'equal)))))

					  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;This section does the opposite of Markov-analyze-multichannel (and all its subfunctions)
;;;Analyze an eventlist, and then REMOVE all of its markov chains from the *stm*
;;;It was really only one simple change, I just decided to rename everything and keep these two functions seperate
(defun markov-deanalyze-multichannel-multiorders (events orders)
	(if (null orders) nil
		(progn
			(markov-deanalyze-events-by-channel events (first orders))
			(markov-deanalyze-multichannel-multiorders events (rest orders)))))
			
(defun markov-deanalyze-events-by-channel (events order &optional (channel 1))
	(if (equal 17 channel) nil
	(progn (markov-deanalyze-this-channel events order channel)
			(markov-deanalyze-events-by-channel events order (+ 1 channel)))))
			
(defun markov-deanalyze-this-channel (events order channel)
	(let ((eventsofchannel (get-events-of-channel events channel)))
		(markov-deanalyze-events eventsofchannel order channel)))

;(defun get-events-of-channel (events channel)
;	(if (null events) nil
;		(if (equal channel (fourth (first events))) (cons (first events) (get-events-of-channel (rest events) channel))
;			(get-events-of-channel (rest events) channel))))

(defun markov-deanalyze-events (events order channel)
    (progn (markov-deanalyze-for-channel (gettimesbetweenontimes (mapcar #'first events)) order 'ontime channel)
			(markov-deanalyze-for-channel (mapcar #'second events) order 'pitch channel)
			(markov-deanalyze-for-channel (mapcar #'third events) order 'duration channel)
			(markov-deanalyze-for-channel (mapcar #'fifth events) order 'dynamic channel)))

					  
(defun markov-deanalyze-for-channel (pitches order type channel)
	(let ((key (list (loop for i from 0 to (1- order) collect (nth i pitches)) type channel)) (next (nth order pitches)))
		(if (null pitches) nil
			(progn	(if (assoc key *stm* :test #'equal)	(remove-from-key-in-stm key next))
					(markov-deanalyze-for-channel (rest pitches) order type channel)))))
		
						
(defun remove-entry-from-stm (key next)
	(if (not (null next))
		(setf *stm*
			(remove (list key (list next))
					*stm* :count 1))
	)
)

(defun remove-from-key-in-stm (key next)
	(if (not (null next))
		(let ((keymap (assoc key *stm* :test #'equal))
				(newkeymap (cons key (list (remove next (second (assoc key *stm* :test #'equal)) :count 1)))))
			(if (null (second newkeymap)) nil ;;Do not remove it if it is the last one. Otherwise the program crashes when it gets to a note that goes nowhere.
				(setf *stm* 
					(cons newkeymap
						(remove keymap *stm* :test #'equal)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					  
					  
;;A function to turn a loaded midi back into its normal tempo					  
 (defun fix-loaded-midi-durations (events)
	(if (null events) nil
		(cons 	(list (* 2 (first (first events)))
						(second (first events))
						(* 2 (third (first events)))
						(fourth (first events))
						(fifth (first events)))
				(fix-loaded-midi-durations (rest events))
		)
	)
)

;;return first n events in each channel
(defun get-first-n-events-of-each-channel (events n &optional (channel 1))
	(if (equal 17 channel) nil
		(append (get-first-n-events (get-events-of-channel events channel) n) (get-first-n-events-of-each-channel events n (+ 1 channel)))))

;;return first n events
(defun get-first-n-events (events n)
	(if (zerop n) nil
		(if (null events) nil
			(cons (first events) (get-first-n-events (rest events) (- n 1))))))

;;create events from the given starting events using the *stm*
(defun create-multichannel-events-from-*stm* (events &optional (channel 1))  
    (if (equal 17 channel) nil
		(let ((eventsofchannel (get-events-of-channel events channel)))
			(append (create-events-from-*stm*-for-channel eventsofchannel channel) (create-multichannel-events-from-*stm* events (+ 1 channel))))))
			
(defun create-events-from-*stm*-for-channel (events channel)
    (if (null events) nil
		(let ((ontimes (add-to-all (get-ontimes-from-between-times (create-values-from-stm (gettimesbetweenontimes (mapcar #'first events)) 'ontime channel)) (first (my-last (butlast events)))))
			(pitches (create-values-from-stm (mapcar #'second events) 'pitch channel))
			(durations (create-values-from-stm (mapcar #'third events) 'duration channel))
			(dynamics (create-values-from-stm (mapcar #'fifth events) 'dynamic channel)))
					(append (butlast events) (create-events-from-components ontimes pitches durations channel dynamics)))))

;;create events from a list of ontimes pitches durations channels and dynamics
(defun create-events-from-components (ontimes pitches durations channel dynamics)
	(cond 	((null ontimes) nil)
			((null pitches) nil)
			((null durations) nil)
			((null dynamics) nil)
			(T (cons (list (first ontimes) (first pitches) (first durations) channel (first dynamics))
						(create-events-from-components (rest ontimes) (rest pitches) (rest durations) channel (rest dynamics))))))
				
(defun create-values-from-stm (previousvalues type channel)
	(if (equal nil (first (last previousvalues))) nil
		(cons 	(first (last previousvalues))
				(create-values-from-stm (append (rest previousvalues) (list (choose-from-stm (list previousvalues type channel)))) type channel))
	)
)

(defun choose-from-stm (key)
	(let ((associated (assoc key *stm* :test #'equal)))
		(if (null associated) nil
			(first (choose 1 (second associated))))))
	
(defun create-ontimes-from-durations (durations starttime)
	(if (null durations) nil
		(cons starttime (create-ontimes-from-durations (rest durations) (+ starttime (first durations))))))
		
(defun create-as-many (avalue alist)
	(if (null alist) nil
		(cons avalue (create-as-many avalue (rest alist)))))
		
(defun add-to-all (alist value)
	(if (null alist) nil
		(if (null value) alist
			(cons (+ value (first alist)) (add-to-all (rest alist) value)))))


			
;;remove the remain events after one of the channels has come to a halt
(defun cut-off-at-last (events)
	(let ((min (get-min (last-n-of-each-channel events 1))))
		(print (last-n-of-each-channel events 1))
		(print min)
		(remove-events-starting-after events min)))

;;return last n events of each channel
(defun last-n-of-each-channel (events n &optional (channel 1))
		(if (equal 17 channel) nil
			(append (last-n (get-events-of-channel events channel) n) (last-n-of-each-channel events n (+ 1 channel)))))

;;return last n events
(defun last-n (events n)
	(if (zerop n) nil
		(if (null events) nil
			(append (last-n (butlast events) (- n 1)) (last events)))))

;;find the time that the first channel comes to a halt
(defun get-min (events &optional (min 999999999))
	(if (null events) min
		(let ((endtime (+ (first (first events)) (third (first events)))))
			(if (<  endtime min)
				(get-min (rest events) endtime)
			(get-min (rest events) min)))))

;;remove all events starting after the given time
(defun remove-events-starting-after (events time)
	(if (null events) nil
		(if (> (first (first events)) time) 
			(remove-events-starting-after (rest events) time)
		(cons (first events) (remove-events-starting-after (rest events) time)))))

;;remove all events tarting before the given time
(defun remove-events-starting-before (events time)
	(if (null events) nil
		(if (< (first (first events)) time) 
			(remove-events-starting-before (rest events) time)
		(cons (first events) (remove-events-starting-before (rest events) time)))))

;;sort the list of events in increasing order of ontimes		
(defun sort-events-by-ontimes (events)
	(sort events #'first-lessp))
	
(defun first-lessp (a b)
        (< (first a) (first b)))
