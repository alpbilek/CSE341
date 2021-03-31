
;This list is a storage list for the numbers esach iteration. 
(defparameter *list2* '())

(defun part3( list1)
	;Takes a list and calls the coltz function and fills the *list2* and prints to the file and clears it for new iteration. 
	(loop for x in list1
		do(coltz x)
		(write-numeric-list "collatz_outputs.txt" *list2*)
		(setq *list2* '())
		)
)
;This method adds values to the primelist.This function implemented cause of push function adds an element to list as a Stack data structure(First in First Out)
(defun add(num)
	(if (null *list2*)
	(push num *list2*)
	(push num (cdr (last *list2*)))
	)
)

(defun coltz(num)
	(add num)
	;This is the base-case
	(cond ((= 1 num)
		;;you need to stop here.
	
		)
	(t
	;If the input is even returns the half of the num
		(cond ((evenp num)
			(coltz (/ num 2))
		
		
			)
		;Else returns the 3n+1 of the num
		(t
			(coltz (+ 1 (* 3 num)))
		
		)
		)
	
	)
	)

)



;Reads from FILE a String and converts it to List and executes prime process
(defun main (filename)
	(let ((in (open filename :if-does-not-exist nil)))
	   (when in
	      (loop for line = (read-line in nil)
	      
	      while line do 
	      	
	      	(setq list2 (string-to-list line))
	      	(part3 list2)
	      )
	      (close in)
	   )
	)
	

)
;Conversion function string to List
(defun string-to-list (str)
        (if (not (streamp str))
           (string-to-list (make-string-input-stream str))
           (if (listen str)
               (cons (read str) (string-to-list str))
               nil)))
               
      
;Writing to the FILE        
(defun write-numeric-list(filename l)
  (with-open-file (out filename :direction :output :if-exists :append :if-does-not-exist :create)
    (format out "~D:" (car l))
    (dolist (segment l)
      (format out "~D " segment))
    (format out "~%")))
    
    
;Calling the Main function
(main "integer_inputs.txt")
