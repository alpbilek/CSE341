

;Takes a List and converts to wondered form.
(defun flattenList (fltlist)
	;This cond checks the given list is a (cons) or not.I mean, is there any different list 		in the list.If the condition is true creates a list and returns it.This cond is base-case also.
	(cond ((atom fltlist)
		(list fltlist))
	;Else part is adds the given lists first element and checks is that element is the last element of the list?
	(t
		(append (flattenList (car fltlist))
			(if (cdr fltlist)
				(flattenList (cdr fltlist))
			)
		
		)	
	
	)	
	
	;This function checks the whole element one by one for "Is this element a list?".The main point is check the elements and if the condition is false add to the new List.
	
	
	)




)
   ;Reads the given list from FILE.(Input came in String form from the FILE and I convert it to the list.)          
(defun main (filename)
	(let ((in (open filename :if-does-not-exist nil)))
	   (when in
	      (loop for line = (read-line in nil)
	      
	      while line do 
	      	
	      	(setq list2 (string-to-list line))
	      	(write-numeric-list "flattened_list.txt" (flattenList list2))
	      )
	      (close in)
	   )
	)
	

)
;Converter string to List
(defun string-to-list (str)
        (if (not (streamp str))
           (string-to-list (make-string-input-stream str))
           (if (listen str)
               (cons (read str) (string-to-list str))
               nil)))


;Writing to FILE
(defun write-numeric-list(filename l)
  (with-open-file (out filename :direction :output :if-exists :append :if-does-not-exist :create)
    (dolist (segment l)
      (format out "~D " segment))
    (format out "~%")))


;Calling the Main function     
(main "nested_list.txt")

