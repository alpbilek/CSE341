

;This list is for the store the prime numbers for finding semi-prime numbers
(defparameter *list2* '())
;This variable is a return value for "prime-helper" function.
(defparameter isprime 0)
;This method adds values to the primelist.This function implemented cause of push function adds an element to list as a Stack data structure(First in First Out)
(defun add(num)
	(if (null *list2*)
	(push num *list2*)
	(push num (cdr (last *list2*)))
	)
)
;Takes two values and checks values prime or not between that 2 values
(defun part2(start stop)
	(prevprime start)
	(loop for i from start to stop
		do
		(prime i)
	)
)
;This funtion implemented cause of take the prime numbers before start point.I did this process cause of the obtaining semi-prime numbers
(defun prevprime (str)
	(loop for p from 2 to (- str 1)
		do(prime-helper p)
		(cond ((= 1 isprime)
			(add p)
			(setf isprime 0))
		)
	)
)
;This function basically isPrime functioon in C.Checks the input value prime or not.
(defun prime-helper (num)
	
	(setf temp_num (- num 1))
	(setf count 0)
	(loop for q from 2 to temp_num
		do
		(if (= 0 (mod num q))
			(incf count)
		)
	)
	(if (= 0 count)
		(setf isprime 1)
	)
	(cond ((= num 1)
	(setf isprime 0)
	)
	)

)
;The main process function
(defun prime(num)
(setf semi-prime-count 0)
	(prime-helper num)
	;If the given value is prime adds to primeList and print "Prime".Then sets isprime as 0.
	(cond ((= 1 isprime)
			(add num)
			(write-Prime "primedistribution.txt" num)
			(setf isprime 0))
	)
	;This part firstly,We divide the given number to prime numbers one by one.
	;Then we check the "is the result of the division operation a prime number".
	;If it is true we can say this number is a semi-prime number.
	(loop for x in *list2*
		do
		(cond ((= 0(mod num x))
			(setf div (/ num x))
			(prime-helper div)
			(cond ((= 1 isprime)
			;This semi-prime-count is a flag.
				(setf semi-prime-count 100)
				(setf isprime 0)
				)
			)
			
		)
		
		)	
	)
	(cond ((= 100 semi-prime-count)
		(write-Semi-Prime "primedistribution.txt" num))
	)
)
;Reads from FILE a String and converts it to List and executes prime process
(defun main (filename)
	(let ((in (open filename :if-does-not-exist nil)))
	   (when in
	      (loop for line = (read-line in nil)
	      
	      while line do 
	      	
	      	(setq list2 (string-to-list line))
	      	(part2 (car list2) (cadr list2))
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
               
               
               
;Writer for Prime functions
(defun write-Prime(filename qq)
  (with-open-file (out filename :direction :output :if-exists :append :if-does-not-exist :create)
      (format out "~D is Prime~%" qq)) 
)
;Writer for Semi-Prime functions
(defun write-Semi-Prime(filename l)
  (with-open-file (out filename :direction :output :if-exists :append :if-does-not-exist :create)
      (format out "~D is Semi-Prime~%" l))    
)
;Calling the Main function
(main "boundries.txt")
