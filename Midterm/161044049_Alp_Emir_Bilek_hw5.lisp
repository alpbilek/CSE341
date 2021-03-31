
(defparameter list2 '())
(defparameter query '())
(defparameter fact '())
(defparameter predicate '())
(defparameter isPred 0)
(defun containsEmptyList(list1)
	(setq temp 0)
	(loop for x in list1
		do
		(if (not x)
			(setq temp 1)
		)
	)
	temp
)
(defun parseLists (list1)	
	(cond ((= 1 (containsEmptyList list1))
			(if (not (car list1))
				(push (cdr list1) query)
				(push (car list1) fact)	
			)

		)
	(t
		(push list1 predicate)
	)
	)
)
(defun isPredicate(keyword intPart)
	(setq temp '())
	(loop for x in predicate
		do
			(cond ((string-equal keyword (car (car x)))
					(if(equal intPart (cdr(car(cdr(car x)))))
						(push x temp)
					)
				)
			)
	)
	(if (not temp)
		(setq isPred 1)
	)
	temp
)
(defun isFact(keyword)
	(setq temp '())
	(loop for x in fact
		do
			(if(string-equal keyword  (car x))
				(push x temp)
			)
	)
	temp	
)
(defun main (param)
	(readFromFile param)
	(loop for x in list2
		do
		(parseLists x)
	)
	(setq MAINRES '())
	(setq carquery (car query))
	(setq queryName (car (car carquery)))
	(setq queryInt (cdr(car(cdr(car carquery)))))
	(setq queryParameter (car(car(cdr(car carquery)))))

	(setq firstLetter (char queryParameter 0))
	(cond ((equal firstLetter (char-upcase firstLetter))
			(if (= isPred 1)
				(setq MAINRES (withoutNameFact carquery queryName))
				(setq MAINRES (withoutName carquery queryName queryInt))
			)
		)
		(t
			(if (= isPred 1)
				(setq MAINRES (withNameFact carquery queryName))
				(setq MAINRES (withName carquery queryName queryInt))
			)
		)

	)

	(writer-Output "output.txt" names)
)
(defun withoutNameFact(carquery queryName)
	(setq list4 (isFact queryName))
	(setq carcarq (car carquery))
	(setq names '())
	(loop for x in list4
		do
		(setq tempquery carcarq)
		(setf (nth 0 (nth 1 tempquery)) (car(car(cdr x))))
		(if (equal x tempquery)
			(push (car(car(cdr x))) names)
		)
	)
	names
)
(defun withNameFact (carquery queryName)
	(setq list4 (isFact queryName))
	(setq counter 0)
	(setq names '())
		(loop for q in list4
			do
			 (if (equal q (car carquery))
			 	(incf counter)
			 )
		)
		(if (> counter 0)
			(push  "true" names)
			(push  "false!"names)
		)
		names
)
(defun withoutName (carquery queryName queryInt)
	(setq wonderedPredicate (isPredicate queryName queryInt))
	(setq conditionsPart (car(cdr(car wonderedPredicate))))
	(setq names '())
	(setq holder '())
	(loop for i in conditionsPart
		do
		(setq intPart (cdr(car (cdr i))))
		(setq factName (car i))
		(setq facts (isFact factName))
		(loop for x in facts
			do
			(if (equal intPart (cdr(car(cdr x))))
				(push  (car(car (cdr x))) names)
			)
			
		)
		(push names holder)
		(setq names '())
		
	)
	(setq holderFirst (car holder))
	(setq holderSecond (cdr holder))
	(loop  for x in holderFirst
		do
		(if (= 1 (containsGivenString (car holderSecond) x)) 
			(push x names)
		)
	)
	names
)
(defun withName (carquery queryName queryInt)
	
	(setq queryParameter (car(car(cdr(car carquery)))))
	(setq wonderedPredicate (isPredicate queryName queryInt))
	(setq conditionsPart (car(cdr(car wonderedPredicate))))
	(setq listSize (list-length conditionsPart))
	(setq counter 0)
	(setq names '())
	(loop for i in conditionsPart
		do
		(setq firstLetter (char(nth 0 (nth 1 i)) 0 ))
		(if (equal firstLetter (char-upcase firstLetter))
			(setf (nth 0 (nth 1 i))queryParameter)	
		)
		
	)

	(loop for x in conditionsPart
		do
		(setq factName (car x))
		(setq list4 (isFact factName))
		(loop for q in list4
			do
			 (if (equal x q)
			 	(incf counter)
			 )
		)
	)
	(if (and (> counter 0)(= counter listSize))
			(push  "true" names)
			(push  "false!" names)
	)
	names
)
(defun containsGivenString (SecondList keyword)
	(setq temp 0)
	(loop for i in SecondList
		do
		(if (string-equal keyword i)
			(setq temp 1)
		)
	)
	temp
)
(defun readFromFile (filename)
(let ((in (open filename :if-does-not-exist nil)))
	   (when in
	      (loop for line = (read-line in nil)
	      
	      while line do 
	      	(push (car (string-to-list line)) list2)
	      )
	      (close in)
	   )
	)
	(setq list2 (reverse list2))
)
(defun string-to-list (str)
        (if (not (streamp str))
           (string-to-list (make-string-input-stream str))
           (if (listen str)
               (cons (read str) (string-to-list str))
               nil)))
(defun writer-Output(filename l)
  (with-open-file (out filename :direction :output :if-exists :append :if-does-not-exist :create)
  	(loop for i in l
  		do(format out "~D ~%" i)
  	))    
)
(main "input.txt")


