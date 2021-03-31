;;ALP EMIR BILEK
;;161044049
(setf *outputList* nil)
(setf operatorList '("+" "-" "/" "*" "(" ")" "**" "," "'"))
(setf keywordList '("and" "or" "not" "equal" "less" "nil" "list" "append" "concat" "set" "deffun" "for" "if" "exit" "load" "disp" "true" "false" "defvar" "while" "for"));;Defines keywords
(setf keywordListstr '("KW_AND" "KW_OR" "KW_NOT" "KW_EQUAL" "KW_LESS" "KW_NIL" "keywordList" "KW_APPEND" "KW_CONCAT" "KW_SET" "KW_DEFFUN" "KW_FOR" "KW_IF" "KW_EXIT" "KW_LOAD" "KW_DISP" "KW_TRUE" "KW_FALSE" "KW_DEFVAR" "KW_WHILE" "KW_FOR"));;These are for printing correspondance keywords
(defun addPUSH(str)
  (push (list str) *outputlist*)

)
(defun IDcheck (line)
    (cond ((digit-char-p (car line)) (progn (add "Error") -1))
          (t (if (not (eq (position (coerce line 'string) keywordList :test #'string=) nil)) (addPUSH (nth (position (coerce line 'string) keywordList :test #'string=) keywordListstr)) (addPUSH "IDENTIFIER")))
    )
)
;;This function is general line check.It means is it 
(defun Lexcheck (line)
    (cond ((and (not (eq (list-length line) 1)) (char= #\0 (car line))) (progn (addPUSH "Error") -1))
          ((not (Alphacheck line)) (progn (addPUSH "Error") -1))
          ((and (eq (list-length line) 1) (char= #\0 (car line))) (push (list "VALUE" (format nil "~{~a~}" line)) *outputlist*))
          (t (if (not (Valuecheck line)) (IDcheck line) (push (list "VALUE" (format nil "~{~a~}" line)) *outputlist*)))
    )
)

;;This function decides is it a value or not.
(defun Valuecheck (line)
    (cond (
          
          (null line) t) ;;If it is null return
          
          ((not (digit-char-p (car line))) nil) ;;Checker for digit or not
          
          (t (Valuecheck(cdr line))) ;;iterate the given line
    )
)
;;This functions check the line is a alphanumeric or not.It means,it decides is it operator or not(keyword and identifier).

(defun Alphacheck(line)
    (cond (
          
          (null line) t) ;;If it is null return
          
          ((not (alphanumericp (car line))) nil) ;;checker for alphanumeric or not
          
          (t (Alphacheck(cdr line))) ;;iterate the given line
    )
)
;;This function is used for given character is a character or operation.
;;Return 1 for characyers
;Returns 0 for operations
(defun charClass (char)
    (cond ( ;;Determine the character class
          
          (alphanumericp char) 1);;For chars and numbers
          
          (t 0);;For operations
    )
)

(defun getAlpha (StR line)
    (let ((c (peek-char nil StR nil))) ;;Iterate the given string one by one recursively
        (cond ((not (eq nil (member (string c) operatorList :test #'string=))) (reverse line)) ;;Checker for operatorlist,if it's containing in push it
              ((or (eq c #\Space) (eq c nil)) (reverse line)) ;;If it is whitespace
              (t (progn (push (read-char StR) line) (getAlpha StR line))) ;;recursive call
        )
    )
)
;;This function used for obtaining the operations type and pushes it to outputlist.
(defun OPpush  (op)
  (if (string= op "\"")
    (addPUSH "OP_OC");;If it is \ push it to the outputlist
  )
  (if (string= op ",")
    (addPUSH "OP_COMMA");;If it is , push it to the outputlist
  )
  (if (string= op "'")
    (addPUSH "OP_QUO");;If it is ' push it to the outputlist
  )
  (if (string= op "*")
    (addPUSH "OP_MULT");;If it is *push it to the outputlist
  )
  (if (string= op ")")
    (addPUSH "OP_CP");;If it is )push it to the outputlist
  )
  (if (string= op "(")
    (addPUSH "OP_OP");;If it is( \) push it to the outputlist
  )
  (if (string= op "**")
    (addPUSH "OP_OC");;If it is **push it to the outputlist
  )
  (if (string= op "/")
    (addPUSH "OP_DIV");;If it is / push it to the outputlist
  )
  (if (string= op "+")
    (addPUSH "OP_PLUS");;If it is + push it to the outputlist
  )
  (if (string= op "-")
    (addPUSH "OP_MINUS");;If it is - push it to the outputlist
  )
)
;;Main function
;;This man function decides is input file given by arguement or not.
;;Acc. to this info program will be executed.
(defun main (param)
  (setq input4 (car *args*)) ;;take argument from keyboard
  (cond ((eq *args* nil) ;;check the null or not
    (terminal 1) ;; if it is null call the terminalfunc
    )
  (t
    (read-file input4) ;;if it is not call the file func
  ) 
  )
  (parser param)
)
;;Reader from keyboard
;;That cond is written for empty strings 
;;If empty string taken from keyboard program will terminated and output of the given inputs will be wrote to output file.Otherwise program keeps asking for new line.
(defun terminal (par)
  (setq input (read-line))  
  
  (cond ((= 3 (length (concatenate 'string input "alp")));;had some issue about checking the empty string so concatenat it with "alp" and check the result's length
    1 ;;if it is 3 so return and terminate
      )
    (t
      (setq *outputList* (union *outputList* (lexer input))) ;;else call the lexer method
      (terminal par) ;;keep doing(recursive)
    )
  )
)
;;Reads from input file line by line
;;Then sends them to lexer function and unite the all lines outputlist.
(defun read-file(filename)
  (let ((in (open filename :if-does-not-exist nil))) ;;open file
     (when in
        (loop for line = (read-line in nil) ;;read file line by line
        while line do 
          (setq *outputList* (union *outputList* (lexer line))) ;;call the lexer with line
        )
        (close in) ;;close file
     )
  )
)
;;Main Lexer function
;;This function calls the required helper functions and it generates an output acc. to conds and pushes to outputlist
;;At the end reverses the outputlist because Common Lisp push method works like a Stack structure.
(defun lexer(strin)
    (setq *outputList* nil) ;;refresh the global list
    (let ((str (make-string-input-stream strin))) ;;convert it to string
        (loop while (not (eq (peek-char nil str nil) nil)) do ;;control it char by char
            
            (let ((c (read-char str)) (line)) ;;read from given string
            
                (cond ((and (char= #\; c) (char= (peek-char nil str nil) #\;)) (progn (addPUSH "COMMENT COMMENT") (return ))) ;;if it is equal to ";" push it as COMMENT
            
                    ((eq (charClass c) 0)  ;;if it is operator
                      (if (and (string= "*" (peek-char nil str nil)) (string= c "*")) (OPpush (concatenate 'string (string c) (string (read-char str)))) (OPpush c))) ;;if it is operator obtain the which one is and send it to opPush method
            
                    ((eq (charClass c) 1) ;;If it is char  call the lexerchecker and send it to him
                      (progn (push c line) (if (eq -2 (Lexcheck (getAlpha str line))) ())));;IF for error check
                )
            )   
        )
    )
    (reverse *outputList*) ;;cause of lisp's push function have to reveres i
)


(defun parser(param)
  (setq CP_counter 0)
  (setq OP_counter 0)
  (setq Syntax_counter 0)
  (loop for i in *outputlist*
    do
      (cond ((equal (car i) "OP_OP")
            (incf OP_counter)
          )
          (t
            (if (equal (car i) "OP_CP")
                (incf CP_counter))
          )

      )
      (if (string-equal (car i) "Error")
        (incf Syntax_counter)
      )

  )
  (if (or (/= CP_counter OP_counter) (> Syntax_counter 0))
    (print "Syntax Error")
    (gppParser param (cdr *outputlist*))
  )
)

(defun gppParser (filename lexeme)
  (setq twoLetter (subseq (car(car lexeme)) 0 2))
  (setq Res 0)
  (if (string-equal twoLetter "OP")
    (setq Res (OP lexeme))
  )
  (if (string-equal twoLetter "KW")
    (print "KW part will be implemented")
  )
  (if (string-equal twoLetter "ID")
    (print "ID part will be implemented")
  )
  (writer filename Res)
)
(defun writer(filename qq)
  (with-open-file (out filename :direction :output :if-exists :append :if-does-not-exist :create)
      (format out "Result is:~D ~%" qq)) 
)
(defun OP (lexeme)
  (setq resut nil)
  (cond  ((equal (car lexeme) '("OP_MINUS"))
    (setq result (- (parse-integer(car(cdr(car(cdr lexeme)))))  (parse-integer(car(cdr(car(cdr (cdr lexeme))))))))
    )
  )
  (cond ((equal (car lexeme) '("OP_PLUS"))
    (setq result (+ (parse-integer(car(cdr(car(cdr lexeme)))))  (parse-integer(car(cdr(car(cdr (cdr lexeme))))))))
  	)
  )
  (if (equal (car lexeme) '("OP_DIV"))
    (setq result (/ (parse-integer(car(cdr(car(cdr lexeme)))))  (parse-integer(car(cdr(car(cdr (cdr lexeme))))))))
  )
  (if (equal (car lexeme) '("OP_MULT"))
    (setq result (* (parse-integer(car(cdr(car(cdr lexeme)))))  (parse-integer(car(cdr(car(cdr (cdr lexeme))))))))
  )
  result
)

(main "parsed_lisp.txt")

