;;Try writing iteratively.

(define (expr num)
  (if (number? num)
    num
  ("Not a number")))

(define-syntax swap
  (syntax-rules ()
    ((swap exprA exprB)
     (let ((tmp exprA))
       (set! exprA exprB)
       (set! exprB tmp)))))

;; The code at first is just me following the schem org docs' minimal interpreter.

;; LISP stores stuff as symbols, which have a print name, "The string of characters that represent it to the programmer", the value cell : the value of that symbol, the function cell : The function definition of the symbol, property list: Assosciated properties to that symbol, and package : Namespace assosciated to that symbol.

;; We can convert a string to a symbol and literally call that symbol (if callable) easily.

;;(define (nameOfVar) 4)
;;(define getVar "nameOfVar") ;;Let's say we define a variable which has it's value to be the name of a supposed symbol.
;;(display (nameOfVar)) ;;Prints 4.
;;(display getVar) ;;Prints "nameOfVar".
;;(display ((eval (string->symbol getVar) (interaction-environment)))) ;;Prints 4 again.

;; Note that it refers to a symbol and not just a procedure, nameOfvar could may as well have been a normal variable.

(define (read-word currentChar)
  (define (read-word-helper word-so-far) ;word-so-far acts as an accumulator.
    (let ((next-character (peek-char)))
      (if (or (char-alphabetic? next-character)
              (char-numeric? next-character))
        (read-word-helper (cons (read-char) word-so-far)) ;Recursive.
        (reverse word-so-far))))
  (string->symbol ;We just create a symbol out of the string, if already a symbol, we get the symbol's pointer itself. As with all things LISP.
  (list->string ;Creates a scheme string object with the given sequence of characters.
  (read-word-helper (list currentChar))))) ;Read ^ downside up. Returns the accumulated list of characters.

(define (read-num currentNum)
  (define (read-num-helper num-so-far)
    (let ((next-digit (peek-char)))
      (if (char-numeric? next-digit)
        (read-num-helper (cons (read-char) num-so-far))
        (reverse num-so-far))))
  (string->number ;We just create a number out of the string.
    (list->string (read-num-helper (list currentNum)))))

;;Tryna write the iterative version of each function.

(define left-paren-token
  (list '*left-paranthesis*)) ;;Quote stops eval, treats it as a literal.

(define right-paren-token
  (list '*right-parenthesis*))

(define semi-colon-token
  (list '*semi-colon*))

(define (ReadChar)
  (let ((currentChar (read-char))) ;intrinsic proc
    (cond ((char-whitespace? currentChar)
           (ReadChar))
           ((eq? currentChar #\( )
           left-paren-token)
           ((eq? currentChar #\) )
            right-paren-token)
           ((eq? currentChar #\; )
            semi-colon-token)
           ((char-alphabetic? currentChar)
            (read-word currentChar))
           ((char-numeric? currentChar)
            (read-word currentChar))
           (else
             (error "Cannot handle this character token")))))

(define (leftParen? ch)
  (eq? ch left-paren-token))

(define (rightParen? ch)
  (eq? ch right-paren-token))

(define (semiColon? ch)
  (eq? ch semi-colon-token))

(define (readList list-so-far)
  (let ((currToken (ReadChar)))
    (cond ((semiColon? currToken)
          (reverse list-so-far))
          ((leftParen? currToken)
           (readList (cons (readList '()) list-so-far)));;When we cons one n sized list to another m sized list, (cons nList mList), the nList now becomes one single element, it's inners are not affected when we try doing map operations ourselves, or when we reverse the order.
          (else
            (readList (cons currToken list-so-far))))))

(define (simpleRead)
  (let ((nextToken (ReadChar)))
           (readList '())))

(define (REPL evaltr)
  (display "rEPL| ")
  (let ((expr (simpleRead)))
    (cond ((or (eq? expr 'halt)  ;eq checks if it referes to the same object, rather than checking the list itself.
                (eq? expr 'exit))
           (display "Exiting the repl"))
          (else
          (write (evaltr expr))
          (newline)
          (REPL evaltr)) ;Tail recursion.
          )))

(define (math-eval-combo expr)
  (let ((operator-name (car expr))
        (arg1 (arithematic-eval (cadr expr))) ;car + cdr of a cons.
        (arg2 (arithematic-eval (caddr expr)))) ;car + cdr + cdr of a cons.
    (cond ((eq? operator-name 'plus)
           (+ arg1 arg2))
          ((eq? operator-name 'minus)
           (- arg1 arg2))
          ((eq? operator-name 'times)
           (* arg1 arg2))
          ((eq? operator-name 'divide)
           (/ arg1 arg2))
          (else
            (error "Can't handle this operator.")))))

(define (arithematic-eval expr)
  (cond
    ((number? expr)
     expr)
    (else
      (math-eval-combo expr))))

(define (getString expr) ;;Since our reader takes in strings as symbols for easier comparison wrt strings.
  (if (symbol? expr) (symbol->string expr) expr))


(define (isRegister? expr)
  (if (char-alphabetic? (string-ref expr 0))
    (<= (string-length expr) 2)
  #f))

(define (isAddress? expr)
  (let ((first-char (string-ref expr 0))
        (last-char  (string-ref expr (- (string-length expr) 1))))
    (or (char-numeric? first-char)
        (char=? last-char #\H))))

(define (isImmediate? op) ;;Since I am trying to make this work for my own emulator. I will only cater to the very specific subset of instructions.
  (not (eq? op 'MOV)))

(define (read8085)
  (let ((currentChar (read-char)))
    (cond ((eof-object? currentChar)
           'eof)
          ((char-whitespace? currentChar)
           (read8085))
          ((eq? currentChar #\;) ;;Need to add comma handling later.
           'semi-colon-token)
          ((char-alphabetic? currentChar)
           (read-word currentChar))
          ((char-numeric? currentChar)
           (read-word currentChar))
          (else
           (error "Cannot handle this character token:" currentChar)))))

(define (readInstruction instructions-so-far)
  (let ((currToken (read8085)))
    (cond ((eq? currToken 'eof)
           (reverse instructions-so-far))
          ((eq? currToken 'semi-colon-token)
           (reverse instructions-so-far))
          (else
           (readInstruction (cons currToken instructions-so-far))))))

(define (readAssembly)
  (readInstruction '()))

(define (ASM evaltr)
  (display "aSM | ")
  (let ((expr (readAssembly)))
    (cond ((or (eq? (car expr) 'halt)
               (eq? (car expr) 'exit))
           (display "Exiting the aSM rEPL"))
          (else
           (write (evaltr expr))
           (newline)
           (ASM evaltr)))))

(define (assembler-eval expr) ;Each expression right now is inside paranthesis.
  (let ((operator-name (car expr)))
    (cond ((isImmediate? operator-name)
          (let ((arg (8085eval (cadr expr))))
            (display "Immediate instruction done: ")
            (display arg)))
          (else
            (let ((arg1 (8085eval (cadr expr)))
                  (arg2 (8085eval (caddr expr))))
                  (display "Non Immediate instruction done: ")
                  (display arg1)
                  (display ", ")
                  (display arg2))))))

(define (8085eval expr)
  (cond
    ((isRegister? (getString expr))
     expr)
    ((isAddress? (getString expr))
     expr)))

(display "type halt/exit with a semicolon to exit!")
(newline)
(ASM assembler-eval)
(newline)
;;(display "Hello world")
;;(display (expr 5))
