#lang eopl

(define lexica
'((white-sp
   (whitespace) skip)
  (comment
   ("//" (arbno (not #\newline))) skip)
  (identificador
   (letter (arbno (or letter digit "?"))) symbol)
  (digitoBinario
   ("b" (or "0" "1") (arbno (or "0" "1"))) string)
  (digitoBinario
   ("-" "b" (or "0" "1") (arbno (or "0" "1"))) string)
  (digitoDecimal
   (digit (arbno digit)) number)
  (digitoDecimal
   ("-" digit (arbno digit)) number)
  (digitoOctal
   ("0x" (or "0" "1" "2" "3" "4" "5" "6" "7")(arbno (or "0" "1" "2" "3" "4" "5" "6" "7"))) string)
  (digitoOctal
   ("-" "0x" (or "0" "1" "2" "3" "4" "5" "6" "7") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7"))) string)
  (digitoHexadecimal
   ("hx" (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))) string)
  (digitoHexadecimal
   ("-" "hx" (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))) string) 
  (flotante
   (digit (arbno digit) "." digit (arbno digit)) number)
  (flotante
   ("-" digit (arbno digit) "." digit (arbno digit)) number)
  ))

(define gramatica
  '(
    (programa ((arbno struct-decl) expresion) a-programa)
    (expresion (bool-expresion) bool-exp)
    (expresion (identificador) var-exp)
    (expresion (numero-exp) num-exp)    
    (expresion ("\"" identificador (arbno identificador) "\"") cadena-exp)
    (expresion (var-decl) decl-exp)

    ;;Listas y arrays
    (expresion ("list" "(" (separated-list expresion ",") ")") lista-exp)
    (expresion ("cons" "(" expresion expresion ")") cons-exp)
    (expresion ("empty") empty-list-exp)
    (expresion ("array" "(" (separated-list expresion ",") ")") array-exp)

    ;;Expresion primitivas
    ;;Primitiva numerica
    (expresion ("(" expresion primitiva expresion ")") prim-num-exp)
    ;;Primitiva booleana
    (expresion (primitivaBooleana "(" (separated-list expresion ",") ")") prim-bool-exp)
    ;;Primitiva listas
    (expresion (primitivaListas "(" expresion ")") prim-list-exp)
    ;;Primitiva array
    (expresion (primitivaArray "(" (separated-list expresion ",") ")") prim-array-exp)
    ;;Primitiva de cadenas
    (expresion (primitivaCadena "(" (separated-list expresion ",") ")") prim-cad-exp)


    ;;Condicionales
    (expresion ("if" expresion "{" expresion "else" expresion "}") if-exp)


    ;;Iteradores
    (expresion ("for" identificador "from" expresion "until" expresion "by" expresion "do" expresion) for-exp)
    (expresion ("while" expresion "{" expresion "}") while-exp)

    ;;Switch
    (expresion ("switch" "(" expresion ")" "{" (arbno "case" expresion ":" expresion) "default" ":" expresion "}") switch-exp)

    ;;Secuenciación y asignación
    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
    (expresion ("set" identificador "=" expresion) set-exp)

    ;;Funciones
    (expresion ("func" "(" (separated-list identificador ",") ")" expresion) func-exp)
    (expresion ("call" expresion "(" (separated-list expresion ",") ")") call-exp)

    ;;Instanciación y uso de estructuras
    (expresion ("new" identificador "(" (separated-list expresion ",") ")") new-struct-exp)
    (expresion ("get" expresion "." identificador) get-struct-exp)
    (expresion ("set-struct" expresion "." identificador "=" expresion) set-struct-exp)

    ;;Reconocimiento de patrones
    (expresion ("match" expresion "{" (arbno regular-exp "=>" expresion) "}") match-exp)

    ;;Numero-exp
    (numero-exp (digitoDecimal) decimal-num)
    (numero-exp (digitoOctal) octal-num)
    (numero-exp (digitoBinario) bin-num)
    (numero-exp (digitoHexadecimal) hex-num)
    (numero-exp (flotante) float-num)
    
    ;;Bool-exp
    (bool-expresion ("true") true-exp)
    (bool-expresion ("false") false-exp)

    ;;primitivas numéricas
    (primitiva ("+") sum-prim)
    (primitiva ("-") minus-prim)
    (primitiva ("*") mult-prim)
    (primitiva ("mod") mod-prim)
    (primitiva ("pow") elevar-prim)
    (primitiva ("<") menor-prim)
    (primitiva (">") mayor-prim)
    (primitiva ("<=") menorigual-prim)
    (primitiva (">=") mayorigual-prim)
    (primitiva ("!=") diferente-prim)
    (primitiva ("==") igual-prim)

    ;;primitiva booleana
    (primitivaBooleana ("and") and-prim)
    (primitivaBooleana ("or") or-prim)
    (primitivaBooleana ("xor") xor-prim)
    (primitivaBooleana ("not") not-prim)

    ;;Primitiva listas
    (primitivaListas ("first") first-primList)
    (primitivaListas ("rest") rest-primList)
    (primitivaListas ("empty?") empty-primList)

    ;;Primitiva arrays
    (primitivaArray ("length") length-primArr)
    (primitivaArray ("index") index-primArr)
    (primitivaArray ("slice") slice-primArr)
    (primitivaArray ("setlist") setlist-primArr)

    ;;Primitiva cadenas
    (primitivaCadena ("concat") concat-primCad)
    (primitivaCadena ("string-length") length-primCad)
    (primitivaCadena ("elementAt") index-primCad)
    
    ;;Variables
    (var-decl ("var" (arbno identificador "=" expresion) "in" expresion) lvar-exp)
    (var-decl ("let" (arbno identificador "=" expresion) "in" expresion) let-exp)
    
    ;;Estructuras de datos
    (struct-decl ("struct" identificador "{" (arbno identificador) "}") struct-exp)

    ;;Expresiones regulares
    (regular-exp (identificador "::" identificador) list-match-exp)
    (regular-exp ("numero" "(" identificador ")") num-match-exp)
    (regular-exp ("cadena" "(" identificador ")") cad-match-exp)
    (regular-exp ("boolean" "(" identificador ")") bool-match-exp)
    (regular-exp ("array" "(" (separated-list identificador ",") ")") array-match-exp)
    (regular-exp ("empty") empty-match-exp)
    (regular-exp ("default") default-match-exp)
    )
  )

(sllgen:make-define-datatypes lexica gramatica)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes lexica gramatica)))



;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser lexica gramatica))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner lexica gramatica))



(define evaluar-programa
  (lambda(pgm)
    (cases programa pgm
      (a-programa (expresion listaExpresion)
        (cond
          [(null? listaExpresion) (eval-expression expresion)]
          [else (eval-expression listaExpresion)]
        )
      )
    )
  )
)


(define eval-expression
  (lambda(exp)
    (cases expresion exp
      ;Expresiones
      (bool-exp (bool-expresion) (eval-bool-expresion bool-expresion))
      (var-exp (identificador) identificador)
      (num-exp (numero-exp) (eval-num-exp numero-exp))
      (cadena-exp (id a) (list id a))
      (decl-exp (var-decl) (eval-var-decl var-decl))
      ;(lista-exp (expresion) (list (eval-expression expresion)))
      ;(cons-exp (exp1 exp2) (list (eval-expression exp1) (eval-expression exp2)))
      ;(empty-list-exp () '())
      ;(array-exp (expresion) (list (eval-expression expresion)))
      (prim-num-exp (exp1 prim exp2) (eval-primitiva-num exp1 prim exp2))
      ;(prim-bool-exp (prim exp) (eval-primitiva-bool prim exp))
      ;(prim-list-exp (exp f) (eval-primitiva-list exp))
      ;(prim-array-exp (exp a) (eval-primitiva-array exp))
      ;(prim-cad-exp (exp a) (eval-primitiva-cadena exp))
      ;(if-exp (exp1 exp2 exp3) (eval-if-exp exp1 exp2 exp3))
      ;(for-exp (id exp1 exp2 exp3 exp4) (eval-for-exp id exp1 exp2 exp3 exp4))
      ;(while-exp (exp1 exp2) (eval-while-exp exp1 exp2))
      ;(switch-exp (exp1 exp2 exp3 a) (eval-switch-exp exp1 exp2 exp3))
      ;(begin-exp (exp1 exp2) (eval-begin-exp exp1 exp2))
      ;(set-exp (id exp) (eval-set-exp id exp))
      ;(func-exp (listId exp) (eval-func-exp listId exp))
      ;(call-exp (exp listExp) (eval-call-exp exp listExp))
      ;(new-struct-exp (id listExp) (eval-new-struct-exp id listExp))
      ;(get-struct-exp (exp id) (eval-get-struct-exp exp id))
      ;(set-struct-exp (exp1 id exp2) (eval-set-struct-exp exp1 id exp2))
      ;(match-exp (exp listExp a) (eval-match-exp exp listExp))
      (else 0)
    )
  )
)

(define eval-primitiva-num
  (lambda (exp1 prim exp2)
    (eval-primitive prim (list (eval-expression exp1) (eval-expression exp2)))
  )
)

(define eval-primitive
  (lambda (prim args)
    (cases primitiva prim
      (sum-prim () (baseSuma args +))
      (minus-prim () (baseResta args -))
      (mult-prim () (baseMult args *))
      (mod-prim () (operation (cdr args) % (car args)))
      ;(elevar-prim () (operation (cdr args) ^ (car args)))
      ;(menor-prim () (< (car args) (cadr args)))
      ;(mayor-prim () (> (car args) (cadr args)))
      ;(menorigual-prim () (<= (car args) (cadr args)))
      ;(mayorigual-prim () (>= (car args) (cadr args)))
      ;(diferente-prim () (!= (car args) (cadr args)))
      ;(igual-prim () (== (car args) (cadr args)))
      (else 0)
    )
  )
)
;(operation args + 0)

(define baseMult
  (lambda (args prim)
    (cond
    [(and (number? (car args)) (number? (cadr args))) (operation args prim 1)] ;Numeros y flotantes
    [(contiene-caracter? (car args) #\b) (multBinario args)] ;Binarios
    ;[(contiene-caracter? (car args) #\h) (multHex args)] ;Hexadecimales
    ;[(contiene-caracter? (car args) #\x) (multOctal args)] ;Octales
    [else eopl:error "Error en la multiplicacion"]
    )
  )
)

(define multBinario
  (lambda (lst)
    (let
      (
        (primerParteNumerica (cadr ((splitString (car lst)) 1)))
        (segundaParteNumerica (cadr ((splitString (cadr lst)) 1)))
        (multiplicar (lambda (k z) (string-append "b"(aux-multiplicar-binaria k z))))
      )
      (multiplicar primerParteNumerica segundaParteNumerica)
    )   
  )
)

(define aux-multiplicar-binaria
  (lambda (bin1 bin2)
    (let* ([num1 (string->number bin1 2)]
           [num2 (string->number bin2 2)]
           [multiplicacion (* num1 num2)]
           [max-length (max (string-length bin1) (string-length bin2))]
           [result (number->string multiplicacion 2)]
           [result-length (string-length result)])
      (if (< result-length max-length)
          (string-append (make-string (- max-length result-length) #\0) result)
          result))))

(define baseSuma
  (lambda (args prim)
    (cond
    [(and (number? (car args)) (number? (cadr args))) (operation args prim 0)] ;Numeros y flotantes
    [(contiene-caracter? (car args) #\b) (sumBinario args)] ;Binarios
    [(contiene-caracter? (car args) #\h) (sumhex args)] ;Hexadecimales
    [(contiene-caracter? (car args) #\x) (sumOctal args)] ;Octales
    [else eopl:error "Error en la suma"]
    )
  )
)

(define baseResta
  (lambda (args prim)
    (cond
    [(and (number? (car args)) (number? (cadr args))) (operation args prim 0)] ;Numeros y flotantes
    [(contiene-caracter? (car args) #\b) (resBinario args)] ;Binarios
    [(contiene-caracter? (car args) #\h) (reshex args)] ;Hexadecimales
    [(contiene-caracter? (car args) #\x) (resOctal args)] ;Octales
    [else eopl:error "Error en la resta"]
    )
  )
)

(define resOctal
  (lambda (lst)
    (let
      (
        (primerParteNumerica (cadr ((splitString (car lst)) 2)))
        (segundaParteNumerica (cadr ((splitString (cadr lst)) 2)))
        (restar (lambda (k z) (string-append "0x"(aux-resta-octal k z))))
      )
      (restar primerParteNumerica segundaParteNumerica)
    )   
  )
)
(define aux-resta-octal
  (lambda (oct1 oct2)
    (let* ([num1 (string->number oct1 8)]
           [num2 (string->number oct2 8)]
           [resta (- num1 num2)]
           [max-length (max (string-length oct1) (string-length oct2))]
           [result (number->string resta 8)]
           [result-length (string-length result)])
      (if (< result-length max-length)
          (string-append (make-string (- max-length result-length) #\0) result)
          result))))

(define resBinario
  (lambda (lst)
    (let
      (
        (primerParteNumerica (cadr ((splitString (car lst)) 1)))
        (segundaParteNumerica (cadr ((splitString (cadr lst)) 1)))
        (restar (lambda (k z) (string-append "b"(aux-resta-binaria k z))))
      )
      (restar primerParteNumerica segundaParteNumerica)
    )   
  )
)

(define reshex
  (lambda (lst)
    (let
      (
        (primerParteNumerica (cadr ((splitString (car lst)) 2)))
        (segundaParteNumerica (cadr ((splitString (cadr lst)) 2)))
        (restar (lambda (k z) (string-append "hx"(aus-resta-hex k z))))
      )
      (restar primerParteNumerica segundaParteNumerica)
    )   
  )
)

(define aus-resta-hex
  (lambda (hex1 hex2)
    (let* ([num1 (string->number hex1 16)]
           [num2 (string->number hex2 16)]
           [resta (- num1 num2)]
           [max-length (max (string-length hex1) (string-length hex2))]
           [result (number->string resta 16)]
           [result-length (string-length result)])
      (if (< result-length max-length)
          (string-append (make-string (- max-length result-length) #\0) result)
          result))))

(define aux-resta-binaria
  (lambda (bin1 bin2)
    (let* ([num1 (string->number bin1 2)]
           [num2 (string->number bin2 2)]
           [resta (- num1 num2)]
           [max-length (max (string-length bin1) (string-length bin2))]
           [result (number->string resta 2)]
           [result-length (string-length result)])
      (if (< result-length max-length)
          (string-append (make-string (- max-length result-length) #\0) result)
          result))))

(define sumhex
  (lambda (lst)
    (let
      (
        (primerParteNumerica (cadr ((splitString (car lst)) 2)))
        (segundaParteNumerica (cadr ((splitString (cadr lst)) 2)))
        (sumar (lambda (k z) (string-append "hx"(aux-suma-hex k z))))
      )
      (sumar primerParteNumerica segundaParteNumerica)
    )   
  )
)

(define aux-suma-hex
  (lambda (hex1 hex2)
    (let* ([num1 (string->number hex1 16)]
           [num2 (string->number hex2 16)]
           [suma (+ num1 num2)]
           [max-length (max (string-length hex1) (string-length hex2))]
           [result (number->string suma 16)]
           [result-length (string-length result)])
      (if (< result-length max-length)
          (string-append (make-string (- max-length result-length) #\0) result)
          result))))

(define sumOctal
  (lambda (lst)
    (let
      (
        (primerParteNumerica (cadr ((splitString (car lst)) 2)))
        (segundaParteNumerica (cadr ((splitString (cadr lst)) 2)))
        (sumar (lambda (k z) (string-append "0x"(aux-suma-octal k z))))
      )
      (sumar primerParteNumerica segundaParteNumerica)
      ;(list primerParteNumerica segundaParteNumerica)
    )   
  )
)
(define aux-suma-octal
  (lambda (oct1 oct2)
    (let* ([num1 (string->number oct1 8)]
           [num2 (string->number oct2 8)]
           [suma (+ num1 num2)]
           [max-length (max (string-length oct1) (string-length oct2))]
           [result (number->string suma 8)]
           [result-length (string-length result)])
      (if (< result-length max-length)
          (string-append (make-string (- max-length result-length) #\0) result)
          result))))

(define sumBinario
  (lambda (lst)
    (let
      (
        (primerParteNumerica (cadr ((splitString (car lst)) 1)))
        (segundaParteNumerica (cadr ((splitString (cadr lst)) 1)))
        (sumar (lambda (k z) (string-append "b"(aux-suma-binaria k z))))
      )
      (sumar primerParteNumerica segundaParteNumerica)
    )   
  )
)

(define (contiene-caracter? cadena caracter)
  (let (
        [longitud (string-length cadena)]
       )
    (let loop (
               [index 0]
              )
      (cond
        [(= index longitud) #f] ; Hemos llegado al final de la cadena sin encontrar el carácter
        [(char=? (string-ref cadena index) caracter) #t] ; Hemos encontrado el carácter
        [else (loop (+ index 1))])))) ; Continuamos buscando en el siguiente índice




(define (aux-suma-binaria bin1 bin2)
  (let* ([num1 (string->number bin1 2)]
         [num2 (string->number bin2 2)]
         [suma (+ num1 num2)]
         [max-length (max (string-length bin1) (string-length bin2))]
         [result (number->string suma 2)]
         [result-length (string-length result)])
    (if (< result-length max-length)
        (string-append (make-string (- max-length result-length) #\0) result)
        result)))

(define splitString
  (lambda (str)
    (lambda (pos)
      (list (substring str 0 pos) (substring str pos (string-length str)))
    )
  )
)

; haz una funcion que recibe un string y recibe un parametro con la posicion del string y parte el string en dos partes
; y retorna una lista con las dos partes
(define string
  (lambda (str)
    (lambda (pos)
      (list (substring str 0 pos) (substring str pos (string-length str)))
    )
  )
)


(define operation
  (lambda (lst f acc)
    (cond
      [(null? lst) acc]
      [else
        (operation (cdr lst) f (f acc (car lst) ))])))


(define eval-var-decl
  (lambda (var)
    (cases var-decl var
      (lvar-exp (ids rands body) 0)
      (let-exp (ids rands body) (eval-lvar-exp ids rands body))
    )
  )
)

(define eval-lvar-exp
  (lambda (ids rands body)
    (let
      (
        (args (eval-rands rands))
      )
      (eval-expression body)
    )
  )
)

(define eval-rands
  (lambda (rands)
    (map (lambda (x) (eval-rand x )) rands)))

(define eval-rand
  (lambda (rand )
    (eval-expression rand)))


(define eval-num-exp
  (lambda (num)
    (cases numero-exp num
      (decimal-num (digitoDecimal) digitoDecimal)
      (octal-num (digito0ctal) digito0ctal)
      (bin-num (digitoBinario) digitoBinario)
      (hex-num (digitoHexadecimal) digitoHexadecimal)
      (float-num (flotante) flotante)
    )
  )
)
;0x213345

(define eval-if-exp
  (lambda (exp1 exp2 exp3)
    (if (eval-bool-expresion exp1)
      (eval-expression exp2)
      (eval-expression exp3)
    )
  )
)

(define eval-bool-expresion
  (lambda(exp)
    (cases bool-expresion exp
      (true-exp () #t)
      (false-exp () #f)
    )
  )
)


;El Interpretador (FrontEnd + Evaluación + señal para lectura +
(define interpretador
  (sllgen:make-rep-loop "--> "
    evaluar-programa
    (sllgen:make-stream-parser 
      lexica
      gramatica)))

(interpretador)

