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


;Funcion que esta en el interpretador
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
;;Representación de ambientes

(define scheme-value?
  (lambda(x)#t))

(define void-value?
  (lambda (v) (set! v v))
)


;Funcion que evalua una expresion de la gramatica
(define eval-expression
  (lambda(exp)
    (cases expresion exp
      ;Expresiones
      (bool-exp (bool-expresion) (eval-bool-expresion bool-expresion))
      (var-exp (identificador) identificador)
      (num-exp (numero-exp) (eval-num-exp numero-exp))
      (cadena-exp (id a) (list id a))
      (decl-exp (var-decl) (eval-var-decl var-decl))
      (lista-exp (listExp) (eval-rands listExp))
      (cons-exp (exp1 exp2) (list (eval-expression exp1) (eval-expression exp2)))
      (empty-list-exp () '())
      (array-exp (expresion) (list (eval-expression expresion)))
      (prim-num-exp (exp1 prim exp2) (eval-primitiva-num exp1 prim exp2))
      (prim-bool-exp (prim listExp) (eval-primitiva-bool prim listExp))
      ;(prim-list-exp (primList exp) (eval-primitiva-list primList exp))
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
;bool-exp --> funcion que evalua una expresion booleana y retorna su valor
(define eval-bool-expresion
  (lambda(exp)
    (cases bool-expresion exp
      (true-exp () #t)
      (false-exp () #f)
    )
  )
)
;********************************************************************************************************************
;num-exp --> funcion que evalua una expresion numerica y retorna su valor
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
;********************************************************************************************************************
;cadena-exp --> funcion que evalua una expresion de cadena y retorna su valor


;decl-exp --> funcion que evalua una expresion de declaracion y retorna su valor
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
;********************************************************************************************************************

;Primitiva Numerica
;prim-num-exp --> funcion que evalua una primitiva numerica y retorna su valor

(define eval-primitiva-num
  (lambda (exp1 prim exp2)
    (eval-primitive prim (eval-expression exp1) (eval-expression exp2))
  )
)
;(baseSuma args +)
(define eval-primitive
  (lambda (prim exp1 exp2)
    (cases primitiva prim
      (sum-prim () (operacionesBase exp1 exp2 +))
      (minus-prim () (operacionesBase exp1 exp2 - ))
      (mult-prim () (operacionesBase exp1 exp2 *))
      (mod-prim () (operacionModulo exp1 exp2))
      (elevar-prim () (operacionesBase exp1 exp2 expt))
      (menor-prim () (operacionBooleana exp1 exp2 <))
      (mayor-prim () (operacionBooleana exp1 exp2 >) )
      (menorigual-prim () (operacionBooleana exp1 exp2 <=))
      (mayorigual-prim () (operacionBooleana exp1 exp2 >=))
      (diferente-prim () (operacionDiferente exp1 exp2) )
      (igual-prim () (operacionBooleana exp1 exp2 = ))
    )
  )
)

;contiene-caracter? --> funcion que recibe una cadena y un caracter y retorna si la cadena contiene el caracter
(define contiene-caracter?
  (lambda (cadena caracter)
    (let
      (
        [longitud (string-length cadena)]
      )
      (let loop
        ([index 0])
        (cond
          [(= index longitud) #f] ; final de la cadena sin encontrar el carácter
          [(char=? (string-ref cadena index) caracter) #t] ; encontrado el carácter
          [else (loop (+ index 1))] ; Buscar en el siguiente índice
        )
      )
    )
  )
)
;splitString --> funcion que recibe un string y recibe un parametro con la posicion del string y parte el string en dos partes
(define splitString
  (lambda (str)
    (lambda (pos)
      (list (substring str 0 pos) (substring str pos (string-length str)))
    )
  )
)

(define operacionesBase
  (lambda (exp1 exp2 prim)
    (cond
      [(and (number? exp1) (number? exp2)) (prim exp1 exp2)] ;Numeros y flotantes
      [(contiene-caracter? exp1 #\b) (operation exp1 exp2 1 "b" 2 prim)] ;Binarios
      [(contiene-caracter? exp1 #\h) (operation exp1 exp2 2 "hx" 16 prim)] ;Hexadecimales
      [(contiene-caracter? exp1 #\x) (operation exp1 exp2 2 "0x" 8 prim)] ;Octales
      [else eopl:error "Error en la operación"]
    )
  )
)
(define operation
  (lambda (exp1 exp2 index complemento base operador)
    (let
      (
        (expresion1 (cadr((splitString exp1) index)))
        (expresion2 (cadr((splitString exp2) index)))
        (sumar (lambda (k z) (string-append complemento (aux-operation k z base operador))))
      )
      (sumar expresion1 expresion2)
    )
  )
)

(define (aux-operation operando1 operando2 base operador)
  (let*
    (
      [num1 (string->number operando1 base)]
      [num2 (string->number operando2 base)]
      [operacion (operador num1 num2)]
      [max-length (max (string-length operando1) (string-length operando2))]
      [result (number->string operacion base)]
      [result-length (string-length result)]
    )
    (if (< result-length max-length) (string-append (make-string (- max-length result-length) #\0) result) result)
  )
)

;Modulo
;Saca modulo de flotantes con desborde 0.01
(define modulo-flotante
  (lambda (a b) (- a (* b (floor (/ a b)))))
)

(define operacionModulo
  (lambda (exp1 exp2)
    (cond
    [(and (number? exp1) (number? exp2)) (modulo-flotante exp1 exp2)] ;Numeros y flotantes
    [(contiene-caracter? exp1 #\b) (operation exp1 exp2 1 "b" 2 modulo)] ;Binarios
    [(contiene-caracter? exp1 #\h) (operation exp1 exp2 2 "hx" 16 modulo)] ;Hexadecimales
    [(contiene-caracter? exp1 #\x) (operation exp1 exp2 2 "0x" 8 modulo)] ;Octales
    [else eopl:error "Error en el modulo"]
    )
  )
)


(define operacionBooleana
  (lambda (exp1 exp2 operando)
    (cond
    [(and (number? exp1) (number? exp2)) (operando exp1 exp2)] ;Numeros y flotantes
    [(contiene-caracter? exp1 #\b) (comparar (list exp1 exp2) 1 2 operando) ] ;Binarios
    [(contiene-caracter? exp1 #\h) (comparar (list exp1 exp2) 2 16 operando) ] ;Hexadecimales
    [(contiene-caracter? exp1 #\x) (comparar (list exp1 exp2) 2 8 operando) ] ;Hexadecimales
    [else eopl:error "Error en la operacion logica"]
    )
  )
)

(define convertir-base
  (lambda (n base)
    (string->number n base)
  )
)

;args = (b100 b10)
;index posicion de la cadena
;base del numero
(define comparar
  (lambda (args index base operando)
    (let
      (
        (primerParteNumerica (convertir-base (cadr ((splitString (car args)) index)) base))
        (segundaParteNumerica (convertir-base (cadr ((splitString (cadr args)) index)) base))
        (resultado (lambda (k z)
          (if
            (operando k z) #true #false
          ))
        )
      )
      (resultado primerParteNumerica segundaParteNumerica)
    )
  )
)

(define operacionDiferente
  (lambda (exp1 exp2)
    (cond
    [(and (number? exp1) (number? exp2)) (not (= exp1 exp2))] ;Numeros y flotantes
    [(contiene-caracter? exp1 #\b) (esDiferente (list exp1 exp2) 1 2) ] ;Binarios
    [(contiene-caracter? exp1 #\h) (esDiferente (list exp1 exp2) 2 16) ] ;Hexadecimales
    [(contiene-caracter? exp1 #\x) (esDiferente (list exp1 exp2) 2 8) ] ;Hexadecimales
    [else eopl:error "Error en el modulo"]
    )
  )
)

(define esDiferente
  (lambda (args index base)
    (let*
      (
        (primerParteNumerica (convertir-base (cadr ((splitString (car args)) index)) base))
        (segundaParteNumerica (convertir-base (cadr ((splitString (cadr args)) index)) base))
        (diferente (lambda ()
          (if
            (not (= primerParteNumerica segundaParteNumerica)) #true #false
          ))
        )
      )
      (diferente)
    )
  )
)

(define eval-primitiva-bool
  (lambda (prim listExp)
    (eval-bool prim (map (lambda (x) (eval-expression x)) listExp))
  )
)
(define eval-bool
  (lambda (prim args)
    (cases primitivaBooleana prim
      (and-prim () (primitivaAnd args))
      (or-prim () (primitivaOr args))
      (xor-prim () (primitivaXOR args))
      (not-prim () (list args))
    )
  )
)

(define primitivaAnd
  (lambda (lst)
    (cond
      [(null? lst) #t]
      [(eq? #f (car lst)) #f]
      [else (primitivaAnd (cdr lst))]
      )
    )
)

(define primitivaOr
  (lambda (lst)
    (cond
      [(null? lst) #f]
      [(eq? #t (car lst)) #t]
      [else (primitivaOr (cdr lst))]
      )
    )
)

(define (primitivaXOR lst)
  (cond
    [(null? lst) #f]  ; Si la lista está vacía, devuelve #f
    [(null? (cdr lst)) #f] ; Si la lista tiene un solo elemento, devuelve #f
    [(eq? (car lst) (cadr lst)) (primitivaXOR (cdr lst))] ; Si los dos primeros son iguales, sigue con el resto
    [else #t] ; Si encuentra dos elementos diferentes, devuelve #t
  )
) 

(define eval-primitiva-list
  (lambda (prim listExp)
    (eval-list prim (map (lambda (x) (eval-expression x)) listExp))
  )
)
(define eval-list
  (lambda (prim args)
    (cases primitivaListas prim
      (first-primList () args)
      ;(rest-primList () (list (cdr args)))
      ;(empty-primList () (list (null? args)))
      (else 0)
    )
  )
)















(define eval-if-exp
  (lambda (exp1 exp2 exp3)
    (if (eval-bool-expresion exp1)
      (eval-expression exp2)
      (eval-expression exp3)
    )
  )
)




;El Interpretador (FrontEnd + Evaluación + señal para lectura +
(define interpretador
  (sllgen:make-rep-loop "Proyecto --> "
          (lambda (pgm) (evaluar-programa pgm))
          (sllgen:make-stream-parser 
            lexica
            gramatica
          )
  )
)

(interpretador)
(provide (all-defined-out))

