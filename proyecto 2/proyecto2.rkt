#lang racket
; con la funcion (funciones) puedes ejecutar el archivo funciones.txt


;funcion para leer un archivo
(define (leer-archivo nombre)
  (define act (open-input-file nombre))
  (define x (read act))
  (close-input-port act)x)


;En archivoProductos cambia "productos.txt" por "productos2.txt" para probar un almacen de 10 x 4
(define archivoProductos "productos.txt")

; funcion para leer el valor de mi archivo ventanilla
(define (Ventanilla)
  (define ventanilla (leer-archivo "ventanilla.txt"))
  ventanilla) 

; funcion para leer el valor de mi archivo productos
(define (Almacen)
  (define almacen (leer-archivo archivoProductos))
  almacen)

; funcion para leer y ejecutar las funciones de mi archivo funciones
(define (funciones)
  (load "funciones.txt"))

;para posicionar la ventanilla en la posicionn (0 0) descomenta esta fucnion
;(regresar-ventanilla) 

;Automata encargado de analizar el lexico
(define estado-inicial 'q0)
(define estado-aceptacion 'q1)
(define lexico '(s b i d))

(define (transicion estado simbolos) ; si todos los simbolos corresponden a el lexico transiciona el estado
  (if (todos-miembros lexico simbolos)
      estado-aceptacion
      estado))

(define (todos-miembros conjunto lista) ; verifica que todos los simbolos de la lista correspondan con el lexico
  (cond
    ((null? lista) #t)
    ((member (car lista) conjunto)
     (todos-miembros conjunto (cdr lista)))
    (else #f)))


;FUNCION ENCARGADA DE ANALIZAR EL LEXICO Y EJECUTAR LA FUNCION DE EL ESTADO ACEPTOR EN EL CASO DE SER CORRECTO
(define (Mover lista)
  (if (eq? (transicion estado-inicial lista) estado-aceptacion)
      (mover lista)
      "La cadena no es aceptada por el autómata."))

; la fucnion mover analiza la lista aceptada por el automata y realiza la accion del movimiento (subir, bajar, izquierda, derecha)
(define (mover lista)
  (cond
    ((null? lista)  (newline)  (display "\n movimiento del carrusel completado con exito. \n"))
    ((eq? (car lista) 'b)(bajar-y-guardar archivoProductos)(Mover (cdr lista)))
    ((eq? (car lista) 's)(subir-y-guardar archivoProductos)(Mover (cdr lista)))
    ((eq? (car lista) 'i)(display (izquierda))(Mover (cdr lista)))
    ((eq? (car lista) 'd)(display (derecha))(Mover (cdr lista)))))
   

;funcion para contar el numero de filas de una matriz
(define (contar-filas matriz)
  (if (null? matriz)
      0
      (+ 1 (contar-filas (cdr matriz)))))
    

;funcion para contar el numero de columnas de una matriz
(define (contar-columnas matriz)
  (if (null? matriz)
      0
      (length (car matriz))))

(display (list "La medida de tu carrusel es de" (contar-filas (Almacen)) "X" (contar-columnas (Almacen))))
(display "\n")


;FUNCION PARA ENCOTRAR LOS PORDUCTOS QUE TIENEN UN INVENTARIO MENOR A N
(define (inventario-menor matriz n)
  (filter (lambda (fila) (not (null? fila))) (map (lambda (fila) (filter (lambda (producto) (< (cadr producto) n)) fila))matriz)))


;FUNCION PARA OBTENER EL TOTAL DEL INVEARIO DE TODO EL ALMACEN
(define (total-almacen matriz)
  (define (sumar-matriz matriz)
    (if (null? matriz)
        0
        (+ (sumar-lista (car matriz)) (sumar-matriz (cdr matriz))))) (sumar-matriz matriz)) 


(define (elemento-total elemento) ;Funcion para obtener el precio total de un producto
  (* (cadr elemento) (caddr elemento)))

(define (sumar-lista lista) ;obtiene el total de una fila
  (if (null? lista)
      0
      (+ (elemento-total (car lista)) (sumar-lista (cdr lista)))))

;FUNCION PARA OBTENER EL TOTAL DE UN PRODUCTO         
(define total-producto
         (lambda (producto)
           (elemento-total (buscar-elemento (Almacen) (car (posicion (Almacen) producto)) (car(cdr (posicion (Almacen) producto)))))))


;funcion para guardar el resultado en un archivo
(define (guardar-en-archivo resultado archivo)
  (call-with-output-file archivo
    (lambda (output-port)(display resultado output-port)) #:exists 'truncate))  



;funcion para subir, mueve la primera fila al final de la lista
  (define (subir matriz)
  (if (null? matriz)
      '() 
        (append (cdr matriz) 
                (list (car matriz)))))

;fucnion para bajar, mueve la ultima fila al principio de la lista
  (define (bajar matriz)
    (if (null? matriz)
        '()
        (append (list (car (reverse matriz))) 
                (reverse (cdr (reverse matriz))))))


;funcion para subir el carruserl y guardar
(define (subir-y-guardar archivo)
  (display "\n")
  (display "subir")

    (guardar-en-archivo (subir  (Almacen)) archivo))
    

;funcion para bajar y guardar el resultado
(define (bajar-y-guardar archivo)
  (display "\n")
  (display "bajar")

    (guardar-en-archivo (bajar (Almacen)) archivo))
    


;derecha, esta funcion suma 1 a la posicion de la columna de la ventanilla (no puede ser mayor al numero de columnas)
(define (derecha)
  (cond
    ((null?  (Ventanilla)) (newline) "Movimiento a la derecha inválido. " )
    ((>= (cadr  (Ventanilla)) (- (contar-columnas (Almacen))1)) (newline) "Movimiento a la derecha inválido")
    (else

     (guardar-en-archivo (list (car (Ventanilla)) (+ 1 (cadr (Ventanilla)))) "ventanilla.txt") (newline) 
     (format "Movimiento a la derecha válido ~a" (Ventanilla)))))

;izquierda, esta fucnion resta 1 a la posicion de la columna de la ventanilla (no puede ser menor a 0)
(define (izquierda)
  (cond
    ((null? (Ventanilla)) (newline) "Movimiento a la izquierda inválido")
    ((<= (cadr (Ventanilla)) 0 ) (newline) "Movimiento a la izquierda inválido")
    (else
     (guardar-en-archivo (list (car (Ventanilla)) (-  (cadr (Ventanilla)) 1)) "ventanilla.txt")(newline) 
     (format "Movimiento a la izquierda válido ~a" (Ventanilla)))))

  

;funcion para saber saber si un elemento existe en una lista, devuelve TRUE si el producto se encuentra en la lista
(define (elemento-en-lista? elemento lista)
  (cond
    ((null? lista) #f) 
    ((equal? (caar lista) elemento) #t) 
    (else (elemento-en-lista? elemento (cdr lista)))))



(define (buscar-con-bajar matriz elemento) ;funcion para saber cuantas veces se necesita usar la fucnion bajar para encontrar el producto
  (cond
    ((null? matriz) 0) 
    ((elemento-en-lista? elemento (car matriz))0) 
    (else(+ 1 (buscar-con-bajar (bajar matriz) elemento)))))


(define (buscar-con-subir matriz elemento) ;funcion para saber cuantas veces se necesita usar la fucnion bajar para encontrar el producto
  (cond
    ((null? matriz) 0) 
    ((elemento-en-lista? elemento (car matriz))0) 
    (else(+ 1 (buscar-con-subir (subir matriz) elemento)))))


(define (bajar-y-guardar-n-veces n matriz archivo) ; ejecuta la funcion bajar-y-guardar n veces
  (if (= n 0)
      '()
      (cons (bajar-y-guardar archivo)
            (bajar-y-guardar-n-veces (- n 1) matriz archivo))))

(define (subir-y-guardar-n-veces n matriz archivo) ; ejecuta la funcion subir-y-guardar n veces
  (if (= n 0)
      '()
      (cons (subir-y-guardar archivo)
            (subir-y-guardar-n-veces (- n 1) matriz archivo))))

;FUNCION PARA SABER EL CAMINO MAS CORTO ENTRE SUBIR Y BAJAR 
(define (camino-corto matriz nuevoElemento archivo)
  (cond
    ((null? matriz) 0)
    ((< (buscar-con-bajar matriz nuevoElemento) (buscar-con-subir matriz nuevoElemento))
     (bajar-y-guardar-n-veces (buscar-con-bajar matriz nuevoElemento) matriz archivo))
    (else
     (subir-y-guardar-n-veces (buscar-con-subir matriz nuevoElemento) matriz archivo))))

;FUNCION PARA SABER LOS MOVIMIENTOS NECESARIOS EN X "DERECHA" O "IZQUIERDA"
(define (posicion-x producto)
  (cond
    ((< (car (cdr (posicion (Almacen) producto))) (car (cdr (Ventanilla)))) (imprime-izquierda (car (cdr (Ventanilla)))))
  
    ((> (car (cdr (posicion (Almacen) producto))) (car (cdr (Ventanilla)))) (imprime-derecha (car (cdr (posicion (Almacen) producto)))))
       
    ((= (car (cdr (posicion (Almacen) producto))) (car (cdr (Ventanilla))))(display ""))))


(define (imprime-izquierda n) ;imprime la palabra izquierda N veces
  (cond
    ((= n 0) (display "\n"))  
     ((imprime-izquierda (- n 1))(display "izquierda \n"))))

(define (imprime-derecha n) ;imprime la palabra derecha N veces
  (cond
    ((= n 0) (display "\n"))  
     ((imprime-derecha (- n 1))(display "derecha \n")))) 




;FUNCIONES DE LA VENTANILLA
(define (ver-ventanilla) ;funcion para ver que producto se encuentra en la ventanilla
  (display "el producto de la ventanilla es ")
  (buscar-elemento (Almacen) (car (Ventanilla)) (car (cdr (Ventanilla)))))

(define (regresar-ventanilla) ;regresa la ventanilla a la posicion (0 0)
   (guardar-en-archivo '(0 0) "ventanilla.txt")) 


;FUNCIONES DE BUSQUEDA
(define (buscar-elemento matriz fila columna) ;busca un elemento en la estructura de datos introducion la fila y la columna
   (regrear-columna (obtener-fila matriz fila) columna))


;funcion para buscar un elemento de una matriz
(define (regrear-columna lista n)
  (cond
    ((= n 0)(car lista))
    ((regrear-columna (cdr lista) (- n 1)))))

;funcion que me devuelve la fila del indice dado
(define (obtener-fila matriz indice)
  (if (or (< indice 0) (null? matriz))
      '() 
      (if (= indice 0)
          (car matriz) 
          (obtener-fila (cdr matriz) (- indice 1))))) 


  
; funcion para encontrar la columna y la fila de un producto, para encotrar la fila utilice la funcion buscar-con-bajar definida anteriormente
(define (posicion matriz producto) ; intorducion el nombre del producto te devuelve su posicion dentro del plano (fila columna)
  (list  (buscar-con-subir matriz producto) (columna producto (obtener-fila matriz (buscar-con-subir matriz producto)))))

(define (columna nombre lista) 
  (elemento-en-lista-aux nombre lista 0))

(define (elemento-en-lista-aux nombre lista posicion) ; esta fucnion me regresa la posicion dela columna
  (cond
    ((null? lista) -1) 
    ((equal? (caar lista) nombre) posicion) 
    (else (elemento-en-lista-aux nombre (cdr lista) (+ posicion 1)))))


;funcion para agregar inventario a un producto
(define (agregar producto cantidad)
  (guardar-en-archivo (actualizar-almacen (Almacen) producto cantidad 'agregar) archivoProductos))

;fucnion para retirar inventario de un producto
(define (retirar producto cantidad)
  (guardar-en-archivo (actualizar-almacen (Almacen) producto cantidad 'retirar) archivoProductos))


(define (actualizar-producto fila producto cantidad operacion) ; funcion que me retorna la fila con el producto actualizado
  (cond
    ((null? fila) '())
    ((equal? (car (car fila)) producto)
     (if (equal? operacion 'agregar)
         (cons (list producto (+ cantidad (cadr (car fila))) (caddr (car fila))) ; si la funcion llamada es agregar suma la cantidad al inventario del producto dado
               (actualizar-producto  (cdr fila) producto cantidad operacion))
         
         (cons (list producto (max 0 (- (cadr (car fila)) cantidad)) (caddr (car fila))) ; si la funcion llamada es retirar resta la cantidad al inventario del producto dado
               (actualizar-producto  (cdr fila) producto cantidad operacion))))
    (else(cons (car fila)
               (actualizar-producto  (cdr fila) producto cantidad operacion))))) 


(define (actualizar-almacen almacen producto cantidad operacion) ;funcion para rearmar la estructura de datos con el inventario actualizado
  (cond
    ((null? almacen) '())
    (else (cons (actualizar-producto  (car almacen) producto cantidad operacion)(actualizar-almacen (cdr almacen) producto cantidad operacion)))))


;FUNCION PARA AGREGAR INVENTARIO
(define (Agregar . args) ; a la funcion Agregar le puedes pasar 1 O 2 parametros y tendra un comportamiento diferente dependido de el numero
  (cond
    ((= (length args) 2)
     (Agregar-dos-argumentos (car args) (cadr args)))
    ((= (length args) 1)
     (Agregar-uno-argumento (car args)))))

;Agregar con dos argumentos busca el producto dado dentro de la estructura de datos, lo actualiza, actualiza la ventanilla e imprime el camino mas corto para llegar a el
(define (Agregar-dos-argumentos producto cantidad) 
  (begin
    (agregar producto cantidad)
    (display "Se agregaron ")
    (display cantidad)
    (display " ")
    (display producto)
    (newline)
    (camino-corto (Almacen) producto archivoProductos)
     (posicion-x producto)
     (newline)
    (guardar-en-archivo (posicion (Almacen) producto) "ventanilla.txt")))

;Agregar con un argumento actualiza el inventario de el producto que se encuentra en la ventanilla
(define (Agregar-uno-argumento cantidad)
  (Agregar (car(buscar-elemento (Almacen) (car (Ventanilla)) (car (cdr (Ventanilla))))) cantidad)) 

;FUNCION PARA RETIRAR INVENTARIO
(define (Retirar . args) ; a la funcion Agregar le puedes pasar 1 O 2 parametros y tendra un comportamiento diferente dependido de el numero
  (cond
    ((= (length args) 2)
     (Retirar-dos-argumentos (car args) (cadr args)))
    ((= (length args) 1)
     (Retirar-uno-argumento (car args)))))

;Retirar con dos argumentos busca el producto dado dentro de la estructura de datos, lo actualiza, actualiza la ventanilla e imprime el camino mas corto para llegar a el
(define (Retirar-dos-argumentos producto cantidad)
  (begin
    (retiro-maximo cantidad producto)
    (retirar producto cantidad)
    (camino-corto (Almacen) producto archivoProductos)
    (posicion-x producto) 
     (newline)
    (guardar-en-archivo (posicion (Almacen) producto) "ventanilla.txt")))

;Retirar con un argumento resta la cantidad a el inventario del producto que se encuentra en la ventanilla
(define (Retirar-uno-argumento cantidad)
  (Retirar (car(buscar-elemento (Almacen) (car (Ventanilla)) (car (cdr (Ventanilla))))) cantidad))


;retiro-maximo verifica que el numero de productos a retirar no sea mayor a el numero que se encuentran en el inventario, en el caso de ser asi retira todo el inventario posible
(define (retiro-maximo cantidad producto)
  (cond
    ((<  (cadr (buscar-elemento (Almacen) (car (posicion (Almacen) producto)) (car(cdr (posicion (Almacen) producto))))) cantidad)
     (display "El numero a retirar es mayor que el numero de productos, se retiraran todos los productos posibles \n")
     (display "se retiraron: ")
     (display (cadr (buscar-elemento (Almacen) (car (posicion (Almacen) producto)) (car(cdr (posicion (Almacen) producto)))))))
   (else
     (display "Se retiraron ")
     (display cantidad)
     (display " ")
     (display producto)
     (newline))))


;final
               



