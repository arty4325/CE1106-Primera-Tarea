#lang racket

(require racket/gui)

(define frame (new frame% [label "Cliente Waze"] [width 300] [height 200]));gui de prueba


(define (crear-archivo nombre-archivo);Funcion para crear un archivo.txt, recibe el nombre del archivo deseado y es importante que cliente y servidor compartan su ubicacion en la compu
  (define archivo (open-output-file nombre-archivo #:exists 'truncate)); crea archivo o lo sobreescribe en caso de ya existir (para evitar caidas)
  (close-output-port archivo));Cierra archivo

(define (leer-archivo nombre-archivo);Funcion para leer el contenido de un archivo, recibe el nombre del acrhivo
  (define archivo-lectura (open-input-file nombre-archivo));Abre archivo
  (let ((contenido-leido (read-line archivo-lectura)));Lee la primera linea disponible (Esto se puede modificar segun necesidades)
    (close-input-port archivo-lectura);Cierra archivo
    contenido-leido));Retorna contenido leido

(define (borrar-contenido nombre-archivo);Funcion para borrar contenido de un archivo (importante para mantener orden de envio de informacion)
  (define archivo (open-output-file nombre-archivo #:exists 'replace));Abre un archivo nuevo con el mismo nombre pero en blanco o lo reemplaza
  (close-output-port archivo));Cierra

(define (escribir-en-archivo nombre-archivo contenido);Funcion para escribir contenido en el arcchivo, recibe nombre de archivo y contenido a escribir (Cada intervencion debe seguir un formato especifico para lograr ser procesada)
  (define archivo (open-output-file nombre-archivo #:exists 'truncate));Abre archivo
  (write-string contenido archivo);Escribe en el archivo 
  (close-output-port archivo));Cierra archivo

(define (leer-archivo-y-esperar nombre-archivo);funcion de ciclo que espera la respuesta del server
  (let loop ()
    (define contenido (leer-archivo nombre-archivo))
    (cond
      [(string=? (substring contenido 0 1) "R");Si la primera linea del archiv contiene una R es porque ya se reciibio una respuesta
       (displayln "Se encontró una 'R' en la primera línea. Saliendo del bucle.")
       ; Salir del bucle cuando se encuentra una 'R'
      ]
      [else
       (sleep 3);Espera 3 segundow antes de la próxima lectura
       (displayln "No se encontró una 'R' en la primera línea. Continuando...")
       (loop)])))

(define (cliente)
; Iniciar el hilo para leer el archivo y esperar 'R'
 (thread (lambda () (leer-archivo-y-esperar "rutas.txt"))))

; Ejemplo de uso:
(crear-archivo "rutas.txt")
(escribir-en-archivo "rutas.txt" "Aqui voy a escribir el grafo\n")
;(displayln (leer-archivo "rutas.txt"))
;(borrar-contenido "rutas.txt")

; Ejecutar la función del cliente
(cliente)

(define button (new button% [parent frame]
                          [label "Botón Vacío"]
                          ))

;Importante comentar la funcion de borrar para probarlo porque si no se va a leer un archivo vacio
(send frame show #t)
