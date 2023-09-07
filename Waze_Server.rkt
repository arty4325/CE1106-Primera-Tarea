#lang racket

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

; Ejemplo de uso:
;(escribir-en-archivo "rutas.txt" "Aqui voy a escribir el grafo\n")
(displayln (leer-archivo "rutas.txt"))
;(borrar-contenido "rutas.txt")

;Importante comentar la funcion de borrar para probarlo porque si no se va a leer un archivo vacio