;;Clase de pdf
(defclass pdf ()
  ((titulo :initarg :titulo :initform "")
	 (autor :initarg :autor :initform "")
	 (materia :initarg :materia :initform "")
	 (palabrasclave :initarg :palabrasclave :initform "")
	 (fechacreacion :initarg :fechacreacion :initform "")))

;;Imprime los slots de una instancia pdf cuyo autor sea el indicado
(defun imprimirautor(elpdf elautor)
  (if (busq elautor (concatenate 'list (slot-value elpdf 'autor)))
  (format t "Titulo - ~S - Autor - ~S - Materia - ~S - PalabrasClave - ~S - Fecha Creacion - ~S~%" (slot-value elpdf 'titulo) (slot-value elpdf 'autor) (slot-value elpdf 'materia) (slot-value elpdf 'palabrasclave) (slot-value elpdf 'fechacreacion)))

;;Hash table que va a contener como key titulo del pdf y como value instancias de pdf
(defparameter *ht* (make-hash-table))

;;Recorre la ht y llama a imprimir los slots de las instancias pdf cuyo autor sea el indicado	
(defun buscarautor (elautor) 
  (maphash #'(lambda (k v) (imprimirautor v elautor)) *ht*))

;; Imprime la Hash table
(defun print-ht ()
 (maphash #'(lambda (k v) (imprimirpdf v)) *ht*))

;;Inserta un pdf en el hash
(defun insert (elpdf) (setf (gethash(slot-value elpdf 'titulo) *ht*) elpdf))