;Variable global para almacenar todos los archivos de musica encontrados en la carpeta.
(defvar *Directorio*)
(setf Directorio nil)
;Variable Global del vector de hastables de metadatos del mp3
(defparameter *vector-metadatos-mp3* (make-array 1 :fill-pointer 0 :adjustable t))

(defun extraer-metadatos-aux (dir nombre-mp3); Extraccion de metadatos del mp3	
	(defparameter *Mdatos-mp3* (make-hash-table))
	(setf (gethash 'Nombre *Mdatos-mp3*) nombre-mp3)
	(run-shell-command (concatenate 'string "cd -- && cd "  dir " && id3 -R -l "    "\""  (subseq nombre-mp3  0 (- (length nombre-mp3) 4)) "\"" "*.mp3"):output "temp.txt" )
	(run-shell-command (concatenate 'string "cd -- && cd " dir " && grep . temp.txt > temp1.txt"))
	(with-open-file (stream "temp1.txt")
		(do ((line (read-line stream nil) (read-line stream nil))) ((null line));ciclo para la deteccion de los metadatos de la lectura del mp3 
			(when (String-Equal (subseq line 0 8) "Filename")
				(setf (gethash 'Filename *Mdatos-mp3*) (string-trim " " (subseq line 10)))
			)
			(when (String-Equal (subseq line 0 5) "Title")
				(setf (gethash 'Title *Mdatos-mp3*) (string-trim " " (subseq line 7)))
			)
			(when (String-Equal (subseq line 0 6) "Artist")
				(setf (gethash 'Artist *Mdatos-mp3*) (string-trim " " (subseq line 8)))
			)
			(when (String-Equal (subseq line 0 5) "Album")
				(setf (gethash 'Album *Mdatos-mp3*) (string-trim " " (subseq line 7)))
			)
			(when (String-Equal (subseq line 0 4) "Year")
				(setf (gethash 'Year *Mdatos-mp3*) (string-trim " " (subseq line 6)))
			)
			(when (String-Equal (subseq line 0 5) "Genre")
				(setf (gethash 'Genre *Mdatos-mp3*) (string-trim " " (subseq line 7 (- (length line) 4))))
			)
			(when (String-Equal (subseq line 0 5) "Track")
				(setf (gethash 'Track *Mdatos-mp3*) (string-trim " " (subseq line 7)))
			)
	)
	)
	;Agrega los metadatos del mp3 al vector global
	(print (gethash 'Filename *Mdatos-mp3* ))
	(vector-push-extend *Mdatos-mp3* *vector-metadatos-mp3*)
)

;Funcion que extrae la informacion de todos los mp3 encontrados en el directorio
(defun extraer-metadatos (direc)
(if (not (NULL Directorio))
	(dotimes (i (list-length Directorio))
	(extraer-metadatos-aux direc (file-namestring (ELT Directorio i)))
	(setf *msj-programa* " Se ha cargado con exito los archivos mp3")
)
(setf *msj-programa* "No se encontraron  o el directorio no es valido")
)
)

;;--------------------------------------------------------------------------------------------------------------------------------------------------
;; Busqueda


(defparameter *vector* (make-array 1 :fill-pointer 0 :adjustable t))


(defun buscar-titulo (titulo)
	(setf *vector* (make-array 1 :fill-pointer 0 :adjustable t))
	(dotimes (i (length *vector-metadatos-mp3*))
	(when (String-Equal (gethash 'Title (ELT *vector-metadatos-mp3* i)) titulo)
	(vector-push-extend (ELT *vector-metadatos-mp3* i) *vector*)))
	(visualizar-todo *vector*)
)


(defun buscar-artista (artist)
	(setf *vector* (make-array 1 :fill-pointer 0 :adjustable t))
	(dotimes (i (length *vector-metadatos-mp3*))
	(when (String-Equal
	(gethash 'Artist (ELT *vector-metadatos-mp3* i)) artist)
	(vector-push-extend
	(ELT *vector-metadatos-mp3* i) *vector*)))
	(visualizar-todo *vector*)
)

(defun buscar-album (album)
	(setf *vector* (make-array 1 :fill-pointer 0 :adjustable t))
	(dotimes (i (length *vector-metadatos-mp3*))
	(when (String-Equal
	(gethash 'Album (ELT *vector-metadatos-mp3* i)) album)
	(vector-push-extend
	(ELT *vector-metadatos-mp3* i) *vector*)))
	(visualizar-todo *vector*)
)


(defun buscar-year (year)
	(setf *vector* (make-array 1 :fill-pointer 0 :adjustable t))
	(dotimes (i (length *vector-metadatos-mp3*))
	(when (String-Equal
	(gethash 'Year (ELT *vector-metadatos-mp3* i)) year)
	(vector-push-extend
	(ELT *vector-metadatos-mp3* i) *vector*)))
	(visualizar-todo *vector*)
)


(defun buscar-genre (genre)
	(setf *vector* (make-array 1 :fill-pointer 0 :adjustable t))
	(dotimes (i (length *vector-metadatos-mp3*))
	(when (String-Equal
	(gethash 'Genre (ELT *vector-metadatos-mp3* i)) genre)
	(vector-push-extend
	(ELT *vector-metadatos-mp3* i) *vector*)))
	(visualizar-todo *vector*)
)


(defun buscar-filename (filename)
(setf *vector* (make-array 1 :fill-pointer 0 :adjustable t))
(dotimes (i (length *vector-metadatos-mp3*))
(when (String-Equal
(gethash 'Filename (ELT *vector-metadatos-mp3* i)) filename)
(vector-push-extend
(ELT *vector-metadatos-mp3* i) *vector*)
)
)
(visualizar-todo *vector*)
)

(defun modifica_directorio (direccion)
(setf Directorio
(directory (concatenate 'string "~/" direccion "/*.mp3"))
)
(extraer-metadatos direccion)
)

;;----------------------------------------------------INTERFACE----------------------------------------


(defparameter *Mostrar-busqueda* '(50 50 50 50 50 50))
(load "~/Descargas/ltk-0.96/ltk.fas"); carga la interfaz grafica

(defun cargar-boton ()
(ltk::clear-text *texto*)
(modifica_directorio (ltk::text *entry1*))
(ltk::append-text *texto* *msj-programa*)
(if (not (NULL Directorio))
(ltk::configure *entry1* :Text " ")
)
)
;Funcion visualizar todos los archivos
(defun visualizar-todo (*lista-disp*)
(ltk::clear-text *texto*)
(unless (= 0 (length *lista-disp*))
(ltk::append-text *texto* "|Filename")
(dotimes (j (-(ELT *Mostrar-busqueda* 0 ) 8))
(ltk::append-text *texto* " ")
)

(ltk::append-text *texto* " |Title")
(dotimes (j (-(ELT *Mostrar-busqueda* 1 ) 5)) 
(ltk::append-text *texto* " ")
)
(ltk::append-text *texto* " |Artist")
(dotimes (j (-(ELT *Mostrar-busqueda* 2 ) 6) )
(ltk::append-text *texto* " ")
)
(ltk::append-text *texto* " |Album")
(dotimes (j (-( ELT *Mostrar-busqueda* 3) 5))
(ltk::append-text *texto* " ")
)
(ltk::append-text *texto* " |Genre")
(dotimes (j (-(ELT *Mostrar-busqueda* 4) 5))
(ltk::append-text *texto* " ")
)
(ltk::append-text *texto* " |Track")
(dotimes (j  (ELT *Mostrar-busqueda* 5) )
(ltk::append-text *texto* " ")
)
(dotimes (i (length *lista-disp*))
(ltk::append-text *texto* (format nil "
|~A"
(gethash 'Filename (ELT *lista-disp* i))))

(dotimes (j
(- (ELT *Mostrar-busqueda* 0)
(length (gethash 'Filename (ELT *lista-disp* i)))))
(ltk::append-text *texto* " ")
)


	(if (NULL (gethash 'Title(ELT *lista-disp* i)))
		(ltk::append-text *texto* " |")
		(ltk::append-text *texto* (format nil " |~A"
		(gethash 'Title (ELT *lista-disp* i))))
	)
	(dotimes (j (- (ELT *Mostrar-busqueda* 1)
		(length (gethash 'Title (ELT *lista-disp* i)))))
		(ltk::append-text *texto* " ")
)


	(if (NULL (gethash 'Artist (ELT *lista-disp* i)))
	(ltk::append-text *texto* " |")
	(ltk::append-text *texto* (format nil " |~A"
	(gethash 'Artist (ELT *lista-disp* i))))
	)
	(dotimes (j
	(- (ELT *Mostrar-busqueda* 1)
	(length (gethash 'Artist (ELT *lista-disp* i)))))
	(ltk::append-text *texto* " ")
	)


	(if (NULL (gethash 'Album (ELT *lista-disp* i)))
	(ltk::append-text *texto* " |")
	(ltk::append-text *texto* (format nil " |~A"
	(gethash 'Album (ELT *lista-disp* i))))
	)
	(dotimes (j
	(- (ELT *Mostrar-busqueda* 1)
	(length (gethash 'Album (ELT *lista-disp* i)))))
	(ltk::append-text *texto* " ")
	)


	(if (NULL (gethash 'Genre (ELT *lista-disp* i)))
	(ltk::append-text *texto* " |")
	(ltk::append-text *texto* (format nil " |~A"
	(gethash 'Genre (ELT *lista-disp* i))))
	)
	(dotimes (j
	(- (ELT *Mostrar-busqueda* 1)
	(length (gethash 'Genre (ELT *lista-disp* i)))))
	(ltk::append-text *texto* " ")
	)


	(if (NULL (gethash 'Track (ELT *lista-disp* i)))
	(ltk::append-text *texto* " |")
	(ltk::append-text *texto* (format nil " |~A"
	(gethash 'Track (ELT *lista-disp* i))))
	)
	(dotimes (j
	(- (ELT *Mostrar-busqueda* 1)
	(length (gethash 'Track (ELT *lista-disp* i)))))
	(ltk::append-text *texto* " ")
	)
	)
	)
(when (= 0 (length *lista-disp*)) (ltk::append-text *texto*
"En la carpeta seleccionada no se encontraron archivos mp3!"))
)

;Variable para mensajes internos del programa
(defvar *msj-programa*)

(defun ventana-principal()
	(ltk::with-ltk ()
	(ltk::wm-title ltk::*tk*"Busqueda de mp3")
	(defvar *frame2* (make-instance 'ltk::frame))
	(defvar *texto* (make-instance 'ltk::text :master *frame2* :width 60  :height 20 :wrap "none" ))
	(defvar *scrolly* (make-instance 'ltk::scrollbar :master *frame2*))
	(ltk::configure *texto* :yscrollcommand
	(concatenate 'string (ltk::widget-path *scrolly*) " set"))
	(ltk::configure *scrolly* :command (concatenate 'string (ltk::widget-path *texto*) " yview"))
	(defvar *scrollx* (make-instance 'ltk::scrollbar :master *frame2* :orientation "horizontal"))
	(ltk::configure *texto* :xscrollcommand
	(concatenate 'string (ltk::widget-path *scrollx*) " set"))
	(ltk::configure *scrollx*
	:command
	(concatenate 'string (ltk::widget-path *texto*) " xview"))
	(ltk::grid *texto* 0 0 :sticky "snew")
	(ltk::grid *scrolly* 0 1 :sticky "ns")
	(ltk::grid *scrollx* 1 0 :sticky "ew")


	(defvar *frame1* (make-instance 'ltk::frame))
	(ltk::grid *frame1* 0 0)

	(ltk::grid *frame2* 1 0)

	(defvar *boton1* (make-instance 'ltk::button
	:Text "Cargar MP3"
	:master *frame1*
	:width 21
	:command (lambda ()
	(modifica_directorio (ltk::text *entry1*)))))
	(ltk::grid *boton1* 2 1 :Sticky "n")

	(defvar *label1* (make-instance 'ltk::label
	:Text " Ingrese el directorio EJ: Escritorio/Musica"
	:master *frame1*))
	(ltk::grid *label1* 0 0 :Sticky "ns")
	(ltk::configure *label1* :foreground :darkblue)

	(defvar *entry1* (make-instance 'ltk::entry
	:master *frame1*
	:width 20))
	(ltk::grid *entry1* 2 0 :Sticky "n")
	(ltk::configure *entry1* :foreground :darkblue)

	(defvar *boton2* (make-instance 'ltk::button
	:Text "Visualizar todos los archivos"
	:master *frame1*
	:command (lambda () (visualizar-todo *vector-metadatos-mp3*))))
	(ltk::grid *boton2* 4 1 :columnspan 2 :Sticky "w" )

	;Seccion Busqueda
	(defvar *labelB* (make-instance 'ltk::label
	:Text "            Realizar busqueda por:"
	:master *frame1*))
	(ltk::grid *labelB* 6 0 :columnspan 2 :Sticky "w")
	(ltk::configure *labelB* :foreground :darkblue)

	(defvar *boton3* (make-instance 'ltk::button
	:Text "Titulo"
	:master *frame1*
	:width 20
	:command (lambda ()
	(buscar-titulo (ltk::text *entry3*)))))
	(ltk::grid *boton3* 8 0 :Sticky "n")

	(defvar *entry3* (make-instance 'ltk::entry
	:master *frame1*
	:width 20))
	(ltk::grid *entry3* 8 1 :Sticky "n")
	(ltk::configure *entry3* :foreground :darkblue)

	(defvar *boton4* (make-instance 'ltk::button
	:Text "Artista"
	:master *frame1*
	:width 20
	:command (lambda ()
	(buscar-artista (ltk::text *entry4*)))))
	(ltk::grid *boton4* 10 0 :Sticky "n")

	(defvar *entry4* (make-instance 'ltk::entry
	:master *frame1*
	:width 20))
	(ltk::grid *entry4* 10 1 :Sticky "n")
	(ltk::configure *entry4* :foreground :darkblue)

	(defvar *boton5* (make-instance 'ltk::button
	:Text "Nombre"
	:master *frame1*
	:width 20
	:command (lambda ()
	(buscar-filename (ltk::text *entry5*)))))
	(ltk::grid *boton5* 12 0 :Sticky "n")

	(defvar *entry5* (make-instance 'ltk::entry
	:master *frame1*
	:width 20))
	(ltk::grid *entry5* 12 1 :Sticky "n")
	(ltk::configure *entry5* :foreground :darkblue)

	(defvar *boton6* (make-instance 'ltk::button
	:Text "Album"
	:master *frame1*
	:width 20
	:command (lambda ()
	(buscar-album (ltk::text *entry6*)))))
	(ltk::grid *boton6* 14 0 :Sticky "n")
	;;(ltk::configure *boton6* :foreground :blue)

	(defvar *entry6* (make-instance 'ltk::entry
	:master *frame1*
	:width 20))
	(ltk::grid *entry6* 14 1 :Sticky "n")
	(ltk::configure *entry6* :foreground :darkblue)

	(defvar *boton7* (make-instance 'ltk::button
		:Text "Genero"
		:master *frame1*
		:width 20
		:command (lambda ()
				(buscar-genre (ltk::text *entry7*)))))
	(ltk::grid *boton7* 16 0 :Sticky "n")
	;;(ltk::place *boton7* 5 5)

	(defvar *entry7* (make-instance 'ltk::entry
	:master *frame1*
	:width 20))
	(ltk::grid *entry7* 16 1 :Sticky "n")
	(ltk::configure *entry7* :foreground :darkblue)

	)
	)
	(ventana-principal)
