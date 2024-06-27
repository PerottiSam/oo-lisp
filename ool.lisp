;;;; -*- Mode: Lisp -*-
;;;; Perotti Samuele 899817


;;;; Per gestire le hash-table
(defparameter *classes-specs* (make-hash-table))
(defun add-class-spec (name class-spec)
  (setf (gethash name *classes-specs*) class-spec))
(defun get-class-spec (name)
  (gethash name *classes-specs*))


;;;; def-class definisce la struttura di una classe e la memorizza in una
;;;; locazione centralizzata (una variabile globale).
(defun def-class (class-name parents &rest part)
  (cond ((not (atom class-name)) (error "Error: Class-name is not an atom"))
	((is-class class-name) (error "Error: Class is already defined"))
	((not (parental-control parents))
	 (error "Error: Parents list is invalid")))
  (add-class-spec
   class-name
   (append (list class-name)
	   (append
	    (list
	     (remove-duplicates parents :from-end T))
	    (list
	     (manipulate-part part
			      (get-supclasses-from-list
			       (remove-duplicates parents :from-end T))
			      )))))
  class-name)


;;;; make: crea una nuova istanza di una classe
(defun make (class-name &rest fields)
  (cond ((not (is-class class-name)) (error "Class-name is not a class"))
	((check-fields class-name fields) (append (list 'oolinst)
						  (append (list class-name)
							  fields)))
	(T (error "Error in field assignment!"))))


;;;; is-class: restituisce T se l’atomo passatogli è il nome di una classe.
(defun is-class (class-name)
  (if (get-class-spec class-name) T NIL))


;;;; is-instance: restituisce T se l’oggetto passatogli è l’istanza di
;;;; una classe qualunque se class-name è T.
;;;; Altrimenti restituisce T solamente se instance è l'istanza di una classe
;;;; che ha class-name come superclasse.
(defun is-instance (instance &optional (class-name T))
  (cond ((and (equal (first instance) 'OOLINST)
	      (equal class-name 'T))
	 T)
	((equal (first instance) 'OOLINST)
	 (not (null
	       (member class-name
		       (append (list (first (rest instance)))
			       (get-supclasses (first (rest instance))))))))
	(T (error "Not an instance!"))))


;;;; field: estrae il valore di un campo da una classe.
(defun field (instance field-name)
  (if (is-instance instance)
      (let ((field-value
	     (get-value-instance (rest (rest instance)) field-name)))
	(if (not (null field-value))
	    field-value
	    (if (null (get-value-classes
		       (append (list (first (rest instance)))
			       (get-supclasses (first (rest instance))))
		       field-name))
		(error "Error: Unknown field")
		(first (rest (get-value-classes
			      (append (list (first (rest instance)))
				      (get-supclasses (first (rest instance))))
			      field-name))))))
      (error "Error: not an intance")))


;;;; field*: estrae il valore da una classe
;;;; percorrendo una catena di attributi.
(defun field* (instance &rest field-names)
  (cond ((null field-names) nil)
	;;;; dato che first field-names puo essere una lista...
	(T (if (listp (first field-names))
	       (if (equal (length (first field-names)) 1)
		   (field instance (first (first field-names)))
		   (field* (field instance (first (first field-names)))
			   (rest (first field-names))))
	       (if (equal (length field-names) 1)
		   (field instance (first field-names))
		   (field* (field instance (first field-names))
			;;;; Qui first fieldnames diventa una lista
			;;;; alla prossima chiamata field*
			   (rest field-names)))))))


;;;; get-value-instance: Recupera il valore di field-name cercando
;;;; direttamente nell'istanza
(defun get-value-instance (fields field-name)
  (cond ((null fields) nil)
	((equal (first fields) field-name) (first (rest fields)))
	(T (get-value-instance (rest (rest fields)) field-name))))


;;;; get-value-classes: Recupera il valore del campo field-name cercando
;;;; ricorsivamente nelle classi passate come lista.
(defun get-value-classes (classes field-name)
  (cond ((null classes) nil)
	((not (null
	       (get-value-part (first (last (get-class-spec (first classes))))
			       field-name)))
	 (get-value-part (first (last (get-class-spec (first classes))))
			 field-name))
	(T (get-value-classes (rest classes) field-name))))


;;;; get-value-part: Recupera il valore del campo field-name cercnado
;;;; 'fields in part
(defun get-value-part (part field-name)
  (cond ((null part) nil)
	((equal (first (first part)) 'fields)
	 (get-value-fields (rest (first part)) field-name))
	(T (get-value-part (rest part) field-name))))


;;;; get-value-fields: Recupera il valore del campo field-name cercando
;;;; nelle definizioni/assegnazioni dei campi
;;;; all'interno di '(fields (...) ...)
(defun get-value-fields (fields field-name)
  (cond ((null fields) nil)
	((equal (first (first fields)) field-name) (first fields))
	(T (get-value-fields (rest fields) field-name))))


;;;; check-fields: Controlla che i campi a cui sono stati assegnati dei valori
;;;; durante la make esistano in class-name o in uno dei suoi parents
;;;; e che abbiano lo stesso tipo (se presente)
(defun check-fields (class-name fields)
  (cond ((null fields) T)
	(T (and (check-single-field class-name (first fields)
				    (first (rest fields)))
		(check-fields class-name (rest (rest fields)))))))


;;;; check-single-field: Controlla che field-name appartenga a class-name
;;;; o un suo parent e che il tipo di field-value sia conforme con quello
;;;; definito durante def-class (se presente)
(defun check-single-field (class-name field-name field-value)
  (let ((found (get-value-classes (append (list class-name)
					  (get-supclasses class-name))
				  field-name)))
    (cond ((null found) (error "Error in fields assignment!"))
	  ((equal (length found) 2) T)
	  ((equal (length found) 3)
	   (is-of-type (first (last found)) field-value)))))


;;;; is-of-type: Genera un errore se value non è del tipo type
(defun is-of-type (type value)
  (cond ((null value) T)
        ((not (null (get-class-spec type)))
	 (not (null (member type
			    (cons (first (rest value))
				  (get-supclasses (first (rest value))))))))
	((equal type T) T)
	(T (if (typep value type) T (error "Error: ~w is not of type ~w"
					   value type)))))


;;;; parental-control si assicura che la lista passata come argomento contenga
;;;; delle Classi precedentemente dichiarate come tali
(defun parental-control (parents)
  (cond ((null parents) T)
	((eql parents '()) T)
	(T (and (is-class (first parents)) (parental-control(rest parents))))))


;;;; manipulate-part: Percorre part, se trova fields,
;;;; chiama check-fields-subtype. Altrimenti se trova un method,
;;;; chiama process-method per creare il metodo.
(defun manipulate-part (part supclasses)
  (cond ((null part) '())
	((equal part '()) '())
	((equal (first (first part)) 'fields)
	 (let ((fields (replace-in-fields (rest (first part)))))
	   (check-defclass-fields-type fields)
	   ;;;; (check-fields-subtype fields supclasses)
	   (append (list (append (list 'fields)
				 (check-fields-subtype fields supclasses)))
		   (manipulate-part (rest part) supclasses))))
	((equal (first (first part)) 'methods)
	 (append (list (append (list 'methods)
			       (manipulate-methods (rest (first part)))))
		 (manipulate-part (rest part) supclasses)))))


;;;; replace-in-fields: Chiama prima replace-atom-field-name e poi
;;;; replace-with-value
(defun replace-in-fields (lst)
  (cond ((null lst) lst)
	(T (let ((fields (replace-atom-field-name lst)))
	     (replace-with-value fields)))))


;;;; replace-atom-field-name: Dato che i field in defclass possono essere
;;;; solamente simboli che equivalgono al nome del campo, li sostituisce nella
;;;; forma classica (field-name value) dove value è NIL
(defun replace-atom-field-name (arg)
  (cond ((null arg) nil)
	((atom (car arg)) (cons (list (car arg) NIL)
				(replace-atom-field-name (cdr arg))))
	(T (cons (car arg) (replace-atom-field-name (cdr arg))))))


;;;; replace-with-value: Sostituisce i simboli con i loro valori assegnati
;;;; tramite defparameter.
(defun replace-with-value (lst)
  (if lst
      (cons (if (consp (car lst))
		(replace-with-value (car lst))
		(if (and (symbolp (car lst))
			 (boundp (car lst)))
		    (symbol-value (car lst))
		    (car lst)))
	    (replace-with-value (cdr lst)))
      nil))


;;;; check-defclass-fields-type: Chiama check-defclass-field-type su ogni
;;;; field dichiarato in defclass
(defun check-defclass-fields-type (fields)
  (cond ((null fields) T)
	(T (and (check-defclass-field-type (first fields))
		(check-defclass-fields-type (rest fields))))))


;;;; check-defclass-field-type: Controlla che i campi dichiarati in defclass
;;;; rispettino il tipo a loro assegnatogli
(defun check-defclass-field-type(field)
  (cond ((equal (length field) 2) T)
	(T (if (is-of-type (first (last field)) (first (rest field)))
	       T
	       (error "Error: ~w is not of type ~w"
		      (first (rest field)) (first (last field)))))))


;;;; check-fields-subtype: Genera un errore se i tipi dei campi RIDEFINITI
;;;; NON sono dei subtype dei campi definiti nelle supclasses
;;;; (o, se il tipo non viene ridefinito, genera errore se il valore
;;;; non è del tipo precedentemente definito dalla supclass)
(defun check-fields-subtype (fields supclasses)
  (cond ((null fields) NIL)
	(T (cons (check-field-subtype (first fields) supclasses)
		 (check-fields-subtype (rest fields) supclasses)))))


;;;; check-field-subtype: genera errore se il tipo del campo passato
;;;; non è un subtype del campo omonimo definito nella prima supclass
;;;; che lo ha
(defun check-field-subtype (field supclasses)
  (cond ((null supclasses) field)
	(T (let ((found (get-value-classes supclasses (first field))))
	     (cond ((null found) field)
		   ((equal (length found) 2) field)
		   (T (if (equal (length field) 2)
			  (if (is-of-type (first (last found))
					  (first (last field)))
			      (list (car field) (second field)
				    (first (last found)))
			      (error "Error: not a value of same type"))
			  (if (not (null (get-class-spec (first (last found)))))
			      (if (class-subtypep
				   (first (last field))
				   (first (last found)))
				  field
				  (error "Error: not a subtype"))
			      (if (subtypep (first (last field))
					    (first (last found)))
				  field
				  (error "Error: not a subtype"))))))))))


;;;; class-subtypep:
(defun class-subtypep (class1 class2)
  (not (null (member class2 (get-supclasses class1)))))


;;;; manipulate-methods: Chiama process-method per ogni metodo
;;;; che trova in methods, e restituisce una nuova definizione dei
;;;; metodi siffata: (method-name #lambda_function#)
(defun manipulate-methods (methods)  
  (if (null methods)
      '()
      (append (list (append (list (first (first methods)))
			    (list (process-method (first (first methods))
						  (rest (first methods))))))
	      (manipulate-methods (rest methods)))))


;;;; get-method: Recupera il metodo data un'istanza
;;;; e il nome del metodo. (Cerca il metodo nella classe dell'istanza o in una
;;;; superclasse di quest'ultima)
(defun get-method (instance method-name)
  (let ((found (get-method-from-part
		(first (last (get-class-spec (first (rest instance)))))
		method-name)))
    (if (not (null found)) found
	(get-method-from-parents (get-supclasses (first (rest instance)))
				 method-name))))


;;;; get-method-from-parents: Recupera il metodo method-name cercandolo
;;;; ricorsivamente nelle classi presenti nella lista parents
(defun get-method-from-parents (parents method-name)
  (cond ((null parents) (error "Method not found for this instance!"))
	(T (let ((found (get-method-from-part
			 (first (last (get-class-spec (first parents))))
			 method-name)))
	     (if (not (null found)) found
		 (get-method-from-parents (rest parents)
					  method-name))))))


;;;; get-method-from-part: Cerca '(methods ...) all'interno di part e lo passa
;;;; a get-method-from-methods.
(defun get-method-from-part (part method-name)
  (cond ((null part) nil)
	((equal part '()) nil)
	((equal (first (first part)) 'methods)
	 (if (not (equal (get-method-from-methods
			  (rest (first part))
			  method-name)
			 nil))
	     (get-method-from-methods (rest (first part)) method-name)
	     (get-method-from-part (rest part) method-name)))
	(T (get-method-from-part (rest part) method-name))))


;;;; get-method-from-methods: Cerca il metodo 'method-name' all'interno di
;;;; della lista methods che contiene i metodi dichiarati durante
;;;; la def-class.
(defun get-method-from-methods (methods method-name)
  (cond ((null methods) nil)
	((equal (first (first methods)) method-name)
	 (first(rest (first methods))))
	(T (get-method-from-methods (rest methods) method-name))))


;;;; get-supclasses: Restituisce una lista contenente TUTTE le superclassi
;;;; della classe passata come argomento
(defun get-supclasses (class)
  (let ((parents (first (rest (get-class-spec class)))))
    (cond ((null parents) nil)
	  (T (get-supclasses-from-list parents)))))


;;;; get-supclasses-from-list: Data una lista di classi restituisce
;;;; la lista contenente le loro superclassi, mantenendo l'ereditarietà
;;;; dato che visita il grafo dei parents in PROFONDITA'
(defun get-supclasses-from-list(parents)
  (cond ((null parents) nil)
	(T (append (list (first parents))
		   (append (get-supclasses-from-list
			    (first (rest (get-class-spec (first parents)))))
			   (get-supclasses-from-list (rest parents)))
		   ))))


;;;; process-method: Assegna a method-name la funzione lambda recuperata
;;;; tramite get-method
(defun process-method (method-name method-spec)
  (setf (fdefinition method-name)
	(lambda (this &rest args)
	  (apply (get-method this method-name) (append (list this) args))))
  (eval (rewrite-method-code method-spec)))


;;;; rewrite-method-code: Restituisce una funzione lambda che ha come
;;;; body (cdr method-spec)
(defun rewrite-method-code (method-spec)
  (cons 'lambda
	(cons (append (list 'this) (car method-spec))
	      (cdr method-spec))))
;;;; end of file --- ool.lisp
