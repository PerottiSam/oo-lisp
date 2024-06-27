# OOΛ
## Perotti Samuele, MAT: 899817

Questo programma Common Lisp è un'estensione per implementare il paradigma
orientato agli oggetti. Consente la creazione di classi, istanze, campi e
metodi, supportando l'ereditarietà.

## Spiegazioni sul Funzionamento

- Quando si va a definire una classe con **def-class**, quest'ultima viene
associata ad una variabile globale che ha come nome il codice hash di
\<class-name> e come valore:\
***(class-name '(parents) '(parts))***\
Dove parts è un insieme di definizione di metodi e campi. I metodi vengono però
sovrascritti a quelli definiti dall'utente in questa maniera
***(method_name \#Lambda function\#)*** dove ***#Lambda Function#*** è
l'***eval*** della espressione Common Lisp definita dall'utente.
Viene inoltre definita una funzione con fdefinition che ha come nome
\<method_name> e come corpo:\
***(apply (get-method this method-name) (append (list this) args))***,\
ovvero un espressione common lisp che serve a recuperare il metodo method_name
all'interno dell classe dell'istanza chiamante (ovvero nella sua classe o
superclassi). Sono dunque le classi ad avere i metodi e non le istanze così da
non dover creare tutti i metodi ogni volta che viene istanziata una classe.

- Quando si va a creare un'istanza con la funzione ***make*** quest'ultima
restituisce l'istanza nella forma\
***(oolinst \<class-name> \<fields-value>)***, dove field-value è una lista che
 contiene ***SOLAMENTE*** le assegnazioni dei valori ai campi effettuate
 durante la make. Se a dei campi non sono stati assegnati dei valori durante la
 creazione dell'istanza, e quindi sono tenuti i valori di default assegnati
 durante la definizione della classe, questi ultimi verranno recuperati
 direttamente dalla classe (o superclassi) durante l'esecuzione dei predicati
 field e field\*.\
Ho scelto di fare cosi per evitare ridondanza nei dati.

## Note Importanti

- T è da considerarsi "undefined" se inserito come type durante la
 definizione di un campo.

## Struttura dei File

Il file Common Lisp `ool.lisp` contiene funzioni per definire classi, creare
istanze, gestire campi ed eseguire metodi. Le funzionalità principali del
 sistema sono:

- **def-class**: Definisce la struttura di una classe con campi e metodi
opzionali.
- **make**: Crea istanze di classi con assegnazioni ai campi opzionali.
- **is-class**: Verifica se l'oggetto passato è una classe
- **is-instance**: Verifica se un oggetto è un'istanza di una classe
- **field e field\***: Accedono al valore di un campo in un'istanza o
attraversano una catena di attributi.

## Primitive implementate

- ### def-class
	Definisce la struttura di una classe e la memorizza in una locazione
	centralizzata (una
variabile globale).
	La sua sintassi è:\
***’(’ def-class \<class-name> \<parents> \<part>\* ’)’***\
dove \<class-name> è un simbolo, \<parents> è una lista (possibilmente vuota)
di simboli, mentre
\<part> è un insieme di field o di definizioni di metodo

- ### make
	 Crea una nuova istanza di una classe. La sintassi è:\
***’(’ make \<class-name> [\<field-name> \<value>]\* ’)’***\
dove \<class-name> e \<field-name> sono simboli, mentre \<value> è un qualunque
	valore Common Lisp.

- ### is-class
	 Restituisce T se l’atomo passatogli è il nome di una classe.
	 La sintassi è:\
***’(’ is-class \<class-name> ’)’***
dove \<class-name> è un simbolo.

- ### is-instance
	Restituisce T se l’oggetto passatogli è l’istanza di una classe. La
	sintassi è:\
***’(’ is-instance \<value> [\<class-name>]’)’***
dove \<class-name> è un simbolo o T (passato per default come parametro &
	optional) e \<value> è un valore qualunque. Se \<class-name> è T allora
		basta che \<value> sia un’istanza qualunque,
altrimenti deve essere un’istanza di una classe che ha \<class-name> come
	superclasse.

- ### field
	Estrae il valore di un campo da una classe. La sintassi è:
***’(’ field \<instance> \<field-name> ’)’***
dove \<instance> è una istanza di una classe e \<field-name> è un simbolo. Il 
	valore ritornato è il valore associato a \<field-name>
	nell’istanza (tale valore potrebbe anche essere ereditato
	dalla classe o da uno dei suoi antenati).
	Se \<field-name> non esiste nella classe dell’istanza
	(ovvero se non è ereditato) allora viene segnalato un errore.

- ### field*
	Estrae il valore da una classe percorrendo una catena di attributi. La
	sintassi è:\
***’(’ field* \<instance> \<field-name>+ ’)’***
dove \<instance> è un’istanza di una classe (nel caso più semplice un simbolo)
	e \<field-name>+ è
una lista non vuota di simboli, che rappresentano attributi nei vari oggetti 
recuperati. Il risultato è il
valore associato all’ultimo elemento di \<field-name>+ nell’ultima
	istanza (tale valore potrebbe anche essere ereditato dalla
	classe o da uno dei suoi antenati).

## Esempi nel PDF della consegna TESTATI

\> (def-class 'person nil '(fields (name "Eve") (age 21 integer)))\
***PERSON***

\> (def-class 'student '(person) '(fields (name "Eva Lu Ator") (university 
"Berkeley" string)) '(methods (talk (&optional (out
*standard-output*)) (format out "My name is \~A~%My age is \~D~%"
(field this 'name) (field this 'age)))))\
***STUDENT***

\> (defparameter eve (make 'person))\
***EVE***

\> (defparameter adam (make 'person 'name "Adam"))\
***ADAM***

\> (defparameter s1 (make 'student 'name "Eduardo De Filippo" 'age 108))\
***S1***

\> (defparameter s2 (make 'student))\
***S2***

\> (defparameter s3 (make 'student 'age "42"))\
***Error "42" is not of type INTEGER***

\> (field eve 'age)\
***21***

\> (field s1 'age)\
***108***

\> (field s2 'name)\
***'Eva Lu Athor'***

\> (field eve 'address)\
***Error: Unknown field***

\> (talk s1)\
***My name is Eduardo De Filippo\
My age is 108\
NIL***

\> (talk eve)\
***Error: Method not found for this instance!***

\> (def-class 'studente-bicocca '(student)
'(methods (talk () (format t "Mi chiamo \~A,\~%e studio alla Bicocca~%"
(field this 'name)))) '(fields (university "UNIMIB")))\
***STUDENTE-BICOCCA***

\> (defparameter ernesto (make 'studente-bicocca 'name "Ernesto"))\
***ERNESTO***

\> (talk ernesto)\
***Mi chiamo Ernesto,\
e studio alla Bicocca\
NIL***

\> (def-class 'other nil 
		'(fields (user s1 student)) 
		'(methods (talk () (format t "Università: ~w" 
(field* this 'user 'university)))))\
***OTHER***

\> (defparameter o (make 'other))\
***o***

\> (talk o)\
***Università: Berkeley***

\> (def-class 'using-integers '() '(fields (x 42 integer)))\
***USING-INTEGERS***

\> (def-class 'using-reals '(using-integers) '(fields (x 42.0 real)))\
***Error: not a subtype***

\> (def-class 'a nil '(fields (numero 8 integer)))\
***a***

\> (def-class 'b '(a) '(fields (numero 9)))\
***b***

\> (def-class 'c '(b) '(fields (numero "Ciao")))\
***Error: Ciao is not of type INTEGER***

## Credits

- Perotti Samuele, MAT: 899817
- UNIMIB, Programming Languages Course
