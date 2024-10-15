| package |
package := Package name: 'Estudio cinematográfico'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #Actor;
	add: #Cinema;
	add: #EquipoDireccion;
	add: #Pelicula;
	add: #Permanente;
	add: #Staff;
	add: #Temporal;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: #(
	'..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin'
	'..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin Legacy Date & Time'
	'..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin Message Box'
	'..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Prompter').

package!

"Class Definitions"!

Object subclass: #Cinema
	instanceVariableNames: 'empleados peliculas'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Pelicula
	instanceVariableNames: 'empleados codigo fechaCreacion titulo staff presupuestoAsignado presupuestoRemanente'
	classVariableNames: 'UltimoCodigoPelicula'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Staff
	instanceVariableNames: 'nombre apellido dni'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Staff subclass: #Permanente
	instanceVariableNames: 'plus'
	classVariableNames: 'SueldoBasico'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Staff subclass: #Temporal
	instanceVariableNames: 'nacionalidad'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Temporal subclass: #Actor
	instanceVariableNames: 'cachet'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Temporal subclass: #EquipoDireccion
	instanceVariableNames: 'porcentaje'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

Cinema guid: (GUID fromString: '{ff10262f-687f-45ed-acb1-3a256550f46f}')!
Cinema comment: ''!
!Cinema categoriesForClass!Unclassified! !
!Cinema methodsFor!

altaEmpleado
|x y nuevoEmp|
x := 'S'.
[x == 'S'] whileTrue: [
	y := ((Prompter prompt: 'Qué tipo de empleado quiere dar de alta? 1) Permanente. 2) Actor. 3) Equipo de Dirección.') asNumber).
	(y = 1) ifTrue: [nuevoEmp := Permanente new].
	(y = 2) ifTrue: [nuevoEmp := Actor new].
	(y = 3) ifTrue: [nuevoEmp := EquipoDireccion new].
	nuevoEmp cargaDatosEmpleado.
	(y = 1) ifTrue: [nuevoEmp cargaPlus].
	(y = 2) ifTrue: [
			nuevoEmp cargaNacionalidad.
			nuevoEmp cargaCachet.
	].
	(y = 3) ifTrue: [
			nuevoEmp cargaNacionalidad.
			nuevoEmp cargaPorcentaje.
	].
	empleados add: nuevoEmp.
	x := ((Prompter prompt: 'Quiere seguir creando empleados? [S/N]: ') asUppercase).
].

!

altaPelicula
|x nuevaP|
x := 'S'.
[x == 'S'] whileTrue: [
	nuevaP := Pelicula new.
	nuevaP cargaPelicula.
	peliculas add: nuevaP.
	x := ((Prompter prompt: 'Quiere seguir creando peliculas? [S/N]: ') asUppercase).
].
!

inicializar
empleados := SortedCollection new.
peliculas := SortedCollection new.!

menu
| opc |
self inicializar.
opc := ((Prompter prompt: 'Ingrese 1)Alta empleado. 2)Alta pelicula. 0)Salir') asNumber).

[opc ~= 0] whileTrue: [
    (opc = 1) ifTrue: [self altaEmpleado].
    (opc = 2) ifTrue: [self altaPelicula].
    (opc ~= 1 and: [opc ~= 2 and: [opc ~= 0]]) ifTrue: [
        MessageBox notify: 'Opción inválida, por favor intente de nuevo.'
    ].
    opc := ((Prompter prompt: 'Ingrese 1)Alta empleado. 2)Alta pelicula. 0)Salir') asNumber).
].
!

resumenMensual
| fechaActual peliculasRecientes peliculasOrdenadas|
fechaActual := Date today. 
peliculasRecientes := Pelicula allInstances select: [ :pel | (fechaActual - pel fechaCreacion) <= 30 ].

peliculasOrdenadas := peliculasRecientes asSortedCollection: [:a :b | a fechaCreacion < b fechaCreacion].

peliculasOrdenadas do: [:pelicula |
	Prompter inform: 'Título: ', pelicula titulo, 
		' | Fecha: ', pelicula fechaCreacion printString, 
		' | Presupuesto: ', pelicula presupuestoAsignado printString, 
		' | Presupuesto Remanente: ', pelicula presupuestoRemanente printString.
	
	Prompter inform: 'Empleados involucrados: '.
	pelicula staff do: [:emp |
		Prompter inform: 'Tipo: ', emp class name, 
			' | Nombre: ', emp nombre, 
			' | Apellido: ', emp apellido, 
			' | Total Cobrado: ', emp costo.
	].
].

! !
!Cinema categoriesForMethods!
altaEmpleado!public! !
altaPelicula!public! !
inicializar!public! !
menu!public! !
resumenMensual!public! !
!

Pelicula guid: (GUID fromString: '{f8b7509b-01a8-4c70-a8e5-76927c9bba12}')!
Pelicula comment: ''!
!Pelicula categoriesForClass!Kernel-Objects! !
!Pelicula methodsFor!

cargaPelicula
| x nom ape switch emp costo bus |
codigo := UltimoCodigoPelicula.
Pelicula incrementarCodigo.
fechaCreacion := Date today.
titulo := (Prompter prompt: 'Ingrese el título de la pelicula: ').
staff := OrderedCollection new.
presupuestoAsignado := ((Prompter prompt: 'Ingrese el presupuesto asignado: ') asNumber).
presupuestoRemanente := presupuestoAsignado.
switch := true.

[switch] whileTrue: [
	nom := Prompter prompt: 'Ingrese el nombre del empleado a buscar: '.
	ape := Prompter prompt: 'Ingrese el apellido del empleado a buscar: '.
	emp := empleados detect: [ :bus | (bus nombre = nom) and: [bus apellido = ape]] ifNone: [nil].

	emp notNil ifTrue: [
		(emp isKindOf: Actor) ifTrue: [
		    costo := emp cachet.
		].
		(emp isKindOf: EquipoDireccion) ifTrue: [
		    costo := emp porcentaje * presupuestoAsignado.
		].
		(emp isKindOf: Permanente) ifTrue: [
		    costo := emp plus + emp SueldoBasico.
		].

        presupuestoRemanente - costo >= 0 ifTrue: [
            staff add: emp.
            presupuestoRemanente := presupuestoRemanente - costo.
            Prompter inform: 'Empleado agregado: ', emp nombre, ' ', emp apellido, ' | Costo: ', costo printString.
        ] ifFalse: [
            Prompter inform: 'El presupuesto no es suficiente para contratar a este empleado'.
        ].
    ] ifFalse: [
        Prompter inform: 'Empleado no encontrado.'.
    ].

    x := Prompter prompt: '¿Quiere seguir agregando empleados? [S/N]: ' asUppercase.
    x = 'N' ifTrue: [switch := false].
].
! !
!Pelicula categoriesForMethods!
cargaPelicula!public! !
!

!Pelicula class methodsFor!

incrementarCodigo
UltimoCodigoPelicula := UltimoCodigoPelicula + 1.!

inicializar
UltimoCodigoPelicula := 0.! !
!Pelicula class categoriesForMethods!
incrementarCodigo!public! !
inicializar!public! !
!

Staff guid: (GUID fromString: '{971b98c0-3129-4105-895a-f37906335f88}')!
Staff comment: ''!
!Staff categoriesForClass!Kernel-Objects! !
!Staff methodsFor!

apellido
^apellido!

cargaDatosEmpleado
nombre := (Prompter prompt: 'Ingrese un nombre:').
apellido := (Prompter prompt: 'Ingrese un apellido:').
dni := (Prompter prompt: 'Ingrese un DNI:').!

nombre
^nombre! !
!Staff categoriesForMethods!
apellido!public! !
cargaDatosEmpleado!public! !
nombre!public! !
!

Permanente guid: (GUID fromString: '{e2d8d135-4785-4316-8c16-bd42d2d77e9b}')!
Permanente comment: ''!
!Permanente categoriesForClass!Kernel-Objects! !
!Permanente methodsFor!

cargaPlus
| x |
"Ingresado el porcentaje, probar que la suma de todos los porcentajes no sea mayor a 100% del presupuesto remanente."
x := ((Prompter prompt: 'Ingrese el porcentaje sobre el presupuesto asignado[0-100]: ') asNumber).
[x > 100 or: [x < 0]] whileTrue: [
    x := ((Prompter prompt: 'Porcentaje inválido. Ingrese un valor entre 0 y 100: ') asNumber).
].
plus := x / 100.! !
!Permanente categoriesForMethods!
cargaPlus!public! !
!

!Permanente class methodsFor!

modificarSueldo
SueldoBasico := Prompter prompt: 'Ingrese el monto del sueldo básico: '! !
!Permanente class categoriesForMethods!
modificarSueldo!public! !
!

Temporal guid: (GUID fromString: '{f1e4ac00-d34e-4d30-a86d-de52123c087d}')!
Temporal comment: ''!
!Temporal categoriesForClass!Kernel-Objects! !
!Temporal methodsFor!

cargaNacionalidad
nacionalidad := Prompter prompt: 'Ingrese la nacionalidad del empleado: '.! !
!Temporal categoriesForMethods!
cargaNacionalidad!public! !
!

Actor guid: (GUID fromString: '{9f238f8c-af05-4f49-b5a8-d568f8d7d3c9}')!
Actor comment: ''!
!Actor categoriesForClass!Kernel-Objects! !
!Actor methodsFor!

cargaCachet
cachet := ((Prompter prompt: 'Ingrese el monto del cachet: ') asNumber).! !
!Actor categoriesForMethods!
cargaCachet!public! !
!

EquipoDireccion guid: (GUID fromString: '{31103256-5119-4f47-84d7-a1b136064783}')!
EquipoDireccion comment: ''!
!EquipoDireccion categoriesForClass!Kernel-Objects! !
!EquipoDireccion methodsFor!

cargaPorcentaje
|x| "Ingresado el porcentaje, probar que la suma de todos los porcentajes no sea mayor a 100% del presupuesto remanente."
x := ((Prompter prompt: 'Ingrese el porcentaje sobre el presupuesto asignado[0-100]: ') asNumber).
[x > 100 or: [x <= 0]] whileTrue: [x := ((Prompter prompt: 'Porcentaje inválido. Ingrese un valor entre 0 y 100: ') asNumber).].
porcentaje := x.! !
!EquipoDireccion categoriesForMethods!
cargaPorcentaje!public! !
!

"Binary Globals"!

