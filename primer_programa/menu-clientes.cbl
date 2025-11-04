IDENTIFICATION DIVISION.
PROGRAM-ID. MENU-CLIENTES.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT ARCHIVO-CLIENTES ASSIGN TO "..\clientes.txt"
        FILE STATUS IS ESTADO-ARCHIVO.
    SELECT ARCHIVO-CONTEO ASSIGN TO "..\secuencia_ids.txt"
        FILE STATUS IS ESTADO-CONTEO.
    SELECT ARCHIVO-TEMPORAL ASSIGN TO "..\clientes_temp.txt"
        FILE STATUS IS ESTADO-TEMP.

DATA DIVISION.
FILE SECTION.
FD ARCHIVO-CLIENTES.
01 REGISTRO-CLIENTE.
   05 ID-CLIENTE         PIC 9(5).
   05 NOMBRE             PIC X(30).
   05 CORREO             PIC X(40).
   05 TELEFONO           PIC X(15).
   05 SALDO              PIC S9(5)V99.
   05 SALTO-LINEA        PIC X VALUE X"0A".

FD ARCHIVO-CONTEO.
01 REGISTRO-CONTEO.
   05 ULTIMO-ID PIC 9(5).

FD ARCHIVO-TEMPORAL.
01 REGISTRO-TEMP.
   05 ID-TEMP         PIC 9(5).
   05 NOMBRE-TEMP     PIC X(30).
   05 CORREO-TEMP     PIC X(40).
   05 TELEFONO-TEMP   PIC X(15).
   05 SALDO-TEMP PIC S9(5)V99.

   05 SALTO-TEMP      PIC X VALUE X"0A".


WORKING-STORAGE SECTION.
*> Estados de apertura de archivos (00=éxito, otros=error)
01 ESTADO-ARCHIVO       PIC XX.
01 ESTADO-CONTEO        PIC XX.
01 ESTADO-TEMP          PIC XX.

*> Control de menús y flujo del programa
01 OPCION-SELECCIONADA  PIC 9.
01 ARCHIVO-CREADO       PIC X.
01 OPCION-SELECCIONADA-REPORTE PIC 9.

*> Estructura para captura de datos del cliente
01 CLIENTE-INGRESADO.
   05 ID-INGRESADO         PIC 9(5).
   05 NOMBRE-INGRESADO     PIC X(30).
   05 CORREO-INGRESADO     PIC X(40).
   05 TELEFONO-INGRESADO   PIC X(15).
   05 SALDO-INGRESADO      PIC S9(5)V99.
   05 SALDO-TEXTO          PIC X(10).

*> Variables para operaciones con IDs y búsquedas
01 CONTADOR-ID          PIC 9(5).
01 ULTIMO-ID-AUX        PIC 9(5).
01 BUSCAR-ID            PIC 9(5).
01 ENCONTRADO           PIC X.
01 ACCION-ID            PIC X(1).

*> Variables para filtros y reportes
01 MONTO-FILTRO         PIC S9(5)V99.
01 ORDEN-SELECCIONADO   PIC X.

*> Tabla en memoria para ordenamiento y procesamiento
01 TABLA-CLIENTES.
   05 CLIENTE-TABLA OCCURS 100 TIMES.
      10 ID-TABLA         PIC 9(5).
      10 NOMBRE-TABLA     PIC X(30).
      10 CORREO-TABLA     PIC X(40).
      10 TELEFONO-TABLA   PIC X(15).
      10 SALDO-TABLA      PIC S9(5)V99.

*> Variables auxiliares para intercambios en ordenamiento
01 AUX-ID       PIC 9(5).
01 AUX-NOMBRE   PIC X(30).
01 AUX-CORREO   PIC X(40).
01 AUX-TELEFONO PIC X(15).
01 AUX-SALDO    PIC S9(5)V99.

*> Índices para bucles y procesamiento de tablas
01 INDICE-1     PIC 9(3).
01 INDICE-2     PIC 9(3).

*> Variables para formato de salida y cálculos
01 SALDO-MOSTRAR        PIC -Z(5).99.
01 TOTAL-MOSTRAR        PIC Z(5).
01 SUMA-SALDOS          PIC S9(7)V99.
01 PROMEDIO-SALDO       PIC S9(5)V99.
01 PROMEDIO-MOSTRAR     PIC -Z(5).99.

PROCEDURE DIVISION.

*> Controla el flujo principal y menú del programa
PROGRAMA-PRINCIPAL.
    OPEN INPUT ARCHIVO-CLIENTES
    OPEN INPUT ARCHIVO-CONTEO

    IF ESTADO-ARCHIVO NOT = "00" OR ESTADO-CONTEO NOT = "00"
        CLOSE ARCHIVO-CLIENTES
        CLOSE ARCHIVO-CONTEO
        PERFORM INICIALIZAR-ARCHIVO
    ELSE
        CLOSE ARCHIVO-CLIENTES
        CLOSE ARCHIVO-CONTEO
    END-IF

    PERFORM UNTIL OPCION-SELECCIONADA = 7
        DISPLAY " "
        DISPLAY "======= MENU DE OPCIONES ======="
        DISPLAY "1. Agregar nuevo cliente"
        DISPLAY "2. Mostrar todos los clientes"
        DISPLAY "3. Buscar cliente por ID"
        DISPLAY "4. Modificar datos de un cliente"
        DISPLAY "5. Eliminar cliente"
        DISPLAY "6. Generar reportes"
        DISPLAY "7. Salir"
        DISPLAY "Seleccione una opcion (1-7):"
        ACCEPT OPCION-SELECCIONADA

        EVALUATE OPCION-SELECCIONADA
            WHEN 1
                PERFORM AGREGAR-CLIENTE
            WHEN 2
                PERFORM MOSTRAR-CLIENTES
            WHEN 3
                PERFORM BUSCAR-CLIENTE
            WHEN 4
                PERFORM ACTUALIZAR-CLIENTE
            WHEN 5
                PERFORM ELIMINAR-CLIENTE
            WHEN 6
                PERFORM GENERAR-REPORTES
            WHEN 7
                DISPLAY "Saliendo del programa..."
            WHEN OTHER
                DISPLAY "Opcion invalida. Intente nuevamente."
        END-EVALUATE
    END-PERFORM

    PERFORM TERMINAR-PROGRAMA.

*> Crea archivos iniciales si no existen
INICIALIZAR-ARCHIVO.
    OPEN OUTPUT ARCHIVO-CLIENTES
    CLOSE ARCHIVO-CLIENTES

    MOVE 0 TO ULTIMO-ID
    OPEN OUTPUT ARCHIVO-CONTEO
    WRITE REGISTRO-CONTEO
    CLOSE ARCHIVO-CONTEO

    MOVE "S" TO ARCHIVO-CREADO
    DISPLAY "Archivos inicializados correctamente."
    .

*> Finaliza la ejecución del programa
TERMINAR-PROGRAMA.
    STOP RUN.

*> Obtiene el último ID usado del archivo de secuencia
LEER-ULTIMO-ID.
    OPEN INPUT ARCHIVO-CONTEO
    READ ARCHIVO-CONTEO
        AT END MOVE 0 TO ULTIMO-ID-AUX
        NOT AT END MOVE ULTIMO-ID TO ULTIMO-ID-AUX
    END-READ
    CLOSE ARCHIVO-CONTEO
    .

*> Actualiza el contador de IDs con el nuevo valor
GUARDAR-NUEVO-ID.
    MOVE ID-CLIENTE TO ULTIMO-ID
    OPEN OUTPUT ARCHIVO-CONTEO
    WRITE REGISTRO-CONTEO
    CLOSE ARCHIVO-CONTEO
    .

*> Añade un nuevo cliente al archivo
AGREGAR-CLIENTE.
    PERFORM LEER-ULTIMO-ID

    ADD 1 TO ULTIMO-ID-AUX
    MOVE ULTIMO-ID-AUX TO ID-CLIENTE

    MOVE SPACES TO CLIENTE-INGRESADO
    MOVE ZEROS TO SALDO-INGRESADO

    PERFORM INGRESAR-DATOS-CLIENTE

    OPEN EXTEND ARCHIVO-CLIENTES

    MOVE NOMBRE-INGRESADO TO NOMBRE
    MOVE CORREO-INGRESADO TO CORREO
    MOVE TELEFONO-INGRESADO TO TELEFONO
    MOVE SALDO-INGRESADO TO SALDO

    WRITE REGISTRO-CLIENTE

    PERFORM GUARDAR-NUEVO-ID

    CLOSE ARCHIVO-CLIENTES
    DISPLAY "Cliente agregado correctamente."
    .

*> Solicita y valida los datos del cliente al usuario
INGRESAR-DATOS-CLIENTE.
    PERFORM UNTIL NOMBRE-INGRESADO NOT = SPACES
        DISPLAY "Ingrese nombre completo:"
        ACCEPT NOMBRE-INGRESADO
        IF NOMBRE-INGRESADO = SPACES
            DISPLAY "El nombre no puede estar vacio."
        END-IF
    END-PERFORM

    PERFORM UNTIL CORREO-INGRESADO NOT = SPACES
        DISPLAY "Ingrese correo electronico:"
        ACCEPT CORREO-INGRESADO
        IF CORREO-INGRESADO = SPACES
            DISPLAY "El correo no puede estar vacio."
        END-IF
    END-PERFORM

    PERFORM UNTIL TELEFONO-INGRESADO NOT = SPACES
        DISPLAY "Ingrese numero de telefono:"
        ACCEPT TELEFONO-INGRESADO
        IF TELEFONO-INGRESADO = SPACES
            DISPLAY "El telefono no puede estar vacio."
        END-IF
    END-PERFORM

    PERFORM UNTIL SALDO-TEXTO NOT = SPACES
        DISPLAY "Ingrese saldo inicial (puede ser negativo o cero):"
        ACCEPT SALDO-TEXTO

        IF SALDO-TEXTO = SPACES
            DISPLAY "El saldo no puede estar vacio."
        END-IF
    END-PERFORM

    MOVE SALDO-TEXTO TO SALDO-INGRESADO
    .

*> Cuenta el total de clientes en el archivo
CONTAR-CLIENTES.
    MOVE 0 TO CONTADOR-ID
    OPEN INPUT ARCHIVO-CLIENTES
    PERFORM UNTIL ESTADO-ARCHIVO = "10"
        READ ARCHIVO-CLIENTES
            AT END MOVE "10" TO ESTADO-ARCHIVO
            NOT AT END ADD 1 TO CONTADOR-ID
        END-READ
    END-PERFORM
    CLOSE ARCHIVO-CLIENTES
    .

*> Lista todos los clientes registrados
MOSTRAR-CLIENTES.
    PERFORM CONTAR-CLIENTES
    IF CONTADOR-ID = 0
        DISPLAY "No hay clientes registrados en el sistema."
    ELSE
        OPEN INPUT ARCHIVO-CLIENTES
        MOVE "00" TO ESTADO-ARCHIVO

        PERFORM UNTIL ESTADO-ARCHIVO = "10"
            READ ARCHIVO-CLIENTES
                AT END MOVE "10" TO ESTADO-ARCHIVO
                NOT AT END
                    MOVE SALDO TO SALDO-MOSTRAR
                    DISPLAY "ID: " ID-CLIENTE
                    DISPLAY "Nombre: " NOMBRE
                    DISPLAY "Correo: " CORREO
                    DISPLAY "Telefono: " TELEFONO
                    DISPLAY "Saldo: " SALDO-MOSTRAR
                    DISPLAY "-----------------------------"
            END-READ
        END-PERFORM

        CLOSE ARCHIVO-CLIENTES
    END-IF
    .

*> Solicita ID para operaciones de búsqueda/actualización/eliminación
PEDIR-ID.
    MOVE "N" TO ENCONTRADO
    MOVE 0 TO BUSCAR-ID
    PERFORM UNTIL BUSCAR-ID NOT = 0
        EVALUATE ACCION-ID
            WHEN "B"
                 DISPLAY "Ingrese el ID que va a Buscar:"
            WHEN "A"
                 DISPLAY "Ingrese el ID que va a Actualizar:"
            WHEN "E"
                DISPLAY "Ingrese el ID que va a Eliminar:"
            WHEN OTHER
                DISPLAY "Ingrese el ID:"
        END-EVALUATE
        ACCEPT BUSCAR-ID
        IF BUSCAR-ID = 0
            DISPLAY "El ID ingresado no es valido."
        END-IF
    END-PERFORM
    .

*> Busca y muestra un cliente por su ID
BUSCAR-CLIENTE.
    PERFORM CONTAR-CLIENTES
    IF CONTADOR-ID = 0
        DISPLAY "No hay clientes registrados en el sistema."
    ELSE
        MOVE "B" TO ACCION-ID
        PERFORM PEDIR-ID

        OPEN INPUT ARCHIVO-CLIENTES
        MOVE "00" TO ESTADO-ARCHIVO

        PERFORM UNTIL ESTADO-ARCHIVO = "10"
            READ ARCHIVO-CLIENTES
                AT END MOVE "10" TO ESTADO-ARCHIVO
                NOT AT END
                    IF ID-CLIENTE = BUSCAR-ID
                        MOVE "S" TO ENCONTRADO
                        MOVE SALDO TO SALDO-MOSTRAR
                        DISPLAY "Cliente encontrado:"
                        DISPLAY "ID: " ID-CLIENTE
                        DISPLAY "Nombre: " NOMBRE
                        DISPLAY "Correo: " CORREO
                        DISPLAY "Telefono: " TELEFONO
                        DISPLAY "Saldo: " SALDO-MOSTRAR
                        MOVE "10" TO ESTADO-ARCHIVO
                    END-IF
            END-READ
        END-PERFORM

        CLOSE ARCHIVO-CLIENTES

        IF ENCONTRADO = "N"
            DISPLAY "No se encontro ningun cliente con ese ID."
        END-IF
    END-IF
    .

*> Modifica los datos de un cliente existente
ACTUALIZAR-CLIENTE.
    PERFORM CONTAR-CLIENTES
    IF CONTADOR-ID = 0
        DISPLAY "No hay clientes registrados en el sistema."
    ELSE
        MOVE "A" TO ACCION-ID
        PERFORM PEDIR-ID
        MOVE "N" TO ENCONTRADO

        OPEN INPUT ARCHIVO-CLIENTES
        OPEN OUTPUT ARCHIVO-TEMPORAL
        MOVE "00" TO ESTADO-ARCHIVO

        PERFORM UNTIL ESTADO-ARCHIVO = "10"
            READ ARCHIVO-CLIENTES
                AT END MOVE "10" TO ESTADO-ARCHIVO
                NOT AT END
                    IF ID-CLIENTE = BUSCAR-ID
                        MOVE "S" TO ENCONTRADO
                        DISPLAY "Cliente encontrado. Ingrese los nuevos datos:"
                        MOVE SPACES TO CLIENTE-INGRESADO
                        MOVE ZEROS TO SALDO-INGRESADO
                        PERFORM INGRESAR-DATOS-CLIENTE

                        MOVE ID-CLIENTE TO ID-TEMP
                        MOVE NOMBRE-INGRESADO TO NOMBRE-TEMP
                        MOVE CORREO-INGRESADO TO CORREO-TEMP
                        MOVE TELEFONO-INGRESADO TO TELEFONO-TEMP
                        MOVE SALDO-INGRESADO TO SALDO-TEMP
                        WRITE REGISTRO-TEMP
                    ELSE
                        MOVE ID-CLIENTE TO ID-TEMP
                        MOVE NOMBRE TO NOMBRE-TEMP
                        MOVE CORREO TO CORREO-TEMP
                        MOVE TELEFONO TO TELEFONO-TEMP
                        MOVE SALDO TO SALDO-TEMP
                        WRITE REGISTRO-TEMP
                    END-IF
            END-READ
        END-PERFORM

        CLOSE ARCHIVO-CLIENTES
        CLOSE ARCHIVO-TEMPORAL

        IF ENCONTRADO = "S"
            CALL "CBL_DELETE_FILE" USING "..\clientes.txt"
            CALL "CBL_RENAME_FILE" USING "..\clientes_temp.txt", "..\clientes.txt"
            DISPLAY "Datos del cliente actualizados correctamente."
        ELSE
            CALL "CBL_DELETE_FILE" USING "..\clientes_temp.txt"
            DISPLAY "No se encontró ningún cliente con ese ID."
        END-IF
    END-IF
    .

*> Elimina un cliente del sistema
ELIMINAR-CLIENTE.
    PERFORM CONTAR-CLIENTES
    IF CONTADOR-ID = 0
        DISPLAY "No hay clientes registrados en el sistema."
    ELSE
        MOVE "E" TO ACCION-ID
        PERFORM PEDIR-ID

        OPEN INPUT ARCHIVO-CLIENTES
        OPEN OUTPUT ARCHIVO-TEMPORAL
        MOVE "00" TO ESTADO-ARCHIVO

        PERFORM UNTIL ESTADO-ARCHIVO = "10"
            READ ARCHIVO-CLIENTES
                AT END MOVE "10" TO ESTADO-ARCHIVO
                NOT AT END
                    IF ID-CLIENTE = BUSCAR-ID
                        MOVE "S" TO ENCONTRADO
                    ELSE
                        MOVE ID-CLIENTE TO ID-TEMP
                        MOVE NOMBRE TO NOMBRE-TEMP
                        MOVE CORREO TO CORREO-TEMP
                        MOVE TELEFONO TO TELEFONO-TEMP
                        MOVE SALDO TO SALDO-TEMP
                        WRITE REGISTRO-TEMP
                    END-IF
            END-READ
        END-PERFORM

        CLOSE ARCHIVO-CLIENTES
        CLOSE ARCHIVO-TEMPORAL

        IF ENCONTRADO = "S"
            DISPLAY "Cliente eliminado correctamente."
            CALL "CBL_DELETE_FILE" USING "..\clientes.txt"
            CALL "CBL_RENAME_FILE" USING "..\clientes_temp.txt", "..\clientes.txt"
        ELSE
            DISPLAY "No se encontro ningun cliente con ese ID."
            CALL "CBL_DELETE_FILE" USING "..\clientes_temp.txt"
        END-IF
    END-IF
    .

*> Menú secundario para generar reportes
GENERAR-REPORTES.
     MOVE ZERO TO OPCION-SELECCIONADA-REPORTE

     PERFORM UNTIL OPCION-SELECCIONADA-REPORTE = 7
        DISPLAY " "
        DISPLAY "======= MENU DE REPORTES ======="
        DISPLAY "1. Listado general de clientes"
        DISPLAY "2. Clientes con saldo mayor a un monto "
        DISPLAY "3. Clientes con saldo negativo o en cero"
        DISPLAY "4. Clientes ordenados por saldo"
        DISPLAY "5. Total de clientes registrados "
        DISPLAY "6. Promedio de saldo general "
        DISPLAY "7. Volver al menu principal"
        DISPLAY "Seleccione una opcion (1-7):"
        ACCEPT OPCION-SELECCIONADA-REPORTE

        EVALUATE OPCION-SELECCIONADA-REPORTE
            WHEN 1
                PERFORM MOSTRAR-CLIENTES
            WHEN 2
                PERFORM CLIENTES-CON-SALDO-MAYOR
            WHEN 3
                PERFORM CLIENTES-SALDO-NEGATIVO-O-CERO
            WHEN 4
                PERFORM CLIENTES-ORDENADOS-POR-SALDO
            WHEN 5
                PERFORM MOSTRAR-TOTAL-CLIENTES
            WHEN 6
                PERFORM CALCULAR-PROMEDIO-SALDO
            WHEN 7
                DISPLAY "Volviendo al menu principal..."
            WHEN OTHER
                DISPLAY "Opcion invalida. Intente nuevamente."
        END-EVALUATE
    END-PERFORM

    PERFORM PROGRAMA-PRINCIPAL
    .

*> Filtra clientes con saldo mayor a un monto específico
CLIENTES-CON-SALDO-MAYOR.
    PERFORM CONTAR-CLIENTES
    IF CONTADOR-ID = 0
        DISPLAY "No hay clientes registrados en el sistema."
    ELSE
        DISPLAY "Ingrese el monto minimo de saldo para filtrar:"
        ACCEPT MONTO-FILTRO

        OPEN INPUT ARCHIVO-CLIENTES
        MOVE "00" TO ESTADO-ARCHIVO
        MOVE 0 TO CONTADOR-ID

        PERFORM UNTIL ESTADO-ARCHIVO = "10"
            READ ARCHIVO-CLIENTES
                AT END MOVE "10" TO ESTADO-ARCHIVO
                NOT AT END
                    IF SALDO > MONTO-FILTRO
                        ADD 1 TO CONTADOR-ID
                        MOVE SALDO TO SALDO-MOSTRAR
                        DISPLAY "ID: " ID-CLIENTE
                        DISPLAY "Nombre: " NOMBRE
                        DISPLAY "Correo: " CORREO
                        DISPLAY "Telefono: " TELEFONO
                        DISPLAY "Saldo: " SALDO-MOSTRAR
                        DISPLAY "-----------------------------"
                    END-IF
            END-READ
        END-PERFORM

        CLOSE ARCHIVO-CLIENTES

        IF CONTADOR-ID = 0
            DISPLAY "No se encontraron clientes con saldo mayor a ese monto."
        END-IF
    END-IF
    .

*> Muestra clientes con saldo negativo o cero
CLIENTES-SALDO-NEGATIVO-O-CERO.
    PERFORM CONTAR-CLIENTES
    IF CONTADOR-ID = 0
        DISPLAY "No hay clientes registrados en el sistema."
    ELSE
        OPEN INPUT ARCHIVO-CLIENTES
        MOVE "00" TO ESTADO-ARCHIVO
        MOVE 0 TO CONTADOR-ID

        PERFORM UNTIL ESTADO-ARCHIVO = "10"
            READ ARCHIVO-CLIENTES
                AT END MOVE "10" TO ESTADO-ARCHIVO
                NOT AT END
                    IF SALDO <= 0
                        ADD 1 TO CONTADOR-ID
                        MOVE SALDO TO SALDO-MOSTRAR
                        DISPLAY "ID: " ID-CLIENTE
                        DISPLAY "Nombre: " NOMBRE
                        DISPLAY "Correo: " CORREO
                        DISPLAY "Telefono: " TELEFONO
                        DISPLAY "Saldo: " SALDO-MOSTRAR
                        DISPLAY "-----------------------------"
                    END-IF
            END-READ
        END-PERFORM

        CLOSE ARCHIVO-CLIENTES

        IF CONTADOR-ID = 0
            DISPLAY "No se encontraron clientes con saldo negativo o en cero."
        END-IF
    END-IF
    .

*> Ordena y muestra clientes por saldo (ascendente/descendente)
CLIENTES-ORDENADOS-POR-SALDO.
    MOVE 0 TO CONTADOR-ID
    OPEN INPUT ARCHIVO-CLIENTES
    MOVE "00" TO ESTADO-ARCHIVO

    PERFORM UNTIL ESTADO-ARCHIVO = "10"
        READ ARCHIVO-CLIENTES
            AT END MOVE "10" TO ESTADO-ARCHIVO
            NOT AT END
                ADD 1 TO CONTADOR-ID
                MOVE ID-CLIENTE TO ID-TABLA(CONTADOR-ID)
                MOVE NOMBRE TO NOMBRE-TABLA(CONTADOR-ID)
                MOVE CORREO TO CORREO-TABLA(CONTADOR-ID)
                MOVE TELEFONO TO TELEFONO-TABLA(CONTADOR-ID)
                MOVE SALDO TO SALDO-TABLA(CONTADOR-ID)
        END-READ
    END-PERFORM

    CLOSE ARCHIVO-CLIENTES

    IF CONTADOR-ID = 0
        DISPLAY "No hay clientes registrados en el sistema."
    ELSE
        DISPLAY "¿Como desea ordenar los saldos?"
        DISPLAY "A. De menor a mayor"
        DISPLAY "B. De mayor a menor"
        DISPLAY "Seleccione una opción (A/B):"
        ACCEPT ORDEN-SELECCIONADO

        MOVE FUNCTION UPPER-CASE(ORDEN-SELECCIONADO) TO ORDEN-SELECCIONADO

        MOVE 1 TO INDICE-1
        PERFORM UNTIL INDICE-1 > CONTADOR-ID
            MOVE INDICE-1 TO INDICE-2
            ADD 1 TO INDICE-2
            PERFORM UNTIL INDICE-2 > CONTADOR-ID
                EVALUATE ORDEN-SELECCIONADO
                    WHEN "A"
                        IF SALDO-TABLA(INDICE-2) < SALDO-TABLA(INDICE-1)
                            PERFORM INTERCAMBIAR-CLIENTES
                        END-IF
                    WHEN "B"
                        IF SALDO-TABLA(INDICE-2) > SALDO-TABLA(INDICE-1)
                            PERFORM INTERCAMBIAR-CLIENTES
                        END-IF
                    WHEN OTHER
                        DISPLAY "Opcion invalida. Se usara orden ascendente por defecto."
                        IF SALDO-TABLA(INDICE-2) < SALDO-TABLA(INDICE-1)
                            PERFORM INTERCAMBIAR-CLIENTES
                        END-IF
                END-EVALUATE
                ADD 1 TO INDICE-2
            END-PERFORM
            ADD 1 TO INDICE-1
        END-PERFORM

        DISPLAY "Clientes ordenados por saldo:"
        MOVE 1 TO INDICE-1
        PERFORM UNTIL INDICE-1 > CONTADOR-ID
            MOVE SALDO-TABLA(INDICE-1) TO SALDO-MOSTRAR
            DISPLAY "ID: " ID-TABLA(INDICE-1)
            DISPLAY "Nombre: " NOMBRE-TABLA(INDICE-1)
            DISPLAY "Correo: " CORREO-TABLA(INDICE-1)
            DISPLAY "Telefono: " TELEFONO-TABLA(INDICE-1)
            DISPLAY "Saldo: " SALDO-MOSTRAR
            DISPLAY "-----------------------------"
            ADD 1 TO INDICE-1
        END-PERFORM
    END-IF
    .

*> Intercambia posiciones en la tabla para ordenamiento
INTERCAMBIAR-CLIENTES.
    MOVE ID-TABLA(INDICE-1) TO AUX-ID
    MOVE NOMBRE-TABLA(INDICE-1) TO AUX-NOMBRE
    MOVE CORREO-TABLA(INDICE-1) TO AUX-CORREO
    MOVE TELEFONO-TABLA(INDICE-1) TO AUX-TELEFONO
    MOVE SALDO-TABLA(INDICE-1) TO AUX-SALDO

    MOVE ID-TABLA(INDICE-2) TO ID-TABLA(INDICE-1)
    MOVE NOMBRE-TABLA(INDICE-2) TO NOMBRE-TABLA(INDICE-1)
    MOVE CORREO-TABLA(INDICE-2) TO CORREO-TABLA(INDICE-1)
    MOVE TELEFONO-TABLA(INDICE-2) TO TELEFONO-TABLA(INDICE-1)
    MOVE SALDO-TABLA(INDICE-2) TO SALDO-TABLA(INDICE-1)

    MOVE AUX-ID TO ID-TABLA(INDICE-2)
    MOVE AUX-NOMBRE TO NOMBRE-TABLA(INDICE-2)
    MOVE AUX-CORREO TO CORREO-TABLA(INDICE-2)
    MOVE AUX-TELEFONO TO TELEFONO-TABLA(INDICE-2)
    MOVE AUX-SALDO TO SALDO-TABLA(INDICE-2)
    .

*> Muestra el conteo total de clientes registrados
MOSTRAR-TOTAL-CLIENTES.
    PERFORM CONTAR-CLIENTES

    IF CONTADOR-ID = 0
        DISPLAY "No hay clientes registrados en el sistema."
    ELSE
        MOVE CONTADOR-ID TO TOTAL-MOSTRAR
        DISPLAY "Total de clientes registrados: " TOTAL-MOSTRAR
    END-IF
    .

*> Calcula y muestra el promedio general de saldos
CALCULAR-PROMEDIO-SALDO.
    MOVE 0 TO CONTADOR-ID
    MOVE 0 TO SUMA-SALDOS
    OPEN INPUT ARCHIVO-CLIENTES
    MOVE "00" TO ESTADO-ARCHIVO

    PERFORM UNTIL ESTADO-ARCHIVO = "10"
        READ ARCHIVO-CLIENTES
            AT END MOVE "10" TO ESTADO-ARCHIVO
            NOT AT END
                ADD 1 TO CONTADOR-ID
                ADD SALDO TO SUMA-SALDOS
        END-READ
    END-PERFORM

    CLOSE ARCHIVO-CLIENTES

    IF CONTADOR-ID = 0
        DISPLAY "No hay clientes registrados en el sistema."
    ELSE
        COMPUTE PROMEDIO-SALDO = SUMA-SALDOS / CONTADOR-ID
        MOVE PROMEDIO-SALDO TO PROMEDIO-MOSTRAR
        DISPLAY "Promedio general de saldo: " PROMEDIO-MOSTRAR
    END-IF
    .
