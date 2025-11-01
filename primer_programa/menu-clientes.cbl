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
01 ESTADO-ARCHIVO       PIC XX.
01 OPCION-SELECCIONADA  PIC 9 VALUE 0.

01 ARCHIVO-CREADO       PIC X VALUE "N".

01 CLIENTE-INGRESADO.
   05 ID-INGRESADO         PIC 9(5).
   05 NOMBRE-INGRESADO     PIC X(30).
   05 CORREO-INGRESADO     PIC X(40).
   05 TELEFONO-INGRESADO   PIC X(15).
   05 SALDO-INGRESADO      PIC S9(5)V99.
   05 SALDO-TEXTO          PIC X(10).

01 SALDO-MOSTRAR        PIC -Z(5).99.
01 CONTADOR-ID          PIC 9(5) VALUE 0.
01 ESTADO-CONTEO PIC XX.
01 ULTIMO-ID-AUX PIC 9(5) VALUE 0.
01 BUSCAR-ID PIC 9(5).
01 ENCONTRADO PIC X VALUE "N".
01 ESTADO-TEMP PIC XX.
01 ACCION-ID PIC X(1) VALUE SPACE.

01 OPCION-SELECCIONADA-REPORTE  PIC 9 VALUE 0.

01 MONTO-FILTRO PIC S9(5)V99.


01 TOTAL-MOSTRAR PIC Z(5).

PROCEDURE DIVISION.

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

TERMINAR-PROGRAMA.
    STOP RUN.

LEER-ULTIMO-ID.
    OPEN INPUT ARCHIVO-CONTEO
    READ ARCHIVO-CONTEO
        AT END MOVE 0 TO ULTIMO-ID-AUX
        NOT AT END MOVE ULTIMO-ID TO ULTIMO-ID-AUX
    END-READ
    CLOSE ARCHIVO-CONTEO
    .

GUARDAR-NUEVO-ID.
    MOVE ID-CLIENTE TO ULTIMO-ID
    OPEN OUTPUT ARCHIVO-CONTEO
    WRITE REGISTRO-CONTEO
    CLOSE ARCHIVO-CONTEO
    .

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
                DISPLAY "Reporte 3"
            WHEN 4
                DISPLAY "Reporte 4"
            WHEN 5
                PERFORM MOSTRAR-TOTAL-CLIENTES
            WHEN 6
                DISPLAY "Reporte 6"
            WHEN 7
                DISPLAY "Volviendo al menu principal..."
            WHEN OTHER
                DISPLAY "Opcion invalida. Intente nuevamente."
        END-EVALUATE
    END-PERFORM

    PERFORM PROGRAMA-PRINCIPAL
    .


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

MOSTRAR-TOTAL-CLIENTES.
    PERFORM CONTAR-CLIENTES

    IF CONTADOR-ID = 0
        DISPLAY "No hay clientes registrados en el sistema."
    ELSE
        MOVE CONTADOR-ID TO TOTAL-MOSTRAR
        DISPLAY "Total de clientes registrados: " TOTAL-MOSTRAR
    END-IF
    .
