# Sistema de Gestión de Clientes en COBOL

Este es un programa **COBOL (formato libre)** que implementa un sistema completo de **gestión de clientes** con operaciones CRUD (Crear, Leer, Actualizar, Eliminar).

---

## Características Principales

- **Gestión completa de clientes:** Agregar, mostrar, buscar, modificar y eliminar clientes.  
- **Almacenamiento persistente:** Los datos se guardan en archivos de texto.  
- **Sistema de IDs automático:** Generación secuencial automática de identificadores.  
- **Manejo de saldos:** Soporte para saldos positivos y negativos.  
- **Módulo de reportes:** Generación de diferentes tipos de reportes.

---

## Funcionalidades

### Menú Principal
1. **Agregar nuevo cliente** - Registra nuevos clientes con datos completos.  
2. **Mostrar todos los clientes** - Lista completa de clientes registrados.  
3. **Buscar cliente por ID** - Búsqueda específica por identificador.  
4. **Modificar datos de un cliente** - Actualización de información.  
5. **Eliminar cliente** - Remoción de clientes del sistema.  
6. **Generar reportes** - Submenú con diferentes tipos de reportes.  
7. **Salir** - Finaliza la aplicación.

---

## Archivos Utilizados

| Archivo | Descripción |
|----------|--------------|
| `clientes.txt` | Base de datos principal de clientes |
| `secuencia_ids.txt` | Control de secuencia para IDs automáticos |
| `clientes_temp.txt` | Archivo temporal para operaciones de eliminación |

---

## Requisitos y Ejecución

### Prerrequisitos
- Tener instalado **OpenCobolIDE** en el sistema.  
- Puedes descargar **COBOL IDE** desde [Launchpad](https://launchpad.net/cobcide/+download).

---

### Ejecución del Programa

Una vez instalado **OpenCobolIDE**:

1. Abre el programa **OpenCobolIDE**.  
2. Carga el archivo `menu-clientes.cbl` dentro del IDE.  
3. Asegúrate de que el modo de compilación esté configurado en **formato libre** (*Free Format*).  
4. Presiona el botón **Run** ▶️ o utiliza el atajo de teclado **F5** para ejecutar el programa.  

---

## Estructura de Datos

Cada cliente contiene los siguientes campos:

| Campo | Descripción | Longitud |
|--------|--------------|----------|
| **ID** | Identificador único | 5 dígitos |
| **Nombre completo** | Nombre del cliente | 30 caracteres |
| **Correo electrónico** | Email del cliente | 40 caracteres |
| **Teléfono** | Número de contacto | 15 caracteres |
| **Saldo** | Permite valores positivos o negativos | numérico |

---

## Objetivo Educativo

Este programa es ideal para **aprender COBOL moderno con sintaxis libre** y comprender el **manejo de archivos secuenciales** en este lenguaje.

---
