# Función para leer el archivo de números
leer_numeros <- function(nombre_archivo) {
  # Verificar si el archivo existe
  if (!file.exists(nombre_archivo)) {
    stop("El archivo no existe")
  }
  
  # Leer el archivo y convertir los datos en un vector de enteros
  numeros <- as.integer(readLines(nombre_archivo))
  
  return(numeros)
}

# Función para calcular estadísticas y cuadrar los números
procesar_numeros <- function(nombre_archivo) {
  # Leer los números desde el archivo
  numeros <- leer_numeros(nombre_archivo)
  
  # Calcular la media, mediana y desviación estándar
  media <- mean(numeros)
  mediana <- median(numeros)
  desviacion_estandar <- sd(numeros)
  
  # Mostrar mensaje si la desviación estándar es mayor a 10
  if (desviacion_estandar > 10) {
    print("Alta variabilidad en los datos")
  }
  
  # Calcular el cuadrado de cada número usando sapply()
  cuadrados <- sapply(numeros, function(x) x^2)
  
  # Crear el archivo de salida 'resultados.txt'
  archivo_salida <- file("resultados.txt", "w")
  
  # Escribir los resultados en el archivo
  cat("Estadísticas de los números:\n", file = archivo_salida)
  cat("Media: ", media, "\n", file = archivo_salida, append = TRUE)
  cat("Mediana: ", mediana, "\n", file = archivo_salida, append = TRUE)
  cat("Desviación Estándar: ", desviacion_estandar, "\n", file = archivo_salida, append = TRUE)
  
  # Escribir mensaje si hay alta variabilidad
  if (desviacion_estandar > 10) {
    cat("Mensaje: Alta variabilidad en los datos.\n", file = archivo_salida, append = TRUE)
  }
  
  # Escribir los cuadrados de los números
  cat("\nCuadrados de los números:\n", file = archivo_salida, append = TRUE)
  cat(paste(cuadrados, collapse = ", "), "\n", file = archivo_salida, append = TRUE)
  
  # Cerrar el archivo
  close(archivo_salida)
}

# Ejecutar el script procesando los números desde 'numeros.txt'
procesar_numeros("numeros.txt")
