# TP1-Lenguajes-IIsem2019

Problema a resolver

Se requiere programar una aplicación en Haskell que haga lo siguiente:
leer un archivo de reseñas cuyo nombre se especifica por  medio de un comando
convertir todas las letras a minúsculas
cambiar todos los caracteres que no sean letras a espacios en blanco
contar los valores ni, np, nip y N
ni: número de reseñas en que aparece la palabra ki
np: número de reseñas que tienen clase p
nip: número de reseñas de clase p en las que aparece la palabra ki
N: número total de reseñas
descartar las palabras con ni < 3; esto reduce enormemente la cantidad de palabras
calcular el valor GI para cada palabra distinta (ki) que aparece en las reseñas
mostrar las m palabras con los mejores valores de GI; en orden descendente; el valor m es un parámetro escogido por el usuario
guardar en un archivo  las palabras junto con sus valores de GI

Para calcular GI, primero se debe calcular la entropía H.
