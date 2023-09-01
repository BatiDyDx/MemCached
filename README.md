# Memcache

Este programa es una implementación de un servidor multithread de cacheado de información.
Esta memcache soporta dos modos de uso, texto y binario. Las indicaciones de uso
de cada de modo se encuentran a continuación.

También se cuenta con una librería implementada en Erlang que provee una interfaz
de uso para tal sistema.

## Compilación de la memcache

Para una facil compilación de la memcache, se provee un archivo Makefile, de modo que se proveen 
distintas funcionalidades.
Para el compilado del programa, se puede hacer uso de la línea:
```
$ make 
```
Alternativamente, la compilación del progrma y la ejecución se pueden reducir en la línea:
```
$ make run
```
Como última opción, compilar el programa sin hacer uso del archivo Makefile se puede lograr compilando
todos los archivos de la carpta ./src, resultando en la línea:
```
$ gcc -o memcached src/bin_processing.c src/cache.c 
         src/common.c src/dalloc.c src/io.c src/ll.c 
         src/log.c src/lru.c 
         src/memcached.c src/sock.c src/stats.c 
         src/text_processing.c src/client_data.c 
         -pthread
```

## Uso de la memcache

Una vez compilado el programa se debe ejecutar la memcache, esto se logra utilizando el siguiente comando:
```
$ ./memcached
```
Existe la posibilidad de cambiar valores internos por defecto de la memcache,como la cantidad de hilos que el programa podrá utilizar (n) ,la cantidad de memoria dedicada (m), la cantidad de celdas de la memcache (c) y la cantidad de regiones (r). Estos valores se pueden modificar en tiempo de ejecución y mediante el uso de banderas:
```
$ ./memcached -nX -mY -cZ -rW
```
Donde X, Y, Z, W son los nuevos valores asignados para cada propiedad de la memcache.

## Remover archivos generados

Para remover los archivos generados por la compilación haciendo uso del archivo Makefile se debe
utilizar el siguiente comando:
```
$ make clean
```
\
\
Programa realizado por Bautista José Peirone y Juan Bautista Figueredo