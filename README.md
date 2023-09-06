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
Alternativamente, la compilación del programa y la ejecución se pueden reducir
en la línea:
```
$ make run
```
Como última opción, compilar el programa sin hacer uso del archivo Makefile se
puede lograr compilando todos los archivos de la carpta ./src, resultando en la
línea:
```
$ gcc -o memcached src/*.c -pthread
```

## Uso de la memcache

Sin embargo, el programa permite modificar distintas configuraciones internas
desde el runtime, sin tener que recompilar. Por esto, el uso del programa es
como sigue
```
./memcached [-n hilos] [-m memoria] [-c celdas] [-r regiones]
```
Todos estos aspectos tienen configuraciones por defecto, luego no es necesario
especificarlas:
* -n se usa para cambiar la cantidad de hilos trabajadores del proceso. Por
defecto, es la cantidad de procesadores del sistema en que se corre
* -m es usado para especificar la cantidad de memoria (en megabytes) a la que se
limita el programa. Por defecto, es un gigabyte.
* -c ajusta la cantidad de celdas en la memcache. Mas de estas suponen menos
colisiones internas, pero mayor gasto de recursos. Por defecto es un millón
de celdas.
* -r indica la cantidad de regiones utilizadas para dividir la cache. Más
regiones supone una mayor especialización de los locks usados, pero también más
uso de recursos. Por defecto es 500

Por otro lado, para correr la memcache en los puertos privilegiados, se corre
con permisos de `sudo`
```
sudo ./memcached
```
El programa suelta estos permisos tras hacer los bindings, por lo que no hay
peligro.

## Remover archivos generados

Para remover los archivos generados por la compilación haciendo uso del archivo
Makefile se debe utilizar el siguiente comando:
```
$ make clean
```

## Uso del cliente
El uso del cliente en Erlang es sencillo. Desde la maquina virtual de Erlang
compilamos y abrimos una conexión como sigue:
```erlang
c("lib/client.erl"), Id = client:start("localhost", 8889).
```
Se debe almacenar el valor de retorno, ya que este identifica la conexión
abierta y es necesario para realizar las peticiones al servidor.

Las peticiones siempre se hacen entonces suministrando el Id, los siguientes
son ejemplos:

* **PUT:**
```erlang
client:put(Id, llave_de_prueba, "valor de prueba").
```
* **GET:**
```erlang
client:get(Id, <<"llave en binario">>).
```
* **DEL:**
```erlang
client:del(Id, 1234567890).
```
* **STATS:**
```erlang
client:stats(Id).
```

Para cerrar una conexión, realizamos
```erlang
client:exit(Id).
```

## Testing del programa
Para testear el servidor utilizamos distintos clientes variando distintos
parametros:
* test_cliente.c
* test_server.c
* cliente_binario.c
* netcat

\
Programa realizado por Bautista José Peirone y Juan Bautista Figueredo