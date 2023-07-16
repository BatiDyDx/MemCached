En memcached.c hay un esqueleto muy imcompleto de una memcache que lo pueden usar como inspiración o no.

La rutina consume_text es la importante ya que se encarga de consumir el texto que está en el buffer y parsearlo, usando la función text_parser implementada en parser.c, la pueden usar o usar su propia versión. 

Hay Makefile de ejemplo con los archivos que se entregan. Pueden usarlo de modelo e ir ampliando las reglas a medida que agregan nuevos fuentes.