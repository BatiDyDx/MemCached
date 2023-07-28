#include <assert.h>     
#include <unistd.h>
#include "sock.h"
#include "common.h"

static char** parse_arguments(char** argv, int argc, int txt_sck, int bin_sck);

int main(int argc, char** argv){

    in_port_t txt_port;
    in_port_t bin_port;

    uid_t uid = getuid(); // consegimos el uid del proceso
    uid_t dest_uid = 1000;
    if (uid == 0) {  
        // se ejecuto desde root
        // conexion con privilegios
        txt_port = 888;
        bin_port = 889;
    }
    else {
        // conexion sin privilegios
        txt_port = 8888;
        bin_port = 8889;
    }
    
    int txt_sck = mk_tcp_sock(txt_port);
    if (text_sck < 0)
      quit("mk_tcp_sock.text");

    int bin_sck = mk_tcp_sock(bin_port);
    if (bin_sck < 0)
      quit("mk_tcp_sock.bin");

    if(uid == 0){
        // en root, bajamos privilegios
        if(setuid(dest_uid) != 0) {
            perror("Error: ");
            return -1;
        }
    }

    char** arg = parse_arguments(argv, argc, txt_sck, bin_sck);
    execv("./memcached", arg);
    return 0;
}

/* Función auxiliar para parseo correcto de argumentos a utilizar en execv */ 
char** parse_arguments(char** argv, int argc, int txt_sck, int bin_sck) {
    char** arg = malloc(sizeof(char*) * (argc + 1));
    argv++;
    for (int i = 0; i < argc - 1; i++)
        arg[i] = argv[i];

    arg[argc - 1] = malloc(5);
    snprintf(arg[argc - 1], 5, "%d", txt_sck);
    arg[argc] = malloc(5);
    snprintf(arg[argc], 5, "%d", bin_sck);
    return arg;
}