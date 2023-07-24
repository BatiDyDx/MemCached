#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include "bin_processing.h"
#include "text_processing.h"

int main(){
    /* binary parser test */
    int fd = open("../tests/stats.bin",O_RDONLY,"rb");
    int ntoks;
    ntoks = bin_handler(fd);
    close(fd);

    /* text parser test */
    fd = open("../tests/put_e_123.txt",O_RDONLY,"rb");
    ntoks = text_handler(fd);
    close(fd);

    return 0;
}

