#include <stdio.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <errno.h>
#include <pthread.h>

#define BUFF_SIZE 1024
#define N 500

int sock[N];
pthread_t nthreads[N];

void* client_thread(void*  a);
struct sockaddr_in servaddr;

int main(){
	int i;
	memset(&servaddr, 0, sizeof(servaddr));
	servaddr.sin_family = AF_INET;
	servaddr.sin_port = htons(8888);
	inet_pton(AF_INET, "127.0.0.1", &servaddr.sin_addr);
	for (i = 0; i < N; i++) {
		sock[i] = socket(AF_INET, SOCK_STREAM, 0);
		if (sock[i] < 0) {
			perror("CLNT: Error creando socket");
			return -1;
		}
		pthread_create(&nthreads[i], NULL,(void (*)(int))client_thread , &i);
	}
	pthread_exit(&nthreads[0]);
	for (i = 0; i < N; i++)
		close(sock[i]);
	return 0;
}

void* client_thread(void* a){
	int i = *(int*) a;
	int rc;
	int cto;
	char buffer[BUFF_SIZE], buffer2[BUFF_SIZE];
	struct timespec tsp;
	tsp.tv_sec = 0;
	tsp.tv_nsec = 100000;
		//printf("Conectando cliente %d...\n", i);

		/*
		 * Esto sólo está para no agotar los puertos efímeros
		 * de la dirección 127.0.0.1, de los cuales hay aprox
		 * 30000. Hacemos un bind() explicito a puertos e IPs
		 * (de origen) distintas a modo de evitarlo. (Todas
		 * las IP 127.x.y.z representan al host local.)
		 */
		
		struct sockaddr_in clientaddr;
		int portnum = i % 10000 + 10000;
		int ipaddrnum = i / 10000 + 10;
		char ipaddr[32];

		memset(&clientaddr, 0, sizeof(clientaddr));
		clientaddr.sin_family = AF_INET;
		clientaddr.sin_port = htons(portnum);
		sprintf(ipaddr, "127.0.0.%d", ipaddrnum);
		inet_pton(AF_INET, ipaddr, &clientaddr.sin_addr);
		rc = bind(sock[i], (struct sockaddr *)&clientaddr, sizeof(clientaddr));
		if (rc < 0) {
			perror("ECHOCLNT: Error llamando a bind()");
			return 	NULL;
		}
	
		rc = connect(sock[i], (struct sockaddr *)&servaddr, sizeof(servaddr));
		if (rc < 0) {
			perror("CLNT: Error conectando");
			return NULL;
		}

		nanosleep(&tsp, NULL);

	while(1){
		int rc;

		sprintf(buffer2, "PUT abc defghi\n", i);
		cto = strlen(buffer2);

		#if 1
			rc = write(sock[i], buffer2, cto);
			if (rc < 0) {
				perror("CLNT: Error escribiendo");
				return NULL;
			}
		#else
			// Escribe de a un byte, tal vez útil para testear el servidor 
			for (i = 0; i < cto; i++) {
				rc = write(sock[i], buffer2+i, 1);
				if (rc < 0) {
					perror("CLNT: Error escribiendo");
					return -1;
				}
			}
		#endif
		cto = read(sock[i], buffer, sizeof(buffer) - 1);
		if (cto < 0) {
			perror("CLNT: Error leyendo");
			return NULL;
		}

		buffer[cto] = 0;
		printf("%s", buffer);
		}
	}
