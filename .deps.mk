common.o: common.c common.h log.h
log.o: log.c log.h
parser.o: parser.c parser.h common.h log.h
sock.o: sock.c common.h log.h
