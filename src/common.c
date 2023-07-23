#include <assert.h>
#include <string.h>
#include <unistd.h>
#include "common.h"

int valid_rq(int code) {
  switch (code) {
    case PUT: case DEL: case GET: case STATS:
      return 1;
    default:
      return 0;
  }
}

const char *code_str(enum code e) {
	switch (e) {
	case PUT:	return "PUT";
	case GET:	return "GET";
	case DEL:	return "DEL";

	case STATS:	return "STATS";

	case OK:	return "OK";
	case EINVALID:	return "EINVALID";
	case ENOTFOUND:	return "ENOTFOUND";
	case EBINARY:	return "EBINARY";
	case EBIG:	return "EBIG";
	case EUNK:	return "EUNK";
  case EOOM:  return "EOOM";

	default:
		assert(0);
		return "";
	}
}
