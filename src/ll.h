#ifndef __LLIST_H__
#define __LLIST_H__ // Listas enlazadas generales

#include "common.h"
#include <assert.h>
#include <stdlib.h>

typedef struct _Node {
    void *data;
    struct _Node *next;
} Node;

typedef Node *List;

/*
** Returns an empty list
*/
List list_init();

/*
** Destroys the list
*/
void list_free(List list, DestroyFunction destroy);

/*
** Returns 1 if the list is empty, 0 otherwise
*/
int list_empty(List list);

/*
** Adds an element to the start of the list
*/
List list_append_start(List list, void *data, CopyFunction copy);

/*
** Removes the first element of the list if its not empty, and returns the
** new first node
*/
List list_remove_start(List list, DestroyFunction destroy);

/*
** Visits all nodes on the list, applying the function to the
** data of each one
*/
void list_visit(List list, VisitFunction f);

/*
** Returns the data from a node that matches with data passed as argument,
** returns NULL if no node matches.
*/
void* list_search(List list, void* data, CompareFunction cmp);

#endif /* __GLIST_H__ */
