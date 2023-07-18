#include "ll.h"
#include <stdio.h>

List list_init() { return NULL; }

void list_free(List list) {
  while (list != NULL) {
    list = list_remove_start(list);
  }
}

int list_empty(List list) { return list == NULL; }

List list_add(List list, Data *data) {
  Node *new_node = malloc(sizeof(Node));
  assert(new_node != NULL);
  new_node->next = list;
  new_node->data = *data;
  return new_node;
}

List list_remove_first(List list) {
  List pnode;
  if (list_empty(list))
      return NULL;
  pnode = list;
  list = list->next;
  free(pnode->data.key);
  free(pnode->data.value);
  free(pnode);
  return list;
}

char* list_search(List list, char mode, char* key, unsigned len) {
  
}

