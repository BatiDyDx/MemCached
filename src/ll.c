#include "ll.h"
#include <stdio.h>

List list_init() { return NULL; }

void list_free(List list, DestroyFunction destroy) {
    Node *node_to_delete;
    while (list != NULL) {
        node_to_delete = list;
        list = list->next;
        destroy(node_to_delete->data);
        free(node_to_delete);
    }
}

int list_empty(List list) { return (list == NULL); }

List list_append_start(List list, void *data, CopyFunction copy) {
    Node *new_node = malloc(sizeof(Node));
    assert(new_node != NULL);
    new_node->next = list;
    new_node->data = copy(data);
    return new_node;
}

List list_remove_start(List list, DestroyFunction destroy) {
    Node *tmp;
    if (glist_empty(list))
        return NULL;
    tmp = list;
    list = list->next;
    destroy(tmp->data);
    free(tmp);
    return list;
}

void list_visit(List list, VisitFunction f) {
    for (Node *node = list; node != NULL; node = node->next)
        f(node->data);
}

void* list_search(List list, void* data, CompareFunction cmp) {
    for (Node *node = list; node != NULL; node = node->next)
        if (cmp(data, node->data) == 0)
            return node->data;
    return NULL;
}
