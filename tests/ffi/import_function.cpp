
#include <stdio.h>

extern "C" {
    bool pz_init_foreign_code(void* t);
}

bool pz_init_foreign_code(void* t) {
    printf("Running pz_init_foreign_code\n");
    return true;
}

