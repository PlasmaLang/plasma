
#include <stdio.h>

#include "../../runtime/pz_common.h"
#include "../../runtime/pz_foreign.h"

extern "C" {
    bool pz_init_foreign_code(void *f, void *gc);
}

using namespace pz;

static unsigned foo(void * stack_, unsigned sp)
{
    printf("Hi mum\n");
    return sp;
}

bool pz_init_foreign_code(void *foreign_, void *gc_)
{
    GCTracer &gc = *reinterpret_cast<GCTracer*>(gc_);
    Foreign *foreign = reinterpret_cast<Foreign*>(foreign_);

    if (!foreign->register_foreign_code(String("ImportFunction"), String("foo"),
                foo, gc))
    {
        return false;
    }

    return true;
}

