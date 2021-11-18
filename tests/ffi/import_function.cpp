
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>

#include "../../runtime/pz_common.h"
#include "../../runtime/pz_foreign.h"
#include "../../runtime/pz_generic_run.h"

extern "C" {
    bool pz_init_foreign_code(void *f, void *gc);
}

using namespace pz;

static unsigned foo(void * stack_, unsigned sp)
{
    printf("Hi mum\n");
    return sp;
}

static unsigned getpid_fn(void * stack_, unsigned sp)
{
    StackValue * stack = reinterpret_cast<StackValue *>(stack_);
    stack[++sp].u32 = getpid();
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
    
    if (!foreign->register_foreign_code(String("ImportFunction"),
                String("getpid"), getpid_fn, gc))
    {
        return false;
    }

    return true;
}

