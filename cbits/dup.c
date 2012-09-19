#include "Rts.h"

// Not available in GHCi
//char  *            info_type       ( StgClosure *closure );
//extern void        printObj        ( StgClosure *obj );

StgWord dupClosureSize(StgClosure *closure) {
    ASSERT(LOOKS_LIKE_CLOSURE_PTR(closure));
    return closure_sizeW(closure);
}

StgWord dupHeapAlloced (StgClosure *closure) {
    ASSERT(LOOKS_LIKE_CLOSURE_PTR(closure));
    //return HEAP_ALLOCED(closure); does not work in GHCI
    return !closure_STATIC(closure);
}

void dupWarning(StgClosure *closure) {
    fprintf(stderr,"Static closure passed to dup!\n");
    //fprintf(stderr,"Type: %s\n", info_type(closure));
    // printObj(closure);
}

void dupDebugPtr(StgWord word) {
    fprintf(stderr,"dupDebugPtr %p\n", word);
}
