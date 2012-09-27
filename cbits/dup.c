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

void dupStaticWarning(StgClosure *closure) {
    fprintf(stderr,"Static closure passed to dup!\n");
    //fprintf(stderr,"Type: %s\n", info_type(closure));
    // printObj(closure);
}

void dupUnsupportedWarning(StgClosure *closure) {
    fprintf(stderr,"Closure of nun-dupable type %d passed to dup!\n", get_itbl(closure)->type);
}

void dupDebugPtr(StgWord word) {
    fprintf(stderr,"dupDebugPtr %p\n", word);
}
