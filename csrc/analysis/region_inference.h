#ifndef OMNI_REGION_INFERENCE_H
#define OMNI_REGION_INFERENCE_H

#include "../compiler/omni_compiler.h" // For access to AST and CFG

// Context for the region inference pass
typedef struct {
    // ... State for building the interaction graph ...
    // e.g., Hash map from Var -> ComponentID
} RegionInferenceCtx;

// Main entry point for the pass
void infer_regions(CompilerCtx* ctx);

#endif // OMNI_REGION_INFERENCE_H
