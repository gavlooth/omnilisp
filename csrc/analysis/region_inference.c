#include "region_inference.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Simple adjacency list for Interaction Graph
typedef struct IGNode {
    char* var;
    char** neighbors;
    size_t count;
    size_t capacity;
    bool visited;
    int component_id;
} IGNode;

typedef struct {
    IGNode* nodes;
    size_t count;
    size_t capacity;
} InteractionGraph;

static void ig_init(InteractionGraph* ig) {
    ig->count = 0;
    ig->capacity = 16;
    ig->nodes = malloc(sizeof(IGNode) * ig->capacity);
    memset(ig->nodes, 0, sizeof(IGNode) * ig->capacity);
}

static IGNode* ig_get_node(InteractionGraph* ig, const char* var) {
    for (size_t i = 0; i < ig->count; i++) {
        if (strcmp(ig->nodes[i].var, var) == 0) {
            return &ig->nodes[i];
        }
    }
    // Add new
    if (ig->count >= ig->capacity) {
        ig->capacity *= 2;
        ig->nodes = realloc(ig->nodes, sizeof(IGNode) * ig->capacity);
    }
    IGNode* node = &ig->nodes[ig->count++];
    node->var = strdup(var);
    node->neighbors = NULL;
    node->count = 0;
    node->capacity = 0;
    node->visited = false;
    node->component_id = -1;
    return node;
}

static void ig_add_edge(InteractionGraph* ig, const char* u, const char* v) {
    if (strcmp(u, v) == 0) return; // Self loop
    
    IGNode* nu = ig_get_node(ig, u);
    IGNode* nv = ig_get_node(ig, v);
    
    // Add v to u
    for (size_t i = 0; i < nu->count; i++) {
        if (strcmp(nu->neighbors[i], v) == 0) return; // Exists
    }
    if (nu->count >= nu->capacity) {
        nu->capacity = nu->capacity == 0 ? 4 : nu->capacity * 2;
        nu->neighbors = realloc(nu->neighbors, sizeof(char*) * nu->capacity);
    }
    nu->neighbors[nu->count++] = strdup(v);
    
    // Add u to v (Undirected)
    if (nv->count >= nv->capacity) {
        nv->capacity = nv->capacity == 0 ? 4 : nv->capacity * 2;
        nv->neighbors = realloc(nv->neighbors, sizeof(char*) * nv->capacity);
    }
    nv->neighbors[nv->count++] = strdup(u);
}

static void ig_free(InteractionGraph* ig) {
    for (size_t i = 0; i < ig->count; i++) {
        free(ig->nodes[i].var);
        for (size_t j = 0; j < ig->nodes[i].count; j++) {
            free(ig->nodes[i].neighbors[j]);
        }
        free(ig->nodes[i].neighbors);
    }
    free(ig->nodes);
}

// ----------------------------------------------------------------------------

static void build_interaction_graph(CompilerCtx* ctx, InteractionGraph* ig) {
    // Iterate over CFG/Instructions to find interactions
    // This requires traversing the AST or IR.
    // For now, we assume we can query 'var_usages' from AnalysisContext
    // But `CompilerCtx` has `analysis`.
    
    // TODO: Implement AST traversal to find:
    // 1. Assignments: let x = y -> edge(x, y)
    // 2. Constructors: let x = cons(a, b) -> edge(x, a), edge(x, b)
    // 3. Calls: f(a, b) -> edge(a, b)
    
    // Stub: Connect everything to everything for safety (One Big Region)
    // This implements the "One Region per Function" fallback until AST traversal is ready.
    // In a real implementation, we would be more selective.
    
    AnalysisContext* actx = ctx->analysis;
    VarUsage* u = actx->var_usages;
    while (u) {
        VarUsage* v = actx->var_usages;
        while (v) {
            ig_add_edge(ig, u->name, v->name);
            v = v->next;
        }
        u = u->next;
    }
}

static void find_components(InteractionGraph* ig) {
    int comp_id = 0;
    for (size_t i = 0; i < ig->count; i++) {
        if (!ig->nodes[i].visited) {
            // BFS/DFS
            IGNode* stack[1024];
            int top = 0;
            stack[top++] = &ig->nodes[i];
            ig->nodes[i].visited = true;
            ig->nodes[i].component_id = comp_id;
            
            while (top > 0) {
                IGNode* curr = stack[--top];
                for (size_t j = 0; j < curr->count; j++) {
                    IGNode* neighbor = ig_get_node(ig, curr->neighbors[j]);
                    if (!neighbor->visited) {
                        neighbor->visited = true;
                        neighbor->component_id = comp_id;
                        stack[top++] = neighbor;
                    }
                }
            }
            comp_id++;
        }
    }
}

void infer_regions(CompilerCtx* ctx) {
    InteractionGraph ig;
    ig_init(&ig);
    
    // 1. Build Graph
    build_interaction_graph(ctx, &ig);
    
    // 2. Connected Components
    find_components(&ig);
    
    // 3. Create Regions
    // For now, assume 1 component = 1 region (due to the stub above).
    // In future, iterate components and create RegionInfo for each.
    
    // Group vars by component
    // Start/End pos calculation
    
    AnalysisContext* actx = ctx->analysis;
    
    // STUB: Create one global region for the function
    RegionInfo* r = omni_region_new(actx, "fn_region");
    r->start_pos = 0;
    r->end_pos = 999999; // Till end of function
    
    // Assign all vars to this region
    VarUsage* u = actx->var_usages;
    while (u) {
        omni_region_add_var(actx, u->name);
        u = u->next;
    }
    
    ig_free(&ig);
}
