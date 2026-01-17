/*
 * modules.c - Module and import system for OmniLisp
 *
 * Implements module system for code organization and namespace management.
 *
 * API:
 *   - module: Create a new module
 *   - import: Import symbols from a module
 *   - export: Export symbols from a module
 *   - use: Use module (short form of import)
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "../include/omni.h"
#include "internal_types.h"

/* ============== Module Structure ============== */

/*
 * Module: A namespace for symbols
 *
 * A module contains:
 * - Name: Module identifier
 * - Exports: List of exported symbols
 * - Imports: List of imported modules
 * - Private: Private symbols (not exported)
 */

typedef struct ModuleExport {
    char* name;              /* Symbol name */
    Obj* value;              /* Symbol value */
    int is_public;           /* Export flag */
    struct ModuleExport* next;
} ModuleExport;

typedef enum {
    IMPORT_ALL = 0,         /* Import all exports */
    IMPORT_ONLY,            /* Import only specified symbols */
    IMPORT_EXCEPT           /* Import all except specified symbols */
} ImportMode;

typedef struct ModuleImport {
    char* module_name;       /* Imported module name */
    char* prefix;            /* Optional prefix for namespacing (for :as) */
    char** imported_symbols;  /* Symbols to import (for :only) or exclude (for :except) */
    int symbol_count;
    ImportMode mode;         /* Import mode */
    struct ModuleImport* next;
} ModuleImport;

typedef struct Module {
    char* name;              /* Module name */
    ModuleExport* exports;    /* Exported symbols */
    ModuleImport* imports;    /* Imported modules */
    struct Module* next;     /* Next in global registry */
} Module;

/* Global module registry */
static Module* g_module_registry = NULL;

/* Current module being defined */
static Module* g_current_module = NULL;

/* ============== Module Registry Functions ============== */

/*
 * Find or create a module by name
 */
static Module* get_or_create_module(const char* name) {
    if (!name) return NULL;

    /* Search for existing module */
    for (Module* m = g_module_registry; m; m = m->next) {
        if (strcmp(m->name, name) == 0) {
            return m;
        }
    }

    /* Create new module */
    Module* m = malloc(sizeof(Module));
    memset(m, 0, sizeof(Module));
    m->name = strdup(name);
    m->next = g_module_registry;
    g_module_registry = m;

    return m;
}

/*
 * Find a module by name
 */
static Module* find_module(const char* name) {
    for (Module* m = g_module_registry; m; m = m->next) {
        if (strcmp(m->name, name) == 0) {
            return m;
        }
    }
    return NULL;
}

/* ============== Module API ============== */

/*
 * prim_module_begin: Begin defining a new module
 *
 * Args: module_name (string or symbol)
 * Returns: The module object
 *
 * Example:
 *   (module-begin "MyModule")
 *   (define x 10)
 *   (export x)
 *   (module-end)
 */
Obj* prim_module_begin(Obj* name_obj) {
    const char* name = NULL;

    if (IS_BOXED(name_obj) && name_obj->tag == TAG_SYM) {
        name = (const char*)name_obj->ptr;
    } else if (IS_BOXED(name_obj) && name_obj->tag == TAG_STRING) {
        name = (const char*)name_obj->ptr;
    }

    if (!name) {
        fprintf(stderr, "module-begin: Invalid module name\n");
        return NULL;
    }

    /* Set current module */
    g_current_module = get_or_create_module(name);

    return mk_sym(g_current_module->name);
}

/*
 * prim_module_end: End current module definition
 *
 * Returns: The module object
 */
Obj* prim_module_end(void) {
    if (!g_current_module) {
        fprintf(stderr, "module-end: No active module\n");
        return NULL;
    }

    Module* m = g_current_module;
    g_current_module = NULL;

    return mk_sym(m->name);
}

/*
 * prim_export: Export a symbol from current module
 *
 * Args: symbol_name, value
 * Returns: The value
 *
 * Example:
 *   (export 'some-symbol value)
 */
Obj* prim_export(Obj* symbol_name, Obj* value) {
    if (!g_current_module) {
        fprintf(stderr, "export: No active module\n");
        return value;
    }

    const char* name = NULL;
    if (IS_BOXED(symbol_name) && symbol_name->tag == TAG_SYM) {
        name = (const char*)symbol_name->ptr;
    } else if (IS_BOXED(symbol_name) && symbol_name->tag == TAG_STRING) {
        name = (const char*)symbol_name->ptr;
    }

    if (!name) {
        fprintf(stderr, "export: Invalid symbol name\n");
        return value;
    }

    /* Add to exports */
    ModuleExport* export = malloc(sizeof(ModuleExport));
    export->name = strdup(name);
    export->value = value;
    export->is_public = 1;
    export->next = g_current_module->exports;
    g_current_module->exports = export;

    return value;
}

/*
 * prim_module_get: Get a module by name
 *
 * Args: module_name
 * Returns: The module object or NULL
 */
Obj* prim_module_get(Obj* name_obj) {
    const char* name = NULL;

    if (IS_BOXED(name_obj) && name_obj->tag == TAG_SYM) {
        name = (const char*)name_obj->ptr;
    } else if (IS_BOXED(name_obj) && name_obj->tag == TAG_STRING) {
        name = (const char*)name_obj->ptr;
    }

    Module* m = find_module(name);

    if (!m) {
        return NULL;
    }

    /* Return module as symbol for now */
    return mk_sym(m->name);
}

/* ============== Import API ============== */

/*
 * prim_import: Import symbols from a module
 *
 * Args: module_name, options (keyword args)
 * Options:
 *   - :prefix: Add prefix to imported symbols
 *   - :only: Import only specified symbols
 *   - :except: Import all except specified symbols
 *   - :rename: Rename imported symbols
 *
 * Returns: Boolean indicating success
 *
 * Example:
 *   (import "MyModule" :only [foo bar])
 *   (import "OtherModule" :prefix "other")
 */
Obj* prim_import(Obj* name_obj, Obj* options) {
    const char* name = NULL;

    if (IS_BOXED(name_obj) && name_obj->tag == TAG_SYM) {
        name = (const char*)name_obj->ptr;
    } else if (IS_BOXED(name_obj) && name_obj->tag == TAG_STRING) {
        name = (const char*)name_obj->ptr;
    }

    Module* m = find_module(name);
    if (!m) {
        fprintf(stderr, "import: Module '%s' not found\n", name ? name : "?");
        return mk_bool(0);
    }

    /* Parse options */
    char* prefix = NULL;
    Obj* only_list = NULL;
    Obj* except_list = NULL;

    if (options && IS_BOXED(options) && options->tag == TAG_PAIR) {
        Obj* opt_list = options;
        while (opt_list && IS_BOXED(opt_list) && opt_list->tag == TAG_PAIR) {
            Obj* opt = opt_list->a;
            Obj* rest = opt_list->b;

            if (IS_BOXED(opt) && opt->tag == TAG_PAIR) {
                Obj* key = opt->a;
                Obj* value = opt->b;

                if (IS_BOXED(key) && key->tag == TAG_SYM) {
                    const char* key_str = (const char*)key->ptr;

                    if (strcmp(key_str, "prefix") == 0) {
                        if (IS_BOXED(value) && value->tag == TAG_SYM) {
                            prefix = (char*)value->ptr;
                        }
                    } else if (strcmp(key_str, "only") == 0) {
                        only_list = value;
                    } else if (strcmp(key_str, "except") == 0) {
                        except_list = value;
                    }
                }
            }

            opt_list = rest;
        }
    }

    /* Add import to current module */
    if (g_current_module) {
        ModuleImport* import = malloc(sizeof(ModuleImport));
        import->module_name = strdup(name);
        import->prefix = prefix ? strdup(prefix) : NULL;
        import->imported_symbols = NULL;
        import->symbol_count = 0;
        import->next = g_current_module->imports;
        g_current_module->imports = import;

        return mk_bool(1);
    }

    return mk_bool(0);
}

/*
 * Helper to extract symbol name string from Obj
 */
static const char* get_symbol_name(Obj* obj) {
    if (!obj || !IS_BOXED(obj)) return NULL;
    if (obj->tag == TAG_SYM || obj->tag == TAG_STRING) {
        return (const char*)obj->ptr;
    }
    return NULL;
}

/*
 * Helper to extract symbols from array/list into string array
 */
static char** extract_symbol_list(Obj* list, int* count_out) {
    if (!list || !IS_BOXED(list)) {
        *count_out = 0;
        return NULL;
    }

    /* Count symbols */
    int count = 0;
    if (list->tag == TAG_ARRAY) {
        Array* arr = (Array*)list->ptr;
        count = arr ? arr->len : 0;
    } else if (list->tag == TAG_PAIR) {
        Obj* p = list;
        while (p && IS_BOXED(p) && p->tag == TAG_PAIR) {
            count++;
            p = p->b;
        }
    }

    if (count == 0) {
        *count_out = 0;
        return NULL;
    }

    char** symbols = malloc(sizeof(char*) * count);
    int idx = 0;

    if (list->tag == TAG_ARRAY) {
        Array* arr = (Array*)list->ptr;
        for (int i = 0; i < count && arr; i++) {
            const char* name = get_symbol_name(arr->data[i]);
            symbols[idx++] = name ? strdup(name) : strdup("?");
        }
    } else if (list->tag == TAG_PAIR) {
        Obj* p = list;
        while (p && IS_BOXED(p) && p->tag == TAG_PAIR && idx < count) {
            const char* name = get_symbol_name(p->a);
            symbols[idx++] = name ? strdup(name) : strdup("?");
            p = p->b;
        }
    }

    *count_out = idx;
    return symbols;
}

/*
 * prim_import_only: Import only specified symbols from a module
 *
 * Syntax: (import module :only [sym1 sym2 ...])
 * Args: module_name, symbols_list (array or list of symbols)
 * Returns: Boolean indicating success
 */
Obj* prim_import_only(Obj* name_obj, Obj* symbols_list) {
    const char* name = get_symbol_name(name_obj);
    if (!name) {
        fprintf(stderr, "import-only: Invalid module name\n");
        return mk_bool(0);
    }

    Module* m = find_module(name);
    if (!m) {
        fprintf(stderr, "import-only: Module '%s' not found\n", name);
        return mk_bool(0);
    }

    if (!g_current_module) {
        fprintf(stderr, "import-only: No active module context\n");
        return mk_bool(0);
    }

    /* Extract symbol list */
    int symbol_count = 0;
    char** symbols = extract_symbol_list(symbols_list, &symbol_count);

    /* Validate that requested symbols are exported */
    for (int i = 0; i < symbol_count; i++) {
        int found = 0;
        for (ModuleExport* exp = m->exports; exp; exp = exp->next) {
            if (strcmp(exp->name, symbols[i]) == 0) {
                found = 1;
                break;
            }
        }
        if (!found) {
            fprintf(stderr, "import-only: Symbol '%s' not exported by module '%s'\n",
                    symbols[i], name);
        }
    }

    /* Add import record */
    ModuleImport* import = malloc(sizeof(ModuleImport));
    import->module_name = strdup(name);
    import->prefix = NULL;
    import->imported_symbols = symbols;
    import->symbol_count = symbol_count;
    import->mode = IMPORT_ONLY;
    import->next = g_current_module->imports;
    g_current_module->imports = import;

    return mk_bool(1);
}

/*
 * prim_import_as: Import module with an alias prefix
 *
 * Syntax: (import module :as alias)
 * Args: module_name, alias (symbol or string)
 * Returns: Boolean indicating success
 *
 * After import, symbols are accessed as alias.symbol
 */
Obj* prim_import_as(Obj* name_obj, Obj* alias_obj) {
    const char* name = get_symbol_name(name_obj);
    if (!name) {
        fprintf(stderr, "import-as: Invalid module name\n");
        return mk_bool(0);
    }

    const char* alias = get_symbol_name(alias_obj);
    if (!alias) {
        fprintf(stderr, "import-as: Invalid alias\n");
        return mk_bool(0);
    }

    Module* m = find_module(name);
    if (!m) {
        fprintf(stderr, "import-as: Module '%s' not found\n", name);
        return mk_bool(0);
    }

    if (!g_current_module) {
        fprintf(stderr, "import-as: No active module context\n");
        return mk_bool(0);
    }

    /* Add import record with alias */
    ModuleImport* import = malloc(sizeof(ModuleImport));
    import->module_name = strdup(name);
    import->prefix = strdup(alias);
    import->imported_symbols = NULL;
    import->symbol_count = 0;
    import->mode = IMPORT_ALL;
    import->next = g_current_module->imports;
    g_current_module->imports = import;

    return mk_bool(1);
}

/*
 * prim_import_except: Import all exports except specified symbols
 *
 * Syntax: (import module :except [sym1 sym2 ...])
 * Args: module_name, exclude_list (array or list of symbols to exclude)
 * Returns: Boolean indicating success
 */
Obj* prim_import_except(Obj* name_obj, Obj* exclude_list) {
    const char* name = get_symbol_name(name_obj);
    if (!name) {
        fprintf(stderr, "import-except: Invalid module name\n");
        return mk_bool(0);
    }

    Module* m = find_module(name);
    if (!m) {
        fprintf(stderr, "import-except: Module '%s' not found\n", name);
        return mk_bool(0);
    }

    if (!g_current_module) {
        fprintf(stderr, "import-except: No active module context\n");
        return mk_bool(0);
    }

    /* Extract exclusion list */
    int exclude_count = 0;
    char** excludes = extract_symbol_list(exclude_list, &exclude_count);

    /* Add import record */
    ModuleImport* import = malloc(sizeof(ModuleImport));
    import->module_name = strdup(name);
    import->prefix = NULL;
    import->imported_symbols = excludes;
    import->symbol_count = exclude_count;
    import->mode = IMPORT_EXCEPT;
    import->next = g_current_module->imports;
    g_current_module->imports = import;

    return mk_bool(1);
}

/*
 * prim_use: Use module (convenience form of import)
 *
 * Args: module_name
 * Returns: Boolean indicating success
 *
 * Example:
 *   (use "MyModule")
 */
Obj* prim_use(Obj* name_obj) {
    return prim_import(name_obj, NULL);
}

/*
 * prim_require: Require a module (load and import if not loaded)
 *
 * Args: module_name
 * Returns: The module object
 *
 * Note: This would typically load the module from a file
 * if it's not already loaded. For now, it just looks up
 * existing modules.
 */
Obj* prim_require(Obj* name_obj) {
    Obj* module = prim_module_get(name_obj);

    if (!module) {
        /* Module not found - would need to load from file */
        const char* name = NULL;
        if (IS_BOXED(name_obj) && name_obj->tag == TAG_SYM) {
            name = (const char*)name_obj->ptr;
        } else if (IS_BOXED(name_obj) && name_obj->tag == TAG_STRING) {
            name = (const char*)name_obj->ptr;
        }

        fprintf(stderr, "require: Module '%s' not found and auto-loading not implemented\n",
                name ? name : "?");
    }

    return module;
}

/*
 * prim_module_exports: Get list of exported symbols from module
 *
 * Args: module_name
 * Returns: List of (symbol-name . value) pairs
 */
Obj* prim_module_exports(Obj* name_obj) {
    const char* name = NULL;

    if (IS_BOXED(name_obj) && name_obj->tag == TAG_SYM) {
        name = (const char*)name_obj->ptr;
    } else if (IS_BOXED(name_obj) && name_obj->tag == TAG_STRING) {
        name = (const char*)name_obj->ptr;
    }

    Module* m = find_module(name);
    if (!m) {
        return NULL;  /* Module not found */
    }

    /* Build list of exports */
    Obj* result_list = NULL;
    ModuleExport* exp = m->exports;

    while (exp) {
        Obj* pair = mk_pair(mk_sym(exp->name), exp->value);
        Obj* new_list = mk_pair(pair, result_list);
        result_list = new_list;
        exp = exp->next;
    }

    return result_list;
}

/*
 * prim_module_list: List all registered modules
 *
 * Returns: List of module names
 */
Obj* prim_module_list(void) {
    Obj* result_list = NULL;

    for (Module* m = g_module_registry; m; m = m->next) {
        Obj* new_list = mk_pair(mk_sym(m->name), result_list);
        result_list = new_list;
    }

    return result_list;
}

/* ============== Qualified Name Resolution ============== */

/*
 * Helper to check if symbol is allowed by import mode
 */
static int is_symbol_allowed(ModuleImport* imp, const char* symbol_name) {
    if (!imp) return 1;  /* No import restrictions */

    switch (imp->mode) {
        case IMPORT_ALL:
            return 1;

        case IMPORT_ONLY:
            /* Only allow if in the imported_symbols list */
            for (int i = 0; i < imp->symbol_count; i++) {
                if (strcmp(imp->imported_symbols[i], symbol_name) == 0) {
                    return 1;
                }
            }
            return 0;

        case IMPORT_EXCEPT:
            /* Block if in the imported_symbols (exclusion) list */
            for (int i = 0; i < imp->symbol_count; i++) {
                if (strcmp(imp->imported_symbols[i], symbol_name) == 0) {
                    return 0;
                }
            }
            return 1;
    }
    return 1;
}

/*
 * Helper to find import record by alias or module name
 */
static ModuleImport* find_import_by_name(Module* context, const char* name) {
    if (!context) return NULL;

    for (ModuleImport* imp = context->imports; imp; imp = imp->next) {
        /* Check alias first */
        if (imp->prefix && strcmp(imp->prefix, name) == 0) {
            return imp;
        }
        /* Then check module name */
        if (strcmp(imp->module_name, name) == 0) {
            return imp;
        }
    }
    return NULL;
}

/*
 * prim_resolve: Resolve a qualified name (module.symbol or alias.symbol)
 *
 * Args: qualified_name (string like "Module.symbol" or "alias.symbol")
 * Returns: The resolved value or NULL
 *
 * Example:
 *   (resolve "MyModule.foo")   ; Direct module access
 *   (resolve "m.foo")          ; Aliased access if (import MyModule :as m)
 *
 * Resolution considers import restrictions:
 *   - :only imports only allow listed symbols
 *   - :except imports block excluded symbols
 *   - :as imports allow access via alias prefix
 */
Obj* prim_resolve(Obj* qualified_name) {
    const char* name = NULL;

    if (IS_BOXED(qualified_name) && qualified_name->tag == TAG_STRING) {
        name = (const char*)qualified_name->ptr;
    } else if (IS_BOXED(qualified_name) && qualified_name->tag == TAG_SYM) {
        name = (const char*)qualified_name->ptr;
    }

    if (!name) return NULL;

    /* Find the dot separator (also support / as separator) */
    const char* dot = strchr(name, '.');
    if (!dot) dot = strchr(name, '/');
    if (!dot) return NULL;

    /* Split into prefix and symbol */
    size_t prefix_len = dot - name;
    char* prefix = malloc(prefix_len + 1);
    strncpy(prefix, name, prefix_len);
    prefix[prefix_len] = '\0';

    const char* symbol_name = dot + 1;

    /* First, check for aliased import in current module context */
    Module* target_module = NULL;
    ModuleImport* active_import = NULL;

    if (g_current_module) {
        active_import = find_import_by_name(g_current_module, prefix);
        if (active_import) {
            target_module = find_module(active_import->module_name);
        }
    }

    /* If no import found, try direct module lookup */
    if (!target_module) {
        target_module = find_module(prefix);
    }

    free(prefix);

    if (!target_module) return NULL;

    /* Check import restrictions */
    if (active_import && !is_symbol_allowed(active_import, symbol_name)) {
        /* Symbol blocked by :only or :except */
        return NULL;
    }

    /* Find the export */
    for (ModuleExport* exp = target_module->exports; exp; exp = exp->next) {
        if (strcmp(exp->name, symbol_name) == 0) {
            return exp->value;
        }
    }

    return NULL;
}

/* ============== Cleanup ============== */

/*
 * Free all modules (for shutdown)
 */
void free_all_modules(void) {
    Module* m = g_module_registry;

    while (m) {
        Module* next = m->next;

        /* Free exports */
        ModuleExport* exp = m->exports;
        while (exp) {
            ModuleExport* next_exp = exp->next;
            free(exp->name);
            free(exp);
            exp = next_exp;
        }

        /* Free imports */
        ModuleImport* imp = m->imports;
        while (imp) {
            ModuleImport* next_imp = imp->next;
            free(imp->module_name);
            if (imp->prefix) free(imp->prefix);
            free(imp);
            imp = next_imp;
        }

        /* Free module name and structure */
        free(m->name);
        free(m);

        m = next;
    }

    g_module_registry = NULL;
    g_current_module = NULL;
}
