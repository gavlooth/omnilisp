module lisp;

import std::io;
import std::collections::list;
import main;
// =============================================================================
// SECTION 4b: CLOSURE CREATION ANALYSIS
// =============================================================================

/**
 * Check if an expression body creates closures that might escape.
 * Used to determine if a lambda needs frame region management.
 *
 * A lambda "creates closures" if its body contains lambda expressions.
 * This is conservative but correct - if a lambda is created, it might escape.
 */
