module lisp;

import std::io;
import std::collections::list;
import main;
// =============================================================================
// SECTION 2: LIGHTNING CONSTANTS
// =============================================================================

// Register IDs are GNU lightning backend-specific enum indices.
// Keep these aligned with the installed lightning headers.
const int JIT_R0 @if(env::X86_64) = 0;   // RAX - scratch / return value
const int JIT_R1 @if(env::X86_64) = 1;   // R10 - scratch
const int JIT_R2 @if(env::X86_64) = 2;   // R11 - scratch
const int JIT_V0 @if(env::X86_64) = 3;   // RBX - callee-saved
const int JIT_V1 @if(env::X86_64) = 4;   // R13 - callee-saved
const int JIT_V2 @if(env::X86_64) = 5;   // R14 - callee-saved
const int JIT_FP @if(env::X86_64) = 15;  // RBP

const int JIT_R0 @if(!env::X86_64) = 4;   // _R9
const int JIT_R1 @if(!env::X86_64) = 5;   // _R10
const int JIT_R2 @if(!env::X86_64) = 6;   // _R11
const int JIT_V0 @if(!env::X86_64) = 11;  // _R19
const int JIT_V1 @if(!env::X86_64) = 12;  // _R20
const int JIT_V2 @if(!env::X86_64) = 13;  // _R21
const int JIT_FP @if(!env::X86_64) = 23;  // _R29

// jit_code_t enum values we need
const int CODE_ARG_L         = 17;
const int CODE_GETARG_L      = 24;
const int CODE_PUSHARGR_L    = 241;
const int CODE_PUSHARGI_L    = 242;
const int CODE_RETR_L        = 258;
const int CODE_RETI_L        = 259;
const int CODE_RETVAL_L      = 266;
const int CODE_MOVR          = 110;
const int CODE_MOVI          = 111;
const int CODE_ADDR          = 43;
const int CODE_SUBR          = 49;
const int CODE_MULR          = 56;
const int CODE_DIVR          = 62;
const int CODE_BEQI          = 193;
const int CODE_BNEI          = 203;
const int CODE_BLEI          = 189;
const int CODE_BGTI          = 199;
const int CODE_BLTR          = 184;
const int CODE_BGER          = 194;
const int CODE_BGTR          = 198;
const int CODE_BLER          = 188;
const int CODE_BEQR          = 192;
const int CODE_BNER          = 202;
const int CODE_JMPI          = 225;
const int CODE_LIVE          = 1;
const int CODE_LDXI_L        = 167;
const int CODE_STXI_L        = 183;
