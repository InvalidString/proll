#include <stdint.h>
#include "allocator.h"
#define UNOP_START 0x1000
#define BINOP_START 0x2000


// Heap

#ifdef STEPDEBUG
typedef enum {
    R = 0xdeadbeef,
    A,
    C,
} Tag;
#else
typedef enum { R, A, C, } Tag;
#endif

typedef struct {
    Tag **ptr;
    isz len;
    isz cap;
} ArrOfTag;

typedef struct{
    Tag tag;
    i32 a;
} HC_A;

typedef struct{
    Tag tag;
    i32 r;
} HC_R;

typedef struct{
    Tag tag;
    i32 sym;
    i32 len;
    i32 data[];
} HC_C;



typedef enum{
    PUTATOM, // a
    PUTVAR,
    PUTREF,
    PUTANON,
    PUTSTRUCT, // f/n


    MARK, // L
    // sets FP
    CALL, // f/n   needs to be followed by jump
    FAIL,
    SETBTP,

    BIND,
    UNIFY,

    // partially static unification
    UATOM,
    UVAR,
    USTRUCT,
    RJUMP,
    POP,
    UREF,
    CHECK,

    SON,
    UP,

    PUSHENV,
    POPENV,
    DELBTP,
    TRY,
    JUMP,
    INIT,
    HALT,
    NO,
    PRUNE,

    // unary operations
    NOT = UNOP_START,
    NEG,

    // binary operations
    ADD = BINOP_START,
    SUB,
    MUL,
    DIV,
    EQ,
    LESS,
    LESSEQ,
    MOD,
    AND,
    OR,
    XOR,
    BAND,
    BOR,
    BXOR,


} Opcode;

typedef struct{
    Opcode op;
    union{
        struct{
            i16 f;
            i16 n;
        };
        i32 arg;
    };
} Instr;

// the word size of the machine
typedef int32_t word;

typedef struct{
    Instr *code;
    i32 pc;

    // frame pointer to support func calls
    i32 fp;
    i32 sp;
    i32 tp;
    i32 bp;

    // loads and stores happen relative to this adress
    word *data;

    Tag *heap_base;
    i32 *trail_base;
    Arena heap;
    Arena trail;

    char* strTab;

    i32 count;
} Machine;


// execute 1 instruction
// return nonzero if machine should HALT
int step(Machine *m, Instr i);


void run(Machine *m);

static inline char* Opcode2str(Opcode op){
    switch(op){
    case PUTATOM:return"PUTATOM";
    case PUTVAR:return"PUTVAR";
    case PUTREF:return"PUTREF";
    case PUTANON:return"PUTANON";
    case PUTSTRUCT:return"PUTSTRUCT";
    case MARK:return"MARK";
    case CALL:return"CALL";
    case FAIL:return"FAIL";
    case SETBTP:return"SETBTP";
    case BIND:return"BIND";
    case UNIFY:return"UNIFY";
    case UATOM:return"UATOM";
    case UVAR:return"UVAR";
    case USTRUCT:return"USTRUCT";
    case POP:return"POP";
    case UREF:return"UREF";
    case CHECK:return"CHECK";
    case SON:return"SON";
    case UP:return"UP";
    case NOT:return"NOT";
    case NEG:return"NEG";
    case ADD:return"ADD";
    case SUB:return"SUB";
    case MUL:return"MUL";
    case DIV:return"DIV";
    case EQ:return"EQ";
    case LESS:return"LESS";
    case LESSEQ:return"LESSEQ";
    case MOD:return"MOD";
    case AND:return"AND";
    case OR:return"OR";
    case XOR:return"XOR";
    case BAND:return"BAND";
    case BOR:return"BOR";
    case BXOR:return"BXOR";
    case PUSHENV:return"PUSHENV";
    case POPENV:return"POPENV";
    case DELBTP:return"DELBTP";
    case TRY:return"TRY";
    case JUMP:return"JUMP";
    case INIT:return"INIT";
    case HALT:return"HALT";
    case NO:return"NO";
    case PRUNE:return"PRUNE";
    case RJUMP:return"RJUMP";
    }
    return 0;
}
