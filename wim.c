#include "wim.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "optget.h"
#define ALLOCATOR_IMPL
#include "allocator.h"

#define error(...) do {                         \
        fprintf(stderr, "Error: "__VA_ARGS__);  \
        fprintf(stderr, "\n");                  \
        exit(1);                                \
    } while(0)

#define HEAPREL(obj) (&(obj)->tag - m->heap_base)



#define H m->heap_base
#define S m->data
#define SP m->sp
#define FP m->fp
#define BP m->bp
#define PC m->pc
#define T m->trail_base
#define TP m->tp

#define H_(v, T) (*(assert(H[v] == T), (HC_ ## T*)&H[v]))

#define posCont S[FP]
#define FPold   S[FP-1]
#define HPold   S[FP-2]
#define TPold   S[FP-3]
#define BPold   S[FP-4]
#define negCont S[FP-5]

void pprintV(Machine *m, i32 val){
    Tag *t = &H[val];
    if(!val) {
        printf("⟨unbound⟩");
        return;
    }
    switch (*t) {
        case R: pprintV(m, H_(val, R).r); break;
        case A: {
            i32 a = H_(val, A).a;
            printf("%s", m->strTab + a);
        } break;
        case C:{
            HC_C* c = &H_(val, C);
            printf("%s(", m->strTab + c->sym);
            for (isz i = 0; i < c->len; i++) {
                if(i > 0) printf(", ");
                pprintV(m, c->data[i]);
            }
            printf(")");
        } break;
    }
}

i32 deref(Machine *m, i32 v){
    if(H[v] == R){
        HC_R ref = *((HC_R*)&H[v]);
        assert(ref.r != v);
        if(ref.r != 0) return deref(m, ref.r);
    }
    return v;
}

void trail(Machine *m, i32 u){
    if(u < S[BP-2]){
        TP++;
        T[TP] = u;
    }
}

void reset(Machine *m, i32 x, i32 y){
    for (i32 u = y; x < u; u--) {
        // 0 and not self
        H_(T[u], R).r = 0;
    }
}

bool check(Machine *m, i32 u, i32 v){
    // TODO ocur check
    return true;
}

bool unify(Machine *m, i32 u, i32 v){
    if(u == v) return true;
    if(H[u] == R){
        if(H[v] == R){
            if(u>v){
                H_(u, R).r = v; trail(m, u); return true;
            } else{
                H_(v, R).r = u; trail(m, v); return true;
            }
        } else if(check(m, u, v)){
            H_(u, R).r = v; trail(m, u); return true;
        } else return false;
    }
    if(H[v] == R){
        if(check(m, v, u)){
            H_(v, R).r = u; trail(m, v); return true;
        } else return false;
    }
    if(H[u] == A && H[v] == A){
        return H_(v, A).a == H_(u, A).a;
    }
    if(H[u] == C && H[v] == C){
        if(H_(u, C).sym != H_(v, C).sym) return false;
        if(H_(u, C).len != H_(v, C).len) return false;
        for (isz i = 0; i < H_(u, C).len; i++) {
            if(!unify(m, H_(u, C).data[i], H_(v, C).data[i])) return false;
        }
        return true;
    }
    return false;
}

void backtrack(Machine *m){
    FP = BP; m->heap.beg = (u8*)(m->heap_base + HPold); // HP = HPold
    reset(m, TPold, TP);
    TP = TPold; PC = negCont;
}

int step(Machine *m, Instr i){
    switch (i.op) {
        case PUTATOM:{
            HC_A *a = new(&m->heap, HC_A);
            a->tag = A;
            a->a = i.arg;
            S[++SP] = HEAPREL(a);
        } break;
        case PUTVAR:{
            HC_R *ref = new(&m->heap, HC_R);
            ref->tag = R;
            ref->r = 0;
            S[++SP] = HEAPREL(ref);
            S[FP + i.arg] = HEAPREL(ref);
        } break;
        case PUTREF:{
            SP++;
            S[SP] = deref(m, S[FP + i.arg]);
        } break;
        case PUTANON:{
            HC_R *ref = new(&m->heap, HC_R);
            ref->tag = R;
            ref->r = 0;
            S[++SP] = HEAPREL(ref);
        } break;
        case PUTSTRUCT:{
            // TODO is i.f big enough?
            HC_C *s = new(&m->heap, HC_C); new(&m->heap, i32, i.n);
            s->tag = C;
            s->sym = i.f;
            s->len = i.n;
            SP -= i.n-1;
            memcpy(s->data, &S[SP], sizeof(i32[i.n]));
            S[SP] = HEAPREL(s);
        } break;
        case MARK:{
            SP += 6;
            S[SP] = i.arg;
            S[SP-1] = FP;
            //TODO
        } break;
        // TODO: assembler needs to generate 2 instructions for "call p/n"
        //       and emit fail if predicate does not exist
        case CALL:{
            FP = SP - i.arg;
            // rest done by jump instruction
        } break;
        case FAIL:{
            backtrack(m);
        } break;
        case SETBTP:{
            HPold = (Tag*)m->heap.beg - m->heap_base;
            TPold = TP;
            BPold = BP;
            BP = FP;
        } break;
        case BIND:{
            HC_R *ref = &H_(S[SP-1], R);
            ref->r = S[SP];
            trail(m, S[SP-1]);
            SP -= 2;
        } break;
        case UNIFY:{
            if(unify(m, S[SP-1], S[SP])) SP -= 2;
            else backtrack(m);
        } break;
        case UATOM:{
            i32 v = S[SP]; SP--;
            switch(H[v]){
                case A:{
                    if(i.arg == H_(v, A).a) break;
                    backtrack(m);
                } break;
                case R:{
                    HC_A *a = new(&m->heap, HC_A);
                    a->tag = A;
                    a->a = i.arg;
                    H_(v, R).r = HEAPREL(a);
                    trail(m, v);
                } break;
                case C:
                    backtrack(m);
                break;
            }
        } break;
        case UVAR: {
            S[FP+i.arg] = S[SP]; SP--;
        } break;
        case USTRUCT:{
            switch (H[S[SP]]) {
                case C:{
                    HC_C c = H_(S[SP], C);
                    if(c.sym == i.f && c.len == i.n) break;
                    backtrack(m);
                } break;
                case R:{

                } break;
                case A:{
                    backtrack(m);
                } break;
            }
            // RJUMP does rest
        } break;
        case RJUMP:{
            if(H[S[SP]] == R) PC = i.arg;
        } break;
        case POP:{
            SP--;
        } break;
        case UREF:{
            if(unify(m, S[SP], deref(m, S[FP + i.arg]))) SP--;
            else backtrack(m);
        } break;
        case CHECK:{
            if(!check(m, S[SP], deref(m, S[FP + i.arg]))) backtrack(m);
        } break;
        case SON:{
            S[SP+1] = deref(m, H_(S[SP], C).data[i.arg-1]); SP++;
        } break;
        case UP:{
            SP--; PC = i.arg;
        } break;

        case NOT:
        case NEG:
        case ADD:
        case SUB:
        case MUL:
        case DIV:
        case EQ:
        case LESS:
        case LESSEQ:
        case MOD:
        case AND:
        case OR:
        case XOR:
        case BAND:
        case BOR:
        case BXOR:
            printf("not implemented op: %s\n", Opcode2str(i.op)); exit(1);
        case PUSHENV:{
            SP = FP + i.arg;
        } break;
        case POPENV:{
            if(FP > BP) SP = FP - 6;
            PC = posCont;
            FP = FPold;
        } break;
        case DELBTP:{
            BP = BPold;
        } break;
        case TRY:{
            negCont = PC;
            PC = i.arg;
        } break;
        case JUMP:{
            PC = i.arg;
        } break;
        case INIT:{
            BP = FP = SP = 5;
            S[0] = i.arg;
            S[1] = S[2] = -1;
            S[3] = 0;
            BP = FP;
        } break;
        case HALT:{
            isz n = i.arg;
            printf("instr count: %d\n", m->count);
            for (isz i = 1; i <= n; i++) {
                printf("%ld: ", i);
                pprintV(m, S[FP+i]);
                puts("");
            }
            if(n == 0) puts("true");
            int c = getc(stdin);
            if(c == EOF) return 1;
            printf("; ");
            backtrack(m);
        } break;
        case NO:{
            puts("false");
            return 1;
        } break;
        case PRUNE:{
            BP = BPold;
        } break;
    }
    return 0;
}

void dumpI(Instr i){
    printf("%s %d", Opcode2str(i.op), i.arg);
}

void run(Machine *m){
    for(;;){
        #ifdef STEPDEBUG
        printf("%d: ", m->pc);
        #endif

        Instr i = m->code[PC];
        PC++;
        if(step(m, i)) return;
        m->count++;


        #ifdef STEPDEBUG
        dumpI(i);
        puts("");
        printf("stack:");
        for (word *i = m->data; i <= m->sp + m->data; i++) printf(" %d", *i);
        puts("");
        printf("fp: %d\n", m->fp);
        printf("sp: %d\n", m->sp);
        printf("hp: %ld\n", (Tag*)m->heap.beg - m->heap_base);
        printf("heap\n");
        for (isz i = 0; i < 200 /*hack, but this is a debug feature*/; i++) {
            Tag* t = &m->heap_base[i];
            switch (*t) {
                case R:{
                    printf("%ld: ", i);
                    printf("R(%d)\n", H_(i, R).r);
                } break;
                case A:{
                    printf("%ld: ", i);
                    i32 a = H_(i, A).a;
                    printf("%s\n", m->strTab + a);
                } break;
                case C:{
                    printf("%ld: ", i);
                    HC_C* c = &H_(i, C);
                    printf("%s(", m->strTab + c->sym);
                    for (isz i = 0; i < c->len; i++) {
                        if(i > 0) printf(", ");
                        printf("%d", c->data[i]);
                    }
                    printf(")\n");

                } break;
            }
        }
        getc(stdin);
        #endif
    }
}



void* mmap_file(char* path, size_t *size);

word stack[0x10000];

int main(int argc, char *argv[]) {

    char* progFile = 0;
    bool shouldRun = 0;
    bool shouldDump = 0;

    OptGetSpec opts[] = {
        {0, 0, "[-d] [-r] <file>", ogp_fail, 0},
        {0, "<file>", "bytecode file", ogp_id, &progFile},
        {'r', "run", "run the program (set by default)", 0, &shouldRun},
        {'d', "dump-instructions", "only dump the program", 0, &shouldDump},
    };
    bool r = optget(opts);
    if(!shouldDump) shouldRun = 1;

    if(!r || !progFile){
        __optget_print_help(argc, argv, (sizeof opts / sizeof opts[0]), opts);
        return 1;
    }


    size_t size;
    Instr *code = mmap_file(progFile, &size);
    isz strTabOff = *(i32*)code;
    char* strTab = (char*)code + *(i32*)code;
    code++;

    size_t len = (strTabOff / sizeof(Instr)) - 1;




    if(shouldDump){
        for (size_t i = 0; i < len; i++) {
            printf("% 3zd: ", i);
            dumpI(code[i]);puts("");
        }
    }

    memset(stack, 0, sizeof(stack));


    Machine m;
    m.code = code;
    m.pc = 0;
    m.data = stack;
    m.sp = -1;
    m.fp = -1;
    m.tp = -1;
    m.bp = 0;
    m.heap = arena_mmap(0x10000);
    m.trail = arena_mmap(0x10000);
    m.heap_base = (Tag*)m.heap.beg;
    m.trail_base = (i32*)m.trail.beg;
    m.strTab = strTab;

    m.count = 0;

    // put dummy at address 0, so we can use it as sentinel
    new(&m.heap, Tag);

    if(shouldRun) run(&m);

    return 0;
}
