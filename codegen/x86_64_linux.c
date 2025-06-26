#ifndef VECTOR_C
#define X86_64_LINUX_C
#include "../vector.c"
#endif

#include "elf.c"

static String_Builder x64_ops;

typedef enum {
    RAX,
    RCX,
    RDX,
    RBX,
    RSP,
    RBP,
    RSI,
    RDI,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
} X86_64_Register;

// REX Prefix byte
typedef struct {
    u8 b : 1;
    u8 x : 1;
    u8 r : 1;
    u8 w : 1;
    u8 pre : 4;
} REX_Prefix;

#define REX_PRE(w, r, x, b) ((union {REX_Prefix p; u8 v; }){.p = {(b), (x), (r), (w), 0x4}}).v

// ModR/M byte
typedef struct {
    u8 rm : 3;
    u8 reg : 3;
    u8 mod : 2;
} ModRM;

#define MODR_M(mod, reg, rm) ((union {ModRM m; u8 v; }){.m = {(rm), (reg), (mod)}}).v

// Opcode
#define MOV_REG 0x8B
#define MOV_MEM 0x89
#define MOV_IMM(reg) (0xB8 + (reg & 7))

#define POP_REG(reg) (0x58 + (reg & 7))
#define POP_MEM 0x8F

#define PUSH_REG(reg) (0x50 + (reg & 7))
#define PUSH_MEM 0xFF

#define RET 0xC3

void load_value_to_reg(const Value *v, X86_64_Register reg) {
    switch (v->value_type) {
    case VALUE_TYPE_INT_LITERAL: {
        bool is64 = (v->int_value & 0xFFFFFFFF00000000) > 0ull;
        u8 w, b;
        if ((w = is64) || (b = (reg > 0x7))) {  // require REX prefix
            sb_append(&x64_ops, REX_PRE(w, 0, 0, b));
        }
        sb_append(&x64_ops, MOV_IMM(reg));
        if (is64) {
            sb_append_buf(&x64_ops, &v->int_value, sizeof(i64));
        } else {
            i32 imm = (i32)v->int_value;
            sb_append_buf(&x64_ops, &imm, sizeof(i32));
        }
    } break;
    default: UNIMPLEMENTED();
    }
}

void move_reg_to_reg(X86_64_Register from, X86_64_Register to) {
    u8 r = from > 0x7;
    u8 b = to > 0x7;
    sb_append(&x64_ops, REX_PRE(1, r, 0, b));
    sb_append(&x64_ops, MOV_MEM);
    sb_append(&x64_ops, MODR_M(3, from & 0x7, to & 0x7));
}

void generate_op(const Op *op) {
    switch (op->type) {
    case OP_TYPE_STORE: UNIMPLEMENTED();
    case OP_TYPE_NEG: UNIMPLEMENTED();
    case OP_TYPE_BINOP: UNIMPLEMENTED();
    case OP_TYPE_RET: {
        load_value_to_reg(&op->val, RAX);
        sb_append(&x64_ops, POP_REG(RBP));
        sb_append(&x64_ops, RET);
    } break;
    default: UNREACHABLE();
    }
}

void generate_func(const Func *func) {
    sb_append(&x64_ops, PUSH_REG(RBP));
    move_reg_to_reg(RSP, RBP);

    if (func->stack_size > 0) {
        UNIMPLEMENTED();
    }

    for (usize i = 0; i < func->ops.size; ++i) {
        const Op *op = func->ops.store + i;
        generate_op(op);
    }
}

void generate_program(const Compiler *c, FILE *out) {
    x64_ops = (String_Builder){0};

    ELF_Codegen_Ctx ctx;
    ELF_Codegen_init(&ctx, c->l->input_file);

    usize op_offset = 0;
    for (usize i = 0; i < c->funcs.size; ++i) {
        const Func *func = c->funcs.store + i;
        generate_func(func);

        String_Builder func_name = {0};
        sb_append_sv(&func_name, &func->name);
        sb_append_null(&func_name);

        ELF_new_func(&ctx, func_name.store, x64_ops.store + op_offset, x64_ops.size - op_offset);
        op_offset = x64_ops.size;

        sb_free(&func_name);
    }

    ELF_Codegen_compile(&ctx);
    ELF_write_o_file(&ctx, out);

    ELF_Codegen_delete(&ctx);
    sb_free(&x64_ops);
}
