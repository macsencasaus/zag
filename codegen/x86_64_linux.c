#ifndef VECTOR_C
#define X86_64_LINUX_C
#include "../vector.c"
#endif

#include "elf.c"

// TODO: add register allocator
typedef struct {
    String_Builder ops;
} X86_64_Ctx;

static X86_64_Ctx x86_64_global_ctx;

#define push_op(op) sb_append(&x86_64_global_ctx.ops, (op))
#define push_multi_op(op, n) sb_append_buf(&x86_64_global_ctx.ops, (op), (n))

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

// Mod addressing modes
#define MOD_MEM 0
#define MOD_MEM8 1
#define MOD_MEM32 2
#define MOD_REG 3

// Opcodes
#define MOV_REG 0x8B
#define MOV_MEM 0x89
#define MOV_IMM(reg) (0xB8 + (reg & 7))
#define MOV_IMM8 0xC6
#define MOV_IMM32 0xC7

#define SUB_IMM32 0x81
#define SUB_IMM8 0x83

#define XOR 0x31

#define POP_REG(reg) (0x58 + (reg & 7))
#define POP_MEM 0x8F

#define PUSH_REG(reg) (0x50 + (reg & 7))
#define PUSH_MEM 0xFF

#define LEAVE 0xC9
#define RET 0xC3

void load_value_to_reg(const Value *v, X86_64_Register reg) {
    switch (v->value_type) {
    case VALUE_TYPE_INT_LITERAL: {
        bool is64 = (v->int_value & 0xFFFFFFFF00000000) > 0ull;
        u8 w, b;
        if ((w = is64) | (b = (reg > 0x7))) {  // require REX prefix
            push_op(REX_PRE(w, 0, 0, b));
        }
        push_op(MOV_IMM(reg));
        if (is64) {
            push_multi_op(&v->int_value, sizeof(i64));
        } else {
            i32 imm = (i32)v->int_value;
            push_multi_op(&imm, sizeof(i32));
        }
    } break;
    case VALUE_TYPE_VAR: {
        bool is64 = v->var.type->size == 8;
        u8 w, r;
        if ((w = is64) | (r = (reg > 0x7))) {  // require REX prefix
            push_op(REX_PRE(w, r, 0, 0));
        }

        usize disp = v->var.stack_index + v->var.type->size;

        if (disp < 255) {
            u8 imm = -disp;
            push_op(MOV_REG);
            push_op(MODR_M(MOD_MEM8, reg, RBP));
            push_op(imm);
        } else {
            u32 imm = -disp;
            push_op(MOV_REG);
            push_op(MODR_M(MOD_MEM32, reg, RBP));
            push_multi_op(&imm, sizeof(imm));
        }
    } break;
    default: UNIMPLEMENTED();
    }
}

void store_reg_to_stack(X86_64_Register reg, usize stack_index, usize size) {
    bool is64 = size == 8;
    u8 w, r;
    if ((w = is64) | (r = (reg > 0x7))) {  // require REX prefix
        push_op(REX_PRE(w, r, 0, 0));
    }

    usize disp = stack_index + size;

    if (disp < 255) {
        u8 imm = -disp;
        push_op(MOV_MEM);
        push_op(MODR_M(MOD_MEM8, reg, RBP));
        push_op(imm);
    } else {
        u32 imm = -disp;
        push_op(MOV_MEM);
        push_op(MODR_M(MOD_MEM32, reg, RBP));
        push_multi_op(&imm, sizeof(imm));
    }
}

void move_reg_to_reg(X86_64_Register from, X86_64_Register to) {
    u8 r = from > 0x7;
    u8 b = to > 0x7;
    push_op(REX_PRE(1, r, 0, b));
    push_op(MOV_MEM);
    push_op(MODR_M(MOD_REG, from & 0x7, to & 0x7));
}

void alloc_rsp(const Func *func) {
    usize stack_size = func->stack_size;
    stack_size += (16 - (stack_size % 16)) % 16;  // 16 byte alignment

    push_op(REX_PRE(1, 0, 0, 0));
    if (stack_size <= 255) {
        u8 imm = (u8)stack_size;
        push_op(SUB_IMM8);
        push_op(MODR_M(MOD_REG, 5, RSP));
        push_op(imm);
    } else {
        assert(stack_size < (usize)(u32)-1);
        u32 imm = (u32)stack_size;
        push_op(SUB_IMM32);
        push_op(MODR_M(MOD_REG, 5, RSP));
        push_multi_op(&imm, sizeof(imm));
    }
}

void zero_reg(X86_64_Register reg) {
    u8 r = reg > 0x7;
    push_op(REX_PRE(1, r, 0, r));
    push_op(XOR);
    push_op(MODR_M(MOD_REG, reg, reg));
}

void generate_op(const Op *op) {
    switch (op->type) {
    case OP_TYPE_STORE: {
        load_value_to_reg(&op->val, RAX);
        store_reg_to_stack(RAX, op->index, op->val.type->size);
    } break;
    case OP_TYPE_NEG: UNIMPLEMENTED();
    case OP_TYPE_BINOP: UNIMPLEMENTED();
    case OP_TYPE_RET: {
        load_value_to_reg(&op->val, RAX);
        push_op(LEAVE);
        push_op(RET);
    } break;
    default: UNREACHABLE();
    }
}

void generate_func(const Func *func) {
    push_op(PUSH_REG(RBP));
    move_reg_to_reg(RSP, RBP);

    if (func->stack_size > 0) {
        alloc_rsp(func);
    }

    for (usize i = 0; i < func->ops.size; ++i) {
        const Op *op = func->ops.store + i;
        generate_op(op);
    }
}

void generate_program(const Compiler *c, FILE *out) {
    X86_64_Ctx *ctx = &x86_64_global_ctx;
    *ctx = (X86_64_Ctx){0};

    ELF_Builder elf_builder;
    ELF_Builder_init(&elf_builder, c->l->input_file);

    usize op_offset = 0;
    for (usize i = 0; i < c->funcs.size; ++i) {
        const Func *func = c->funcs.store + i;
        generate_func(func);

        String_Builder func_name = {0};
        sb_append_sv(&func_name, &func->name);
        sb_append_null(&func_name);

        ELF_new_func(&elf_builder, func_name.store, ctx->ops.store + op_offset, ctx->ops.size - op_offset);
        op_offset = ctx->ops.size;

        sb_free(&func_name);
    }

    ELF_Builder_compile(&elf_builder);
    ELF_write_o_file(&elf_builder, out);

    ELF_Builder_delete(&elf_builder);
    sb_free(&ctx->ops);
}
