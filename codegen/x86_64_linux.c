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

INLINE char *push_op(u8 op) {
    char *r = x86_64_global_ctx.ops.store + x86_64_global_ctx.ops.size;
    sb_append(&x86_64_global_ctx.ops, op);
    return r;
}

INLINE char *push_multi_op(const void *ops, usize n) {
    char *r = x86_64_global_ctx.ops.store + x86_64_global_ctx.ops.size;
    sb_append_buf(&x86_64_global_ctx.ops, ops, n);
    return r;
}

INLINE char *push_u32(u32 v) {
    char *r = x86_64_global_ctx.ops.store + x86_64_global_ctx.ops.size;
    sb_append_buf(&x86_64_global_ctx.ops, (char *)&v, sizeof(v));
    return r;
}

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

static X86_64_Register x86_64_linux_registers[] = {RDI, RSI, RDX, RCX, R8, R9};

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

#define ADD 0x01

#define SUB 0x29
#define SUB_IMM32 0x81
#define SUB_IMM8 0x83

#define IMUL (char[]){0x0F, 0xAF}

#define IDIV 0xF7

#define AND 0x21

#define XOR 0x31

#define OR 0x09

#define POP_REG(reg) (0x58 + (reg & 7))
#define POP_MEM 0x8F

#define PUSH_REG(reg) (0x50 + (reg & 7))
#define PUSH_MEM 0xFF

#define NEG 0xF7

#define CMP_IMM8 0x83
#define CMP 0x39

#define TEST 0x85

#define JMP_REL8 0xEB
#define JMP_REL32 0xE9

#define JE_REL8 0x74
#define JE_REL32 (char[]){0x0F, 0x84}

#define JNE_REL8 0x75
#define JNE_REL32 (char[]){0x0F, 0x85}

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
        bool is64 = v->type->size == 8;
        u8 w, r;
        if ((w = is64) | (r = (reg > 0x7))) {  // require REX prefix
            push_op(REX_PRE(w, r, 0, 0));
        }

        usize disp = v->stack_index + v->type->size;

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

void alloc_rsp(const Value *func) {
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
    case OP_TYPE_ASSIGN: {
        load_value_to_reg(op->val, RAX);
        store_reg_to_stack(RAX, op->result, op->val->type->size);
    } break;
    case OP_TYPE_NEG: {
        load_value_to_reg(op->val, RAX);
        if (op->val->type->size == 8)
            push_op(REX_PRE(1, 0, 0, 0));
        push_op(NEG);
        push_op(MODR_M(MOD_REG, 3, RAX));
        store_reg_to_stack(RAX, op->result, op->val->type->size);
    } break;
    case OP_TYPE_LNOT: {
        load_value_to_reg(op->val, RAX);
        if (op->val->type->size == 8)
            push_op(REX_PRE(1, 0, 0, 0));
        push_op(CMP_IMM8);
    } break;
    case OP_TYPE_BINOP: {
        usize size = op->lhs->type->size;
        bool is64 = size == 8;
        bool is_signed = op->lhs->type->kind == TYPE_KIND_INT_SIGNED;

        load_value_to_reg(op->lhs, RAX);
        load_value_to_reg(op->rhs, RCX);
        switch (op->op) {
        case TOKEN_TYPE_PLUS: {
            if (is64)
                push_op(REX_PRE(1, 0, 0, 0));
            push_op(ADD);
            push_op(MODR_M(MOD_REG, RCX, RAX));
        } break;
        case TOKEN_TYPE_MINUS: {
            if (is64)
                push_op(REX_PRE(1, 0, 0, 0));
            push_op(SUB);
            push_op(MODR_M(MOD_REG, RCX, RAX));
        } break;
        case TOKEN_TYPE_ASTERISK: {
            if (is64)
                push_op(REX_PRE(1, 0, 0, 0));
            if (is_signed) {
                push_multi_op(IMUL, sizeof(IMUL));
                push_op(MODR_M(MOD_REG, RAX, RCX));
            } else
                UNIMPLEMENTED();
        } break;
        case TOKEN_TYPE_SLASH: {
            if (is64)
                push_op(REX_PRE(1, 0, 0, 0));
            if (is_signed) {
                push_op(IDIV);
                push_op(MODR_M(MOD_REG, 7, RCX));
            } else
                UNIMPLEMENTED();
        } break;
        case TOKEN_TYPE_PERCENT: {
            if (is64)
                push_op(REX_PRE(1, 0, 0, 0));
            if (is_signed) {
                push_op(IDIV);
                push_op(MODR_M(MOD_REG, 7, RCX));
                move_reg_to_reg(RDX, RAX);
            } else
                UNIMPLEMENTED();
        } break;
        case TOKEN_TYPE_XOR: {
            if (is64)
                push_op(REX_PRE(1, 0, 0, 0));
            push_op(XOR);
            push_op(MODR_M(MOD_REG, RCX, RAX));
        } break;
        case TOKEN_TYPE_BAND: {
            if (is64)
                push_op(REX_PRE(1, 0, 0, 0));
            push_op(AND);
            push_op(MODR_M(MOD_REG, RCX, RAX));
        } break;
        case TOKEN_TYPE_BOR: {
            if (is64)
                push_op(REX_PRE(1, 0, 0, 0));
            push_op(OR);
            push_op(MODR_M(MOD_REG, RCX, RAX));
        } break;
        default: UNREACHABLE();
        }
        store_reg_to_stack(RAX, op->result, size);
    } break;
    case OP_TYPE_RET: {
        load_value_to_reg(op->val, RAX);
        push_op(LEAVE);
        push_op(RET);
    } break;
    default: UNREACHABLE();
    }
}

void generate_func(const Value *func) {
    if (func->params.size > 6) UNIMPLEMENTED();
    push_op(PUSH_REG(RBP));
    move_reg_to_reg(RSP, RBP);

    if (func->stack_size > 0) {
        alloc_rsp(func);
    }

    for (usize i = 0; i < func->params.size; ++i) {
        const Func_Param *param = get_param(func, i);
        const Type *param_type = get_param_type(func, i);
        store_reg_to_stack(x86_64_linux_registers[i], param->index, param_type->size);
    }

    for (usize i = 0; i < func->ops.size; ++i) {
        const Op *op = func->ops.store[i];
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
        const Value *func = c->funcs.store[i];
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
