#ifndef VECTOR_C
#define X86_64_LINUX_C
#include "../vector.c"
#endif

#include "elf.c"

typedef struct {
    usize label_id;
    i32 pos;
} X86_64_Label;

typedef struct {
    usize label_id;
    i32 *disp;
    i32 pos;
} X86_64_Label_Patches;

typedef struct {
    String_Builder ops;

    Dynamic_Array(X86_64_Label) labels;
    Dynamic_Array(X86_64_Label_Patches) patches;
} X86_64_Ctx;

static X86_64_Ctx x86_64_global_ctx;

void push_x86_patch(usize label_id, i32 *disp, i32 pos) {
    X86_64_Ctx *ctx = &x86_64_global_ctx;
    for (usize i = 0; i < ctx->labels.size; ++i) {
        X86_64_Label *label = ctx->labels.store + i;

        if (label->label_id == label_id) {
            *disp = label->pos - pos;
            return;
        }
    }

    da_append(&ctx->patches, ((X86_64_Label_Patches){label_id, disp, pos}));
}

void push_x86_label(usize label_id, i32 pos) {
    X86_64_Ctx *ctx = &x86_64_global_ctx;
    da_append(&ctx->labels, ((X86_64_Label){label_id, pos}));

    for (usize i = 0; i < ctx->patches.size; ++i) {
        X86_64_Label_Patches *patch = ctx->patches.store + i;

        if (patch->label_id == label_id) {
            *patch->disp = pos - patch->pos;
            da_remove(&ctx->patches, i);
            --i;
        }
    }
}

INLINE i32 x86_pos(void) {
    return x86_64_global_ctx.ops.size;
}

INLINE char *push_x86_op(u8 op) {
    char *r = x86_64_global_ctx.ops.store + x86_64_global_ctx.ops.size;
    sb_append(&x86_64_global_ctx.ops, op);
    return r;
}

INLINE char *push_x86_multi_op(const void *ops, usize n) {
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

#define WORD_PRE() 0x66

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
#define MOD_MEM_BYTE 1
#define MOD_MEM_DWORD 2
#define MOD_REG 3

// Opcodes
#define MOV_REG 0x8B
#define MOV_REG_BYTE 0x8a
#define MOV_MEM 0x89
#define MOV_MEM_BYTE 0x88

#define MOV_IMM(reg) (0xB8 + (reg & 7))

#define LEA 0x8D

#define ADD 0x01

#define SUB 0x29
#define SUB_IMM 0x81
#define SUB_IMM_BYTE 0x83

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
#define TEST_BYTE 0x84

#define JMP 0xE9

#define JE (char[]){0x0F, 0x84}

#define LEAVE 0xC9
#define RET 0xC3

void move_imm_to_reg(u64 value, X86_64_Register reg) {
    u8 w = value > UINT32_MAX,
       b = reg > 0x7;

    if (w || b)
        push_x86_op(REX_PRE(w, 0, 0, b));

    push_x86_op(MOV_IMM(reg));
    if (w) {
        push_x86_multi_op(&value, sizeof(value));
    } else {
        i32 imm = (i32)value;
        push_x86_multi_op(&imm, sizeof(imm));
    }
}

void move_stack_to_reg(usize stack_index, usize size, X86_64_Register reg) {
    u8 w = size == 8,
       r = reg > 0x7;

    if (size == 2)
        push_x86_op(WORD_PRE());

    if (w || r)
        push_x86_op(REX_PRE(w, r, 0, 0));

    u8 mov_code = size == 1 ? MOV_REG_BYTE : MOV_REG;
    push_x86_op(mov_code);

    usize disp = stack_index + size;

    if (disp <= UINT8_MAX) {
        u8 imm = -disp;
        push_x86_op(MODR_M(MOD_MEM_BYTE, reg, RBP));
        push_x86_op(imm);
    } else {
        u32 imm = -disp;
        push_x86_op(MODR_M(MOD_MEM_DWORD, reg, RBP));
        push_x86_multi_op(&imm, sizeof(imm));
    }
}

void store_reg_to_stack(X86_64_Register reg, usize stack_index, usize size) {
    u8 w = size == 8,
       r = reg > 0x7;

    if (size == 2)
        push_x86_op(WORD_PRE());

    if (w || r)
        push_x86_op(REX_PRE(w, r, 0, 0));

    u8 mov_code = size == 1 ? MOV_MEM_BYTE : MOV_MEM;

    usize disp = stack_index + size;
    if (disp <= UINT8_MAX) {
        u8 imm = -disp;
        push_x86_op(mov_code);
        push_x86_op(MODR_M(MOD_MEM_BYTE, reg, RBP));
        push_x86_op(imm);
    } else {
        u32 imm = -disp;
        push_x86_op(mov_code);
        push_x86_op(MODR_M(MOD_MEM_DWORD, reg, RBP));
        push_x86_multi_op(&imm, sizeof(imm));
    }
}

void move_reg_to_reg(X86_64_Register from, X86_64_Register to) {
    u8 r = from > 0x7,
       b = to > 0x7;

    push_x86_op(REX_PRE(1, r, 0, b));
    push_x86_op(MOV_MEM);
    push_x86_op(MODR_M(MOD_REG, from & 0x7, to & 0x7));
}

void load_reg_to_reg_addr(X86_64_Register from, X86_64_Register to, usize size) {
    u8 w = size == 8,
       r = from > 0x7,
       b = to > 0x7;

    if (size == 2)
        push_x86_op(WORD_PRE());

    if (w || r || b)
        push_x86_op(REX_PRE(w, r, 0, b));

    if (size == 1)
        push_x86_op(MOV_MEM_BYTE);
    else
        push_x86_op(MOV_MEM);

    push_x86_op(MODR_M(MOD_MEM, from & 0x7, to & 0x7));
}

void load_reg_addr_to_reg(X86_64_Register from, X86_64_Register to, usize size) {
    u8 w = size == 8,
       r = to > 0x7,
       b = from > 0x7;

    if (size == 2)
        push_x86_op(WORD_PRE());

    if (w || r || b)
        push_x86_op(REX_PRE(w, r, 0, b));

    if (size == 1)
        push_x86_op(MOV_REG_BYTE);
    else
        push_x86_op(MOV_REG);

    push_x86_op(MODR_M(MOD_MEM, to & 0x7, from & 0x7));
}

void load_effective_address(usize stack_index, usize size, X86_64_Register reg) {
    u8 r = reg > 0x7;

    push_x86_op(REX_PRE(1, r, 0, 0));
    push_x86_op(LEA);

    usize disp = stack_index + size;

    if (disp <= UINT8_MAX) {
        u8 imm = -disp;
        push_x86_op(MODR_M(MOD_MEM_BYTE, reg, RBP));
        push_x86_op(imm);
    } else {
        u32 imm = -disp;
        push_x86_op(MODR_M(MOD_MEM_DWORD, reg, RBP));
        push_x86_multi_op(&imm, sizeof(imm));
    }
}

void load_value_to_reg(const Value *v, X86_64_Register reg) {
    switch (v->value_type) {
    case VALUE_TYPE_INT_LITERAL: {
        move_imm_to_reg(v->int_value, reg);
    } break;

    case VALUE_TYPE_VAR: {
        move_stack_to_reg(v->stack_index, v->type->size, reg);
    } break;

    case VALUE_TYPE_DEREF: {
        move_stack_to_reg(v->stack_index, 8, RAX);
        load_reg_addr_to_reg(RAX, reg, v->type->size);
    } break;

    default: UNIMPLEMENTED();
    }
}
void alloc_rsp(usize stack_size) {
    stack_size += (16 - (stack_size % 16)) % 16;  // 16 byte alignment

    push_x86_op(REX_PRE(1, 0, 0, 0));
    if (stack_size <= 255) {
        u8 imm = (u8)stack_size;
        push_x86_op(SUB_IMM_BYTE);
        push_x86_op(MODR_M(MOD_REG, 5, RSP));
        push_x86_op(imm);
    } else {
        assert(stack_size < (usize)(u32)-1);
        u32 imm = (u32)stack_size;
        push_x86_op(SUB_IMM);
        push_x86_op(MODR_M(MOD_REG, 5, RSP));
        push_x86_multi_op(&imm, sizeof(imm));
    }
}

void test_registers(X86_64_Register reg1, X86_64_Register reg2, usize size) {
    u8 w = size == 8,
       r = reg1 > 0x7,
       b = reg2 > 0x7;

    if (size == 2)
        push_x86_op(WORD_PRE());

    if (w || r || b)
        push_x86_op(REX_PRE(w, r, 0, b));

    if (size == 1)
        push_x86_op(TEST_BYTE);
    else
        push_x86_op(TEST);

    push_x86_op(MODR_M(MOD_REG, reg1 & 0x7, reg2 & 0x7));
}

void generate_op(const Op *op) {
    X86_64_Ctx *ctx = &x86_64_global_ctx;
    const Value *val = op->val;

    switch (op->type) {
    case OP_TYPE_ASSIGN: {
        load_value_to_reg(val, RAX);
        store_reg_to_stack(RAX, op->result, val->type->size);
    } break;

    case OP_TYPE_STORE: {
        load_value_to_reg(val, RAX);
        move_stack_to_reg(op->result, 8, RCX);
        load_reg_to_reg_addr(RAX, RCX, val->type->size);
    } break;

    case OP_TYPE_NEG: {
        load_value_to_reg(val, RAX);
        if (val->type->size == 8)
            push_x86_op(REX_PRE(1, 0, 0, 0));
        push_x86_op(NEG);
        push_x86_op(MODR_M(MOD_REG, 3, RAX));
        store_reg_to_stack(RAX, op->result, val->type->size);
    } break;

    case OP_TYPE_REF: {
        load_effective_address(val->stack_index, val->type->size, RAX);
        store_reg_to_stack(RAX, op->result, 8);
    } break;

    case OP_TYPE_LABEL: {
        i32 pos = x86_pos();
        push_x86_label(op->label_id, pos);
    } break;

    case OP_TYPE_JMP: {
        push_x86_op(JMP);
        i32 *disp = (i32 *)(ctx->ops.store + ctx->ops.size);
        push_u32(0);
        push_x86_patch(op->label_id, disp, x86_pos());
    } break;

    case OP_TYPE_JMPZ: {
        load_value_to_reg(val, RAX);
        test_registers(RAX, RAX, val->type->size);

        push_x86_multi_op(JE, sizeof(JE));
        i32 *disp = (i32 *)(ctx->ops.store + ctx->ops.size);
        push_u32(0);
        push_x86_patch(op->label_id, disp, x86_pos());
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
                push_x86_op(REX_PRE(1, 0, 0, 0));
            push_x86_op(ADD);
            push_x86_op(MODR_M(MOD_REG, RCX, RAX));
        } break;

        case TOKEN_TYPE_MINUS: {
            if (is64)
                push_x86_op(REX_PRE(1, 0, 0, 0));
            push_x86_op(SUB);
            push_x86_op(MODR_M(MOD_REG, RCX, RAX));
        } break;

        case TOKEN_TYPE_ASTERISK: {
            if (is64)
                push_x86_op(REX_PRE(1, 0, 0, 0));
            if (is_signed) {
                push_x86_multi_op(IMUL, sizeof(IMUL));
                push_x86_op(MODR_M(MOD_REG, RAX, RCX));
            } else
                UNIMPLEMENTED();
        } break;

        case TOKEN_TYPE_SLASH: {
            if (is64)
                push_x86_op(REX_PRE(1, 0, 0, 0));
            if (is_signed) {
                push_x86_op(IDIV);
                push_x86_op(MODR_M(MOD_REG, 7, RCX));
            } else
                UNIMPLEMENTED();
        } break;

        case TOKEN_TYPE_PERCENT: {
            if (is64)
                push_x86_op(REX_PRE(1, 0, 0, 0));
            if (is_signed) {
                push_x86_op(IDIV);
                push_x86_op(MODR_M(MOD_REG, 7, RCX));
                move_reg_to_reg(RDX, RAX);
            } else
                UNIMPLEMENTED();
        } break;

        case TOKEN_TYPE_XOR: {
            if (is64)
                push_x86_op(REX_PRE(1, 0, 0, 0));
            push_x86_op(XOR);
            push_x86_op(MODR_M(MOD_REG, RCX, RAX));
        } break;

        case TOKEN_TYPE_BAND: {
            if (is64)
                push_x86_op(REX_PRE(1, 0, 0, 0));
            push_x86_op(AND);
            push_x86_op(MODR_M(MOD_REG, RCX, RAX));
        } break;

        case TOKEN_TYPE_BOR: {
            if (is64)
                push_x86_op(REX_PRE(1, 0, 0, 0));
            push_x86_op(OR);
            push_x86_op(MODR_M(MOD_REG, RCX, RAX));
        } break;

        default: UNREACHABLE();
        }

        store_reg_to_stack(RAX, op->result, size);
    } break;

    case OP_TYPE_RET: {
        load_value_to_reg(val, RAX);
        push_x86_op(LEAVE);
        push_x86_op(RET);
    } break;

    default: UNREACHABLE();
    }
}

void generate_func(const Value *func) {
    if (func->params.size > 6) UNIMPLEMENTED();
    push_x86_op(PUSH_REG(RBP));
    move_reg_to_reg(RSP, RBP);

    if (func->stack_size > 0)
        alloc_rsp(func->stack_size);

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

        assert(ctx->patches.size == 0);
    }

    ELF_Builder_compile(&elf_builder);
    ELF_write_o_file(&elf_builder, out);

    ELF_Builder_delete(&elf_builder);

    sb_free(&ctx->ops);
    da_delete(&ctx->labels);
    da_delete(&ctx->patches);
}
