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
    usize disp_idx;
    i32 pos;
} X86_64_Label_Patches;

typedef struct {
    sv symbol_name;
    i32 pos;
} X86_64_Relocation_Patch;

typedef struct {
    usize data_offset;
    i32 pos;
} X86_64_Data_Patch;

typedef struct {
    String_Builder ops;

    Dynamic_Array(X86_64_Label) labels;
    Dynamic_Array(X86_64_Label_Patches) label_patches;

    Dynamic_Array(X86_64_Relocation_Patch) rela_patches;

    Dynamic_Array(X86_64_Data_Patch) data_patches;
} X86_64_Ctx;

static X86_64_Ctx x86_64_global_ctx;

void push_x86_patch(usize label_id, usize disp_idx, i32 pos) {
    X86_64_Ctx *ctx = &x86_64_global_ctx;
    for (usize i = 0; i < ctx->labels.size; ++i) {
        X86_64_Label *label = ctx->labels.store + i;

        if (label->label_id == label_id) {
            *(i32 *)(ctx->ops.store + disp_idx) = label->pos - pos;
            return;
        }
    }

    da_append(&ctx->label_patches, ((X86_64_Label_Patches){label_id, disp_idx, pos}));
}

void push_x86_label(usize label_id, i32 pos) {
    X86_64_Ctx *ctx = &x86_64_global_ctx;
    da_append(&ctx->labels, ((X86_64_Label){label_id, pos}));

    for (usize i = 0; i < ctx->label_patches.size; ++i) {
        X86_64_Label_Patches *patch = ctx->label_patches.store + i;

        if (patch->label_id == label_id) {
            *(i32 *)(ctx->ops.store + patch->disp_idx) = pos - patch->pos;
            da_remove(&ctx->label_patches, i);
            --i;
        }
    }
}

INLINE void push_x86_rela_patch(sv symbol_name, i32 pos) {
    X86_64_Ctx *ctx = &x86_64_global_ctx;
    da_append(&ctx->rela_patches, ((X86_64_Relocation_Patch){symbol_name, pos}));
}

INLINE void push_x86_data_patch(usize data_offset, i32 pos) {
    X86_64_Ctx *ctx = &x86_64_global_ctx;
    da_append(&ctx->data_patches, ((X86_64_Data_Patch){data_offset, pos}));
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

#define ADD_BYTE 0x00
#define ADD 0x01

#define SUB_BYTE 0x28
#define SUB 0x29
#define SUB_IMM 0x81
#define SUB_IMM_BYTE 0x83

#define MUL_BYTE 0xF6
#define MUL 0xF7

#define AND_BYTE 0x20
#define AND 0x21

#define OR_BYTE 0x08
#define OR 0x09

#define XOR_BYTE 0x30
#define XOR 0x31

#define SHL_BYTE 0xD2
#define SHL 0xD3

#define POP_REG(reg) (0x58 + (reg & 7))
#define POP_MEM 0x8F

#define PUSH_REG(reg) (0x50 + (reg & 7))
#define PUSH_MEM 0xFF

#define NEG_BYTE 0xF6
#define NEG 0xF7

#define CMP_BYTE 0x38
#define CMP 0x39

#define TEST 0x85
#define TEST_BYTE 0x84

#define JMP 0xE9

#define CALL_REL 0xE8
#define CALL_ABS 0xFF

#define LEAVE 0xC9
#define RET 0xC3

// the following opcodes require this prefix
#define TWO_BYTE_ESC 0x0F

#define JE 0x84

#define MOVZX 0xB6

#define SETA 0x97
#define SETAE 0x93
#define SETB 0x92
#define SETBE 0x96
#define SETE 0x94
#define SETG 0x9F
#define SETGE 0x9D
#define SETL 0x9C
#define SETLE 0x9E
#define SETNE 0x95

static u8 lookup_cmp_op[BINOP_COUNT] = {
    [BINOP_ULT] = SETB,
    [BINOP_SLT] = SETL,
    [BINOP_ULE] = SETBE,
    [BINOP_SLE] = SETLE,
    [BINOP_UGT] = SETA,
    [BINOP_SGT] = SETG,
    [BINOP_UGE] = SETAE,
    [BINOP_SGE] = SETGE,
    [BINOP_EQ] = SETE,
    [BINOP_NE] = SETNE,
};

void move_imm_to_reg(u64 value, X86_64_Register reg) {
    bool w = value > UINT32_MAX,
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
    bool w = size == 8,
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
    bool w = size == 8,
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
    bool r = from > 0x7,
         b = to > 0x7;

    push_x86_op(REX_PRE(1, r, 0, b));
    push_x86_op(MOV_MEM);
    push_x86_op(MODR_M(MOD_REG, from & 0x7, to & 0x7));
}

void load_reg_to_reg_addr(X86_64_Register from, X86_64_Register to, usize size) {
    bool w = size == 8,
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
    bool w = size == 8,
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
    bool r = reg > 0x7;

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
    case VALUE_TYPE_COUNT: UNREACHABLE();

    case VALUE_TYPE_INT_LITERAL: {
        move_imm_to_reg(v->int_value, reg);
    } break;

    case VALUE_TYPE_VAR: {
        move_stack_to_reg(v->stack_index, v->type->size, reg);
    } break;

    case VALUE_TYPE_FUNC:
    case VALUE_TYPE_EXTERN_FUNC: {
        bool r = reg > 0x7;
        push_x86_op(REX_PRE(1, r, 0, 0));
        push_x86_op(LEA);
        push_x86_op(MODR_M(MOD_MEM, reg, 5));
        push_x86_rela_patch(v->name, x86_pos());
        push_u32(0);
    } break;

    case VALUE_TYPE_DEREF: {
        move_stack_to_reg(v->stack_index, 8, RAX);
        load_reg_addr_to_reg(RAX, reg, v->type->size);
    } break;

    case VALUE_TYPE_DATA_OFFSET: UNIMPLEMENTED();
    case VALUE_TYPE_INIT_LIST: UNIMPLEMENTED();
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
    bool w = size == 8,
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

void generate_binop(const Op *binop) {
    usize size = binop->lhs->type->size;
    bool is64 = size == 8,
         is16 = size == 2,
         is8 = size == 1;

    load_value_to_reg(binop->lhs, RAX);
    load_value_to_reg(binop->rhs, RCX);

    if (is16)
        push_x86_op(WORD_PRE());
    if (is64)
        push_x86_op(REX_PRE(1, 0, 0, 0));

    switch (binop->op) {
    case BINOP_COUNT: UNREACHABLE();

    case BINOP_ADD: {
        push_x86_op(is8 ? ADD_BYTE : ADD);
        push_x86_op(MODR_M(MOD_REG, RCX, RAX));
    } break;

    case BINOP_SUB: {
        push_x86_op(is8 ? SUB_BYTE : SUB);
        push_x86_op(MODR_M(MOD_REG, RCX, RAX));
    } break;

    case BINOP_MUL: {
        push_x86_op(is8 ? MUL_BYTE : MUL);
        push_x86_op(MODR_M(MOD_REG, 4, RCX));
    } break;

    case BINOP_IMUL: {
        push_x86_op(is8 ? MUL_BYTE : MUL);
        push_x86_op(MODR_M(MOD_REG, 5, RCX));
    } break;

    case BINOP_DIV: {
        push_x86_op(is8 ? MUL_BYTE : MUL);
        push_x86_op(MODR_M(MOD_REG, 6, RCX));
    } break;

    case BINOP_IDIV: {
        push_x86_op(is8 ? MUL_BYTE : MUL);
        push_x86_op(MODR_M(MOD_REG, 7, RCX));
    } break;

    case BINOP_MOD: {
        push_x86_op(is8 ? MUL_BYTE : MUL);
        push_x86_op(MODR_M(MOD_REG, 6, RCX));
        move_reg_to_reg(RDX, RAX);
    } break;

    case BINOP_IMOD: {
        push_x86_op(is8 ? MUL_BYTE : MUL);
        push_x86_op(MODR_M(MOD_REG, 7, RCX));
        move_reg_to_reg(RDX, RAX);
    } break;

    case BINOP_ULT:
    case BINOP_SLT:
    case BINOP_ULE:
    case BINOP_SLE:
    case BINOP_UGT:
    case BINOP_SGT:
    case BINOP_UGE:
    case BINOP_SGE:
    case BINOP_EQ:
    case BINOP_NE: {
        push_x86_op(is8 ? CMP_BYTE : CMP);
        push_x86_op(MODR_M(MOD_REG, RCX, RAX));

        push_x86_op(TWO_BYTE_ESC);
        u8 op = lookup_cmp_op[binop->op];
        assert(op);
        push_x86_op(op);
        push_x86_op(MODR_M(MOD_REG, 0, RAX));

        if (!is8) {
            if (is16)
                push_x86_op(WORD_PRE());
            if (is64)
                push_x86_op(REX_PRE(1, 0, 0, 0));

            push_x86_op(TWO_BYTE_ESC);
            push_x86_op(MOVZX);
            push_x86_op(MODR_M(MOD_REG, RAX, RAX));
        }
    } break;

    case BINOP_AND: {
        push_x86_op(is8 ? AND_BYTE : AND);
        push_x86_op(MODR_M(MOD_REG, RCX, RAX));
    } break;

    case BINOP_OR: {
        push_x86_op(is8 ? OR_BYTE : OR);
        push_x86_op(MODR_M(MOD_REG, RCX, RAX));
    } break;

    case BINOP_XOR: {
        push_x86_op(is8 ? XOR_BYTE : XOR);
        push_x86_op(MODR_M(MOD_REG, RCX, RAX));
    } break;

    case BINOP_SHL:
    case BINOP_ASHR:
    case BINOP_LSHR: {
        push_x86_op(is8 ? SHL_BYTE : SHL);
        // clang-format off
        u8 reg = binop->op == BINOP_SHL     ? 4
               : binop->op == BINOP_ASHR    ? 7
              /* binop->op == BINOP_ASHL */ : 5;
        // clang-format on
        push_x86_op(MODR_M(MOD_REG, reg, RAX));
    } break;
    }

    store_reg_to_stack(RAX, binop->result, size);
}

void generate_op(const Op *op) {
    const Value *val = op->val;

    switch (op->type) {
    case OP_TYPE_COUNT: UNREACHABLE();

    case OP_TYPE_ASSIGN: {
        load_value_to_reg(val, RAX);
        store_reg_to_stack(RAX, op->result, val->type->size);
    } break;

    case OP_TYPE_STORE: {
        load_value_to_reg(val, RAX);
        move_stack_to_reg(op->result, 8, RCX);
        load_reg_to_reg_addr(RAX, RCX, val->type->size);
    } break;

    case OP_TYPE_NEG:
    case OP_TYPE_BNOT: {
        usize size = val->type->size;
        bool is64 = size == 8,
             is16 = size == 2,
             is8 = size == 1;

        load_value_to_reg(val, RAX);

        if (is16)
            push_x86_op(WORD_PRE());
        if (is64)
            push_x86_op(REX_PRE(1, 0, 0, 0));

        push_x86_op(is8 ? NEG_BYTE : NEG);
        push_x86_op(MODR_M(MOD_REG, op->type == OP_TYPE_BNOT ? 2 : 3, RAX));
        store_reg_to_stack(RAX, op->result, val->type->size);
    } break;

    case OP_TYPE_LNOT: {
        usize size = val->type->size;
        bool is64 = size == 8,
             is16 = size == 2,
             is8 = size == 1;

        load_value_to_reg(val, RAX);

        if (is16)
            push_x86_op(WORD_PRE());
        if (is64)
            push_x86_op(REX_PRE(1, 0, 0, 0));

        push_x86_op(is8 ? TEST_BYTE : TEST);
        push_x86_op(MODR_M(MOD_REG, RAX, RAX));

        push_x86_op(TWO_BYTE_ESC);
        push_x86_op(SETE);
        push_x86_op(MODR_M(MOD_REG, 0, RAX));

        if (!is8) {
            if (is16)
                push_x86_op(WORD_PRE());
            if (is64)
                push_x86_op(REX_PRE(1, 0, 0, 0));

            push_x86_op(TWO_BYTE_ESC);
            push_x86_op(MOVZX);
            push_x86_op(MODR_M(MOD_REG, RAX, RAX));
        }

        store_reg_to_stack(RAX, op->result, val->type->size);
    } break;

    case OP_TYPE_REF: {
        if (val->value_type == VALUE_TYPE_DATA_OFFSET) {
            push_x86_op(REX_PRE(1, 0, 0, 0));
            push_x86_op(LEA);
            push_x86_op(MODR_M(MOD_MEM, RAX, 5));
            push_x86_data_patch(val->offset, x86_pos());
            push_u32(0);
        } else {
            load_effective_address(val->stack_index, val->type->size, RAX);
        }
        store_reg_to_stack(RAX, op->result, 8);
    } break;

    case OP_TYPE_LABEL: {
        i32 pos = x86_pos();
        push_x86_label(op->label_id, pos);
    } break;

    case OP_TYPE_JMP: {
        push_x86_op(JMP);
        usize pos = x86_pos();
        push_u32(0);
        push_x86_patch(op->label_id, pos, x86_pos());
    } break;

    case OP_TYPE_JMPZ: {
        load_value_to_reg(val, RAX);
        test_registers(RAX, RAX, val->type->size);

        push_x86_op(TWO_BYTE_ESC);
        push_x86_op(JE);
        usize pos = x86_pos();
        push_u32(0);
        push_x86_patch(op->label_id, pos, x86_pos());
    } break;

    case OP_TYPE_BINOP: {
        generate_binop(op);
    } break;

    case OP_TYPE_RET: {
        load_value_to_reg(val, RAX);
        push_x86_op(LEAVE);
        push_x86_op(RET);
    } break;

    case OP_TYPE_CALL: {
        if (op->params.size > 6)
            UNIMPLEMENTED();

        for (usize i = 0; i < op->params.size; ++i) {
            const Value *param = *da_at(&op->params, i);
            load_value_to_reg(param, x86_64_linux_registers[i]);
        }

        if (op->func->value_type == VALUE_TYPE_FUNC ||
            op->func->value_type == VALUE_TYPE_EXTERN_FUNC) {
            push_x86_op(CALL_REL);
            push_x86_rela_patch(op->func->name, x86_pos());
            push_u32(0);
        } else {
            load_value_to_reg(op->func, RAX);
            push_x86_op(CALL_ABS);
            push_x86_op(MODR_M(MOD_MEM, 2, RAX));
        }

        store_reg_to_stack(RAX, op->result, op->func->type->ret->size);
    } break;
    }
}

void generate_func(const Value *func) {
    if (func->params->len > 6) UNIMPLEMENTED();
    push_x86_op(PUSH_REG(RBP));
    move_reg_to_reg(RSP, RBP);

    if (func->stack_size > 0)
        alloc_rsp(func->stack_size);

    for (usize i = 0; i < func->params->len; ++i) {
        const Func_Param *param = get_param_const(func, i);
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

    String_Hash_Table symbols = {0};
    sht_init(&symbols, sizeof(usize), 0);

    ELF_Builder elf_builder;
    ELF_Builder_init(&elf_builder, c->l->input_file);

    ELF_add_data(&elf_builder, c->data.store, c->data.size);

    for (usize i = 0; i < c->extern_funcs.size; ++i) {
        const Value *func = c->extern_funcs.store[i];
        usize symbol_idx = ELF_add_external_func(&elf_builder, func->name);
        *(usize *)sht_get(&symbols, SV_SPREAD(func->name)) = symbol_idx;
    }

    usize op_offset = 0;
    for (usize i = 0; i < c->funcs.size; ++i) {
        const Value *func = c->funcs.store[i];
        generate_func(func);

        usize symbol_idx = ELF_new_func(&elf_builder, func->name, ctx->ops.store + op_offset, ctx->ops.size - op_offset);
        op_offset = ctx->ops.size;

        *(usize *)sht_get(&symbols, SV_SPREAD(func->name)) = symbol_idx;
        assert(ctx->label_patches.size == 0);
    }

    for (usize i = 0; i < ctx->data_patches.size; ++i) {
        X86_64_Data_Patch *patch = da_at(&ctx->data_patches, i);
        ELF_add_data_reloc(&elf_builder, patch->pos, patch->data_offset);
    }

    for (usize i = 0; i < ctx->rela_patches.size; ++i) {
        const X86_64_Relocation_Patch *rela_patch = da_at(&ctx->rela_patches, i);
        usize *symbol_idx = sht_try_get(&symbols, SV_SPREAD(rela_patch->symbol_name));
        assert(symbol_idx);
        ELF_add_relocation(&elf_builder, rela_patch->pos, *symbol_idx);
    }

    ELF_Builder_compile(&elf_builder);
    ELF_write_o_file(&elf_builder, out);

    ELF_Builder_delete(&elf_builder);

    sht_free(&symbols);

    sb_free(&ctx->ops);
    da_delete(&ctx->labels);
    da_delete(&ctx->label_patches);
}
