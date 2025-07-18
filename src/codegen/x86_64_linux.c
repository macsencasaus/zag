#ifndef ZAG_C
#define X86_64_LINUX_C
#include "../zag.c"
#endif

#include "elfbuilder.c"

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

void x86_64_push_label_patch(usize label_id, usize disp_idx, i32 pos) {
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

void x86_64_push_label(usize label_id, i32 pos) {
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

INLINE void x86_64_push_rela_patch(sv symbol_name, i32 pos) {
    X86_64_Ctx *ctx = &x86_64_global_ctx;
    da_append(&ctx->rela_patches, ((X86_64_Relocation_Patch){symbol_name, pos}));
}

INLINE void x86_64_push_data_patch(usize data_offset, i32 pos) {
    X86_64_Ctx *ctx = &x86_64_global_ctx;
    da_append(&ctx->data_patches, ((X86_64_Data_Patch){data_offset, pos}));
}

INLINE i32 x86_64_pos(void) {
    return x86_64_global_ctx.ops.size;
}

INLINE char *x86_64_push_op(u8 op) {
    char *r = x86_64_global_ctx.ops.store + x86_64_global_ctx.ops.size;
    sb_append(&x86_64_global_ctx.ops, op);
    return r;
}

INLINE char *x86_64_push_multi_op(const void *ops, usize n) {
    char *r = x86_64_global_ctx.ops.store + x86_64_global_ctx.ops.size;
    sb_append_buf(&x86_64_global_ctx.ops, ops, n);
    return r;
}

INLINE char *x86_64_push_u32(u32 v) {
    char *r = x86_64_global_ctx.ops.store + x86_64_global_ctx.ops.size;
    sb_append_buf(&x86_64_global_ctx.ops, (char *)&v, sizeof(v));
    return r;
}

typedef enum {
    X86_64_RAX,
    X86_64_RCX,
    X86_64_RDX,
    X86_64_RBX,
    X86_64_RSP,
    X86_64_RBP,
    X86_64_RSI,
    X86_64_RDI,
    X86_64_R8,
    X86_64_R9,
    X86_64_R10,
    X86_64_R11,
    X86_64_R12,
    X86_64_R13,
    X86_64_R14,
    X86_64_R15,
} X86_64_Register;

static X86_64_Register x86_64_linux_registers[] = {X86_64_RDI, X86_64_RSI, X86_64_RDX, X86_64_RCX, X86_64_R8, X86_64_R9};

#define X86_64_WORD_PRE() 0x66

// REX Prefix byte
typedef struct {
    u8 b : 1;
    u8 x : 1;
    u8 r : 1;
    u8 w : 1;
    u8 pre : 4;
} REX_Prefix;

#define X86_64_REX_PRE(w, r, x, b) ((union {REX_Prefix p; u8 v; }){.p = {(b), (x), (r), (w), 0x4}}).v

// ModR/M byte
typedef struct {
    u8 rm : 3;
    u8 reg : 3;
    u8 mod : 2;
} ModRM;

#define X86_64_MOD_REG_RM(mod, reg, rm) ((union {ModRM m; u8 v; }){.m = {(rm), (reg), (mod)}}).v

// Mod addressing modes
#define X86_64_MOD_MEM 0
#define X86_64_MOD_MEM_BYTE 1
#define X86_64_MOD_MEM_DWORD 2
#define X86_64_MOD_REG 3

// Opcodes
#define X86_64_MOV_REG 0x8B
#define X86_64_MOV_REG_BYTE 0x8B
#define X86_64_MOV_MEM 0x89
#define X86_64_MOV_MEM_BYTE 0x88

#define X86_64_MOV_IMM(reg) (0xB8 + (reg & 7))

#define X86_64_X86_64_X86_64_LEA 0x8D

#define X86_64_ADD_BYTE 0x00
#define X86_64_ADD 0x01

#define X86_64_SUB_BYTE 0x28
#define X86_64_SUB 0x29
#define X86_64_SUB_IMM 0x81
#define X86_64_SUB_IMM_BYTE 0x83

#define X86_64_MUL_BYTE 0xF6
#define X86_64_MUL 0xF7

#define X86_64_AND_BYTE 0x20
#define X86_64_AND 0x21

#define X86_64_OR_BYTE 0x08
#define X86_64_OR 0x09

#define X86_64_XOR_BYTE 0x30
#define X86_64_XOR 0x31

#define X86_64_SHL_BYTE 0xD2
#define X86_64_SHL 0xD3

#define X86_64_POP_REG(reg) (0x58 + (reg & 7))
#define X86_64_POP_MEM 0x8F

#define X86_64_PUSH_REG(reg) (0x50 + (reg & 7))
#define X86_64_PUSH_MEM 0xFF

#define X86_64_NEG_BYTE 0xF6
#define X86_64_NEG 0xF7

#define X86_64_CMP_BYTE 0x38
#define X86_64_CMP 0x39

#define X86_64_TEST 0x85
#define X86_64_TEST_BYTE 0x84

#define X86_64_JMP 0xE9

#define X86_64_CALL_REL 0xE8
#define X86_64_CALL_ABS 0xFF

#define X86_64_LEAVE 0xC9
#define RET 0xC3

// the following opcodes require this prefix
#define TWO_BYTE_ESC 0x0F

#define JE 0x84

#define MOVZX 0xB6

#define X86_64_SETA 0x97
#define X86_64_SETAE 0x93
#define X86_64_SETB 0x92
#define X86_64_SETBE 0x96
#define X86_64_SETE 0x94
#define X86_64_SETG 0x9F
#define X86_64_SETGE 0x9D
#define X86_64_SETL 0x9C
#define X86_64_SETLE 0x9E
#define X86_64_SETNE 0x95

static u8 x86_64_lookup_cmp_op[BINOP_COUNT] = {
    [BINOP_ULT] = X86_64_SETB,
    [BINOP_SLT] = X86_64_SETL,
    [BINOP_ULE] = X86_64_SETBE,
    [BINOP_SLE] = X86_64_SETLE,
    [BINOP_UGT] = X86_64_SETA,
    [BINOP_SGT] = X86_64_SETG,
    [BINOP_UGE] = X86_64_SETAE,
    [BINOP_SGE] = X86_64_SETGE,
    [BINOP_EQ] = X86_64_SETE,
    [BINOP_NE] = X86_64_SETNE,
};

void x86_64_move_imm_to_reg(u64 value, X86_64_Register reg) {
    bool w = value > UINT32_MAX,
         b = reg > 0x7;

    if (w || b)
        x86_64_push_op(X86_64_REX_PRE(w, 0, 0, b));

    x86_64_push_op(X86_64_MOV_IMM(reg));
    if (w) {
        x86_64_push_multi_op(&value, sizeof(value));
    } else {
        i32 imm = (i32)value;
        x86_64_push_multi_op(&imm, sizeof(imm));
    }
}

void x86_64_move_stack_to_reg(usize stack_index, usize size, X86_64_Register reg) {
    bool w = size == 8,
         r = reg > 0x7;

    if (size == 2)
        x86_64_push_op(X86_64_WORD_PRE());

    if (w || r)
        x86_64_push_op(X86_64_REX_PRE(w, r, 0, 0));

    u8 mov_code = size == 1 ? X86_64_MOV_REG_BYTE : X86_64_MOV_REG;
    x86_64_push_op(mov_code);

    usize disp = stack_index + size;

    if (disp <= UINT8_MAX) {
        u8 imm = -disp;
        x86_64_push_op(X86_64_MOD_REG_RM(X86_64_MOD_MEM_BYTE, reg, X86_64_RBP));
        x86_64_push_op(imm);
    } else {
        u32 imm = -disp;
        x86_64_push_op(X86_64_MOD_REG_RM(X86_64_MOD_MEM_DWORD, reg, X86_64_RBP));
        x86_64_push_multi_op(&imm, sizeof(imm));
    }
}

void x86_64_store_reg_to_stack(X86_64_Register reg, usize stack_index, usize size) {
    bool w = size == 8,
         r = reg > 0x7;

    if (size == 2)
        x86_64_push_op(X86_64_WORD_PRE());

    if (w || r)
        x86_64_push_op(X86_64_REX_PRE(w, r, 0, 0));

    u8 mov_code = size == 1 ? X86_64_MOV_MEM_BYTE : X86_64_MOV_MEM;

    usize disp = stack_index + size;
    if (disp <= UINT8_MAX) {
        u8 imm = -disp;
        x86_64_push_op(mov_code);
        x86_64_push_op(X86_64_MOD_REG_RM(X86_64_MOD_MEM_BYTE, reg, X86_64_RBP));
        x86_64_push_op(imm);
    } else {
        u32 imm = -disp;
        x86_64_push_op(mov_code);
        x86_64_push_op(X86_64_MOD_REG_RM(X86_64_MOD_MEM_DWORD, reg, X86_64_RBP));
        x86_64_push_multi_op(&imm, sizeof(imm));
    }
}

void x86_64_move_reg_to_reg(X86_64_Register from, X86_64_Register to) {
    bool r = from > 0x7,
         b = to > 0x7;

    x86_64_push_op(X86_64_REX_PRE(1, r, 0, b));
    x86_64_push_op(X86_64_MOV_MEM);
    x86_64_push_op(X86_64_MOD_REG_RM(X86_64_MOD_REG, from & 0x7, to & 0x7));
}

void x86_64_load_reg_to_reg_addr(X86_64_Register from, X86_64_Register to, usize size) {
    bool w = size == 8,
         r = from > 0x7,
         b = to > 0x7;

    if (size == 2)
        x86_64_push_op(X86_64_WORD_PRE());

    if (w || r || b)
        x86_64_push_op(X86_64_REX_PRE(w, r, 0, b));

    if (size == 1)
        x86_64_push_op(X86_64_MOV_MEM_BYTE);
    else
        x86_64_push_op(X86_64_MOV_MEM);

    x86_64_push_op(X86_64_MOD_REG_RM(X86_64_MOD_MEM, from & 0x7, to & 0x7));
}

void x86_64_load_reg_addr_to_reg(X86_64_Register from, X86_64_Register to, usize size) {
    bool w = size == 8,
         r = to > 0x7,
         b = from > 0x7;

    if (size == 2)
        x86_64_push_op(X86_64_WORD_PRE());

    if (w || r || b)
        x86_64_push_op(X86_64_REX_PRE(w, r, 0, b));

    if (size == 1)
        x86_64_push_op(X86_64_MOV_REG_BYTE);
    else
        x86_64_push_op(X86_64_MOV_REG);

    x86_64_push_op(X86_64_MOD_REG_RM(X86_64_MOD_MEM, to & 0x7, from & 0x7));
}

void x86_64_load_effective_address(usize stack_index, usize size, X86_64_Register reg) {
    bool r = reg > 0x7;

    x86_64_push_op(X86_64_REX_PRE(1, r, 0, 0));
    x86_64_push_op(X86_64_X86_64_X86_64_LEA);

    usize disp = stack_index + size;

    if (disp <= UINT8_MAX) {
        u8 imm = -disp;
        x86_64_push_op(X86_64_MOD_REG_RM(X86_64_MOD_MEM_BYTE, reg, X86_64_RBP));
        x86_64_push_op(imm);
    } else {
        u32 imm = -disp;
        x86_64_push_op(X86_64_MOD_REG_RM(X86_64_MOD_MEM_DWORD, reg, X86_64_RBP));
        x86_64_push_multi_op(&imm, sizeof(imm));
    }
}

void x86_64_load_value_to_reg(const Value *v, X86_64_Register reg) {
    if (v->type->kind == TYPE_KIND_ARRAY)
        UNIMPLEMENTED();
    switch (v->value_type) {
    case VALUE_TYPE_COUNT: UNREACHABLE();

    case VALUE_TYPE_INT_LITERAL: {
        x86_64_move_imm_to_reg(v->int_value, reg);
    } break;

    case VALUE_TYPE_VAR: {
        x86_64_move_stack_to_reg(v->stack_index, v->type->size, reg);
    } break;

    case VALUE_TYPE_FUNC:
    case VALUE_TYPE_EXTERN_FUNC: {
        bool r = reg > 0x7;
        x86_64_push_op(X86_64_REX_PRE(1, r, 0, 0));
        x86_64_push_op(X86_64_X86_64_X86_64_LEA);
        x86_64_push_op(X86_64_MOD_REG_RM(X86_64_MOD_MEM, reg, 5));
        x86_64_push_rela_patch(v->name, x86_64_pos());
        x86_64_push_u32(0);
    } break;

    case VALUE_TYPE_DEREF: {
        x86_64_move_stack_to_reg(v->stack_index, 8, X86_64_RAX);
        x86_64_load_reg_addr_to_reg(X86_64_RAX, reg, v->type->size);
    } break;

    case VALUE_TYPE_DATA_OFFSET: {
        usize size = v->type->size;
        bool w = size == 8,
             r = reg > 0x7;

        if (size == 2)
            x86_64_push_op(X86_64_WORD_PRE());

        if (w || r)
            x86_64_push_op(X86_64_REX_PRE(w, r, 0, 0));

        if (size == 1)
            x86_64_push_op(X86_64_MOV_REG_BYTE);
        else
            x86_64_push_op(X86_64_MOV_REG);

        x86_64_push_op(X86_64_MOD_REG_RM(X86_64_MOD_MEM, reg & 0x7, 5));
        x86_64_push_data_patch(v->offset, x86_64_pos());
        x86_64_push_u32(0);
    } break;

    case VALUE_TYPE_INIT_LIST: UNREACHABLE();
    }
}
void x86_64_alloc_rsp(usize stack_size) {
    stack_size += (16 - (stack_size % 16)) % 16;  // 16 byte alignment

    x86_64_push_op(X86_64_REX_PRE(1, 0, 0, 0));
    if (stack_size <= 255) {
        u8 imm = (u8)stack_size;
        x86_64_push_op(X86_64_SUB_IMM_BYTE);
        x86_64_push_op(X86_64_MOD_REG_RM(X86_64_MOD_REG, 5, X86_64_RSP));
        x86_64_push_op(imm);
    } else {
        ZAG_ASSERT(stack_size < (usize)(u32)-1);
        u32 imm = (u32)stack_size;
        x86_64_push_op(X86_64_SUB_IMM);
        x86_64_push_op(X86_64_MOD_REG_RM(X86_64_MOD_REG, 5, X86_64_RSP));
        x86_64_push_multi_op(&imm, sizeof(imm));
    }
}

void x86_64_test_registers(X86_64_Register reg1, X86_64_Register reg2, usize size) {
    bool w = size == 8,
         r = reg1 > 0x7,
         b = reg2 > 0x7;

    if (size == 2)
        x86_64_push_op(X86_64_WORD_PRE());

    if (w || r || b)
        x86_64_push_op(X86_64_REX_PRE(w, r, 0, b));

    if (size == 1)
        x86_64_push_op(X86_64_TEST_BYTE);
    else
        x86_64_push_op(X86_64_TEST);

    x86_64_push_op(X86_64_MOD_REG_RM(X86_64_MOD_REG, reg1 & 0x7, reg2 & 0x7));
}

void x86_64_generate_binop(const Op *binop) {
    usize size = binop->lhs->type->size;
    bool is64 = size == 8,
         is16 = size == 2,
         is8 = size == 1;

    x86_64_load_value_to_reg(binop->lhs, X86_64_RAX);
    x86_64_load_value_to_reg(binop->rhs, X86_64_RCX);

    if (is16)
        x86_64_push_op(X86_64_WORD_PRE());
    if (is64)
        x86_64_push_op(X86_64_REX_PRE(1, 0, 0, 0));

    switch (binop->op) {
    case BINOP_COUNT: UNREACHABLE();

    case BINOP_ADD: {
        x86_64_push_op(is8 ? X86_64_ADD_BYTE : X86_64_ADD);
        x86_64_push_op(X86_64_MOD_REG_RM(X86_64_MOD_REG, X86_64_RCX, X86_64_RAX));
    } break;

    case BINOP_SUB: {
        x86_64_push_op(is8 ? X86_64_SUB_BYTE : X86_64_SUB);
        x86_64_push_op(X86_64_MOD_REG_RM(X86_64_MOD_REG, X86_64_RCX, X86_64_RAX));
    } break;

    case BINOP_MUL: {
        x86_64_push_op(is8 ? X86_64_MUL_BYTE : X86_64_MUL);
        x86_64_push_op(X86_64_MOD_REG_RM(X86_64_MOD_REG, 4, X86_64_RCX));
    } break;

    case BINOP_IMUL: {
        x86_64_push_op(is8 ? X86_64_MUL_BYTE : X86_64_MUL);
        x86_64_push_op(X86_64_MOD_REG_RM(X86_64_MOD_REG, 5, X86_64_RCX));
    } break;

    case BINOP_DIV: {
        x86_64_push_op(is8 ? X86_64_MUL_BYTE : X86_64_MUL);
        x86_64_push_op(X86_64_MOD_REG_RM(X86_64_MOD_REG, 6, X86_64_RCX));
    } break;

    case BINOP_IDIV: {
        x86_64_push_op(is8 ? X86_64_MUL_BYTE : X86_64_MUL);
        x86_64_push_op(X86_64_MOD_REG_RM(X86_64_MOD_REG, 7, X86_64_RCX));
    } break;

    case BINOP_MOD: {
        x86_64_push_op(is8 ? X86_64_MUL_BYTE : X86_64_MUL);
        x86_64_push_op(X86_64_MOD_REG_RM(X86_64_MOD_REG, 6, X86_64_RCX));
        x86_64_move_reg_to_reg(X86_64_RDX, X86_64_RAX);
    } break;

    case BINOP_IMOD: {
        x86_64_push_op(is8 ? X86_64_MUL_BYTE : X86_64_MUL);
        x86_64_push_op(X86_64_MOD_REG_RM(X86_64_MOD_REG, 7, X86_64_RCX));
        x86_64_move_reg_to_reg(X86_64_RDX, X86_64_RAX);
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
        x86_64_push_op(is8 ? X86_64_CMP_BYTE : X86_64_CMP);
        x86_64_push_op(X86_64_MOD_REG_RM(X86_64_MOD_REG, X86_64_RCX, X86_64_RAX));

        x86_64_push_op(TWO_BYTE_ESC);
        u8 op = x86_64_lookup_cmp_op[binop->op];
        ZAG_ASSERT(op);
        x86_64_push_op(op);
        x86_64_push_op(X86_64_MOD_REG_RM(X86_64_MOD_REG, 0, X86_64_RAX));

        if (!is8) {
            if (is16)
                x86_64_push_op(X86_64_WORD_PRE());
            if (is64)
                x86_64_push_op(X86_64_REX_PRE(1, 0, 0, 0));

            x86_64_push_op(TWO_BYTE_ESC);
            x86_64_push_op(MOVZX);
            x86_64_push_op(X86_64_MOD_REG_RM(X86_64_MOD_REG, X86_64_RAX, X86_64_RAX));
        }
    } break;

    case BINOP_AND: {
        x86_64_push_op(is8 ? X86_64_AND_BYTE : X86_64_AND);
        x86_64_push_op(X86_64_MOD_REG_RM(X86_64_MOD_REG, X86_64_RCX, X86_64_RAX));
    } break;

    case BINOP_OR: {
        x86_64_push_op(is8 ? X86_64_OR_BYTE : X86_64_OR);
        x86_64_push_op(X86_64_MOD_REG_RM(X86_64_MOD_REG, X86_64_RCX, X86_64_RAX));
    } break;

    case BINOP_XOR: {
        x86_64_push_op(is8 ? X86_64_XOR_BYTE : X86_64_XOR);
        x86_64_push_op(X86_64_MOD_REG_RM(X86_64_MOD_REG, X86_64_RCX, X86_64_RAX));
    } break;

    case BINOP_SHL:
    case BINOP_ASHR:
    case BINOP_LSHR: {
        x86_64_push_op(is8 ? X86_64_SHL_BYTE : X86_64_SHL);
        // clang-format off
        u8 reg = binop->op == BINOP_SHL     ? 4
               : binop->op == BINOP_ASHR    ? 7
              /* binop->op == BINOP_ASHL */ : 5;
        // clang-format on
        x86_64_push_op(X86_64_MOD_REG_RM(X86_64_MOD_REG, reg, X86_64_RAX));
    } break;
    }

    x86_64_store_reg_to_stack(X86_64_RAX, binop->result, size);
}

void x86_64_generate_op(const Op *op) {
    const Value *val = op->val;

    switch (op->type) {
    case OP_TYPE_COUNT: UNREACHABLE();

    case OP_TYPE_ASSIGN: {
        x86_64_load_value_to_reg(val, X86_64_RAX);
        (void)op->result;
        (void)val->type->size;
        x86_64_store_reg_to_stack(X86_64_RAX, op->result, val->type->size);
    } break;

    case OP_TYPE_STORE: {
        x86_64_load_value_to_reg(val, X86_64_RAX);
        x86_64_move_stack_to_reg(op->result, 8, X86_64_RCX);
        x86_64_load_reg_to_reg_addr(X86_64_RAX, X86_64_RCX, val->type->size);
    } break;

    case OP_TYPE_NEG:
    case OP_TYPE_BNOT: {
        usize size = val->type->size;
        bool is64 = size == 8,
             is16 = size == 2,
             is8 = size == 1;

        x86_64_load_value_to_reg(val, X86_64_RAX);

        if (is16)
            x86_64_push_op(X86_64_WORD_PRE());
        if (is64)
            x86_64_push_op(X86_64_REX_PRE(1, 0, 0, 0));

        x86_64_push_op(is8 ? X86_64_NEG_BYTE : X86_64_NEG);
        x86_64_push_op(X86_64_MOD_REG_RM(X86_64_MOD_REG, op->type == OP_TYPE_BNOT ? 2 : 3, X86_64_RAX));
        x86_64_store_reg_to_stack(X86_64_RAX, op->result, val->type->size);
    } break;

    case OP_TYPE_LNOT: {
        usize size = val->type->size;
        bool is64 = size == 8,
             is16 = size == 2,
             is8 = size == 1;

        x86_64_load_value_to_reg(val, X86_64_RAX);

        if (is16)
            x86_64_push_op(X86_64_WORD_PRE());
        if (is64)
            x86_64_push_op(X86_64_REX_PRE(1, 0, 0, 0));

        x86_64_push_op(is8 ? X86_64_TEST_BYTE : X86_64_TEST);
        x86_64_push_op(X86_64_MOD_REG_RM(X86_64_MOD_REG, X86_64_RAX, X86_64_RAX));

        x86_64_push_op(TWO_BYTE_ESC);
        x86_64_push_op(X86_64_SETE);
        x86_64_push_op(X86_64_MOD_REG_RM(X86_64_MOD_REG, 0, X86_64_RAX));

        if (!is8) {
            if (is16)
                x86_64_push_op(X86_64_WORD_PRE());
            if (is64)
                x86_64_push_op(X86_64_REX_PRE(1, 0, 0, 0));

            x86_64_push_op(TWO_BYTE_ESC);
            x86_64_push_op(MOVZX);
            x86_64_push_op(X86_64_MOD_REG_RM(X86_64_MOD_REG, X86_64_RAX, X86_64_RAX));
        }

        x86_64_store_reg_to_stack(X86_64_RAX, op->result, val->type->size);
    } break;

    case OP_TYPE_REF: {
        if (val->value_type == VALUE_TYPE_DATA_OFFSET) {
            x86_64_push_op(X86_64_REX_PRE(1, 0, 0, 0));
            x86_64_push_op(X86_64_X86_64_X86_64_LEA);
            x86_64_push_op(X86_64_MOD_REG_RM(X86_64_MOD_MEM, X86_64_RAX, 5));
            x86_64_push_data_patch(val->offset, x86_64_pos());
            x86_64_push_u32(0);
        } else {
            x86_64_load_effective_address(val->stack_index, val->type->size, X86_64_RAX);
        }
        x86_64_store_reg_to_stack(X86_64_RAX, op->result, 8);
    } break;

    case OP_TYPE_LABEL: {
        i32 pos = x86_64_pos();
        x86_64_push_label(op->label_id, pos);
    } break;

    case OP_TYPE_JMP: {
        x86_64_push_op(X86_64_JMP);
        usize pos = x86_64_pos();
        x86_64_push_u32(0);
        x86_64_push_label_patch(op->label_id, pos, x86_64_pos());
    } break;

    case OP_TYPE_JMPZ: {
        x86_64_load_value_to_reg(val, X86_64_RAX);
        x86_64_test_registers(X86_64_RAX, X86_64_RAX, val->type->size);

        x86_64_push_op(TWO_BYTE_ESC);
        x86_64_push_op(JE);
        usize pos = x86_64_pos();
        x86_64_push_u32(0);
        x86_64_push_label_patch(op->label_id, pos, x86_64_pos());
    } break;

    case OP_TYPE_BINOP: {
        x86_64_generate_binop(op);
    } break;

    case OP_TYPE_RET: {
        x86_64_load_value_to_reg(val, X86_64_RAX);
        x86_64_push_op(X86_64_LEAVE);
        x86_64_push_op(RET);
    } break;

    case OP_TYPE_CALL: {
        if (op->params->len > 6)
            UNIMPLEMENTED();

        for (usize i = 0; i < op->params->len; ++i) {
            const Value *param = *at(op->params, i);
            x86_64_load_value_to_reg(param, x86_64_linux_registers[i]);
        }

        if (op->func->value_type == VALUE_TYPE_FUNC ||
            op->func->value_type == VALUE_TYPE_EXTERN_FUNC) {
            x86_64_push_op(X86_64_CALL_REL);
            x86_64_push_rela_patch(op->func->name, x86_64_pos());
            x86_64_push_u32(0);
        } else {
            x86_64_load_value_to_reg(op->func, X86_64_RAX);
            x86_64_push_op(X86_64_CALL_ABS);
            x86_64_push_op(X86_64_MOD_REG_RM(X86_64_MOD_MEM, 2, X86_64_RAX));
        }

        x86_64_store_reg_to_stack(X86_64_RAX, op->result, op->func->type->ret->size);
    } break;
    }
}

void x86_64_generate_func(const Value *func) {
    if (func->params->len > 6) UNIMPLEMENTED();
    x86_64_push_op(X86_64_PUSH_REG(X86_64_RBP));
    x86_64_move_reg_to_reg(X86_64_RSP, X86_64_RBP);

    if (func->stack_size > 0)
        x86_64_alloc_rsp(func->stack_size);

    for (usize i = 0; i < func->params->len; ++i) {
        const Func_Param *param = get_param_const(func, i);
        const Type *param_type = get_param_type(func, i);
        x86_64_store_reg_to_stack(x86_64_linux_registers[i], param->index, param_type->size);
    }

    for (usize i = 0; i < func->ops.size; ++i) {
        const Op *op = func->ops.store[i];
        x86_64_generate_op(op);
    }
}

void x86_64_generate_program(const Compiler *c, FILE *out) {
    X86_64_Ctx *ctx = &x86_64_global_ctx;
    *ctx = (X86_64_Ctx){0};

    String_Hash_Table symbols = {0};
    sht_init(&symbols, sizeof(usize), 0);

    Elf_Builder elf_builder;
    Elf_Builder_init(&elf_builder, c->l->input_file);

    Elf_add_data(&elf_builder, c->data.store, c->data.size);

    for (usize i = 0; i < c->extern_funcs.size; ++i) {
        const Value *func = c->extern_funcs.store[i];
        usize symbol_idx = Elf_add_external_func(&elf_builder, func->name);
        *(usize *)sht_get(&symbols, SV_SPREAD(func->name)) = symbol_idx;
    }

    usize op_offset = 0;
    for (usize i = 0; i < c->funcs.size; ++i) {
        const Value *func = c->funcs.store[i];
        x86_64_generate_func(func);

        usize symbol_idx = Elf_new_func(&elf_builder, func->name, ctx->ops.store + op_offset, ctx->ops.size - op_offset);
        op_offset = ctx->ops.size;

        *(usize *)sht_get(&symbols, SV_SPREAD(func->name)) = symbol_idx;
        ZAG_ASSERT(ctx->label_patches.size == 0);
    }

    for (usize i = 0; i < ctx->data_patches.size; ++i) {
        X86_64_Data_Patch *patch = da_at(&ctx->data_patches, i);
        Elf_add_data_reloc(&elf_builder, patch->pos, patch->data_offset);
    }

    for (usize i = 0; i < ctx->rela_patches.size; ++i) {
        const X86_64_Relocation_Patch *rela_patch = da_at(&ctx->rela_patches, i);
        usize *symbol_idx = sht_try_get(&symbols, SV_SPREAD(rela_patch->symbol_name));
        ZAG_ASSERT(symbol_idx);
        Elf_add_relocation(&elf_builder, rela_patch->pos, *symbol_idx);
    }

    Elf_Builder_compile(&elf_builder);
    Elf_write_o_file(&elf_builder, out);

    Elf_Builder_delete(&elf_builder);

    sht_free(&symbols);

    sb_free(&ctx->ops);
    da_delete(&ctx->labels);
    da_delete(&ctx->label_patches);
    da_delete(&ctx->rela_patches);
    da_delete(&ctx->data_patches);
}
