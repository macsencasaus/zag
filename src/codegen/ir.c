#ifndef ZAG_C
#define IR_C
#include "../zag.c"
#endif

#include <ctype.h>

char *get_ir_type_name(const Type *type) {
    if (type->kind == TYPE_KIND_PTR ||
        type->kind == TYPE_KIND_FN ||
        type->kind == TYPE_KIND_ARRAY)
        return "ptr";

    switch (type->size) {
    case 1: return "i8";
    case 2: return "i16";
    case 4: return "i32";
    case 8: return "i64";
    }

    UNIMPLEMENTED();
}

void print_ir_val(const Value *val, FILE *out) {
    if (val->type->kind == TYPE_KIND_ARRAY)
        fprintf(out, "%s ", get_ir_type_name(val->type->internal));
    else
        fprintf(out, "%s ", get_ir_type_name(val->type));
    switch (val->value_type) {
    case VALUE_TYPE_COUNT: UNREACHABLE();
    case VALUE_TYPE_INT_LITERAL: {
        fprintf(out, "%ld", val->int_value);
    } break;
    case VALUE_TYPE_VAR: {
        fprintf(out, "%%s[%zu]", val->stack_index);
    } break;
    case VALUE_TYPE_EXTERN_FUNC:
    case VALUE_TYPE_FUNC: {
        fprintf(out, "%.*s", SV_FMT(val->name));
    } break;
    case VALUE_TYPE_DEREF: {
        fprintf(out, "deref %%s[%zu]", val->stack_index);
    } break;
    case VALUE_TYPE_DATA_OFFSET: {
        fprintf(out, "%%data[%zu]", val->offset);
    } break;
    case VALUE_TYPE_INIT_LIST: UNIMPLEMENTED();
    }
}

static char *binop_inst_lookup[BINOP_COUNT] = {
    [BINOP_ADD] = "add",
    [BINOP_SUB] = "sub",
    [BINOP_IMUL] = "imul",
    [BINOP_MUL] = "mul",
    [BINOP_IDIV] = "idiv",
    [BINOP_DIV] = "div",
    [BINOP_IMOD] = "imod",
    [BINOP_MOD] = "mod",
    [BINOP_SLT] = "slt",
    [BINOP_ULT] = "ult",
    [BINOP_SLE] = "sle",
    [BINOP_ULE] = "ule",
    [BINOP_SGT] = "sgt",
    [BINOP_UGT] = "ugt",
    [BINOP_SGE] = "sge",
    [BINOP_UGE] = "uge",
    [BINOP_EQ] = "eq",
    [BINOP_NE] = "ne",
    [BINOP_AND] = "and",
    [BINOP_OR] = "or",
    [BINOP_XOR] = "xor",
    [BINOP_SHL] = "shl",
    [BINOP_ASHR] = "ashr",
    [BINOP_LSHR] = "lshr",
};

void print_ir_op(const Op *op, FILE *out) {
    switch (op->type) {
    case OP_TYPE_COUNT: UNREACHABLE();
    case OP_TYPE_ASSIGN: {
        fprintf(out, "    %%s[%zu] = ", op->result);
        print_ir_val(op->val, out);
    } break;
    case OP_TYPE_STORE: {
        fprintf(out, "    store [%%s[%zu]], ", op->result);
        print_ir_val(op->val, out);
    } break;
    case OP_TYPE_RET: {
        fprintf(out, "    ret ");
        print_ir_val(op->val, out);
    } break;
    case OP_TYPE_REF: {
        fprintf(out, "    %%s[%zu] = ref ", op->result);
        print_ir_val(op->val, out);
    } break;
    case OP_TYPE_NEG: {
        fprintf(out, "    %%s[%zu] = neg ", op->result);
        print_ir_val(op->val, out);
    } break;
    case OP_TYPE_BNOT: {
        fprintf(out, "    %%s[%zu] = bnot ", op->result);
        print_ir_val(op->val, out);
    } break;
    case OP_TYPE_LNOT: {
        fprintf(out, "    %%s[%zu] = lnot ", op->result);
        print_ir_val(op->val, out);
    } break;
    case OP_TYPE_BINOP: {
        const char *op_lit = binop_inst_lookup[op->op];
        if (!op_lit) UNIMPLEMENTED();
        fprintf(out, "    %%s[%zu] = %s ", op->result, op_lit);
        print_ir_val(op->lhs, out);
        fprintf(out, ", ");
        print_ir_val(op->rhs, out);
    } break;
    case OP_TYPE_LABEL: {
        fprintf(out, ".L%zu:", op->label_id);
    } break;
    case OP_TYPE_JMP: {
        fprintf(out, "    jmp .L%zu", op->label_id);
    } break;
    case OP_TYPE_JMPZ: {
        fprintf(out, "    jmpz .L%zu, ", op->label_id);
        print_ir_val(op->val, out);
    } break;
    case OP_TYPE_CALL: {
        const Type *return_type = op->func->type->ret;
        if (op->func->value_type == VALUE_TYPE_FUNC ||
            op->func->value_type == VALUE_TYPE_EXTERN_FUNC) {
            fprintf(out, "    %%s[%zu] = %s call @%.*s(",
                    op->result, get_ir_type_name(return_type), SV_FMT(op->func->name));
        } else {
            fprintf(out, "    %%s[%zu] = %s call [", op->result, get_ir_type_name(return_type));
            print_ir_val(op->func, out);
            fprintf(out, "](");
        }
        for (usize i = 0; i < op->params.size; ++i) {
            const Value *param = op->params.store[i];
            print_ir_val(param, out);

            if (i < op->params.size - 1)
                fprintf(out, ", ");
        }
        fprintf(out, ")");
    } break;
    }
    fprintf(out, "\n");
}

void print_ir_func(const Value *func, FILE *out) {
    fprintf(out, "@%.*s[%zu](", SV_FMT(func->name), func->stack_size);
    for (usize i = 0; i < func->params->len; ++i) {
        fprintf(out, "%s", get_ir_type_name(get_param_type(func, i)));

        if (i < func->params->len - 1)
            fprintf(out, ", ");
    }
    fprintf(out, ") %s:\n", get_ir_type_name(func->type->ret));

    for (usize i = 0; i < func->ops.size; ++i) {
        const Op *op = func->ops.store[i];
        print_ir_op(op, out);
    }
    fprintf(out, "\n");
}

void print_ir_funcs(const Compiler *c, FILE *out) {
    for (usize i = 0; i < c->funcs.size; ++i) {
        const Value *func = c->funcs.store[i];
        print_ir_func(func, out);
    }
}

void print_ir_external_funcs(const Compiler *c, FILE *out) {
    for (usize i = 0; i < c->extern_funcs.size; ++i) {
        const Value *func = c->extern_funcs.store[i];
        fprintf(out, "extern @%.*s(", SV_FMT(func->name));
        for (usize i = 0; i < func->params->len; ++i) {
            fprintf(out, "%s", get_ir_type_name(get_param_type(func, i)));

            if (i < func->params->len - 1)
                fprintf(out, ", ");
        }
        fprintf(out, ") %s\n", get_ir_type_name(func->type->ret));
    }
    fprintf(out, "\n");
}

void print_readable_char(FILE *out, const char *buf, u32 idx) {
    char ch = buf[idx];
    if (isprint(ch) && !isspace(ch))
        fprintf(out, "%c", ch);
    else
        fprintf(out, ".");
}

void print_ir_data(const Compiler *c, FILE *out) {
    const u32 data_line_length = 12;
    const u32 target_readable_col = 50;

    fprintf(out, "\nData:\n");

    u32 col = 0;
    u32 n = c->data.size;
    for (u32 i = 0; i < n; ++i) {
        if (i % data_line_length == 0) {
            if (i != 0) {
                for (u32 j = 0; j < target_readable_col - col; ++j)
                    fprintf(out, " ");
                col = 0;

                for (u32 j = i - data_line_length; j < i; ++j)
                    print_readable_char(out, c->data.store, j);

                fprintf(out, "\n");
            }
            col += fprintf(out, "%3x: ", i);
        }
        col += fprintf(out, "%2X ", c->data.store[i]);
    }

    for (u32 j = 0; j < target_readable_col - col; ++j)
        fprintf(out, " ");
    u32 s = (n / data_line_length) * data_line_length;
    for (u32 j = s; j < n; ++j) {
        print_readable_char(out, c->data.store, j);
    }
    fprintf(out, "\n");
}

void print_description(FILE *out) {
    fprintf(out,
            "; Zag Readable Intermediate Representation\n"
            "; Function header formatted as:\n"
            "; @func[<max-stack-size>](<param-type>,...)\n"
            "\n");
}

void print_ir_program(const Compiler *c, FILE *out) {
    print_description(out);
    print_ir_external_funcs(c, out);
    print_ir_funcs(c, out);
    if (c->data.size > 0)
        print_ir_data(c, out);
}
