#ifndef VECTOR_C
#define IR_C
#include "../vector.c"
#endif

char *get_ir_type_name(const Type *type) {
    if (type->kind == TYPE_KIND_PTR || type->kind == TYPE_KIND_FN)
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
    fprintf(out, "%s ", get_ir_type_name(val->type));
    switch (val->value_type) {
    case VALUE_TYPE_INT_LITERAL: {
        fprintf(out, "%ld", val->int_value);
    } break;
    case VALUE_TYPE_VAR: {
        fprintf(out, "%%s[%zu]", val->stack_index);
    } break;
    case VALUE_TYPE_FUNC: {
        fprintf(out, "%.*s", SV_FMT(&val->name));
    } break;
    case VALUE_TYPE_DEREF: {
        fprintf(out, "deref %%s[%zu]", val->stack_index);
    } break;
    default: UNIMPLEMENTED();
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
        fprintf(out, "    %%s[%zu] = %s call @%.*s(",
                op->result, get_ir_type_name(op->func->type->ret), SV_FMT(&op->func->name));
        for (usize i = 0; i < op->params.size; ++i) {
            const Value *param = op->params.store[i];
            print_ir_val(param, out);

            if (i < op->params.size - 1)
                fprintf(out, ", ");
        }
        fprintf(out, ")");
    } break;
    default: UNIMPLEMENTED();
    }
    fprintf(out, "\n");
}

void print_ir_func(const Value *func, FILE *out) {
    fprintf(out, "@%.*s[%zu](", SV_FMT(&func->name), func->stack_size);
    for (usize i = 0; i < func->params.size; ++i) {
        fprintf(out, "%s", get_ir_type_name(get_param_type(func, i)));

        if (i < func->params.size - 1)
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

void print_description(FILE *out) {
    fprintf(out,
            "; V3 Readable Intermediate Representation\n"
            "; Function header formatted as:\n"
            "; @func[<max-stack-size>](<param-type>,...)\n"
            "\n");
}

void print_ir_program(const Compiler *c, FILE *out) {
    print_description(out);
    print_ir_funcs(c, out);
}
