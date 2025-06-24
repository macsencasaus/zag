#ifndef VECTOR_C
#define IR_C
#include "../vector.c"
#endif

void print_ir_val(const Value *val, FILE *out) {
    fprintf(out, "%s ", val->type->name);
    switch (val->value_type) {
    case VALUE_TYPE_INT_LITERAL: {
        fprintf(out, "%ld", val->int_value);
    } break;
    case VALUE_TYPE_VAR: {
        fprintf(out, "%%s[%zu]", val->var.stack_index);
    } break;
    default: UNIMPLEMENTED();
    }
}

void print_ir_op(const Op *op, FILE *out) {
    switch (op->type) {
    case OP_TYPE_STORE: {
        fprintf(out, "    %%s[%zu] = ", op->index);
        print_ir_val(&op->val, out);
        fprintf(out, "\n");
    } break;
    case OP_TYPE_RET: {
        fprintf(out, "    ret ");
        print_ir_val(&op->val, out);
        fprintf(out, "\n");
    } break;
    case OP_TYPE_NEG: {
        fprintf(out, "    %%s[%zu] = neg ", op->index);
        print_ir_val(&op->val, out);
        fprintf(out, "\n");
    } break;
    default: UNIMPLEMENTED();
    }
}

void print_ir_func(const Func *func, FILE *out) {
    fprintf(out, "%.*s(%zu,%zu):\n", SV_FMT(&func->name), func->params.size, func->stack_size);
    for (usize i = 0; i < func->ops.size; ++i) {
        const Op *op = func->ops.store + i;
        print_ir_op(op, out);
    }
    fprintf(out, "\n");
}

void print_ir_funcs(const Compiler *c, FILE *out) {
    for (usize i = 0; i < c->funcs.size; ++i) {
        const Func *func = c->funcs.store + i;
        print_ir_func(func, out);
    }
}

void print_description(FILE *out) {
    fprintf(out,
            "; V3 Readable Intermediate Representation\n"
            "; Functions contain two fields:\n"
            "; - The number of parameters\n"
            "; - Maximum stack size (in bytes)\n"
            "\n");
}

void print_ir_program(const Compiler *c, FILE *out) {
    print_description(out);
    print_ir_funcs(c, out);
}
