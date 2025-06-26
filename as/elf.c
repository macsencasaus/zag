#ifndef ELF_C
#define ELF_C

#ifndef VECTOR_C
#define X86_64_LINUX_C
#include "../vector.c"
#endif

#include <elf.h>

#define NULL_SECTION_IDX 0
#define TEXT_SECTION_IDX 1
#define NOTE_GNU_STACK_SECTION_IDX 2
#define SYMTAB_SECTION_IDX 3
#define STRTAB_SECTION_IDX 4
#define SHSTRTAB_SECTION_IDX 5

usize st_append(Dynamic_Array(uint8_t) *st, const char *section_name) {
    usize off = st->size;
    usize n = strlen(section_name) + 1;
    da_append_buf(st, section_name, n);
    return off;
}

typedef struct {
    Elf64_Ehdr header;

    Dynamic_Array(uint8_t) text;

    Dynamic_Array(Elf64_Sym) symbols;

    Dynamic_Array(uint8_t) strtab;
    Dynamic_Array(uint8_t) shstrtab;

    Dynamic_Array(Elf64_Shdr) section_headers;
} ELF_Codegen_Ctx;

void ELF_Codegen_init(ELF_Codegen_Ctx *ctx, const char *filename) {
    *ctx = (ELF_Codegen_Ctx){0};

    usize null_name_off_strtab = st_append(&ctx->strtab, "");
    assert(null_name_off_strtab == 0);
    da_append(&ctx->symbols, (Elf64_Sym){0});

    usize filename_off = st_append(&ctx->strtab, filename);
    Elf64_Sym file_symbol = {
        .st_name = filename_off,
        .st_info = ELF64_ST_INFO(STB_LOCAL, STT_FILE),
        .st_other = STV_DEFAULT,
        .st_shndx = SHN_ABS,
        .st_value = 0,
        .st_size = 0,
    };
    da_append(&ctx->symbols, file_symbol);

    Elf64_Sym text_section_symbol = {
        .st_name = 0,
        .st_info = ELF64_ST_INFO(STB_LOCAL, STT_SECTION),
        .st_other = STV_DEFAULT,
        .st_shndx = TEXT_SECTION_IDX,
        .st_value = 0,
        .st_size = 0,
    };
    da_append(&ctx->symbols, text_section_symbol);
}

void ELF_Codegen_delete(ELF_Codegen_Ctx *ctx) {
    da_delete(&ctx->text);
    da_delete(&ctx->symbols);
    da_delete(&ctx->strtab);
    da_delete(&ctx->shstrtab);
    da_delete(&ctx->section_headers);
}

void ELF_Codegen_compile(ELF_Codegen_Ctx *ctx) {
    usize offset = sizeof(Elf64_Ehdr);

    // sections

    // null
    usize null_name_off = st_append(&ctx->shstrtab, "");
    assert(null_name_off == 0);
    assert(NULL_SECTION_IDX == ctx->section_headers.size);
    da_append(&ctx->section_headers, (Elf64_Shdr){0});

    // text
    usize text_name_off = st_append(&ctx->shstrtab, ".text");
    assert(TEXT_SECTION_IDX == ctx->section_headers.size);
    Elf64_Shdr text_section_header = {
        .sh_name = text_name_off,
        .sh_type = SHT_PROGBITS,
        .sh_flags = SHF_ALLOC | SHF_EXECINSTR,
        .sh_addr = 0,
        .sh_offset = offset,
        .sh_size = ctx->text.size,
        .sh_link = 0,
        .sh_info = 0,
        .sh_addralign = 1,
        .sh_entsize = 0,
    };
    da_append(&ctx->section_headers, text_section_header);

    offset += text_section_header.sh_size;

    // note.gnu-stack
    usize note_gnu_stack_name_off = st_append(&ctx->shstrtab, ".note.GNU-stack");
    assert(NOTE_GNU_STACK_SECTION_IDX == ctx->section_headers.size);
    Elf64_Shdr note_gnu_stack_section_header = {
        .sh_name = note_gnu_stack_name_off,
        .sh_type = SHT_PROGBITS,
        .sh_offset = offset,
        .sh_addralign = 1,
    };
    da_append(&ctx->section_headers, note_gnu_stack_section_header);

    // symtab
    usize symtab_name_off = st_append(&ctx->shstrtab, ".symtab");
    assert(SYMTAB_SECTION_IDX == ctx->section_headers.size);
    Elf64_Shdr symtab_section_header = {
        .sh_name = symtab_name_off,
        .sh_type = SHT_SYMTAB,
        .sh_flags = 0,
        .sh_addr = 0,
        .sh_offset = offset,
        .sh_size = ctx->symbols.size * sizeof(Elf64_Sym),
        .sh_link = STRTAB_SECTION_IDX,
        .sh_info = 3,  // first non-local symbol
        .sh_addralign = 8,
        .sh_entsize = sizeof(Elf64_Sym),
    };
    da_append(&ctx->section_headers, symtab_section_header);

    offset += symtab_section_header.sh_size;

    // strtab
    assert(STRTAB_SECTION_IDX == ctx->section_headers.size);
    usize strtab_name_off = st_append(&ctx->shstrtab, ".strtab");
    Elf64_Shdr strtab_section_header = {
        .sh_name = strtab_name_off,
        .sh_type = SHT_STRTAB,
        .sh_flags = 0,
        .sh_addr = 0,
        .sh_offset = offset,
        .sh_size = ctx->strtab.size,
        .sh_link = 0,
        .sh_info = 0,
        .sh_addralign = 1,
        .sh_entsize = 0,
    };
    da_append(&ctx->section_headers, strtab_section_header);

    offset += strtab_section_header.sh_size;

    // shstrtab
    usize shstrtab_name_off = st_append(&ctx->shstrtab, ".shstrtab");
    assert(SHSTRTAB_SECTION_IDX == ctx->section_headers.size);
    Elf64_Shdr shstrtab_section_header = {
        .sh_name = shstrtab_name_off,
        .sh_type = SHT_STRTAB,
        .sh_flags = 0,
        .sh_addr = 0,
        .sh_offset = offset,
        .sh_size = ctx->shstrtab.size,
        .sh_link = 0,
        .sh_info = 0,
        .sh_addralign = 1,
        .sh_entsize = 0,
    };
    da_append(&ctx->section_headers, shstrtab_section_header);

    offset += shstrtab_section_header.sh_size;

    // header
    ctx->header = (Elf64_Ehdr){
        .e_ident = {
            [EI_MAG0] = ELFMAG0,
            [EI_MAG1] = ELFMAG1,
            [EI_MAG2] = ELFMAG2,
            [EI_MAG3] = ELFMAG3,
            [EI_CLASS] = ELFCLASS64,
            [EI_DATA] = ELFDATA2LSB,
            [EI_VERSION] = EV_CURRENT,
            [EI_OSABI] = ELFOSABI_NONE,
            [EI_ABIVERSION] = 0,
        },
        .e_type = ET_REL,
        .e_machine = EM_X86_64,
        .e_version = EV_CURRENT,
        .e_entry = 0,
        .e_phoff = 0,
        .e_shoff = offset,
        .e_flags = 0,
        .e_ehsize = sizeof(Elf64_Ehdr),
        .e_phentsize = 0,
        .e_phnum = 0,
        .e_shentsize = sizeof(Elf64_Shdr),
        .e_shnum = ctx->section_headers.size,
        .e_shstrndx = SHSTRTAB_SECTION_IDX,
    };
}

void ELF_write_o_file(const ELF_Codegen_Ctx *ctx, FILE *out) {
    Dynamic_Array(uint8_t) o = {0};

    da_append_buf(&o, &ctx->header, sizeof(Elf64_Ehdr));
    da_append_buf(&o, ctx->text.store, ctx->text.size);
    da_append_buf(&o, ctx->symbols.store, ctx->symbols.size * sizeof(Elf64_Sym));
    da_append_buf(&o, ctx->strtab.store, ctx->strtab.size);
    da_append_buf(&o, ctx->shstrtab.store, ctx->shstrtab.size);
    da_append_buf(&o, ctx->section_headers.store, ctx->section_headers.size * sizeof(Elf64_Shdr));

    assert(fwrite(o.store, 1, o.size, out) == o.size);

    da_delete(&o);
}

void ELF_new_func(ELF_Codegen_Ctx *ctx, const char *name,
                  const char *code, usize n) {
    usize name_tab_off = st_append(&ctx->strtab, name);

    usize text_off = ctx->text.size;
    da_append_buf(&ctx->text, code, n);

    Elf64_Sym symbol = {
        .st_name = name_tab_off,
        .st_info = ELF64_ST_INFO(STB_GLOBAL, STT_FUNC),
        .st_other = STV_DEFAULT,
        .st_shndx = TEXT_SECTION_IDX,
        .st_value = text_off,
        .st_size = n,
    };

    da_append(&ctx->symbols, symbol);
}

#endif  // ELF_C
