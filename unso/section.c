#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <elf.h>

extern char* getstr(void* data, int n, int i);

void dump_SHT_PROGBITS_section(void* data, size_t length, int i) {
    Elf32_Ehdr* ehdr = (Elf32_Ehdr*)data;
    Elf32_Shdr* shdrs = (Elf32_Shdr*)((char*)data + ehdr->e_shoff);
    int shnum = ehdr->e_shnum;
    int shentsize = ehdr->e_shentsize;

    assert(sizeof(Elf32_Shdr) == ehdr->e_shentsize);
}


void dump_SHT_SYMTAB_section(void* data, size_t length, int i) {
    Elf32_Ehdr* ehdr = (Elf32_Ehdr*)data;
    Elf32_Shdr* shdrs = (Elf32_Shdr*)((char*)data + ehdr->e_shoff);
    Elf32_Sym* symbols = (Elf32_Sym*)((char*)data + shdrs[i].sh_offset);
    int shnum = ehdr->e_shnum;
    int shentsize = ehdr->e_shentsize;
    int entnum;
    unsigned char st_info;
    char info[3];   /* local/global/weak, data/function/section/file */
    int j;

    assert(sizeof(Elf32_Shdr) == ehdr->e_shentsize &&
            sizeof(Elf32_Sym) == shdrs[i].sh_entsize);

    entnum = shdrs[i].sh_size / shdrs[i].sh_entsize;
    assert(entnum > 0);
    
    info[2] = '\0';
    printf("entnum=%d\n", entnum);
    puts("    [ Nr]  value size  info  other   shndx   name");
    for (j = 0; j < entnum; ++j) {
        st_info = symbols[j].st_info;
        if (ELF32_ST_BIND(st_info) == STB_LOCAL)
            info[0] = 'l';
        else if (ELF32_ST_BIND(st_info) == STB_GLOBAL)
            info[0] = 'g';
        else if (ELF32_ST_BIND(st_info) == STB_WEAK)
            info[0] = 'w';
        else
            info[0] = '?';

        if (ELF32_ST_TYPE(st_info) == STT_OBJECT)
            info[1] = 'd';
        else if (ELF32_ST_TYPE(st_info) == STT_FUNC)
            info[1] = 'F';
        else if (ELF32_ST_TYPE(st_info) == STT_SECTION)
            info[1] = 's';
        else if (ELF32_ST_TYPE(st_info) == STT_FILE)
            info[1] = 'f';
        else if (ELF32_ST_TYPE(st_info) == STT_NOTYPE)
            info[1] = ' ';
        else
            info[1] = '?';

        printf("    [%3d] %6d %4d  [%2s]    %2d   %6d  %-12s\n",
                j,
                symbols[j].st_value,
                symbols[j].st_size,
                info,
                symbols[j].st_other,
                symbols[j].st_shndx,
                (symbols[j].st_name == 0) ? "<UNNAMED>" :
                    get_str(data, shdrs[i].sh_link, symbols[j].st_name));
    }
}


void dump_SHT_STRTAB_section(void* data, size_t length, int i) {
    Elf32_Ehdr* ehdr = (Elf32_Ehdr*)data;
    Elf32_Shdr* shdrs = (Elf32_Shdr*)((char*)data + ehdr->e_shoff);
    int shnum = ehdr->e_shnum;
    int shentsize = ehdr->e_shentsize;
    char* p = (char*)data + shdrs[i].sh_offset;
    char* end = p + shdrs[i].sh_size;
    int n;

    assert(sizeof(Elf32_Shdr) == ehdr->e_shentsize);

    assert(*p == '\0');
    n = 0;
    printf("    [%3d] %s\n", n, p);
    ++p;

    assert(*p != '\0');
    while (p < end) {
        ++n;
        printf("    [%3d] %s\n", n, p);
        p += strlen(p) + 1;
    }
}


void dump_SHT_RELA_section(void* data, size_t length, int i) {
    Elf32_Ehdr* ehdr = (Elf32_Ehdr*)data;
    Elf32_Shdr* shdrs = (Elf32_Shdr*)((char*)data + ehdr->e_shoff);
    int shnum = ehdr->e_shnum;
    int shentsize = ehdr->e_shentsize;

    assert(sizeof(Elf32_Shdr) == ehdr->e_shentsize);
}


void dump_SHT_HASH_section(void* data, size_t length, int i) {
    Elf32_Ehdr* ehdr = (Elf32_Ehdr*)data;
    Elf32_Shdr* shdrs = (Elf32_Shdr*)((char*)data + ehdr->e_shoff);
    int shnum = ehdr->e_shnum;
    int shentsize = ehdr->e_shentsize;

    assert(sizeof(Elf32_Shdr) == ehdr->e_shentsize);
}


void dump_SHT_DYNAMIC_section(void* data, size_t length, int i) {
    Elf32_Ehdr* ehdr = (Elf32_Ehdr*)data;
    Elf32_Shdr* shdrs = (Elf32_Shdr*)((char*)data + ehdr->e_shoff);
    int shnum = ehdr->e_shnum;
    int shentsize = ehdr->e_shentsize;

    assert(sizeof(Elf32_Shdr) == ehdr->e_shentsize);
}


void dump_SHT_NOTE_section(void* data, size_t length, int i) {
    Elf32_Ehdr* ehdr = (Elf32_Ehdr*)data;
    Elf32_Shdr* shdrs = (Elf32_Shdr*)((char*)data + ehdr->e_shoff);
    int shnum = ehdr->e_shnum;
    int shentsize = ehdr->e_shentsize;

    assert(sizeof(Elf32_Shdr) == ehdr->e_shentsize);
}


void dump_SHT_NOBITS_section(void* data, size_t length, int i) {
    Elf32_Ehdr* ehdr = (Elf32_Ehdr*)data;
    Elf32_Shdr* shdrs = (Elf32_Shdr*)((char*)data + ehdr->e_shoff);
    int shnum = ehdr->e_shnum;
    int shentsize = ehdr->e_shentsize;

    assert(sizeof(Elf32_Shdr) == ehdr->e_shentsize);
}


void dump_SHT_REL_section(void* data, size_t length, int i) {
    Elf32_Ehdr* ehdr = (Elf32_Ehdr*)data;
    Elf32_Shdr* shdrs = (Elf32_Shdr*)((char*)data + ehdr->e_shoff);
    int shnum = ehdr->e_shnum;
    int shentsize = ehdr->e_shentsize;

    assert(sizeof(Elf32_Shdr) == ehdr->e_shentsize);
}


void dump_SHT_SHLIB_section(void* data, size_t length, int i) {
    Elf32_Ehdr* ehdr = (Elf32_Ehdr*)data;
    Elf32_Shdr* shdrs = (Elf32_Shdr*)((char*)data + ehdr->e_shoff);
    int shnum = ehdr->e_shnum;
    int shentsize = ehdr->e_shentsize;

    assert(sizeof(Elf32_Shdr) == ehdr->e_shentsize);
}


void dump_SHT_DYNSYM_section(void* data, size_t length, int i) {
    Elf32_Ehdr* ehdr = (Elf32_Ehdr*)data;
    Elf32_Shdr* shdrs = (Elf32_Shdr*)((char*)data + ehdr->e_shoff);
    int shnum = ehdr->e_shnum;
    int shentsize = ehdr->e_shentsize;

    assert(sizeof(Elf32_Shdr) == ehdr->e_shentsize);
}


void dump_SHT_INIT_ARRAY_section(void* data, size_t length, int i) {
    Elf32_Ehdr* ehdr = (Elf32_Ehdr*)data;
    Elf32_Shdr* shdrs = (Elf32_Shdr*)((char*)data + ehdr->e_shoff);
    int shnum = ehdr->e_shnum;
    int shentsize = ehdr->e_shentsize;

    assert(sizeof(Elf32_Shdr) == ehdr->e_shentsize);
}


void dump_SHT_FINI_ARRAY_section(void* data, size_t length, int i) {
    Elf32_Ehdr* ehdr = (Elf32_Ehdr*)data;
    Elf32_Shdr* shdrs = (Elf32_Shdr*)((char*)data + ehdr->e_shoff);
    int shnum = ehdr->e_shnum;
    int shentsize = ehdr->e_shentsize;

    assert(sizeof(Elf32_Shdr) == ehdr->e_shentsize);
}


void dump_SHT_PREINIT_ARRAY_section(void* data, size_t length, int i) {
    Elf32_Ehdr* ehdr = (Elf32_Ehdr*)data;
    Elf32_Shdr* shdrs = (Elf32_Shdr*)((char*)data + ehdr->e_shoff);
    int shnum = ehdr->e_shnum;
    int shentsize = ehdr->e_shentsize;

    assert(sizeof(Elf32_Shdr) == ehdr->e_shentsize);
}


void dump_SHT_GROUP_section(void* data, size_t length, int i) {
    Elf32_Ehdr* ehdr = (Elf32_Ehdr*)data;
    Elf32_Shdr* shdrs = (Elf32_Shdr*)((char*)data + ehdr->e_shoff);
    int shnum = ehdr->e_shnum;
    int shentsize = ehdr->e_shentsize;

    assert(sizeof(Elf32_Shdr) == ehdr->e_shentsize);
}


void dump_SHT_SYMTAB_SHNDX_section(void* data, size_t length, int i) {
    Elf32_Ehdr* ehdr = (Elf32_Ehdr*)data;
    Elf32_Shdr* shdrs = (Elf32_Shdr*)((char*)data + ehdr->e_shoff);
    int shnum = ehdr->e_shnum;
    int shentsize = ehdr->e_shentsize;

    assert(sizeof(Elf32_Shdr) == ehdr->e_shentsize);
}


void dump_SHT_NUM_section(void* data, size_t length, int i) {
    Elf32_Ehdr* ehdr = (Elf32_Ehdr*)data;
    Elf32_Shdr* shdrs = (Elf32_Shdr*)((char*)data + ehdr->e_shoff);
    int shnum = ehdr->e_shnum;
    int shentsize = ehdr->e_shentsize;

    assert(sizeof(Elf32_Shdr) == ehdr->e_shentsize);
}


void dump_SHT_GNU_LIBLIST_section(void* data, size_t length, int i) {
    Elf32_Ehdr* ehdr = (Elf32_Ehdr*)data;
    Elf32_Shdr* shdrs = (Elf32_Shdr*)((char*)data + ehdr->e_shoff);
    int shnum = ehdr->e_shnum;
    int shentsize = ehdr->e_shentsize;

    assert(sizeof(Elf32_Shdr) == ehdr->e_shentsize);
}


void dump_SHT_CHECKSUM_section(void* data, size_t length, int i) {
    Elf32_Ehdr* ehdr = (Elf32_Ehdr*)data;
    Elf32_Shdr* shdrs = (Elf32_Shdr*)((char*)data + ehdr->e_shoff);
    int shnum = ehdr->e_shnum;
    int shentsize = ehdr->e_shentsize;

    assert(sizeof(Elf32_Shdr) == ehdr->e_shentsize);
}


void dump_SHT_SUNW_move_section(void* data, size_t length, int i) {
    Elf32_Ehdr* ehdr = (Elf32_Ehdr*)data;
    Elf32_Shdr* shdrs = (Elf32_Shdr*)((char*)data + ehdr->e_shoff);
    int shnum = ehdr->e_shnum;
    int shentsize = ehdr->e_shentsize;

    assert(sizeof(Elf32_Shdr) == ehdr->e_shentsize);
}


void dump_SHT_SUNW_COMDAT_section(void* data, size_t length, int i) {
    Elf32_Ehdr* ehdr = (Elf32_Ehdr*)data;
    Elf32_Shdr* shdrs = (Elf32_Shdr*)((char*)data + ehdr->e_shoff);
    int shnum = ehdr->e_shnum;
    int shentsize = ehdr->e_shentsize;

    assert(sizeof(Elf32_Shdr) == ehdr->e_shentsize);
}


void dump_SHT_SUNW_syminfo_section(void* data, size_t length, int i) {
    Elf32_Ehdr* ehdr = (Elf32_Ehdr*)data;
    Elf32_Shdr* shdrs = (Elf32_Shdr*)((char*)data + ehdr->e_shoff);
    int shnum = ehdr->e_shnum;
    int shentsize = ehdr->e_shentsize;

    assert(sizeof(Elf32_Shdr) == ehdr->e_shentsize);
}


void dump_SHT_GNU_verdef_section(void* data, size_t length, int i) {
    Elf32_Ehdr* ehdr = (Elf32_Ehdr*)data;
    Elf32_Shdr* shdrs = (Elf32_Shdr*)((char*)data + ehdr->e_shoff);
    int shnum = ehdr->e_shnum;
    int shentsize = ehdr->e_shentsize;

    assert(sizeof(Elf32_Shdr) == ehdr->e_shentsize);
}


void dump_SHT_GNU_verneed_section(void* data, size_t length, int i) {
    Elf32_Ehdr* ehdr = (Elf32_Ehdr*)data;
    Elf32_Shdr* shdrs = (Elf32_Shdr*)((char*)data + ehdr->e_shoff);
    int shnum = ehdr->e_shnum;
    int shentsize = ehdr->e_shentsize;

    assert(sizeof(Elf32_Shdr) == ehdr->e_shentsize);
}


void dump_SHT_GNU_versym_section(void* data, size_t length, int i) {
    Elf32_Ehdr* ehdr = (Elf32_Ehdr*)data;
    Elf32_Shdr* shdrs = (Elf32_Shdr*)((char*)data + ehdr->e_shoff);
    int shnum = ehdr->e_shnum;
    int shentsize = ehdr->e_shentsize;

    assert(sizeof(Elf32_Shdr) == ehdr->e_shentsize);
}

