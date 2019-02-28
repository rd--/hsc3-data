/*

DX7 FORMAT=9 SYSEX files have some fields bit-packed.

  PACKED DATA : 128 * 32 = 4096
UNPACKED DATA : 155 * 32 = 4960

dx7-unpack can unpack and repack data in either binary or text (hexadecimal) formats

dx7-unpack unpack prints the 4960 element unpacked sysex data as plain text (310 * 16)

dx7-unpack repack reads text input as written by unpack and writes binary sysex data.

http://homepages.abdn.ac.uk/mth192/pages/dx7/sysex-format.txt
http://sourceforge.net/u/tedfelix/dx7dump/

gcc-8.2.0 -O3 : unpack->repack is not identity, -O0 works

*/

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "c-common/dx7.h"
#include "c-common/int.h"

void print_u8_as_hex(FILE *fp,u8 *b,int n)
{
    for(int i = 0;i < n;i ++) {
	fprintf(fp,"%02X%c",b[i],i % 16 == 15 ? '\n' : ' ');
    }
}

void fail_if(bool p,char *err)
{
    if (p) {
	printf("DX7-UNPACK> FAIL: %s\n",err);
	exit(EXIT_FAILURE);
    }
}

void read_unpacked(FILE *fp,u8 *b,int n)
{
    for(int i = 0;i < n;i ++) {
        unsigned int x;
        char c;
	int err = fscanf(fp,"%02X%c",&x,&c);
        fail_if(err != 2,"FSCANF");
        b[i] = (u8)x;
    }
}

void do_unpack(char *fn,bool opt_in_binary,bool opt_out_binary)
{
    fail_if(!opt_in_binary,"UNPACK: TEXT");
    FILE *fp = fn ? fopen(fn,"rb") : stdin;
    fail_if(fp == NULL,"FOPEN == NULL");
    u8 sysex_data[4104];
    size_t err = fread(sysex_data, 1, 4104, fp);
    fail_if(err != 4104,"FREAD");
    fclose(fp);
    struct dx7_fmt9_sysex *sysex = (struct dx7_fmt9_sysex *)sysex_data;
    dx7_fmt9_sysex_verify(sysex);
    u8 data[4960];
    dx7_unpack_fmt9_sysex(*sysex,data);
    if(opt_out_binary) {
        fwrite(data,1,4960,stdout);
    } else {
        print_u8_as_hex(stdout,data,4960);
    }
}

void do_repack(char *fn,bool opt_in_binary,bool opt_out_binary)
{
    FILE *fp = fn ? fopen(fn,"r") : stdin;
    fail_if(fp == NULL,"FOPEN");
    u8 data[4960];
    if(opt_in_binary) {
        fread(data,1,4960,fp);
    } else {
        read_unpacked(fp,data,4960);
    }
    fclose(fp);
    struct dx7_fmt9_sysex sysex = dx7_pack_fmt9_sysex(data);
    dx7_fmt9_sysex_verify(&sysex);
    if(opt_out_binary) {
        size_t err = fwrite(&sysex, 1, 4104, stdout);
        fail_if(err != 4104,"FWRITE");
    } else {
        print_u8_as_hex(stdout,(u8*)&sysex,4104);
    }
}

struct dx7_unpack_opt
{
    bool repack;
    bool in_binary;
    bool out_binary;
    char *fn;
};

void dx7_unpack(struct dx7_unpack_opt opt)
{
    if(opt.repack) {
        do_repack(opt.fn,opt.in_binary,opt.out_binary);
    } else {
        do_unpack(opt.fn,opt.in_binary,opt.out_binary);
    }
}

int main(int argc, char *argv[])
{
    fail_if(argc < 4 || argc > 5,"dx7-unpack unpack|repack text|binary text|binary [sysex-file]");
    u64_verify_eq("OP",sizeof(struct dx7_operator_packed),17);
    u64_verify_eq("VC",sizeof(struct dx7_voice_packed),128);
    u64_verify_eq("SYSEX",sizeof(struct dx7_fmt9_sysex),4104);
    struct dx7_unpack_opt opt;
    opt.repack = strncmp(argv[1],"repack",6) == 0;
    opt.in_binary = strncmp(argv[2],"binary",6) == 0;
    opt.out_binary = strncmp(argv[3],"binary",6) == 0;
    opt.fn = argc == 5 ? argv[4] : NULL;
    dx7_unpack(opt);
    return 0;
}
