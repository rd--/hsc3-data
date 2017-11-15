/*

DX7 SYSEX files have some fields bit-packed.

dx7-unpack can unpack and repack files.

dx7-unpack unpack prints the 4960 element unpacked sysex data as plain text (310 * 16)

dx7-unpack repack reads text input as written by unpack and writes binary sysex data.

http://homepages.abdn.ac.uk/mth192/pages/dx7/sysex-format.txt
http://sourceforge.net/u/tedfelix/dx7dump/

*/

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef unsigned char u8;
typedef uint64_t u64;

struct operator_packed
{
    u8 op_00_10[11];
    u8 op_11 : 2; u8 op_12 : 2; u8 : 4;
    u8 op_13 : 3; u8 op_20 : 4; unsigned : 1;
    u8 op_14 : 2; u8 op_15 : 3; u8 : 3;
    u8 op_16;
    u8 op_17 : 1; u8 op_18 : 5; u8 : 2;
    u8 op_19;
};

struct voice_packed
{
    struct operator_packed op[6];
    u8 vc_126_133[8];
    u8 vc_134 : 5; u8 : 3;
    u8 vc_135 : 3; u8 vc_136 : 1; u8 : 4;
    u8 vc_137_140[4];
    u8 vc_141 : 1; u8 vc_142 : 3; u8 vc_143 : 4;
    u8 vc_144;
    u8 vc_145_154[10];
};

struct dx7_sysex
{
    u8 sy_begin[6];
    struct voice_packed vc[32];
    u8 sy_end[2];
};

u8 dx7_sy_begin[6] = {0xF0,0x43,0x00,0x09,0x20,0x00};

u8 dx7_checksum(u8 *p)
{
    u8 sum = 0;
    for (int i = 0; i < (4104 - 8); ++i) {
        sum += (p[i] & 0x7F);
   }
    sum = (~sum) + 1;
    sum &= 0x7F;
    return sum;
}

void verify_eq(char *err,u64 p,u64 q)
{
    if(p != q) {
	printf("DX7-UNPACK> VERIFY_EQ: %s: %lX != %lX\n",err,p,q);
	exit(EXIT_FAILURE);
    }
}

#define verify_eq_u8(err,p,q) verify_eq(err,(u64)p,(u64)q)

void dx7_verify(struct dx7_sysex *sysex)
{
    verify_eq_u8("F0",sysex->sy_begin[0],0xF0);
    verify_eq_u8("43",sysex->sy_begin[1],0x43);
    verify_eq_u8("00",sysex->sy_begin[2],0x00);
    verify_eq_u8("09",sysex->sy_begin[3],0x09);
    verify_eq_u8("20",sysex->sy_begin[4],0x20);
    verify_eq_u8("00",sysex->sy_begin[5],0x00);
    verify_eq_u8("CS",dx7_checksum((u8 *)sysex->vc),sysex->sy_end[0]);
    verify_eq_u8("F7",sysex->sy_end[1],0xF7);
}

void unpack_operator(struct operator_packed o,u8 *b)
{
    memcpy(b,o.op_00_10,11);
    b[11] = o.op_11;
    b[12] = o.op_12;
    b[13] = o.op_13;
    b[14] = o.op_14;
    b[15] = o.op_15;
    b[16] = o.op_16;
    b[17] = o.op_17;
    b[18] = o.op_18;
    b[19] = o.op_19;
    b[20] = o.op_20;
}

struct operator_packed pack_operator(u8 *b)
{
    struct operator_packed o;
    memcpy(&(o.op_00_10),b,11);
    o.op_11 = b[11];
    o.op_12 = b[12];
    o.op_13 = b[13];
    o.op_14 = b[14];
    o.op_15 = b[15];
    o.op_16 = b[16];
    o.op_17 = b[17];
    o.op_18 = b[18];
    o.op_19 = b[19];
    o.op_20 = b[20];
    return o;
}

/* 155 = 6 * 21 + 29 ; 6 * 21 = 126 */
void unpack_voice(struct voice_packed v,u8 *b)
{
    for (int i = 0;i < 6;i ++) {
	unpack_operator(v.op[i],b + (i * 21));
    }
    memcpy(b + 126,v.vc_126_133,8);
    b[134] = v.vc_134;
    b[135] = v.vc_135;
    b[136] = v.vc_136;
    memcpy(b + 137,v.vc_137_140,4);
    b[141] = v.vc_141;
    b[142] = v.vc_142;
    b[143] = v.vc_143;
    b[144] = v.vc_144;
    memcpy(b + 145,v.vc_145_154,10);
}

struct voice_packed pack_voice(u8 *b)
{
    struct voice_packed v;
    for (int i = 0;i < 6;i ++) {
	v.op[i] = pack_operator(b + (i * 21));
    }
    memcpy(&(v.vc_126_133),b + 126,8);
    v.vc_134 = b[134];
    v.vc_135 = b[135];
    v.vc_136 = b[136];
    memcpy(&(v.vc_137_140),b + 137,4);
    v.vc_141 = b[141];
    v.vc_142 = b[142];
    v.vc_143 = b[143];
    v.vc_144 = b[144];
    memcpy(&(v.vc_145_154),b + 145,10);
    return v;
}

/* 4960 = 32 * 155 */
void unpack_dx7_sysex(struct dx7_sysex s,u8 *b)
{
    for (int i = 0;i < 32;i ++) {
	unpack_voice(s.vc[i],b + (i * 155));
    }
}

struct dx7_sysex pack_dx7_sysex(u8 *b)
{
    struct dx7_sysex s;
    memcpy(&(s.sy_begin),&dx7_sy_begin,6);
    for (int i = 0;i < 32;i ++) {
	s.vc[i] = pack_voice(b + (i * 155));
    }
    s.sy_end[0] = dx7_checksum((u8 *)&(s.vc[0]));
    s.sy_end[1] = 0xF7;
    return s;
}

void print_unpacked(u8 *b,int n)
{
    for(int i = 0;i < n;i ++) {
	printf("%02X%c",b[i],i % 16 == 15 ? '\n' : ' ');
    }
}

void fail_if(bool p,char *err)
{
    if (p) {
	printf("DX7-UNPACK> FAIL: %s\n",err);
	exit(EXIT_FAILURE);
    }
}

void fread_unpacked(FILE *fp,u8 *b,int n)
{
    for(int i = 0;i < n;i ++) {
        unsigned int x;
        char c;
	int err = fscanf(fp,"%02X%c",&x,&c);
        fail_if(err != 2,"FSCANF");
        b[i] = (u8)x;
    }
}

void do_unpack(char *fn)
{
    FILE *fp = fopen(fn,"rb");
    fail_if(fp == NULL,"FOPEN == NULL");
    u8 sysex_data[4104];
    size_t err = fread(sysex_data, 1, 4104, fp);
    fail_if(err != 4104,"FREAD");
    fclose(fp);
    struct dx7_sysex *sysex = (struct dx7_sysex *)sysex_data;
    dx7_verify(sysex);
    u8 data[4960];
    unpack_dx7_sysex(*sysex,data);
    print_unpacked(data,4960);
}

void do_repack(char *fn)
{
    FILE *fp = fopen(fn,"r");
    fail_if(fp == NULL,"FOPEN");
    u8 data[4960];
    fread_unpacked(fp,data,4960);
    fclose(fp);
    struct dx7_sysex sysex = pack_dx7_sysex(data);
    dx7_verify(&sysex);
    size_t err = fwrite(&sysex, 1, 4104, stdout);
    fail_if(err != 4104,"FWRITE");
}

int main(int argc, char *argv[])
{
    fail_if(argc != 3,"dx7-unpack pack|unpack sysex-file");
    verify_eq("OP",sizeof(struct operator_packed),17);
    verify_eq("VC",sizeof(struct voice_packed),128);
    verify_eq("SYSEX",sizeof(struct dx7_sysex),4104);
    if(strncmp(argv[1],"repack",4) == 0) {
        do_repack(argv[2]);
    } else {
        do_unpack(argv[2]);
    }
    return 0;
}
