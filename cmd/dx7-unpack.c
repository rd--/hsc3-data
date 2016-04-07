/*

DX7 SYSEX files have some fields bit-packed.

dx7-unpack prints the unpacked sysex data as plain text.

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

u8 d7x_checksum(u8 *p)
{
    u8 sum = 0;
    for (int i = 0; i < 4096; ++i) {
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
    verify_eq_u8("00",sysex->sy_begin[2],0);
    verify_eq_u8("09",sysex->sy_begin[3],0x09);
    verify_eq_u8("20",sysex->sy_begin[4],0x20);
    verify_eq_u8("00",sysex->sy_begin[5],0);
    verify_eq_u8("CS",d7x_checksum((u8 *)sysex->vc),sysex->sy_end[0]);
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

/* 155 = 6 * 21 + 29 ; 6 * 21 = 126 */
void unpack_voice(struct voice_packed v,u8 *b)
{
    for (int i = 0;i < 6;i ++) {
	unpack_operator(v.op[i],b + (i*21));
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

/* 4960 = 32 * 155 */
void unpack_dx7_sysex(struct dx7_sysex s,u8 *b)
{
    for (int i = 0;i < 32;i ++) {
	unpack_voice(s.vc[i],b + (i * 155));
    }
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

int main(int argc, char *argv[])
{
    fail_if(argc != 2,"dx7-unpack sysex-file");
    verify_eq("OP",sizeof(struct operator_packed),17);
    verify_eq("VC",sizeof(struct voice_packed),128);
    verify_eq("SYSEX",sizeof(struct dx7_sysex),4104);
    FILE *fp = fopen(argv[1],"rb");
    fail_if(fp == NULL,"FOPEN == NULL");
    u8 sysex_data[4104];
    size_t sysex_size = fread(sysex_data, 1, 4104, fp);
    fail_if(sysex_size != 4104,"SYSEX_SIZE != 4104");
    fclose(fp);
    struct dx7_sysex *sysex = (struct dx7_sysex *)sysex_data;
    dx7_verify(sysex);
    u8 data[4960];
    unpack_dx7_sysex(*sysex,data);
    print_unpacked(data,4960);
    return 0;
}
