/*

DX7 FORMAT=9 SYSEX files have some fields bit-packed.

  PACKED DATA : 128 * 32 = 4096
UNPACKED DATA : 155 * 32 = 4960

dx7-unpack unpacks and repacks this data structure

data is read and written in plain-text format as sequences of hexadecimal numbers

http://homepages.abdn.ac.uk/mth192/pages/dx7/sysex-format.txt
http://sourceforge.net/u/tedfelix/dx7dump/

*/

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "c-common/int.h"
#include "c-common/dx7.h"

void fail_if(bool p,char *err)
{
    if (p) {
	printf("DX7-UNPACK> FAIL: %s\n",err);
	exit(EXIT_FAILURE);
    }
}

void print_u8_as_hex(u8 *b,int n)
{
    for(int i = 0;i < n;i ++) {
	printf("%02X",b[i]);
        if (i < n - 1) {
            putchar(' ');
        }
    }
}

void read_u8_as_hex(u8 *b,int n)
{
    for(int i = 0;i < n;i++) {
        unsigned int x;
        char c;
	int err = scanf("%02X%c",&x,&c);
        fail_if(err == 0,"SCANF?");
        fail_if(err == 1 && i != n - 1,"EOF?");
        b[i] = (u8)x;
    }
}

void do_unpack(void)
{
    struct dx7_bank_packed bnk;
    u8 dat[4960];
    read_u8_as_hex((u8*)(&bnk), 4096);
    dx7_unpack_bank(bnk,dat);
    print_u8_as_hex(dat,4960);
}

void do_pack()
{
    u8 dat[4960];
    read_u8_as_hex(dat,4960);
    struct dx7_bank_packed bnk = dx7_pack_bank(dat);
    print_u8_as_hex((u8*)&bnk,4096);
}

int main(int argc, char *argv[])
{
    fail_if(argc != 2,"dx7-unpack unpack|pack");
    u64_verify_eq("OP",sizeof(struct dx7_operator_packed),17);
    u64_verify_eq("VC",sizeof(struct dx7_voice_packed),128);
    u64_verify_eq("BK",sizeof(struct dx7_bank_packed),4096);
    if (strncmp(argv[1],"pack",4) == 0) {
        do_pack();
    } else if (strncmp(argv[1],"unpack",6) == 0) {
        do_unpack();
    } else {
        fprintf(stderr,"dx7-unpack unpack|pack");
    }
    return 0;
}
