/*

DX7 FORMAT=9 SYSEX files have some fields bit-packed.

  PACKED DATA : 128 * 32 = 4096
UNPACKED DATA : 155 * 32 = 4960

dx7-unpack unpacks and repacks this data structure

data is read and written in plain-text format as sequences of two-byte hexadecimal numbers (00 - FF)

http://homepages.abdn.ac.uk/mth192/pages/dx7/sysex-format.txt
http://sourceforge.net/u/tedfelix/dx7dump/

*/

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "r-common/c/int.h"
#include "r-common/c/dx7.h"
#include "r-common/c/failure.h"

void print_u8_as_hex(u8 *b,int n)
{
    for(int i = 0;i < n;i ++) {
	printf("%02X",b[i]);
    }
}

void read_u8_as_hex(u8 *b,int n)
{
    for(int i = 0;i < n;i++) {
        unsigned int x;
	int err = scanf("%02X",&x);
        if(err != 1) {
	    die("DX7-UNPACK> SCANF FAILED AT WORD %d OF %d\n",i,n);
	}
        b[i] = (u8)x;
    }
}

void do_unpack(void)
{
    struct dx7_voice_packed vc;
    u8 dat[155];
    while(!feof(stdin)) {
        read_u8_as_hex((u8*)(&vc), 128);
        dx7_unpack_voice(vc,dat);
        print_u8_as_hex(dat,155);
    }
}

void do_pack()
{
    u8 dat[155];
    while(!feof(stdin)) {
        read_u8_as_hex(dat,155);
        struct dx7_voice_packed vc = dx7_pack_voice(dat);
        print_u8_as_hex((u8*)&vc,128);
    }
}

int main(int argc, char *argv[])
{
    const char usage[] = "usage: dx7-unpack unpack|pack\n";
    die_when(argc != 2,usage);
    u64_verify_eq("OP",sizeof(struct dx7_operator_packed),17);
    u64_verify_eq("VC",sizeof(struct dx7_voice_packed),128);
    u64_verify_eq("BK",sizeof(struct dx7_bank_packed),4096);
    char *cmd = argv[1];
    if (strncmp(cmd,"pack",4) == 0) {
        do_pack();
    } else if (strncmp(cmd,"unpack",6) == 0) {
        do_unpack();
    } else if (strncmp(cmd,"-h",2) == 0) {
        printf(usage);
    } else {
        die("unknown command: %s\n",cmd);
    }
    return 0;
}
