#define WEBVIEW_IMPLEMENTATION
#include "webview.h"

int main(int argc, char *argv[]) {
    if(argc == 5) {
        webview("wv",argv[1], atoi(argv[2]), atoi(argv[3]), argv[4][0] == 't');
        return 0;
    } else {
        fprintf(stderr,"wv uri width height resizeable\n");
        return -1;
    }
}
