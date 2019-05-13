/* gcc -i ~/opt/src/webview wv.c -DWEBVIEW_GTK=1 `pkg-config --cflags --libs gtk+-3.0 webkit2gtk-4.0` -o wv */

#define WEBVIEW_IMPLEMENTATION
#include "webview.h"

/* ./wv file:///tmp/t.html 500 300 false */
int main(int argc, char *argv[]) {
    if(argc == 5) {
        webview("wv",argv[1], atoi(argv[2]), atoi(argv[3]), argv[4][0] == 't');
        return 0;
    } else {
        fprintf(stderr,"wv uri width height resizeable\n");
        return -1;
    }
}
