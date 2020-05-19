/* https://github.com/zserge/webview */

#define WEBVIEW_IMPLEMENTATION
#define WEBVIEW_GTK
#include <ctype.h>
#include "webview.h"

int main(int argc, char *argv[]) {
    if(argc == 5) {
        int rsz = tolower(argv[4][0]) == 't' ? WEBVIEW_HINT_NONE : WEBVIEW_HINT_FIXED;
        webview::webview w(true, nullptr);
        w.set_title("wv");
        w.set_size(atoi(argv[2]), atoi(argv[3]), rsz);
	w.navigate(argv[1]);
	w.run();
        return 0;
    } else {
        fprintf(stderr,"wv uri width height resizeable\n");
        return -1;
    }
}
