#include <stdio.h>

int main (void) {
    char cmd[] = "printf %s 'a' | shasum -a256";
    for (int i = 0; i < 255; i++) {
        cmd[11] = i;
        system (cmd);
    }
}
