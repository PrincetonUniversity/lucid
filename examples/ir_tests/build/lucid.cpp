
#include "libs/mgr.h"
int main(int argc, char **argv) {
  start_switchd(argc, argv);
  return join_switchd();
}