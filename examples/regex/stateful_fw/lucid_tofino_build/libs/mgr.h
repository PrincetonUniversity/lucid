/*=====================================================================
=            Helper functions for simple custom switchds.             =
=====================================================================*/
#include <iostream>
#include <thread>         // std::this_thread::sleep_for
#include <chrono>         // std::chrono::seconds

#include <bf_rt/bf_rt_info.hpp>
#include <bf_rt/bf_rt_init.hpp>
#include <bf_rt/bf_rt_common.h>
#include <bf_rt/bf_rt_table_key.hpp>
#include <bf_rt/bf_rt_table_data.hpp>
#include <bf_rt/bf_rt_table.hpp>
#include <getopt.h>
#include <unistd.h>

#ifdef __cplusplus
extern "C" {
#endif
#include <bf_switchd/bf_switchd.h>
#include <traffic_mgr/traffic_mgr.h>
#include <port_mgr/bf_port_if.h>
#include <tofino/bf_pal/bf_pal_port_intf.h>
// #include <tm_port.h>
#ifdef __cplusplus
}
#endif

using namespace std;

// config
#define PIPE_COUNT 2
#define ALL_PIPES 0xffff

// Session to the driver. 
pipe_sess_hdl_t sess_hdl;
dev_target_t dev_tgt;
bf_switchd_context_t *switchd_ctx;

// helpers
static void parse_options(bf_switchd_context_t *sd_ctx,
                          int argc,char **argv) {
  int option_index = 0;
  enum opts {
    OPT_INSTALLDIR = 1,
    OPT_CONFFILE,
    OPT_KERNEL_PKT    
  };
  static struct option options[] = {
      {"help", no_argument, 0, 'h'},
      {"kernel-pkt", no_argument, 0, OPT_KERNEL_PKT},
      {"install-dir", required_argument, 0, OPT_INSTALLDIR},
      {"conf-file", required_argument, 0, OPT_CONFFILE}
    };

  while (1) {
    int c = getopt_long(argc, argv, "h", options, &option_index);

    if (c == -1) {
      break;
    }
    switch (c) {
      case OPT_INSTALLDIR:
        sd_ctx->install_dir = strdup(optarg);
        printf("Install Dir: %s\n", sd_ctx->install_dir);
        break;
      case OPT_CONFFILE:
        sd_ctx->conf_file = strdup(optarg);
        printf("Conf-file : %s\n", sd_ctx->conf_file);
        break;
      case OPT_KERNEL_PKT:
        sd_ctx->kernel_pkt = true;
        printf("using kernel packet\n");
        break;
      case 'h':
      case '?':
        printf(
            "Usage : %s --install-dir <path to where the SDE is "
            "installed> --conf-file <full path to the conf file "
            "(p4prog.conf)>\n", argv[0]);
        exit(c == 'h' ? 0 : 1);
        break;
      default:
        printf("Invalid option\n");
        exit(0);
        break;
    }
  }
  if (sd_ctx->install_dir == NULL) {
    printf("ERROR : --install-dir must be specified\n");
    exit(0);
  }

  if (sd_ctx->conf_file == NULL) {
    printf("ERROR : --conf-file must be specified\n");
    exit(0);
  }
}

void print_hex_memory(void *mem, int len) {
  int i;
  unsigned char *p = (unsigned char *)mem;
  for (i=0;i<len;i++) {
    printf("0x%02x ", p[i]);
    // if (i%16==0)
    //   printf("\n");
  }
  printf("\n");
}

// public functions to actually use.
void start_switchd(int argc, char **argv){
  // start bf_switchd in the backround. 
  if ((switchd_ctx = (bf_switchd_context_t *) calloc(
           1, sizeof(bf_switchd_context_t))) == NULL) {
    printf("Cannot Allocate switchd context\n");
    exit(1);
  }
  parse_options(switchd_ctx, argc, argv);
  // switchd_ctx->shell_set_ucli = 1;
  switchd_ctx->running_in_background = true;
  switchd_ctx->dev_sts_thread = true;
  switchd_ctx->dev_sts_port = 7777; // Use default status port of 7777.


  bf_status_t status = bf_switchd_lib_init(switchd_ctx);
  std::cout << "bf_switchd manager startup complete." << std::endl;
}

int join_switchd() {
  // Join threads in switchd so that it doesn't auto-quit
  pthread_join(switchd_ctx->tmr_t_id, NULL);
  pthread_join(switchd_ctx->dma_t_id, NULL);
  pthread_join(switchd_ctx->int_t_id, NULL);
  pthread_join(switchd_ctx->pkt_t_id, NULL);
  return 0;  
}
