/**
 *
 * Empty booster. Reads packets, sends them back.
 *
 */

#include <stdio.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/ip.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netinet/udp.h>
#include <arpa/inet.h>
#include <linux/if_packet.h>
#include <net/ethernet.h>
#include <net/if.h>
#include <sys/ioctl.h>
#include <cstring>
#include <iostream>

using namespace std;

#define MTU 1500

uint64_t byteCt, pktCt;

// Memory dump helper.
void print_hex_memory(void *mem, int len);

int main(int argc, char *argv[]) {
    int opt = 0;
    char *if_name = nullptr;
    while ((opt = getopt(argc, argv, "i:")) != EOF) {
        switch (opt) {
        case 'i':
            if_name = optarg;
            break;
        default:
            printf("\nNot yet defined opt = %d\n", opt);
            abort();
        }
    }

    if (if_name == nullptr) {
        cerr << "Interface name not provided. Use -i <interface>" << endl;
        return 1;
    }

    cout << "booster running on interface: " << if_name << endl;

    int sockfd;
    if ((sockfd = socket(AF_PACKET, SOCK_RAW, htons(ETH_P_ALL))) < 0) {
        perror("socket");
        return 1;
    }

    struct ifreq ifr;
    memset(&ifr, 0, sizeof(ifr));
    strncpy(ifr.ifr_name, if_name, IFNAMSIZ - 1);

    if (ioctl(sockfd, SIOCGIFINDEX, &ifr) < 0) {
        perror("ioctl");
        close(sockfd);
        return 1;
    }

    struct sockaddr_ll saddr;
    memset(&saddr, 0, sizeof(saddr));
    saddr.sll_family = AF_PACKET;
    saddr.sll_ifindex = ifr.ifr_ifindex;
    saddr.sll_protocol = htons(ETH_P_ALL);

    if (bind(sockfd, (struct sockaddr *)&saddr, sizeof(saddr)) < 0) {
        perror("bind");
        close(sockfd);
        return 1;
    }

    uint8_t buffer[MTU];
    while (true) {
        ssize_t numbytes = recvfrom(sockfd, buffer, MTU, 0, NULL, NULL);
        if (numbytes < 0) {
            perror("recvfrom");
            close(sockfd);
            return 1;
        }

        const struct ether_header *ethernetHeader = (struct ether_header *)buffer;
        if (ntohs(ethernetHeader->ether_type) == ETH_P_IP) {
            pktCt += 1;
            printf("Packet %ld\n", pktCt);
            printf("Packet length: %ld\n", numbytes);
            printf("raw bytes: \n");
            print_hex_memory((void *)ethernetHeader, 14);
            printf("...\n");

            // Send the packet back out
            if (sendto(sockfd, buffer, numbytes, 0, (struct sockaddr *)&saddr, sizeof(saddr)) < 0) {
                perror("sendto");
                close(sockfd);
                return 1;
            }
        }
    }

    close(sockfd);
    return 0;
}

void print_hex_memory(void *mem, int len) {
    int i;
    unsigned char *p = (unsigned char *)mem;
    for (i = 0; i < len; i++) {
        printf("0x%02x ", p[i]);
    }
    printf("\n");
}