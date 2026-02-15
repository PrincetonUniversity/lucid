/*
 * Copyright (c) 2015 Christiano F. Haesbaert <haesbaert@haesbaert.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#ifdef __linux__
#define USE_AF_PACKET
#else
#define USE_BPF /* Best bet */
#endif

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>

#include <net/ethernet.h>

#ifdef USE_AF_PACKET
#include <linux/if_packet.h>
#include <linux/filter.h>
#endif	/* USE_AF_PACKET */

#ifdef USE_BPF
#include <net/bpf.h>
#endif	/* USE_BPF */

#include <netinet/in.h>

#include <net/if.h>

#include <arpa/inet.h>

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <fcntl.h>

#include "caml/memory.h"
#include "caml/fail.h"
#include "caml/unixsupport.h"
#include "caml/signals.h"
#include "caml/alloc.h"
#include "caml/custom.h"
#include "caml/bigarray.h"

#ifdef USE_BPF

#define FILTER bpf_insn

int
bpf_sethdrcmplt(int fd, int v)
{
	if (ioctl(fd, BIOCSHDRCMPLT, &v) == -1) {
		uerror("BIOCSHDRCMPLT", Nothing);
		return (-1);
	}
	return (0);
}

int
bpf_open(void)
{
	int i, fd = -1;
	char path[16];

	for (i = 0; i < 10; i++) {
		snprintf(path, sizeof(path), "/dev/bpf%d", i);
		caml_enter_blocking_section();
		fd = open(path, O_RDWR);
		caml_leave_blocking_section();
		if (fd == -1 && errno == EBUSY)
			continue;
		break;
	}

	if (fd == -1)
		uerror("bpf_open", Nothing);

	return (fd);
}

int
bpf_seesent(int fd, u_int opt)
{
	int r;

	caml_enter_blocking_section();
	r = ioctl(fd, BIOCSSEESENT, &opt);
	caml_leave_blocking_section();
	if (r == -1)
		uerror("bpf_seesent", Nothing);

	return (r);
}

int
bpf_setblen(int fd, u_int len)
{
	int r;

	caml_enter_blocking_section();
	r = ioctl(fd, BIOCSBLEN, &len);
	caml_leave_blocking_section();
	if (r == -1)
		uerror("bpf_setblen", Nothing);

	return (r);
}

int
bpf_setif(int fd, const char *ifname)
{
	struct ifreq ifreq;
	int r;

	bzero(&ifreq, sizeof(ifreq));
	strlcpy(ifreq.ifr_name, ifname, sizeof (ifreq.ifr_name));
	caml_enter_blocking_section();
	r = ioctl(fd, BIOCSETIF, &ifreq);
	caml_leave_blocking_section();
	if (r == -1)
		uerror("bpf_setif", Nothing);

	return (r);
}

int
bpf_setimmediate(int fd, u_int opt)
{
	int r;

	caml_enter_blocking_section();
	r = ioctl(fd, BIOCIMMEDIATE, &opt);
	caml_leave_blocking_section();
	if (r == -1)
		uerror("bpf_setimmediate", Nothing);

	return (r);
}

int
bpf_setfilter(int fd, value vfilter)
{
	int r;
	struct bpf_program prog;

	if (vfilter == Val_int(0))
		return (0);
	prog.bf_len = caml_string_length(Field(vfilter, 0)) /
	    sizeof(struct bpf_insn);
	prog.bf_insns = (struct bpf_insn *) String_val(Field(vfilter, 0));

	caml_enter_blocking_section();
	r = ioctl(fd, BIOCSETF, &prog);
	caml_leave_blocking_section();

	if (r == -1)
		uerror("bpf_setfilter", Nothing);

	return (r);
}

int
bpf_setpromisc(int fd, int promisc)
{
	int r = 0;

	if (promisc) {
		caml_enter_blocking_section();
		r = ioctl(fd, BIOCPROMISC, NULL);
		caml_leave_blocking_section();
	}

	if (r == -1)
		uerror("bpf_setpromisc", Nothing);

	return (0);
}

CAMLprim value
caml_rawlink_open(value vfilter, value vpromisc, value vifname)
{
	CAMLparam3(vfilter, vpromisc, vifname);
	int fd;

	if ((fd = bpf_open()) == -1)
		CAMLreturn(Val_unit);
	if (bpf_seesent(fd, 0) == -1)
		CAMLreturn(Val_unit);
	if (bpf_sethdrcmplt(fd, 1) == -1)
		CAMLreturn(Val_unit);
	if (bpf_setblen(fd, UNIX_BUFFER_SIZE) == -1)
		CAMLreturn(Val_unit);
	if (bpf_setfilter(fd, vfilter) == -1)
		CAMLreturn(Val_unit);
	if (bpf_setif(fd, String_val(vifname)) == -1)
		CAMLreturn(Val_unit);
	if (bpf_setimmediate(fd, 1) == -1)
		CAMLreturn(Val_unit);
	if (bpf_setpromisc(fd, Bool_val(vpromisc)) == -1)
		CAMLreturn(Val_unit);

	CAMLreturn (Val_int(fd));
}

CAMLprim value
caml_bpf_align(value va, value vb)
{
	CAMLparam2(va, vb);
	CAMLlocal1(v);
	uint32_t a, b;

	a = Int_val(va);
	b = Int_val(vb);

	v = Val_int(BPF_WORDALIGN (a + b));

	CAMLreturn (v);
}

#endif	/* USE_BPF */

#ifdef USE_AF_PACKET

/*
 * Welcome to linux where glibc insists in not providing strlcpy.
 */
size_t
strlcpy(char *dst, const char *src, size_t dsize)
{
	const char *osrc = src;
	size_t nleft = dsize;

	/* Copy as many bytes as will fit. */
	if (nleft != 0) {
		while (--nleft != 0) {
			if ((*dst++ = *src++) == '\0')
				break;
		}
	}

	/* Not enough room in dst, add NUL and traverse rest of src. */
	if (nleft == 0) {
		if (dsize != 0)
			*dst = '\0';		/* NUL-terminate dst */
		while (*src++)
			;
	}

	return(src - osrc - 1);	/* count does not include NUL */
}

#define FILTER sock_filter

int
af_packet_open(void)
{
	int fd;

	caml_enter_blocking_section();
	fd = socket(AF_PACKET, SOCK_RAW, htons(ETH_P_ALL));
	caml_leave_blocking_section();

	if (fd == -1)
		uerror("af_packet_open", Nothing);

	return (fd);
}

int
af_packet_setif(int fd, const char *ifname)
{
	int r, ifidx;
	struct sockaddr_ll sll;

	ifidx = if_nametoindex(ifname);
	if (ifidx == 0) {
		uerror("af_packet_setif: if_nametoindex", Nothing);
		return (-1);
	}

	bzero(&sll, sizeof(sll));
	sll.sll_family = AF_PACKET;
	sll.sll_ifindex = ifidx;
	sll.sll_protocol = htons(ETH_P_ALL);

	caml_enter_blocking_section();
	r = bind(fd, (struct sockaddr *) &sll, sizeof(sll));
	caml_leave_blocking_section();

	if (r == -1)
		uerror("af_packet_setif: bind", Nothing);

	return (r);
}

int
af_packet_setfilter(int fd, value vfilter)
{
	int r;
	struct sock_fprog prog;

	if (vfilter == Val_int(0))
		return (0);

	prog.len = caml_string_length(Field(vfilter, 0)) /
	    sizeof(struct sock_filter);
	prog.filter = (struct sock_filter *) String_val(Field(vfilter, 0));

	caml_enter_blocking_section();
	r = setsockopt(fd, SOL_SOCKET, SO_ATTACH_FILTER, &prog, sizeof(prog));
	caml_leave_blocking_section();

	if (r == -1)
		uerror("af_packet_setfilter", Nothing);

	return (r);
}

int
af_packet_setpromisc(int fd, int promisc, const char *ifname)
{
	int r = 0;
	struct packet_mreq mr;
	int ifidx;

	ifidx = if_nametoindex(ifname);
	if (ifidx == 0)
		uerror("af_set_promisc: if_nametoindex", Nothing);

	if (promisc) {
		bzero(&mr, sizeof(mr));
		mr.mr_ifindex = ifidx;
		mr.mr_type = PACKET_MR_PROMISC;
		caml_enter_blocking_section();
		r = setsockopt(fd, SOL_PACKET, PACKET_ADD_MEMBERSHIP,
		    &mr, sizeof(mr));
		caml_leave_blocking_section();
		if (r == -1)
			uerror("af_set_promisc: PACKET_ADD_MEMBERSHIP",
			    Nothing);
	}

	return (r);
}

CAMLprim value
caml_rawlink_open(value vfilter, value vpromisc, value vifname)
{
	CAMLparam3(vfilter, vpromisc, vifname);
	int fd;

	if ((fd = af_packet_open()) == -1)
		CAMLreturn (Val_unit);
	if (af_packet_setfilter(fd, vfilter) == -1)
		CAMLreturn (Val_unit);
	if (af_packet_setif(fd, String_val(vifname)) == -1)
		CAMLreturn (Val_unit);
	if (af_packet_setpromisc(fd, Bool_val(vpromisc),
	    String_val(vifname)) == -1)
		CAMLreturn (Val_unit);

	CAMLreturn (Val_int(fd));
}

/* dummy, not called */
CAMLprim value
caml_bpf_align(value va, value vb)
{
	CAMLparam2(va, vb);
	CAMLreturn (Val_int(0));
}

#endif	/* USE_AF_PACKET */

CAMLprim value
caml_driver(value vunit)
{
	CAMLparam0();
#ifdef AF_PACKET
	CAMLreturn (Val_int(0));
#else
	CAMLreturn (Val_int(1));
#endif
}

/* Filters */
CAMLprim value
caml_dhcp_server_filter(value vunit)
{
	CAMLparam0();
	CAMLlocal1(vfilter);
	struct FILTER dhcp_bpf_filter[] = {
		/* Make sure this is an IP packet... */
		BPF_STMT (BPF_LD + BPF_H + BPF_ABS, 12),
		BPF_JUMP (BPF_JMP + BPF_JEQ + BPF_K, ETHERTYPE_IP, 0, 8),

		/* Make sure it's a UDP packet... */
		BPF_STMT (BPF_LD + BPF_B + BPF_ABS, 23),
		BPF_JUMP (BPF_JMP + BPF_JEQ + BPF_K, IPPROTO_UDP, 0, 6),

		/* Make sure this isn't a fragment... */
		BPF_STMT(BPF_LD + BPF_H + BPF_ABS, 20),
		BPF_JUMP(BPF_JMP + BPF_JSET + BPF_K, 0x1fff, 4, 0),

		/* Get the IP header length... */
		BPF_STMT (BPF_LDX + BPF_B + BPF_MSH, 14),

		/* Make sure it's to the right port... */
		BPF_STMT (BPF_LD + BPF_H + BPF_IND, 16),
		BPF_JUMP (BPF_JMP + BPF_JEQ + BPF_K, 67, 0, 1), /* patch */

		/* If we passed all the tests, ask for the whole packet. */
		BPF_STMT(BPF_RET+BPF_K, (u_int)-1),

		/* Otherwise, drop it. */
		BPF_STMT(BPF_RET+BPF_K, 0),
	};

	vfilter = caml_alloc_string(sizeof(dhcp_bpf_filter));
	memcpy(Bp_val(vfilter), dhcp_bpf_filter, sizeof(dhcp_bpf_filter));

	CAMLreturn (vfilter);
}

CAMLprim value
caml_dhcp_client_filter(value vunit)
{
	CAMLparam0();
	CAMLlocal1(vfilter);
	struct FILTER dhcp_bpf_filter[] = {
		/* Make sure this is an IP packet... */
		BPF_STMT (BPF_LD + BPF_H + BPF_ABS, 12),
		BPF_JUMP (BPF_JMP + BPF_JEQ + BPF_K, ETHERTYPE_IP, 0, 8),

		/* Make sure it's a UDP packet... */
		BPF_STMT (BPF_LD + BPF_B + BPF_ABS, 23),
		BPF_JUMP (BPF_JMP + BPF_JEQ + BPF_K, IPPROTO_UDP, 0, 6),

		/* Make sure this isn't a fragment... */
		BPF_STMT(BPF_LD + BPF_H + BPF_ABS, 20),
		BPF_JUMP(BPF_JMP + BPF_JSET + BPF_K, 0x1fff, 4, 0),

		/* Get the IP header length... */
		BPF_STMT (BPF_LDX + BPF_B + BPF_MSH, 14),

		/* Make sure it's to the right port... */
		BPF_STMT (BPF_LD + BPF_H + BPF_IND, 16),
		BPF_JUMP (BPF_JMP + BPF_JEQ + BPF_K, 68, 0, 1), /* patch */

		/* If we passed all the tests, ask for the whole packet. */
		BPF_STMT(BPF_RET+BPF_K, (u_int)-1),

		/* Otherwise, drop it. */
		BPF_STMT(BPF_RET+BPF_K, 0),
	};

	vfilter = caml_alloc_string(sizeof(dhcp_bpf_filter));
	memcpy(Bp_val(vfilter), dhcp_bpf_filter, sizeof(dhcp_bpf_filter));

	CAMLreturn (vfilter);
}

/* From lwt, so we can read into a Cstruct.t */
CAMLprim value caml_unix_bytes_read(value val_fd, value val_buf, value val_ofs,
    value val_len)
{
	long ret;
	ret = read(Int_val(val_fd),
	    (char *)Caml_ba_array_val(val_buf)->data + Long_val(val_ofs),
	    Long_val(val_len));
	if (ret == -1) uerror("read", Nothing);
	return Val_long(ret);
}
