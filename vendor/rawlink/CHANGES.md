## v2.1 (2022-08-26)

* Fix broken dune files
* Include `dhcp_client_filter` and `dhcp_server_filter` in Eio
* Include an actual interface for Eio
* Fix Eio types on `send_packet`

## v2.0 (2022-08-26)

* Eio support via rawlink-eio
* Lwt split into rawlink-lwt
* More and better testing binaries
* Update to dune 3.2 format
* Split common bits in Rawlink_lowlevel

## v1.2 (2022-04-14)

* Fix linking against non-lwt rawlink
* Implement promiscuous mode
* Update caml_ calls in rawlink_stubs.c

## v1.1 (2022-04-01)

* Use Host_endian instead of Little_endian, requires Cstruct >= 6.1.0,
this will make rawlink work correctly on big endian machines
* Adapt to modern Cstruct
* const/deconstify string/bytes

## v1.0 (2019-01-17)

This release splits the Lwt package into a separate
`rawlink.lwt` ocamlfind package.  Existing users of `Lwt_rawlink`
can just rename the ocamlfind package `rawlink` to `rawlink.lwt`
to get the previous functionality.

* Add a `dhcp_client_filter` for DHCP client port (#8 by @yomimono)
* Rename `dhcp_filter` to `dhcp_server_filter` (#8 by @yomimono)
* Support Lwt 4.0 (#10 by @hannesm)
* Port build system to Dune (#11 by @avsm)

## 0.7 (2017-11-26)

* Fix compilation on POSIX-like systems (such as FreeBSD) by including netinet/in.h

## 0.6 (2017-11-24)

* Support ocaml 4.06
* Fix cstruct linking

## 0.5 (2017-04-16)

* Convert to topkg

## 0.4 (2016-12-17)

* Convert to ppx

## 0.3 (2015-08-30)

* Fix fd leak in bpf_open
* Fix linux send function

## 0.2 (2015-08-28)

* Fix to bpf_split_buf

## 0.1 (2015-10-09)

* Initial release
