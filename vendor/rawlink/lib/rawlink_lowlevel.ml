(*
 * Copyright (c) 2022 Christiano F. Haesbaert <haesbaert@haesbaert.org>
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
 *)

(* This module is supposed to be used internally only *)

[%%cstruct
type bpf_hdr = {
    bh_sec: uint32_t;
    bh_usec: uint32_t;
    bh_caplen: uint32_t;
    bh_datalen: uint32_t;
    bh_hdrlen: uint16_t;
} [@@host_endian]]

type driver =
  | AF_PACKET
  | BPF

external opensock: ?filter:string -> ?promisc:bool -> string -> Unix.file_descr = "caml_rawlink_open"
external dhcp_server_filter: unit -> string = "caml_dhcp_server_filter"
external dhcp_client_filter: unit -> string = "caml_dhcp_client_filter"
external driver: unit -> driver = "caml_driver"
external unix_bytes_read: Unix.file_descr -> Cstruct.buffer -> int -> int -> int =
  "caml_unix_bytes_read"
external bpf_align: int -> int -> int = "caml_bpf_align"

let bpf_split_buffer buffer len =
  let rec loop buffer n packets =
    if n <= 0 then
      List.rev packets
    else
      let bh_caplen = Int32.to_int (get_bpf_hdr_bh_caplen buffer) in
      let bh_datalen = Int32.to_int (get_bpf_hdr_bh_datalen buffer) in
      let bh_hdrlen = get_bpf_hdr_bh_hdrlen buffer in
      let nextoff = bpf_align bh_hdrlen bh_caplen in
      if bh_caplen <> bh_datalen then
        loop (Cstruct.shift buffer nextoff) (n - nextoff) packets
      else
        let pkt = Cstruct.create bh_datalen in
        Cstruct.blit buffer bh_hdrlen pkt 0 bh_datalen;
        loop (Cstruct.shift buffer nextoff) (n - nextoff) (pkt :: packets)
  in
  loop buffer len []

let process_input buf len =
  match driver () with
  | AF_PACKET ->
    let dst = Cstruct.create len in
    Cstruct.blit buf 0 dst 0 len;
    [ dst ]
  | BPF -> bpf_split_buffer buf len
