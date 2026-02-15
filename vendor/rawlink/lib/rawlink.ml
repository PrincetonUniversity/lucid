(*
 * Copyright (c) 2015-2022 Christiano F. Haesbaert <haesbaert@haesbaert.org>
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

[@@@warning "-32-37"]

module Lowlevel = Rawlink_lowlevel

type t = {
  fd : Unix.file_descr;
  packets : Cstruct.t list ref;
  buffer : Cstruct.t;
}

let dhcp_server_filter = Lowlevel.dhcp_server_filter
let dhcp_client_filter = Lowlevel.dhcp_client_filter

let open_link ?filter ?(promisc=false) ifname =
  { fd = Lowlevel.opensock ?filter ~promisc ifname;
    packets = ref [];
    buffer = Cstruct.create 65536 }

let close_link t = Unix.close t.fd

let send_packet t buf =
  let len = Cstruct.length buf in
  let n = Unix.write t.fd (Cstruct.to_bytes buf) 0 len in
  if n = 0 then
    raise (Unix.Unix_error(Unix.EPIPE, "send_packet: socket closed", ""))
  else if n <> len then
    raise (Unix.Unix_error(Unix.ENOBUFS, "send_packet: short write", ""))

let rec read_packet t =
  match !(t.packets) with
  | hd :: tl -> t.packets := tl; hd
  | [] ->
    let n = Lowlevel.unix_bytes_read t.fd t.buffer.Cstruct.buffer 0 t.buffer.Cstruct.len in
    if n = 0 then
      failwith "Link socket closed";
    t.packets := Lowlevel.process_input t.buffer n;
    read_packet t
