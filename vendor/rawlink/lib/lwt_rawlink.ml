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

open Lwt.Infix

type t = {
  fd : Lwt_unix.file_descr;
  packets : Cstruct.t list ref;
  buffer : Cstruct.t;
}

let dhcp_server_filter = Lowlevel.dhcp_server_filter
let dhcp_client_filter = Lowlevel.dhcp_client_filter

let open_link ?filter ?(promisc=false) ifname =
  let fd = Lwt_unix.of_unix_file_descr (Lowlevel.opensock ?filter:filter ~promisc ifname) in
  let () = Lwt_unix.set_blocking fd false in
  { fd; packets = ref []; buffer = (Cstruct.create 65536) }

let close_link t = Lwt_unix.close t.fd

let rec read_packet t =
  match !(t.packets) with
  | hd :: tl -> t.packets := tl; Lwt.return hd
  | [] ->
    Lwt_bytes.read t.fd t.buffer.Cstruct.buffer 0 t.buffer.Cstruct.len
    >>= (fun n ->
        if n = 0 then
          failwith "Link socket closed";
        t.packets := Lowlevel.process_input t.buffer n;
        read_packet t)

let send_packet t buf =
  let len = Cstruct.length buf in
  Lwt_bytes.write t.fd buf.Cstruct.buffer 0 len
  >>= (fun n ->
      if n = 0 then
        Lwt.fail (Unix.Unix_error(Unix.EPIPE, "send_packet: socket closed", ""))
      else if n <> len then
        Lwt.fail (Unix.Unix_error(Unix.ENOBUFS, "send_packet: short write", ""))
      else
        Lwt.return_unit)
