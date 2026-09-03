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

module Lowlevel = Rawlink_lowlevel

type t = {
  flow : Eio_unix.socket;
  fd : Unix.file_descr;
  packets : Cstruct.t list ref;
  buffer : Cstruct.t;
}

let dhcp_server_filter = Lowlevel.dhcp_server_filter
let dhcp_client_filter = Lowlevel.dhcp_client_filter

let open_link ?filter ?(promisc=false) ifname ~sw =
  let fd = Lowlevel.opensock ?filter:filter ~promisc ifname in
  let flow = Eio_unix.FD.as_socket ~sw ~close_unix:true fd in
  { flow; fd; packets = ref []; buffer = (Cstruct.create 65536) }

let close_link t = t.flow#close

let send_packet t buf = t.flow#copy @@ Eio.Flow.cstruct_source [ buf ]

let rec read_packet t =
  match !(t.packets) with
  | hd :: tl -> t.packets := tl; hd
  | [] ->
    let n = Eio.Flow.read t.flow t.buffer in
    t.packets := Lowlevel.process_input t.buffer n;
    read_packet t
