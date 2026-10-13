open! Core

module Stable = struct
  module V1 = struct
    type t =
      { business_date : Date.Stable.V1.t
      ; seq : int
      ; journal_length : int
      ; journal_md5 : string
      ; journal_head : string
      ; rows : Balances.Day_row.Stable.V1.t list
      ; state : Balances.Stable.V1.t
      }
    [@@deriving bin_io, sexp_of]
  end
end

include Stable.V1

let digest = Bin_prot.Shape.eval_to_digest_string Stable.V1.bin_shape_t

let write t path =
  Or_error.try_with (fun () ->
    Core_unix.mkdir_p (Filename.dirname path);
    (* Write to a temporary name and rename, so a crash never leaves half a snapshot under
       the real name. *)
    let tmp = path ^ ".tmp" in
    Out_channel.write_all
      tmp
      ~data:
        (Framing.header ~digest
         ^ fst (Framing.encode_frame ~prev:Framing.genesis Stable.V1.bin_writer_t t));
    Core_unix.rename ~src:tmp ~dst:path)
;;

let read path =
  Or_error.try_with_join (fun () ->
    let%bind.Or_error decoded =
      Framing.decode ~accept:[ digest ] Stable.V1.bin_reader_t (In_channel.read_all path)
    in
    match decoded.records, decoded.tail with
    | [ t ], Clean -> Ok t
    | _ ->
      Or_error.error_s [%message "damaged snapshot" ~tail:(decoded.tail : Framing.Tail.t)])
  |> Or_error.tag_s ~tag:[%message "Snapshot.read" path]
;;

let default_path ~journal date = journal ^ ".snapshots" ^/ Date.to_string date ^ ".snap"

let check_prefix t contents =
  if String.length contents < t.journal_length
  then
    Or_error.error_s
      [%message
        "journal is shorter than the snapshot covers"
          ~journal_length:(String.length contents : int)
          ~covered:(t.journal_length : int)]
  else (
    let md5 = Md5.to_hex (Md5.digest_string (String.prefix contents t.journal_length)) in
    if String.equal md5 t.journal_md5
    then Ok ()
    else
      Or_error.error_s
        [%message
          "journal prefix does not match the snapshot" ~expected:t.journal_md5 ~found:md5])
;;

let verify t ~journal =
  Or_error.try_with_join (fun () ->
    let open Or_error.Let_syntax in
    let contents = In_channel.read_all journal in
    let%bind () = check_prefix t contents in
    let%bind decoded =
      Framing.decode
        ~accept:Record.compatible_digests
        Record.Stable.V1.bin_reader_t
        (String.prefix contents t.journal_length)
    in
    let%bind state = Balances.of_records decoded.records in
    if not (String.equal decoded.head t.journal_head)
    then Or_error.error_s [%message "journal head hash differs from the snapshot"]
    else if Balances.next_seq state <> t.seq + 1
    then Or_error.error_s [%message "snapshot sequence number does not match the journal"]
    else if not (Balances.equal state t.state)
    then Or_error.error_s [%message "replayed state differs from the snapshot"]
    else if not ([%equal: Balances.Day_row.t list] (Balances.day_rows state) t.rows)
    then Or_error.error_s [%message "replayed day rows differ from the snapshot"]
    else Ok ())
  |> Or_error.tag_s ~tag:[%message "Snapshot.verify" journal]
;;
