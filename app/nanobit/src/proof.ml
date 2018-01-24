open Core
open Snark_params

type t = Tick.Proof.t

let to_string = Tick_curve.Proof.to_string
let of_string = Tick_curve.Proof.of_string

  (* TODO: Figure out what the right thing to do is for conversion failures *)
let ({ Bin_prot.Type_class.
        reader = bin_reader_t
      ; writer = bin_writer_t
      ; shape = bin_shape_t
      } as bin_t)
  =
  Bin_prot.Type_class.cnv Fn.id to_string of_string
    String.bin_t

let { Bin_prot.Type_class.read = bin_read_t; vtag_read = __bin_read_t__ } = bin_reader_t
let { Bin_prot.Type_class.write = bin_write_t; size = bin_size_t } = bin_writer_t