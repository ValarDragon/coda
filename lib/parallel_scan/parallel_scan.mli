open Core_kernel

module Ring_buffer : sig
  type 'a t [@@deriving sexp, bin_io]

  val read_all : 'a t -> 'a list

  val read_k : 'a t -> int -> 'a list

  val iter : 'a t -> f:('a -> unit) -> unit
end

module State : sig
  module Job : sig
    type ('a, 'd) t = Merge of 'a option * 'a option | Base of 'd option
    [@@deriving bin_io, sexp]
  end

  module Completed_job : sig
    type 'a t = Lifted of 'a | Merged of 'a [@@deriving bin_io, sexp]
  end

  type ('a, 'd) t [@@deriving sexp, bin_io]

  val jobs : ('a, 'd) t -> ('a, 'd) Job.t Ring_buffer.t

  val fold_chronological :
    ('a, 'd) t -> init:'acc -> f:('acc -> ('a, 'd) Job.t -> 'acc) -> 'acc

  val copy : ('a, 'd) t -> ('a, 'd) t

  module Hash : sig
    type t = Digestif.SHA256.t
  end

  val hash : ('a, 'd) t -> ('a -> string) -> ('d -> string) -> Hash.t
end

module type Spec_intf = sig
  type data [@@deriving sexp_of]

  type accum [@@deriving sexp_of]

  type output [@@deriving sexp_of]
end

module Available_job : sig
  type ('a, 'd) t = Base of 'd | Merge of 'a * 'a [@@deriving sexp]
end

val start : parallelism_log_2:int -> ('a, 'd) State.t

val next_k_jobs :
  state:('a, 'd) State.t -> k:int -> ('a, 'd) Available_job.t list Or_error.t

val next_jobs : state:('a, 'd) State.t -> ('a, 'd) Available_job.t list

val enqueue_data : state:('a, 'd) State.t -> data:'d list -> unit Or_error.t

val free_space : state:('a, 'd) State.t -> int

val fill_in_completed_jobs :
     state:('a, 'd) State.t
  -> completed_jobs:'a State.Completed_job.t list
  -> 'a option Or_error.t

val last_emitted_value : ('a, 'd) State.t -> 'a option
