open Core_kernel
open Async_kernel

module type S = sig
  type location

  val load
    : location -> Blockchain.t option Deferred.t

  val persist
    : location
    -> [ `Change_head of Blockchain.t ] Linear_pipe.Reader.t
    -> unit
end

module Filesystem : S with type location = string