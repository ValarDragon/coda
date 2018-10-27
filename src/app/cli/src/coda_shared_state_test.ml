open Core
open Async
open Coda_worker
open Coda_main
open Coda_base

module Make
    (Ledger_proof : Ledger_proof_intf)
    (Kernel : Kernel_intf with type Ledger_proof.t = Ledger_proof.t)
    (Coda : Coda_intf.S with type ledger_proof = Ledger_proof.t) :
  Integration_test_intf.S =
struct
  module Coda_processes = Coda_processes.Make (Ledger_proof) (Kernel) (Coda)
  open Coda_processes
  module Coda_worker_testnet =
    Coda_worker_testnet.Make (Ledger_proof) (Kernel) (Coda)

  let name = "coda-shared-state-test"

  let main proposal_interval () =
    let log = Logger.create () in
    let log = Logger.child log name in
    let n = 2 in
    let should_propose i = i = 0 in
    let snark_work_public_keys i =
      if i = 0 then Some Genesis_ledger.high_balance_pk else None
    in
    let receiver_pk = Genesis_ledger.low_balance_pk in
    let sender_sk = Genesis_ledger.high_balance_sk in
    let send_amount = Currency.Amount.of_int 10 in
    let fee = Currency.Fee.of_int 0 in
    let%bind testnet =
      Coda_worker_testnet.test ?proposal_interval log n should_propose
        snark_work_public_keys Protocols.Coda_pow.Work_selection.Seq
    in
    let rec go i =
      let%bind () = after (Time.Span.of_sec 1.) in
      let%bind () =
        Coda_worker_testnet.Api.send_transaction testnet 0 sender_sk
          receiver_pk send_amount fee
      in
      if i > 0 then go (i - 1) else return ()
    in
    go 40

  let command =
    let open Command.Let_syntax in
    Command.async ~summary:"Test that workers share states"
      (let%map_open proposal_interval = flag "proposal-interval" ~doc:"MILLIS proposal interval in proof of sig" (optional int) in
      main proposal_interval)
end
