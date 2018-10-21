open Core
open Import
open Snark_params
open Snarky
open Tick
open Let_syntax
open Currency

module Merkle_tree = Snarky.Merkle_tree.Checked(Tick)(struct
    type value = Pedersen.Checked.Digest.t
    type var = Pedersen.Checked.Digest.var
    let typ = Pedersen.Checked.Digest.typ

    let hash ~height h1 h2 =
      let to_triples (bs : Pedersen.Checked.Digest.Unpacked.var) =
        Bitstring_lib.Bitstring.pad_to_triple_list ~default:Boolean.false_
          (bs :> Boolean.var list)
      in
      let open Let_syntax in
      (* TODO: Think about if choose_preimage_var is ok *)
      let%bind h1 = Pedersen.Checked.Digest.choose_preimage h1
      and h2 = Pedersen.Checked.Digest.choose_preimage h2
      in
      Pedersen.Checked.digest_triples
        ~init:Hash_prefix.merkle_tree.(height)
        (to_triples h1 @ to_triples h2)

    let assert_equal h1 h2 = Field.Checked.Assert.equal h1 h2

    let if_ = Field.Checked.if_
  end)
    (struct
      include Account
      let hash = Checked.digest
    end)

let depth = Snark_params.ledger_depth

include Data_hash.Make_full_size()

type path = Pedersen.Digest.t list

type _ Request.t +=
  | Get_path    : Account.Index.t -> path Request.t
  | Get_element : Account.Index.t -> (Account.t * path) Request.t
  | Set         : Account.Index.t * Account.t -> unit Request.t
  | Find_index  : Public_key.Compressed.t -> Account.Index.t Request.t

let reraise_merkle_requests (With { request; respond }) =
  match request with
  | Merkle_tree.Get_path addr ->
    respond (Reraise (Get_path addr))
  | Merkle_tree.Set (addr, account) ->
    respond (Reraise (Set (addr, account)))
  | Merkle_tree.Get_element addr ->
    respond (Reraise (Get_element addr))
  | _ -> unhandled


(*
   [modify_account t pk ~filter ~f] implements the following spec:

   - finds an account [account] in [t] for [pk] at path [addr] where [filter account] holds.
     note that the account is not guaranteed to have public key [pk]; it might be a new account
     created to satisfy this request.
   - returns a root [t'] of a tree of depth [depth]
   which is [t] but with the account [f account] at path [addr].
*)
let modify_account t pk ~(filter:Account.var -> (unit, _) Checked.t) ~f =
  with_label __LOC__ begin
    let%bind addr =
      request_witness Account.Index.Unpacked.typ
        As_prover.(
          map (read Public_key.Compressed.typ pk)
            ~f:(fun s -> Find_index s))
    in
    handle
      (Merkle_tree.modify_req ~depth (var_to_hash_packed t) addr ~f:(fun account ->
        let%bind () = filter account in
        f account))
      reraise_merkle_requests
    >>| var_of_hash_packed
  end

(*
   [modify_account_send t pk ~f] implements the following spec:

   - finds an account [account] in [t] at path [addr] whose public key is [pk] OR it is a fee transfer and is an empty account
   - returns a root [t'] of a tree of depth [depth]
   which is [t] but with the account [f account] at path [addr].
*)
let modify_account_send t pk ~is_fee_transfer ~f = modify_account t pk ~filter:(fun account ->
        let%bind account_already_there = Public_key.Compressed.Checked.equal account.public_key pk in
        let%bind account_not_there = Public_key.Compressed.Checked.equal account.public_key Public_key.Compressed.(var_of_t zero) in
        let%bind fee_transfer = Boolean.(account_not_there && is_fee_transfer) in
        Boolean.Assert.any [account_already_there; fee_transfer]
) ~f

(*
   [modify_account_recv t ~pk ~f] implements the following spec:

   - finds an account [account] in [t] at path [addr] whose public key is [pk] OR which is an empty account
   - returns a root [t'] of a tree of depth [depth]
   which is [t] but with the account [f account] at path [addr].
*)
let modify_account_recv t pk ~f = modify_account t pk ~filter:(fun account ->
        let%bind account_already_there = Public_key.Compressed.Checked.equal account.public_key pk in
        let%bind account_not_there = Public_key.Compressed.Checked.equal account.public_key Public_key.Compressed.(var_of_t zero) in
        Boolean.Assert.any [account_already_there; account_not_there]
) ~f
