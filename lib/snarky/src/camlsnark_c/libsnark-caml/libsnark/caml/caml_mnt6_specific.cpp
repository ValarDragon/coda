#include <libsnark/caml/caml_mnt6.hpp>
#include <libsnark/gadgetlib1/gadgets/verifiers/r1cs_ppzksnark_verifier_gadget.hpp>

extern "C" {
using namespace libsnark;


// G1 functions

libff::bigint<libff::mnt6_q_limbs>*  camlsnark_mnt6_g1_get_x(libff::G1<ppT>* g) {
  return new libff::bigint<libff::mnt6_q_limbs>(g->X().as_bigint());
}

libff::bigint<libff::mnt6_q_limbs>*  camlsnark_mnt6_g1_get_y(libff::G1<ppT>* g) {
  return new libff::bigint<libff::mnt6_q_limbs>(g->Y().as_bigint());
}

libff::G1<ppT>* camlsnark_mnt6_g1_of_field(libff::bigint<libff::mnt6_r_limbs>* k) {
  libff::G1<ppT>* g = new libff::G1<ppT>();
  g->one();
  return new libff::G1<ppT>(*k * *g);
}

bool camlsnark_mnt6_bg_proof_double_pairing_check(
  libff::G1<ppT>* ys_p,
  libff::G2<ppT>* delta_prime_p,
  libff::G1<ppT>* z_p,
  libff::G2<ppT>* delta_p
){
  libff::G1<ppT> ys = *ys_p;
  libff::G2<ppT> delta_prime = *delta_prime_p;
  libff::G1<ppT> z = *z_p;
  libff::G2<ppT> delta = *delta_p;
  libff::Fqk<ppT> lhs = mnt6_ate_double_miller_loop(mnt6_ate_precompute_G1(ys), mnt6_ate_precompute_G2(delta_prime), mnt6_ate_precompute_G1(-z), mnt6_ate_precompute_G2(delta));
  libff::GT<ppT> result = mnt6_final_exponentiation(lhs);
  return (result == libff::GT<ppT>::one());
}

// G2 functions

std::vector<libff::bigint<libff::mnt6_q_limbs>>*  camlsnark_mnt6_g2_get_x(libff::G2<ppT>* g) {
  std::vector<libff::Fq<ppT>> field_elts = g->X().coordinates();
  std::vector<libff::bigint<libff::mnt6_q_limbs>>* result = new std::vector<libff::bigint<libff::mnt6_q_limbs>>();
  for (auto &elt : field_elts) {
    result->push_back(elt.as_bigint());
  }
  return result;
}

std::vector<libff::bigint<libff::mnt6_q_limbs>>*  camlsnark_mnt6_g2_get_y(libff::G2<ppT>* g) {
  std::vector<libff::Fq<ppT>> field_elts = g->Y().coordinates();
  std::vector<libff::bigint<libff::mnt6_q_limbs>>* result = new std::vector<libff::bigint<libff::mnt6_q_limbs>>();
  for (auto &elt : field_elts) {
    result->push_back(elt.as_bigint());
  }
  return result;
  }

// verification key

void camlsnark_mnt6_emplace_bits_of_field(std::vector<bool>* v, FieldT &x) {
  size_t field_size = FieldT::size_in_bits();
  auto n = x.as_bigint();
  for (size_t i = 0; i < field_size; ++i) {
    v->emplace_back(n.test_bit(i));
  }
}

std::vector<bool>* camlsnark_mnt6_verification_key_other_to_bool_vector(
    r1cs_ppzksnark_verification_key<other_curve_ppT>* vk
) {
  return new std::vector<bool>(
      r1cs_ppzksnark_verification_key_variable<ppT>::get_verification_key_bits(*vk));
}

std::vector<FieldT>* camlsnark_mnt6_verification_key_other_to_field_vector(
    r1cs_ppzksnark_verification_key<other_curve_ppT>* r1cs_vk
) {
  const size_t input_size_in_elts = r1cs_vk->encoded_IC_query.rest.indices.size(); // this might be approximate for bound verification keys, however they are not supported by r1cs_ppzksnark_verification_key_variable
  const size_t vk_size_in_bits = r1cs_ppzksnark_verification_key_variable<ppT>::size_in_bits(input_size_in_elts);

  protoboard<FieldT> pb;
  pb_variable_array<FieldT> vk_bits;
  vk_bits.allocate(pb, vk_size_in_bits, "vk_bits");
  r1cs_ppzksnark_verification_key_variable<ppT> vk(pb, vk_bits, input_size_in_elts, "translation_step_vk");
  vk.generate_r1cs_witness(*r1cs_vk);

  return new std::vector<FieldT>(vk.all_vars.get_vals(pb));
}

// verification key variable

r1cs_ppzksnark_verification_key_variable<ppT>* camlsnark_mnt6_r1cs_ppzksnark_verification_key_variable_create(
    protoboard<FieldT>* pb,
    pb_variable_array<FieldT>* all_bits,
    int input_size) {
  return new r1cs_ppzksnark_verification_key_variable<ppT>(*pb, *all_bits, input_size, "verification_key_variable");
}

int camlsnark_mnt6_r1cs_ppzksnark_verification_key_variable_size_in_bits_for_input_size(int input_size) {
  return r1cs_ppzksnark_verification_key_variable<ppT>::size_in_bits(input_size);
}

void camlsnark_mnt6_r1cs_ppzksnark_verification_key_variable_delete(
    r1cs_ppzksnark_verification_key_variable<ppT>* vk) {
  delete vk;
}

void camlsnark_mnt6_r1cs_ppzksnark_verification_key_variable_generate_r1cs_constraints(
    r1cs_ppzksnark_verification_key_variable<ppT>* vk) {
  vk->generate_r1cs_constraints(false);
}

void camlsnark_mnt6_r1cs_ppzksnark_verification_key_variable_generate_r1cs_witness(
    r1cs_ppzksnark_verification_key_variable<ppT>* vkv,
    r1cs_ppzksnark_verification_key<other_curve_ppT>* vk) {
  vkv->generate_r1cs_witness(*vk);
}

// proof variable

r1cs_ppzksnark_proof_variable<ppT>* camlsnark_mnt6_r1cs_ppzksnark_proof_variable_create(
    protoboard<FieldT>* pb) {
  return new r1cs_ppzksnark_proof_variable<ppT>(*pb, "proof_variable");
}

void camlsnark_mnt6_r1cs_ppzksnark_proof_variable_delete(
    r1cs_ppzksnark_proof_variable<ppT>* p) {
  delete p;
}

void camlsnark_mnt6_r1cs_ppzksnark_proof_variable_generate_r1cs_constraints(
    r1cs_ppzksnark_proof_variable<ppT>* p) {
  p->generate_r1cs_constraints();
}

void camlsnark_mnt6_r1cs_ppzksnark_proof_variable_generate_r1cs_witness(
    r1cs_ppzksnark_proof_variable<ppT>* pv,
    r1cs_ppzksnark_proof<other_curve_ppT>* p) {
  pv->generate_r1cs_witness(*p);
}

// verifier gadget

r1cs_ppzksnark_verifier_gadget<ppT>* camlsnark_mnt6_r1cs_ppzksnark_verifier_gadget_create(
    protoboard<FieldT>* pb,
    r1cs_ppzksnark_verification_key_variable<ppT>* vk,
    pb_variable_array<FieldT>* input,
    int elt_size,
    r1cs_ppzksnark_proof_variable<ppT>* proof,
    pb_variable<FieldT>* result) {
  return new r1cs_ppzksnark_verifier_gadget<ppT>(*pb, *vk, *input, elt_size, *proof, *result, "verifier_gadget");
}

void camlsnark_mnt6_r1cs_ppzksnark_verifier_gadget_delete(
    r1cs_ppzksnark_verifier_gadget<ppT>* g) {
  delete g;
}

void camlsnark_mnt6_r1cs_ppzksnark_verifier_gadget_generate_r1cs_constraints(
    r1cs_ppzksnark_verifier_gadget<ppT>* g) {
  g->generate_r1cs_constraints();
}

void camlsnark_mnt6_r1cs_ppzksnark_verifier_gadget_generate_r1cs_witness(
    r1cs_ppzksnark_verifier_gadget<ppT>* g) {
  g->generate_r1cs_witness();
}

}
