theory Thm
  imports Main Term Properties Context
begin

record ctyp =
  cert_E :: certificate
  T_E :: "typ"
  maxidx_E :: nat
  sorts_E :: "sort set"

record cterm =
  cert_E :: certificate
  t_E :: "term"
  T_E :: "typ"
  maxidx_E :: nat
  sorts_E :: "sort set"

record "thm" =
  cert_E :: certificate
  tags_E :: properties
  maxidx_E :: nat
  shyps_E :: "sort set"
  hyps_E :: "term set"
  (*no ff?*)
  prop_E :: "term"

print_record "thm"

fun add_inst :: "((indexname \<times> typ) \<times> cterm) \<Rightarrow> (certificate \<times> sort set) \<Rightarrow> ((indexname \<times> typ) \<times> (term \<times> nat)) \<times>
        (certificate \<times> sort set)" where
  "add_inst ((idn,ty),ct) (cert,sorts) = (((idn,ty), (t_E ct, 0)), (cert,sorts))"

fun instantiate :: "((indexname \<times> sort) \<times> ctyp) list \<times> ((indexname \<times> typ) \<times> cterm) list \<Rightarrow>
    thm \<Rightarrow> thm" where
  "instantiate _ x = x"

end