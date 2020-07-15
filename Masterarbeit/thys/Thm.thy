theory Thm
  imports Main Term Properties Context
begin

record ctyp =
  cert :: certificate
  T :: "typ"
  maxidx :: nat
  sorts :: "sort set"

record cterm =
  cert :: certificate
  t :: "term"
  T :: "typ"
  maxidx :: nat
  sorts :: "sort set"

record "thm" =
  cert :: certificate
  tags :: properties
  maxidx :: nat
  shyps :: "sort set"
  hyps :: "term set"
  (*no ff?*)
  "prop" :: "term"


fun instantiate :: "((indexname \<times> sort) \<times> ctyp) list \<times> ((indexname \<times> typ) \<times> cterm) list \<Rightarrow>
    thm \<Rightarrow> thm" where
  "instantiate _ x = x"

end