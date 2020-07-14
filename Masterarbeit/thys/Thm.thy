theory Thm
imports Main Term
begin

record ctyp = 
  T :: "typ"
  maxidx :: nat

record cterm = 
  t :: "term"
  T :: "typ"
  maxidx :: nat

record "thm" =
  "prop" :: "term"


fun instantiate :: "((indexname \<times> sort) \<times> ctyp) list \<times> ((indexname \<times> typ) \<times> cterm) list \<Rightarrow>
    thm \<Rightarrow> thm" where
"instantiate _ x = x"

end