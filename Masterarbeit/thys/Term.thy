theory Term
  imports Main HOL.String
begin

type_synonym name = string
type_synonym indexname = "name \<times> nat"

type_synonym "class" = name
type_synonym sort = "class set"


datatype "typ" = 
    Type name "typ list"
  | TFree name sort
  | TVar indexname sort

datatype "term" = 
  Const name "typ"
  | Free name "typ"
  | Var indexname "typ"
  | Bound nat
  | Abs name "typ" "term"
  | App "term" "term" (infixr "$" 65)

value "Const ''n'' (Type ''T'' []) $ Const ''n'' (Type ''T'' [])"

end