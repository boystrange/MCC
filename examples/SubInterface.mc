
interface A { M₁, M₂ }
interface B { M₁, M₂, M₃ }
interface Random { }

process P(self : A!) =
  new random : Random in
  case random ? 1 of
  { free ▸ done
  & free ▸ self!M₁ | P(self)
  & free ▸ self!M₂ | P(self) }

process C(self : B?) =
  case self ? *(M₁ + M₂ + M₃) of
  { free ▸ done
  & M₁ ▸ C(self)
  & M₂ ▸ C(self)
  & M₃ ▸ C(self) }

process Main =
  new a : A in
  new b : B in
  { P(a)
  | C(a) // OK because C receives more than what can be stored in a
  | P(b) // OK because P produces less than what can be stored in b
  | C(b) }