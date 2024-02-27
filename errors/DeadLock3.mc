// this example illustrates why the multiplicity of dependencies is
// important: the *same* dependency between a and b arises twice,
// which may lead to a deadlock

interface A { M₁(B!), M₂(B?) }
interface B { M(B?) }

process Main =
  new a : A in
  new b : B in
  { a!M₁(b)
  | a!M₂(b)
  | case a ? M₁·M₂ of
    { M₁(x) ▸ case a ? M₂ of
      	      { M₂(y) ▸ a?free.x!M(y) } } }