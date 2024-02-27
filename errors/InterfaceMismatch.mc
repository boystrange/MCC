
interface A { X }

interface B { Y }

interface C { M(B?) }

process Main =
  new a : A in
  new c : C in
  { c!M(a)
  | case c ? M of
    { M(x) ▸ x?free.c?free.done } }
