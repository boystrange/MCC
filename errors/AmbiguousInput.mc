
interface A { A, B, C }

process Main =
  new u : A in
  { u!A | u!B
  | case u ? (A·B + C·B) of
    { A ▸ case u ? B of
      	  { B ▸ u?free.done }
    & B ▸ case u ? C of
      	  { C ▸ u?free.done } } }