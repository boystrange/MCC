
interface A { A, B, C }

process Main =
  new u : A in
  { u!A | u!B
  | case u ? (A·B + C·B) of
    { A ▸ case u ? B of
      	  { B ▸ u?free.done }
    & C ▸ case u ? B of
      	  { B ▸ u?free.done } } }