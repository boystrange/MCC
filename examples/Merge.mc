
interface A { A, B, C }
interface B { }

process Main =
  new mail : A in
  new random : B in
  { mail!B // can send B because it's in {A,B} ∩ {B,C}
  | case random ? 1 of
    { free ▸ case mail ? A + B of
    	     { A ▸ mail?free.done
	     & B ▸ mail?free.done }
    & free ▸ case mail ? B + C of
    	     { B ▸ mail?free.done
	     & C ▸ mail?free.done } } }