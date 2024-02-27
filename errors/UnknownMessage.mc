
interface A { }

process Main =
  new a : A in
  { a!M
  | case a ? M of
    { A ▸ a?free.done } }