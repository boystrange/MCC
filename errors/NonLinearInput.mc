
interface A { }

process Main =
  new a : A in
  { a?free.done
  | a?free.done }
