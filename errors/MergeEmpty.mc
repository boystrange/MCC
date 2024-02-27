
interface A { A, B }
interface B { }

process Forever(x : A!) = Forever(x)

process Main =
  new mail : A in
  new random : B in
  { Forever(mail)
  | case random ? 1 of
    { free ▸ case mail ? A of
    	     { A ▸ mail?free.done }
    & free ▸ case mail ? B of
    	     { B ▸ mail?free.done } } }
