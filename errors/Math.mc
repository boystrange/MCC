
interface Int { Succ }

process Drop(x : Int?) =
  case x ? *Succ of
  { free ▸ done
  & Succ ▸ Drop(x) }

process Copy(x : Int?, y : Int!) =
  case x ? *Succ of
  { free ▸ done
  & Succ ▸ y!Succ | Copy(x, y) }

process Dup(x : Int?, y : Int!, z : Int!) =
  case x ? *Succ of
  { free ▸ done
  & Succ ▸ y!Succ | z!Succ | Dup(x, y, z) }

process Add(x : Int?, y : Int?, z : Int!) =
  Copy(x, z) | Copy(y, z)

process Mul(x : Int?, y : Int?, z : Int!) =
  case x ? *Succ of
  { free ▸ Drop(y)
  & Succ ▸ new y' : Int in
    	   { Dup(y, y', z) | Mul(x, y', z) } }

process Fibo(m : Int?, n : Int?, k : Int?, out : Int!) =
  case k ? *Succ of
  { free ▸ Copy(m, out) | Drop(n)
  & Succ ▸ new tmp : Int in
    	   new m' : Int in
	   new n' : Int in
    	   { Dup(n, m', tmp)
	   | Add(m, tmp, n')
	   | Fibo(m', n', k, out) } }