
interface Value  { }
interface Future { Put(Value!), Get(User!) }
interface User   { Reply(Value!) }

process Future(self : Future?) =
  case self ? Put·*Get of
  { Put(x) ▸ ResolvedFuture(self, x) }

process ResolvedFuture(self : Future?, x : Value!) =
  case self ? *Get of
  { Put(x)    ▸ self?fail "future resolved twice"
  & Get(user) ▸ user!Reply(x) | ResolvedFuture(self, x)
  & free      ▸ done }

process User(self : User?, future : Future!) =
  { future!Get(self)
  | case self ? Reply of
    { Reply(x) ▸ self?free.done } }

process Main =
  new future : Future in
  new v : Value in
  new user₁ : User in
  new user₂ : User in
  { v?free.done
  | Future(future)
  | future!Put(v)
  | User(user₁, future)
  | User(user₂, future) }