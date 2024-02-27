
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

process Main =
  new future : Future in
  new user : User in
  { future!Get(user)
  | Future(future)
  | case user ? Reply of
    { Reply(x) ▸ future!Put(x) | user?free.done } }
