
interface Lock { Acquire(User!), Release() }
interface User { Reply(Lock!) }

process Lock(self : Lock?) =
  case self ? *Acquire of
  { free ▸ done
  & Acquire(user) ▸
    { user!Reply(self)
    | case self ? Release·*Acquire of
      { Release ▸ Lock(self) } }
  & Release ▸ self?fail "free lock released" }

process Main =
  new lock : Lock in
  { Lock(lock) | lock!Release }
