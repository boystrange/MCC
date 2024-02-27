
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

process User(self : User?) =
  case self ? *Reply of
  { free ▸ done
  & Reply(lock) ▸ lock!Release | User(self) }

process Main =
  new lock : Lock in
  new user : User in
  { Lock(lock)
  | lock!Acquire(user)
  | lock!Acquire(user)
  | User(user) }
