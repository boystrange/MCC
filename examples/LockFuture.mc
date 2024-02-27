// Some users compete for acquiring a lock. Each user creates a
// local mailbox into which the notification that they have acquired
// the lock is sent

interface Lock { Acquire(Future!), Release() }
interface Future { Reply(Lock!) }

process Lock(self : Lock?) =
  case self ? *Acquire of
  { free ▸ done
  & Acquire(user) ▸
    { user!Reply(self)
    | case self ? Release·*Acquire of
      { Release ▸ Lock(self) } }
  & Release ▸ self?fail "free lock released" }

process User(lock : Lock!) =
  new future : Future in
  { lock!Acquire(future)
  | case future ? Reply of
    { Reply(lock) ▸ lock!Release | future?free.done } }

process Main =
  new lock : Lock in
  { Lock(lock)
  | User(lock)
  | User(lock)
  | User(lock) }
