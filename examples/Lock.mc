// Two users compete for acquiring a lock. The notification that
// they have acquired the lock is sent to their mailbox

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

process User(self : User?, lock : Lock!) =
  { lock!Acquire(self)
  | case self ? Reply of
    { Reply(lock) ▸ lock!Release | self?free.done } }

process Main =
  new lock : Lock in
  new user₁ : User in
  new user₂ : User in
  { Lock(lock) | User(user₁, lock) | User(user₂, lock) }
