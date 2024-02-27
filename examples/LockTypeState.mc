// A typestate-oriented lock, where the state of the lock is
// explicitly modeled with two messages sFREE and sBUSY

interface Lock { FREE, BUSY, Acquire(Future!), Release() }
interface Future { Reply(Lock!) }

process Lock(self : Lock?) =
  case self ? *Acquire·(FREE + BUSY·Release) of
  { FREE ▸ case self ? *Acquire of
    	   { free ▸ done
	   & Acquire(user) ▸ user!Reply(self) | self!BUSY | Lock(self)
	   & Release ▸ self?fail "free lock released" }
  & BUSY ▸ case self ? *Acquire·Release of
    	   { Release ▸ self!FREE | Lock(self) } }

process User(lock : Lock!) =
  new future : Future in
  { lock!Acquire(future)
  | case future ? Reply of
    { Reply(lock) ▸ lock!Release | future?free.done } }

process Main =
  new lock : Lock in
  { Lock(lock)
  | lock!FREE
  | User(lock)
  | User(lock)
  | User(lock) }
