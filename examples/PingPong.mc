interface Pinger { Pong(Ponger!) }
interface Ponger { Ping(Pinger!) }

process Pong(self : Ponger?) =
  case self ? *Ping of
  { free ▸ done
  & Ping(pinger) ▸ pinger!Pong(self) | Pong(self) }

process Ping(self : Pinger?, ponger : Ponger!) =
  { ponger!Ping(self)
  | case self ? Pong of
    { Pong(ponger) ▸ Ping(self, ponger) } }