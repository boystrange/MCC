
interface Server { Print() }
interface Choice { }

process Server(self : Server?) =
  case self ? *Print of
  { free  ▸ done
  & Print ▸ Server(self) }

process Client(server : Server!) =
  new random : Choice in
  { case random ? 1 of
    { free ▸ done
    & free ▸ server!Print | Client(server) } }
