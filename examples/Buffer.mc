interface Buffer { Put(User!), Get(User!) }
interface User { ReplyPut(Buffer!), ReplyGet(Buffer!) }

process Buffer(self : Buffer?) =
  case self ? Put of
  { Put(user) ▸
    { user!ReplyPut(self)
    | case self ? Get of
      { Get(user) ▸ user!ReplyGet(self) | Buffer(self) } } }

process User(self : User?, buffer : Buffer!) =
  { buffer!Put(self)
  | case self ? ReplyPut of
    { ReplyPut(buffer) ▸
      { buffer!Get(self)
      | case self ? ReplyGet of
        { ReplyGet(buffer) ▸ User(self, buffer) } } } }

process Main =
  new buffer : Buffer in
  new user : User in
  { Buffer(buffer) | User(user, buffer) }