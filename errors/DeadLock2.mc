// typical case of mutual dependencies between two mailboxes. One
// process sends message m to b only after it has received m from a,
// the other does the opposite

interface A { M }

process Main =
  new a : A in
  new b : A in
  { case a ? M of
    { M ▸ a?free.b!M }
  | case b ? M of
    { M ▸ b?free.a!M } }