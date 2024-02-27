interface Mailbox { Send(Mailbox?) }

process Main =
  new a : Mailbox in a!Send(a)