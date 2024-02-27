
interface A { }

process Client(self : A?) =
  self?free.done

process Main = done
