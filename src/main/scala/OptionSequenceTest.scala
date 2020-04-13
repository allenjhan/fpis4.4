object OptionSequenceTest extends App {
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    def inner(l: List[Option[A]], acc: Option[List[A]]): Option[List[A]] = {
      l match {
        case List() => acc
        case x::xs => x match {
          case None => None
          case Some(value) => inner(xs, acc.map(z => value :: z ))
        }
      }
    }
    inner(a, Some(List())).map(z => z.reverse)
  }


  println(sequence(List(Some(1),Some(2), Some(3))))
  println(sequence(List(Some(1),Some(2), Some(3), None)))
}
