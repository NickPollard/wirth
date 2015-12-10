package wirth

/*
  A derived value of type T, that is - one that has been automatically derived by the compiler
  through implicit resolution
 */
case class Derived[T](t: T)

object Derived {
  implicit class DerivedOps[T](t: T) {
    def derived : Derived[T] = Derived(t)
  }
}
