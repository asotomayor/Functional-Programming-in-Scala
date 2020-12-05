trait test {

  implicit def air: Int = 7

  implicit def condense(implicit gas: Int): Float = 2.45673f

  implicit def freeze(implicit liquid: Float): Double = 2.9

  implicitly[Float](air)

}
