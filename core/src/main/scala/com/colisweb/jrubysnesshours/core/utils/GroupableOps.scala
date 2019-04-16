package com.colisweb.jrubysnesshours.core.utils

import scala.collection.generic.CanBuildFrom
import scala.collection.{GenTraversableOnce, mutable}

object GroupableOps {

  /**
    * Comes from https://gist.github.com/guizmaii/12d19bda2d97878a0ea6549752931bbe
    *
    * TODO: To delete when Updated to Scala 2.13
    */
  private[core] implicit final class ToGroupable[A, Coll[X] <: GenTraversableOnce[X]](coll: Coll[A]) {

    // https://github.com/scala/scala/blob/v2.13.0-M5/src/library/scala/collection/Iterable.scala#L578
    def groupMap[K, B, To](key: A => K)(f: A => B)(implicit bf: CanBuildFrom[Coll[A], B, To]): Map[K, To] = {
      val m = mutable.Map.empty[K, mutable.Builder[B, To]]
      for (elem <- coll) {
        val k    = key(elem)
        val bldr = m.getOrElseUpdate(k, bf())
        bldr += f(elem)
      }
      var result = Map.empty[K, To]
      for ((k, v) <- m) {
        result = result + ((k, v.result()))
      }
      result
    }
  }
}
