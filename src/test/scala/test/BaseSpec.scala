package test

import org.specs2.mutable.Specification
import org.specs2.specification.core.Fragments

/**
 * Created by wacharint on 5/27/15.
 */
trait BaseSpec extends Specification {
    override def map(fs : => Fragments) =
    {
        sequential ^ fs
    }
}
