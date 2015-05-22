package test

import com.marketmaker.math.MathHelper
import org.specs2.mutable.Specification

/**
 * Created by wacharint on 5/19/15.
 */
class SuperpSpec extends Specification with TestConfiguration {

    sequential

    """GetMax""" should {

        """return the maximum value""" in {

            // Setup
            val data = Map[(Int,Int),Double]((1,2) -> 2,(3,3) -> 10,(3,4) -> 4)
            val sup = new MathHelper

            // Execute
            val ret = sup.getMax(data)

            // Verify
            ret mustEqual (3,3)
            data(ret) mustEqual 10
        }
    }

    """GetMin""" should {

        """return the minimum value""" in {

            // Setup
            val data = Map[(Int,Int),Double]((1,2) -> 2,(3,3) -> 10,(3,4) -> 4)
            val sup = new MathHelper

            // Execute
            val ret = sup.getMin(data)

            // Verify
            ret mustEqual (1,2)
            data(ret) mustEqual 2
        }
    }
}
