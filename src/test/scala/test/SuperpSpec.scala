package test

import com.marketmaker.math.Superp
import org.specs2.mutable.Specification

/**
 * Created by wacharint on 5/19/15.
 */
class SuperpSpec extends Specification with TestConfiguration {

    sequential

    """GetMax""" should {

        """return the maximum value""" in {

            // Setup
            val data = Seq[(Int,Double)]((1,2),(2,3),(3,4))
            val sup = new Superp

            // Execute
            val ret = sup.getMax(data)

            // Verify
            ret._1 mustEqual 3
            ret._2 mustEqual 4
        }
    }

    """GetMin""" should {

        """return the minimum value""" in {

            // Setup
            val data = Seq[(Int,Double)]((1,2),(2,3),(3,4))
            val sup = new Superp

            // Execute
            val ret = sup.getMin(data)

            // Verify
            ret._1 mustEqual 1
            ret._2 mustEqual 2
        }
    }
}
