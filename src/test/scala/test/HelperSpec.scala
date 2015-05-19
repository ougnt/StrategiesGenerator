/**
 * Created by wacharint on 5/17/15.
 */

package test

import com.marketmaker.helper.Helper
import com.marketmaker.repositories.Phy
import org.specs2.mutable._

class HelperSpec extends Specification with TestConfiguration {

    sequential

    """save to phy table""" should {

        """save the data when reach the interval""" in {

            // setup
            val phys = Seq[Phy](new Phy(1000, 1, 10), new Phy(1000, 2, 20), new Phy(1000, 3, 30))
            Helper.deleteAllPhy

            // execute
            phys.foreach(phy => Helper.addPhy(phy.time, phy.inv, phy.value))

            // verify
            val result = Helper.getStrategies(Seq[(Int,Int)](
                (1000,1),
                (1000,2),
                (1000,3)
            ))

            result.size mustEqual 3
            result.filter(r => r.time == 1000 && r.inv == 1)(0).value mustEqual 10
            result.filter(r => r.time == 1000 && r.inv == 2)(0).value mustEqual 20
            result.filter(r => r.time == 1000 && r.inv == 3)(0).value mustEqual 30
            Helper.orderPhyQueue.size mustEqual 0
        }
        """not save the data when haven't reach the interval""" in {


            // setup
            val phys = Seq[Phy](new Phy(1000, 1, 10), new Phy(1000, 2, 20))
            Helper.deleteAllPhy

            // execute
            phys.foreach(phy => Helper.addPhy(phy.time, phy.inv, phy.value))

            // verify
            val result = Helper.getStrategies(Seq[(Int,Int)](
                (1000,1),
                (1000,2)
            ))

            result.size mustEqual 0
            Helper.orderPhyQueue.size mustEqual 2
        }
    }
}
