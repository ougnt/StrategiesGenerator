/**
 * Created by wacharint on 5/17/15.
 */

package test

import com.marketmaker.helper.RepositoryHelper
import com.marketmaker.repositories.{OrderValue, Phy}
import org.specs2.mutable._

class HelperSpec extends Specification with TestConfiguration {

    sequential

    """save to phy table""" should {

        """save the data when reach the interval""" in {

            // setup
            val repositoryHelper = new RepositoryHelper
            val phys = Seq[Phy](new Phy(1000, 1, 1, 10), new Phy(1000, 2, 1, 20), new Phy(1000, 3, 1, 30))
            repositoryHelper.deleteAllPhy

            // execute
            phys.foreach(phy => repositoryHelper.addPhy(phy.time, phy.inv, 1, phy.value))

            // verify
            val result = repositoryHelper.getPhys(Seq[(Int,Int,Byte)](
                (1000,1,1),
                (1000,2,1),
                (1000,3,1)
            ))

            result.size mustEqual 3
            result.filter(r => r.time == 1000 && r.inv == 1)(0).value mustEqual 10
            result.filter(r => r.time == 1000 && r.inv == 2)(0).value mustEqual 20
            result.filter(r => r.time == 1000 && r.inv == 3)(0).value mustEqual 30
            repositoryHelper.orderPhyQueue.size mustEqual 0
        }
        """not save the data when haven't reach the interval""" in {


            // setup
            val repositoryHelper = new RepositoryHelper
            val phys = Seq[Phy](new Phy(1000, 1, 1, 10), new Phy(1000, 2, 1, 20))
            repositoryHelper.deleteAllPhy

            // execute
            phys.foreach(phy => repositoryHelper.addPhy(phy.time, phy.inv, 1, phy.value))

            // verify
            val result = repositoryHelper.getPhys(Seq[(Int,Int,Byte)](
                (1000,1,1),
                (1000,2,1)
            ))

            result.size mustEqual 0
            repositoryHelper.orderPhyQueue.size mustEqual 2
        }
    }

    """save to orderValue table""" should {

        """save the data when reach the interval""" in {

            // setup
            val repositoryHelper = new RepositoryHelper
            val orderValues = Seq[OrderValue](new OrderValue(1000, 1, 1, 10, 11), new OrderValue(1000, 2, 1, 20, 22), new OrderValue(1000, 3, 1, 30, 33))
            repositoryHelper.deleteAllPhy

            // execute
            orderValues.foreach(value => repositoryHelper.addOrderValue(value))

            // verify
            val result = repositoryHelper.getOrderValue(Seq[(Int,Int,Byte)](
                (1000,1,1),
                (1000,2,1),
                (1000,3,1)
            ))

            result.size mustEqual 3
            result.filter(r => r.time == 1000 && r.inventory == 1).head.limitOrderStrategyValue mustEqual 10
            result.filter(r => r.time == 1000 && r.inventory == 1).head.marketOrderStrategyValue mustEqual 11
            result.filter(r => r.time == 1000 && r.inventory == 2).head.limitOrderStrategyValue mustEqual 20
            result.filter(r => r.time == 1000 && r.inventory == 2).head.marketOrderStrategyValue mustEqual 22
            result.filter(r => r.time == 1000 && r.inventory == 3).head.limitOrderStrategyValue mustEqual 30
            result.filter(r => r.time == 1000 && r.inventory == 3).head.marketOrderStrategyValue mustEqual 33
            repositoryHelper.orderOrderValueQueue.size mustEqual 0
        }
        """not save the data when haven't reach the interval""" in {


            // setup
            val repositoryHelper = new RepositoryHelper
            val orderValues = Seq[OrderValue](new OrderValue(1000, 1, 1, 10, 11), new OrderValue(1000, 2, 1, 20, 22))
            repositoryHelper.deleteAllPhy

            // execute
            orderValues.foreach(value => repositoryHelper.addOrderValue(value))

            // verify
            val result = repositoryHelper.getPhys(Seq[(Int,Int,Byte)](
                (1000,1,1),
                (1000,2,1)
            ))

            result.size mustEqual 0
            repositoryHelper.orderOrderValueQueue.size mustEqual 2
        }
    }
}
