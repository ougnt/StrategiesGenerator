/**
 * Created by wacharint on 5/17/15.
 */

package test

import com.marketmaker.helper.RepositoryHelper
import com.marketmaker.repositories.{MarketMakerStrategy, Strategy, OrderValue, Phy}
import org.specs2.matcher.Matcher
import org.specs2.mutable._

class HelperSpec extends Specification with TestConfiguration with BaseSpec {

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
            val orderValues = Seq[OrderValue](
                new OrderValue(1000, 1, 1, 10, 11, Strategy.LimitBuyOrderAtTheMarket, 10, Strategy.LimitSellOrderAtTheMarket, 20, Strategy.MarketBuyOrder, 30),
                new OrderValue(1000, 2, 1, 20, 22, Strategy.LimitBuyOrderAtTheMarketPlusOneSpread, 100, Strategy.LimitSellOrderAtTheMarketMinusOneSpread, 200, Strategy.MarketSellOrder, 300),
                new OrderValue(1000, 3, 1, 30, 33, Strategy.LimitBuyOrderAtTheMarket, 1000, Strategy.LimitSellOrderAtTheMarket, 2000, Strategy.MarketBuyOrder, 3000))
            repositoryHelper.deleteAllOrderValue

            // execute
            orderValues.foreach(value => repositoryHelper.addOrderValue(value))

            // verify
            val result = repositoryHelper.getOrderValue(Seq[(Int,Int,Byte)](
                (1000,1,1),
                (1000,2,1),
                (1000,3,1)
            ))

            result.size mustEqual 3
            val inv1Res = result.find(r => r.time == 1000 && r.inventory == 1 && r.spread == 1).get
            val inv2Res = result.find(r => r.time == 1000 && r.inventory == 2 && r.spread == 1).get
            val inv3Res = result.find(r => r.time == 1000 && r.inventory == 3 && r.spread == 1).get

            inv1Res must beOrderValue(orderValues.find(r => r.time == 1000 && r.inventory == 1 && r.spread == 1).get)
            inv2Res must beOrderValue(orderValues.find(r => r.time == 1000 && r.inventory == 2 && r.spread == 1).get)
            inv3Res must beOrderValue(orderValues.find(r => r.time == 1000 && r.inventory == 3 && r.spread == 1).get)

            repositoryHelper.orderOrderValueQueue.size mustEqual 0
        }
        """not save the data when haven't reach the interval""" in {


            // setup
            val repositoryHelper = new RepositoryHelper
            val orderValues = Seq[OrderValue](
                new OrderValue(1000, 1, 1, 10, 11, Strategy.LimitBuyOrderAtTheMarket, 10, Strategy.LimitSellOrderAtTheMarket, 20, Strategy.MarketBuyOrder, 30),
                new OrderValue(1000, 2, 1, 20, 22, Strategy.LimitBuyOrderAtTheMarketPlusOneSpread, 100, Strategy.LimitSellOrderAtTheMarketMinusOneSpread, 200, Strategy.MarketSellOrder, 300)
            )
            repositoryHelper.deleteAllOrderValue

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

    """save to marketMakerStrategy table""" should {

        """save the data when reach the interval""" in {

            // setup
            val repositoryHelper = new RepositoryHelper
            val marketMakerStrategies = Seq[MarketMakerStrategy](
                new MarketMakerStrategy(){
                    time = 0
                    inventory = 1
                    spread = 1
                    limitBuyOrderType = 10
                    limitBuyOrderSize = 11
                    limitSellOrderType = 13
                    limitSellOrderSize = 14
                    marketOrderType = 15
                    marketOrderSize = 16
                    feeStructure = 17},
                new MarketMakerStrategy(){
                    time = 0
                    inventory = 1
                    spread = 2
                    limitBuyOrderType = 20
                    limitBuyOrderSize = 21
                    limitSellOrderType = 23
                    limitSellOrderSize = 24
                    marketOrderType = 25
                    marketOrderSize = 26
                    feeStructure = 27},
                new MarketMakerStrategy(){
                    time = 0
                    inventory = 1
                    spread = 3
                    limitBuyOrderType = 30
                    limitBuyOrderSize = 31
                    limitSellOrderType = 33
                    limitSellOrderSize = 34
                    marketOrderType = 35
                    marketOrderSize = 36
                    feeStructure = 37}
            )
            repositoryHelper.deleteAllMarketMakerStrategy

            // execute
            marketMakerStrategies.foreach(s => repositoryHelper.addMarketMakerStrategy(s))

            // verify
            val result = repositoryHelper.getMarketMakerStrategy()

            result.size mustEqual 3
            val inv1Res = result.find(r => r.time == 0 && r.inventory == 1 && r.spread == 1).get
            val inv2Res = result.find(r => r.time == 0 && r.inventory == 1 && r.spread == 2).get
            val inv3Res = result.find(r => r.time == 0 && r.inventory == 1 && r.spread == 3).get

            inv1Res must beMarketMakerStrategy(marketMakerStrategies.find(r => r.time == 0 && r.inventory == 1 && r.spread == 1).get)
            inv2Res must beMarketMakerStrategy(marketMakerStrategies.find(r => r.time == 0 && r.inventory == 1 && r.spread == 2).get)
            inv3Res must beMarketMakerStrategy(marketMakerStrategies.find(r => r.time == 0 && r.inventory == 1 && r.spread == 3).get)

            repositoryHelper.orderMarketMakerStrategyQueue.size mustEqual 0
        }
        """not save the data when haven't reach the interval""" in {


            // setup
            val repositoryHelper = new RepositoryHelper
            val marketMakerStrategies = Seq[MarketMakerStrategy](
                new MarketMakerStrategy(){
                    time = 0
                    inventory = 1
                    spread = 1
                    limitBuyOrderType = 10
                    limitBuyOrderSize = 11
                    limitSellOrderType = 13
                    limitSellOrderSize = 14
                    marketOrderType = 15
                    marketOrderSize = 16
                    feeStructure = 17},
                new MarketMakerStrategy(){
                    time = 0
                    inventory = 1
                    spread = 2
                    limitBuyOrderType = 20
                    limitBuyOrderSize = 21
                    limitSellOrderType = 23
                    limitSellOrderSize = 24
                    marketOrderType = 25
                    marketOrderSize = 26
                    feeStructure = 27}
            )
            repositoryHelper.deleteAllMarketMakerStrategy

            // execute
            marketMakerStrategies.foreach(s => repositoryHelper.addMarketMakerStrategy(s))

            // verify
            val result = repositoryHelper.getMarketMakerStrategy()

            result.size mustEqual 0
            repositoryHelper.orderMarketMakerStrategyQueue.size mustEqual 2
        }
    }
}
