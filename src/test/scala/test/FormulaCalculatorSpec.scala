package test

import java.math.MathContext

import com.marketmaker.formula.{FormulaCalculatorTrait, FormulaCalculator}
import com.marketmaker.helper.RepositoryHelper
import com.marketmaker.math.MathHelper
import com.marketmaker.repositories.{OrderValue, Phy, Strategy, Order}
import org.specs2.matcher.Matcher
import org.specs2.mutable.Specification

/**
 * Created by wacharint on 5/19/15.
 */
class FormulaCalculatorSpec extends Specification with TestObservedValue with TestConfiguration with BaseSpec {

    val mathHelper = new MathHelper
    sequential

    """value of the spread function""" should {

        """return the right value when the starting spread = 1""" in {

            // Setup
            val calculator = new TestFormulaCalculator
            val phys = Map[Int,Double]( 1 -> 10.1, 2 -> 11.2, 3 -> 12.3)
            implicit val currentSpread : Byte = 1

            // Execute
            val result = calculator.testValueOfSpread(phys, spreadTransitionMatrix)

            // Verify
            result mustEqual 0.99
        }

        """return the right value when the starting spread = 2""" in {

            // Setup
            val calculator = new TestFormulaCalculator
            val phys = Map[Int,Double]( 1 -> 10.1, 2 -> 11.2, 3 -> 12.3)
            implicit val currentSpread : Byte = 2

            // Execute
            val result = calculator.testValueOfSpread(phys, spreadTransitionMatrix)

            // Verify
            result mustEqual 0.0
        }

        """return the right value when the starting spread = 3""" in {

            // Setup
            val calculator = new TestFormulaCalculator
            val phys = Map[Int,Double]( 1 -> 10.1, 2 -> 11.2, 3 -> 12.3)
            implicit val currentSpread : Byte = 3

            // Execute
            val result = calculator.testValueOfSpread(phys, spreadTransitionMatrix)

            // Verify
            result mustEqual -0.99
        }
    }

    """value of the limit order""" should {

        """be calculated correctly when the order is Aggressive when the current spread = 1""" in {

            // Setup
            val calculator = new TestFormulaCalculator
            implicit val currentSpread : Byte = 1

            // Execute
            val ret = calculator.testValueOfLimitOrder(0.3, 10, 9, 3, isAggressive = true)

            // Verify
            ret mustEqual 0.255
        }

        """be calculated correctly when the order is not Aggressive when the current spread = 1""" in {

            // Setup
            val calculator = new TestFormulaCalculator
            implicit val currentSpread : Byte = 1

            // Execute
            val ret = calculator.testValueOfLimitOrder(0.3, 10, 9, 3, isAggressive = false)

            // Verify
            ret mustEqual 0.345
        }

        """be calculated correctly when the order is Aggressive when the current spread = 2""" in {

            // Setup
            val calculator = new TestFormulaCalculator
            implicit val currentSpread : Byte = 2

            // Execute
            val ret = calculator.testValueOfLimitOrder(0.3, 10, 9, 3, isAggressive = true)

            // Verify
            ret mustEqual 0.3
        }

        """be calculated correctly when the order is not Aggressive when the current spread = 2""" in {

            // Setup
            val calculator = new TestFormulaCalculator
            implicit val currentSpread : Byte = 2

            // Execute
            val ret = calculator.testValueOfLimitOrder(0.3, 10, 9, 3, isAggressive = false)

            // Verify
            ret mustEqual 0.39
        }

        """be calculated correctly when the order is Aggressive when the current spread = 3""" in {

            // Setup
            val calculator = new TestFormulaCalculator
            implicit val currentSpread : Byte = 3

            // Execute
            val ret = calculator.testValueOfLimitOrder(0.3, 10, 9, 3, isAggressive = true)

            // Verify
            ret mustEqual 0.345
        }

        """be calculated correctly when the order is not Aggressive when the current spread = 3""" in {

            // Setup
            val calculator = new TestFormulaCalculator
            implicit val currentSpread : Byte = 3

            // Execute
            val ret = calculator.testValueOfLimitOrder(0.3, 10, 9, 3, isAggressive = false)

            // Verify
            ret mustEqual 0.435
        }
    }

    """the market system""" should {

        """be able to generate all phys at the terminal from a scratch""" in {

            // Setup
            val repositoryHelper = new RepositoryHelper
            repositoryHelper.deleteAllPhy
            val calculator = new FormulaCalculator
            implicit val currentSpread : Byte = 1

            // Execute
            calculator.addPhyAtTerminal

            // Verify
            var interestedPhys = Seq[(Int,Int,Byte)]()
            for(inv : Int <- -maximumNumberOfContract to maximumNumberOfContract) {

                interestedPhys = interestedPhys ++ Seq[(Int,Int,Byte)]((0,inv,currentSpread))
            }
            val result = repositoryHelper.getPhys(interestedPhys)

            result.size mustEqual 21
            result.foreach( res => res.time mustEqual 0)
            result.foreach( res => res.value mustEqual - mathHelper.round(Math.abs(res.inv) * currentSpread * tickSize / 2, 8) )
            success
        }
    }

    """The formula of the limit order position""" should {

        """be able to solved the bid order""" in {

            // Setup
            implicit val currentSpread : Byte = 1
            implicit val currentTime : Int = 0
            val calculator = new FormulaCalculator()
            calculator.addPhyAtTerminal

            // Execute
            val ret = calculator.supBuyLimitOrder(currentHoldingInventory = 1)

            // Verify
            ret._1 must beAppliedToLimitBidOrderConstrains(maximumNumberOfContract, 1)
        }

        """be able to solved the ask order""" in {

            // Setup
            implicit val currentSpread : Byte = 1
            implicit val currentTime : Int = 0
            val calculator = new FormulaCalculator()
            calculator.addPhyAtTerminal

            // Execute
            val ret = calculator.supSellLimitOrder(currentHoldingInventory = 1)

            // Verify
            ret._1 must beAppliedToLimitAskOrderConstrains(maximumNumberOfContract, 1)
        }
    }

    """Value of the market order""" should {

        """be able to correctly calculated""" in {

            // Setup
            val calculator = new TestFormulaCalculator
            implicit val currentSpread : Byte = 1

            // Execute
            val ret = calculator.testValueOfMarketOrder(11, -2)

            // Verify
            ret mustEqual 10.9
        }

        """return the maximum value when sub""" in {

            // Setup
            val calculator = new FormulaCalculator
            implicit val currentTime : Int = 0
            implicit val currentSpread : Byte = 1

            calculator.addPhyAtTerminal

            // Execute
            val ret = calculator.supMarketOrder(5)

            // Verify
            ret._2 mustEqual 0.0
            ret._1.orderSize must beBetween(0, 5)
            ret._1.orderPosition mustEqual Strategy.MarketSellOrder
        }
    }

    """calculate phy at the earlier time function""" should {

        """calculate and add the phy to the database""" in {

            // Setup
            implicit val currentTime : Int = 0
            implicit var currentSpread : Byte = 3
            val calculator = new FormulaCalculator
            val currentInventory : Short = -3

            calculator.repositoryHelper.deleteAllPhy
            calculator.addPhyAtTerminal

            var interestedPhys = Seq[(Int,Int,Byte)]()
            var inv : Int = 0

            for(inv <- -maximumNumberOfContract to maximumNumberOfContract) {

                interestedPhys = interestedPhys ++ Seq[(Int,Int,Byte)](
                    (currentTime,inv,1),
                    (currentTime,inv,2),
                    (currentTime,inv,3))

            }

            val currentPhys = calculator.repositoryHelper.getPhys(interestedPhys)

            // Execute
            calculator.calculatePhyAtEarlyTime(currentInventory.asInstanceOf[Short], currentPhys)
            calculator.repositoryHelper.forceUpdatePhyTable

            // Verify
            val res = calculator.repositoryHelper.getPhys(Seq[(Int,Int,Byte)](
                (500,currentInventory,1),
                (500,currentInventory,2),
                (500,currentInventory,3)
            ))
            res.size mustEqual 3
            res.find(p => p.spread == 1).get.value mustEqual -0.06009
            res.find(p => p.spread == 2).get.value mustEqual -0.18009
            res.find(p => p.spread == 3).get.value mustEqual -0.33009
        }

        """calculate and add the phys of all inventories level to the database""" in {

            // Setup
            val repositoryHelper = new RepositoryHelper
            val calculator = new FormulaCalculator
            implicit val currentTime : Int = 0
            repositoryHelper.deleteAllPhy
            calculator.addPhyAtTerminal

            // Execute
            calculator.calculatePhyAtEarlyTimes

            // Verify
            val res = repositoryHelper.getPhys(currentTime + marketClockInterval)
            res.size mustEqual 63
        }

        """calculate all phy in the table""" in {

            // Setup
            val calculator = new FormulaCalculator
            calculator.repositoryHelper.deleteAllPhy

            // Execute
            calculator.calculateAllPhyTable(2000)

            // Verify
            val res = calculator.repositoryHelper.getPhys()
            res.size mustEqual 378
        }
    }

    """calculate value of the limit order value""" should {

        """provide the correct value""" in {

            // Setting
            implicit val currentTime : Int = 0
            implicit val currentSpread : Byte = 1
            val repositoryHelper = new RepositoryHelper()
            val mockCalculator = new MockFormulaCalculatorForTestCalculateValueOfLimitOrder
            val fakeInventory : Short = 10

            mockCalculator.repositoryHelper = repositoryHelper
            mockCalculator.repositoryHelper.deleteAllPhy
            mockCalculator.repositoryHelper.deleteAllOrderValue
            mockCalculator.addPhyAtTerminal

            val fakeCurrentsPhy = mockCalculator.repositoryHelper.getPhys()
            val fakeEarlierPhy = new Phy(500,fakeInventory,currentSpread,1.0)

            // Execute
            val res = mockCalculator.calculateOrderValue(fakeInventory, fakeCurrentsPhy, fakeEarlierPhy)
            mockCalculator.repositoryHelper.forceUpdateOrderValueTable

            // Verify
            val currentPhyValue = fakeCurrentsPhy.find(p => p.spread == currentSpread && p.inv == fakeInventory).get.value
            val earlierPhyValue = fakeEarlierPhy.value

            val expectedLimitOrderValue = - (currentPhyValue - earlierPhyValue) - 10 - 20 - 30 + 40
            val expectedMarketOrderValue = currentPhyValue - 22
            val resultOrderValue = mockCalculator.repositoryHelper.getOrderValue().head
            val expectedOrderValue = new OrderValue(
                currentTime,
            fakeInventory,
            currentSpread,
            expectedLimitOrderValue,
            expectedMarketOrderValue,
            Strategy.LimitBuyOrderAtTheMarket,
            2,
            Strategy.LimitSellOrderAtTheMarket,
            2,
            Strategy.MarketSellOrder,
            3
            )
            resultOrderValue must beOrderValue(expectedOrderValue)
        }

        """provide all data""" in {

            // Setting
            val repositoryHelper = new RepositoryHelper()
            val calculator = new FormulaCalculator
            calculator.repositoryHelper = repositoryHelper
            calculator.repositoryHelper.deleteAllOrderValue
            databaseSavedInterval = 50
            calculator.calculateAllPhyTable(2000)
            calculator.repositoryHelper.forceUpdatePhyTable

            // Execute
            calculator.calculateAllOrderValue(2000)
            calculator.repositoryHelper.forceUpdateOrderValueTable

            // Verify
            val resOrderValue = calculator.repositoryHelper.getOrderValue()
            resOrderValue.size mustEqual 315
        }
    }

    def beAppliedToLimitBidOrderConstrains(maximumHoldingInventory : Int, currentHoldingInventory : Int) : Matcher[Order] = (source : Order) => (

        source.orderSize <= maximumHoldingInventory &&
        source.orderSize + currentHoldingInventory <= maximumHoldingInventory &&
            (source.orderPosition == Strategy.LimitBuyOrderAtTheMarket ||
                source.orderPosition == Strategy.LimitBuyOrderAtTheMarketPlusOneSpread),
        """Invalid limit bid order generated"""
        )

    def beAppliedToLimitAskOrderConstrains(maximumHoldingInventory : Int, currentHoldingInventory : Int) : Matcher[Order] = (source : Order) => (

        source.orderSize <= maximumHoldingInventory &&
            source.orderSize - currentHoldingInventory <= maximumHoldingInventory &&
            (source.orderPosition == Strategy.LimitSellOrderAtTheMarket ||
                source.orderPosition == Strategy.LimitSellOrderAtTheMarketMinusOneSpread),
        """Invalid limit ask order generated"""
        )
}

class TestFormulaCalculator extends FormulaCalculatorTrait {

    def testValueOfLimitOrder(lamdaValue : Double,
                              phyIfTheOrderMatched : Double,
                              phyBeforTheOrderMatched : Double,
                              orderSize : Int ,
                              isAggressive : Boolean)
                             (implicit currentSpread: Byte) : Double = {

        valueOfLimitOrder(lamdaValue, phyIfTheOrderMatched, phyBeforTheOrderMatched, orderSize, isAggressive)
    }

    def testValueOfSpread(phys : Map[Int,Double],
            spreadChange : Map[(Int,Int),Double] = spreadTransitionMatrix)
        (implicit currentSpread: Byte) : Double = {

        valueOfSpread(phys, spreadChange)
    }

    def testValueOfMarketOrder(phyWhenTheOrderMatch : Double,
                               orderSize : Int)
                              (implicit currentSpread : Byte) : Double = {
        valueOfMarketOrder(phyWhenTheOrderMatch, orderSize)
    }

    override protected def supLimitOrder(currentHoldingInventory: Int, isBidOrder: Boolean)(implicit currentSpread: Byte, currentTime: Int): (Order, Double) = null

    override def supMarketOrder(currentHoldingInventory: Int)(implicit currentSpread: Byte, currentTime: Int): (Order, Double) = ???
}

class MockFormulaCalculatorForTestCalculateValueOfLimitOrder extends FormulaCalculatorTrait {

    override def valueOfSpread(phys : Map[Int,Double],
                               spreadChange : Map[(Int,Int),Double] = spreadTransitionMatrix)
                              (implicit currentSpread: Byte) : Double = 10

    override def supBuyLimitOrder(currentHoldingInventory : Int)
                                 (implicit currentSpread : Byte, currentTime : Int) : (Order,Double) =
        (new Order(2,Strategy.LimitBuyOrderAtTheMarket), 20)

    override def supSellLimitOrder(currentHoldingInventory : Int)
                                 (implicit currentSpread : Byte, currentTime : Int) : (Order,Double) =
        (new Order(2,Strategy.LimitSellOrderAtTheMarket), 30)

    override def inventoryPunishment(currentInventory : Int) : Double = 40

    override def supMarketOrder(currentHoldingInventory : Int)
                               (implicit currentSpread : Byte, currentTime : Int) : (Order,Double) =
        (new Order(3,Strategy.MarketSellOrder), 22)

    override protected def supLimitOrder(currentHoldingInventory: Int, isBidOrder: Boolean)(implicit currentSpread: Byte, currentTime: Int): (Order, Double) = null
}
