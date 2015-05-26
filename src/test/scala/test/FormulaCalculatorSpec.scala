package test

import java.math.MathContext

import com.marketmaker.formula.{FormulaCalculatorTrait, FormulaCalculator}
import com.marketmaker.helper.RepositoryHelper
import com.marketmaker.math.MathHelper
import com.marketmaker.repositories.{Strategy, Order}
import org.specs2.matcher.Matcher
import org.specs2.mutable.Specification

/**
 * Created by wacharint on 5/19/15.
 */
class FormulaCalculatorSpec extends Specification with TestObservedValue with TestConfiguration {

    val mathHelper = new MathHelper
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
            RepositoryHelper.deleteAllPhy
            val calculator = new FormulaCalculator
            implicit val currentSpread : Byte = 1

            // Execute
            calculator.addPhyAtTerminal

            // Verify
            var interestedPhys = Seq[(Int,Int,Byte)]()
            for(inv : Int <- -maximumNumberOfContract to maximumNumberOfContract) {

                interestedPhys = interestedPhys ++ Seq[(Int,Int,Byte)]((0,inv,currentSpread))
            }
            val result = RepositoryHelper.getPhys(interestedPhys)

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
}
