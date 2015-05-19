package test

import java.math.MathContext

import com.marketmaker.formula.FormulaCalculator
import org.specs2.mutable.Specification

/**
 * Created by wacharint on 5/19/15.
 */
class FormulaCalculatorSpec extends Specification with TestObservedValue {

    """value of the spread function""" should {

        """return the right value when the starting spread = 1""" in {

            // Setup
            val calculator = new FormulaCalculator
            val phys = Map[Int,Double]( 1 -> 10.1, 2 -> 11.2, 3 -> 12.3)

            // Execute
            val result = calculator.valueOfSpread(1, phys, spreadTransitionMatrix)

            // Verify
            result mustEqual 0.99
        }

        """return the right value when the starting spread = 2""" in {

            // Setup
            val calculator = new FormulaCalculator
            val phys = Map[Int,Double]( 1 -> 10.1, 2 -> 11.2, 3 -> 12.3)

            // Execute
            val result = calculator.valueOfSpread(2, phys, spreadTransitionMatrix)

            // Verify
            result mustEqual 0.0
        }

        """return the right value when the starting spread = 3""" in {

            // Setup
            val calculator = new FormulaCalculator
            val phys = Map[Int,Double]( 1 -> 10.1, 2 -> 11.2, 3 -> 12.3)

            // Execute
            val result = calculator.valueOfSpread(3, phys, spreadTransitionMatrix)

            // Verify
            result mustEqual -0.99
        }
    }
}
