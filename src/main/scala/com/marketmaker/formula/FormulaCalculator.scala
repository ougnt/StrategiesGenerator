package com.marketmaker.formula

/**
 * Created by wacharint on 5/19/15.
 */

class FormulaCalculator extends ObservedVariable {

    //val phys = Map[Int,Double]( 1 -> 10.1, 2 -> 11.2, 3 -> 12.3)

    def valueOfSpread(currentSpread: Int, phys : Map[Int,Double], spreadChange : Map[(Int,Int),Double] = spreadTransitionMatrix) : Double = {

        var ret : Double = 0
        var j : Int = 1

        for( j <- 1 to 3){
            ret = ret + ( spreadChange((currentSpread, j)) * (phys(j) - phys(currentSpread)) )
        }

        BigDecimal.decimal(ret).setScale(8, BigDecimal.RoundingMode.HALF_UP).doubleValue()
    }
}
