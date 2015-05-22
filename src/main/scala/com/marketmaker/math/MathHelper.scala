package com.marketmaker.math

/**
 * Created by wacharint on 5/19/15.
 */
class MathHelper {

        def getMax(values : Map[(Int,Int) , Double]): (Int,Int) = {

        val sortedValues = values.toSeq.sortBy(v => v._2)
        sortedValues.last._1
    }

    def getMin(values : Map[(Int,Int) , Double]): (Int,Int) = {

        val sortedValues = values.toSeq.sortBy(v => v._2)
        sortedValues(0)._1
    }

    def round(value : Double, decimal : Int) : Double = {

        BigDecimal.decimal(value).setScale(decimal, BigDecimal.RoundingMode.HALF_UP).doubleValue()
    }
}
