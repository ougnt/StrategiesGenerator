package com.marketmaker.math

/**
 * Created by wacharint on 5/19/15.
 */
class Superp {

    def getMax(values : Seq[(Int,Double)]): (Int,Double) = {

        val sortedValues = values.sortBy(v => v._2)
        sortedValues.last
    }

    def getMin(values : Seq[(Int,Double)]): (Int,Double) = {

        val sortedValues = values.sortBy(v => v._2)
        sortedValues(0)
    }
}
