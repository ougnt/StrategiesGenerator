package com.marketmaker

import com.marketmaker.formula.FormulaCalculator
import com.marketmaker.helper.Configuration

object MainFunction extends Configuration{
    def main(args: Array[String]): Unit = {

        val calculator = new FormulaCalculator
        calculator.repositoryHelper.deleteAllPhy
        calculator.repositoryHelper.deleteAllOrderValue

        Console.println("Calculate Phy table")
        calculator.calculateAllPhyTable(300000)
        calculator.repositoryHelper.forceUpdatePhyTable

        Console.println("Calculate order value table")
        calculator.calculateAllOrderValue(300000)
        calculator.repositoryHelper.forceUpdateOrderValueTable
   }
}
