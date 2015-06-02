package com.marketmaker.formula

/**
 * Created by wacharint on 5/19/15.
 */

import java.util.Date

import com.marketmaker.helper.{Configuration, RepositoryHelper}
import com.marketmaker.math._
import com.marketmaker.repositories.{OrderValue, Order, Strategy, Phy}

trait FormulaCalculatorTrait extends MarketParameters {

    val mathHelper = new MathHelper
    var repositoryHelper = new RepositoryHelper

    protected def valueOfSpread(phys : Map[Int,Double],
                      spreadChange : Map[(Int,Int),Double] = spreadTransitionMatrix)
                     (implicit currentSpread: Byte) : Double = {

        var ret : Double = 0
        var j : Int = 1

        for( j <- 1 to 3){
            ret = ret + ( spreadChange((currentSpread, j)) * (phys(j) - phys(currentSpread)) )
        }

        mathHelper.round(ret, decimalPoint)
    }

    protected def valueOfLimitOrder(lamdaValue : Double,
                                    phyIfTheOrderMatched : Double,
                                    phyBeforTheOrderMatched : Double,
                                    orderSize : Int ,
                                    isAggressive : Boolean)
                            (implicit currentSpread: Byte) : Double = {

        val ret = if(isAggressive) {

            lamdaValue * (
                phyIfTheOrderMatched
                    - phyBeforTheOrderMatched
                    + ( orderSize * (( currentSpread * tickSize / 2 ) - tickSize ) )
                )
        } else {

            lamdaValue * (
                phyIfTheOrderMatched
                    - phyBeforTheOrderMatched
                    + ( currentSpread * tickSize / 2 * orderSize ) )
        }

        mathHelper.round(ret, decimalPoint)
    }

    protected def valueOfMarketOrder(phyWhenTheOrderMatch : Double,
                                        orderSize : Int)
                                    (implicit currentSpread : Byte) : Double = {

//        phyBeforeTheOrderMatch - phyWhenTheOrderMatch - ( currentSpread * tickSize * orderSize / 2 )
        phyWhenTheOrderMatch - ( currentSpread * tickSize * math.abs(orderSize) / 2 )
    }

    protected def valueOfInventoryPunishment(currentInventory : Int) : Double = {

        riskAverseLevel * currentInventory * currentInventory
    }

    def addPhyAtTerminal(implicit databaseName : String, databaseSaveInterval : Short)  = {

        for( inv <- -maximumNumberOfContract to maximumNumberOfContract) {

            var currentSpread : Byte = 1
            repositoryHelper.addPhy(time = 0, inv, currentSpread, - math.abs(inv) * currentSpread * tickSize / 2)
            currentSpread = 2
            repositoryHelper.addPhy(time = 0, inv, currentSpread, - math.abs(inv) * currentSpread * tickSize / 2)
            currentSpread = 3
            repositoryHelper.addPhy(time = 0, inv, currentSpread, - math.abs(inv) * currentSpread * tickSize / 2)
        }

        repositoryHelper.forceUpdatePhyTable
    }


    def calculateOrderValue(currentInventory : Short, currentPhys : Seq[Phy], earlierPhy : Phy)
                           (implicit currentTime : Int, currentSpread : Byte, databaseName : String, databaseSavedInterval : Short) = {

        val phyMap = Map[Int, Double](
            1 -> currentPhys.find(p => p.spread == 1 && p.time == currentTime && p.inv == currentInventory).get.value,
            2 -> currentPhys.find(p => p.spread == 2 && p.time == currentTime && p.inv == currentInventory).get.value,
            3 -> currentPhys.find(p => p.spread == 3 && p.time == currentTime && p.inv == currentInventory).get.value
        )
        val calculatedValueOfSpread = valueOfSpread(phyMap, spreadTransitionMatrix)
        val calculatedSupBuyLimitOrder = supBuyLimitOrder(currentInventory)
        val calculatedSupSellLimitOrder = supSellLimitOrder(currentInventory)
        val calculatedInventoryPunishment = inventoryPunishment(currentInventory)

        val calculatedSupMarketOrder = supMarketOrder(currentInventory)

        val valueOfCurrentPhy = currentPhys.find(p => p.spread == currentSpread && p.time == currentTime && p.inv == currentInventory).get.value

        val valueOfLimitOrderStrategy = - (valueOfCurrentPhy - earlierPhy.value) -
            calculatedValueOfSpread -
            calculatedSupBuyLimitOrder._2 -
            calculatedSupSellLimitOrder._2  +
            calculatedInventoryPunishment

        val valueOfMarketOrderStrategy = valueOfCurrentPhy - calculatedSupMarketOrder._2

        repositoryHelper.addOrderValue(new OrderValue(currentTime,
        currentInventory,
        currentSpread,
        valueOfLimitOrderStrategy,
        valueOfMarketOrderStrategy,
        calculatedSupBuyLimitOrder._1.orderPosition,
        calculatedSupBuyLimitOrder._1.orderSize,
        calculatedSupSellLimitOrder._1.orderPosition,
        calculatedSupSellLimitOrder._1.orderSize,
        calculatedSupMarketOrder._1.orderPosition,
        calculatedSupMarketOrder._1.orderSize))
    }

    def calculateAllOrderValue(marketMaxTime : Int)(implicit databaseName : String, databaseSavedInterval : Short) = {

        var time : Int = 0
        for(time <- 0 to marketMaxTime by marketClockInterval) {

            val phyAtTheTimes = repositoryHelper.getPhys(time)
            val phyAtEarlierTimes = repositoryHelper.getPhys(time + 500)
            var inventory : Short = 0
            for(inventory <- -maximumNumberOfContract to maximumNumberOfContract by 1) {

                var spread : Byte = 1
                for(spread <- 1 to 3 by 1) {

                    implicit val currentTime : Int = time
                    implicit val currentSpread : Byte = spread.asInstanceOf[Byte]
                    val phyAtEarlierTIme: Phy = phyAtEarlierTimes.find(p => p.time == time + 500 && p.inv == inventory && p.spread == spread).
                        getOrElse(new Phy(time, inventory, spread.asInstanceOf[Byte], 0))
                    calculateOrderValue(inventory.asInstanceOf[Short],
                        phyAtTheTimes,
                        phyAtEarlierTIme)
                }
            }
            Console.println("%s : Calculating order value Table : %s/%s".format(new Date(), time,marketMaxTime))
        }
    }


    def supBuyLimitOrder(currentHoldingInventory : Int)
                        (implicit currentSpread : Byte, currentTime : Int) : (Order,Double) = {
        supLimitOrder(currentHoldingInventory, isBidOrder = true)
    }

    def supSellLimitOrder(currentHoldingInventory : Int)
                         (implicit currentSpread : Byte, currentTime : Int) : (Order,Double) = {
        supLimitOrder(currentHoldingInventory, isBidOrder = false)
    }

    def inventoryPunishment(currentInventory : Int) : Double = valueOfInventoryPunishment(currentInventory)


    protected def supLimitOrder(currentHoldingInventory : Int, isBidOrder : Boolean)
                             (implicit currentSpread : Byte, currentTime : Int) : (Order,Double)


    protected def getStrategies(strategies : Map[(Int,Int) , Double],
                              isAggressive : Boolean,
                              isBidOrder : Boolean,
                              lamda : Double,
                              phys : Seq[Phy],
                              currentHoldingInventory : Int)
                             (implicit currentSpread : Byte, currentTime : Int) : Map[(Int, Int), Double] = {

        var returnStrategies = strategies
        val targetInventory = 0
        for(targetInventory <- 0 to maximumNumberOfContract - currentHoldingInventory) {

            val inventoryChange = if(isBidOrder) targetInventory else -targetInventory
            val phyBeforeMatch : Phy =
                phys.find(phy => phy.time == currentTime && phy.inv == currentHoldingInventory) orNull

            val phyAfterMatch : Phy =
                phys.find(phy => phy.time == currentTime && phy.inv == currentHoldingInventory + inventoryChange) orNull

            if(phyAfterMatch == null || phyBeforeMatch == null) {

                // the order is not valid, skip to the next loop
            } else {

                val value = valueOfLimitOrder(lamda,
                    phyAfterMatch.value,
                    phyBeforeMatch.value,
                    targetInventory,
                    isAggressive)

                if (isBidOrder && !isAggressive) {

                    returnStrategies += (Strategy.LimitBuyOrderAtTheMarket, targetInventory) -> value
                } else if (isBidOrder && isAggressive) {

                    returnStrategies += (Strategy.LimitBuyOrderAtTheMarketPlusOneSpread, targetInventory) -> value
                } else if (!isBidOrder && isAggressive) {

                    returnStrategies += (Strategy.LimitSellOrderAtTheMarketMinusOneSpread, targetInventory) -> value
                } else {

                    returnStrategies += (Strategy.LimitSellOrderAtTheMarket, targetInventory) -> value
                }
            }
        }

        returnStrategies
    }


    def supMarketOrder(currentHoldingInventory : Int)
                      (implicit currentSpread : Byte, currentTime : Int) : (Order,Double)

}

class FormulaCalculator extends FormulaCalculatorTrait with Configuration {

    def calculateAllPhyTable(endTime : Int) =  {

        addPhyAtTerminal
        val counter : Int = 0
        val endCount = endTime / marketClockInterval
        for(counter <- 0 to endCount) {

            implicit val currentTime : Int = counter * marketClockInterval
            calculatePhyAtEarlyTimes
            Console.println("%s : Calculating Phy Table : %s/%s".format(new Date(), counter,endCount))
        }
    }

    def calculatePhyAtEarlyTimes(implicit currentTime : Int) = {

        // create target intent to get phys
        var targetInventory = -maximumNumberOfContract

        val currentPhys = repositoryHelper.getPhys(currentTime)

        // looping generate phy at earlier time
        for(targetInventory <- -maximumNumberOfContract to maximumNumberOfContract) {

            calculatePhyAtEarlyTime(targetInventory.asInstanceOf[Short], currentPhys)
        }
        repositoryHelper.forceUpdatePhyTable
    }

    def calculatePhyAtEarlyTime(currentInventory : Short, currentPhys : Seq[Phy])(implicit currentTime : Int) = {

        implicit var currentSpread : Byte = 1
        var spread = 1

        for(spread <- 1 to 3) {

            try {
                currentSpread = spread.asInstanceOf[Byte]
                val phys = currentPhys.filter(p => p.time == currentTime && p.inv == currentInventory)

                val currentPhyMap = Map[Int, Double](
                    1 -> phys.find(p => p.spread == 1).get.value,
                    2 -> phys.find(p => p.spread == 2).get.value,
                    3 -> phys.find(p => p.spread == 3).get.value
                )

                val bestLimitBuyOrder = supBuyLimitOrder(currentInventory)
                val bestLimitSellOrder = supSellLimitOrder(currentInventory)

                val valueOfPhyAtTimeBefore = currentPhyMap(currentSpread) + valueOfSpread(currentPhyMap) +
                    bestLimitBuyOrder._2 +
                    bestLimitSellOrder._2 -
                    inventoryPunishment(currentInventory)

                val phyAtTimeBefore: Phy = new Phy(
                    currentTime + marketClockInterval,
                    currentInventory,
                    currentSpread,
                    valueOfPhyAtTimeBefore)

                repositoryHelper.addPhy(phyAtTimeBefore)
            } catch
            {
                case e : Exception => {
                    System.out.println(e.getMessage)
                }
            }
        }
    }

    override def supMarketOrder(currentHoldingInventory : Int)
                               (implicit currentSpread : Byte, currentTime : Int) : (Order,Double) = {

        var strategies = Map[(Int,Int), Double]()
        var orderSize : Int = 0

        val phys = repositoryHelper.getPhys(currentTime)

        val phyBeforeMatch = phys.find(phy => phy.time == currentTime && phy.inv == currentHoldingInventory)

        if(phyBeforeMatch.isEmpty) {
            throw new NullPointerException("""Cannot find the phy with time = %s and inventory = %s""".format(currentTime,
                currentHoldingInventory))
        }

        for(orderSize <- -maximumNumberOfContract to maximumNumberOfContract) {

            if(currentHoldingInventory + orderSize > maximumNumberOfContract || currentHoldingInventory + orderSize < -maximumNumberOfContract) {

                // do nothing
            } else {

                val phyAfterMatch = phys.find(phy => phy.time == currentTime && phy.inv == currentHoldingInventory + orderSize).get
                val orderSide = if(orderSize > 0) Strategy.MarketBuyOrder else Strategy.MarketSellOrder
                val newStrategy = (orderSide, orderSize) -> valueOfMarketOrder(phyAfterMatch.value, math.abs(orderSize))

                strategies = strategies ++ Map[(Int,Int),Double](newStrategy)
            }
        }

        val bestStrategy = mathHelper.getMax(strategies)

        (new Order(math.abs(bestStrategy._2), bestStrategy._1), phyBeforeMatch.get.value - strategies(bestStrategy))
    }

    protected def supLimitOrder(currentHoldingInventory : Int, isBidOrder : Boolean)
                             (implicit currentSpread : Byte, currentTime : Int) : (Order,Double) = {

        val side = if(isBidOrder) "BID" else "ASK"
        var strategies = Map[(Int,Int), Double]()
        val lamdaAtTheMarket : Double = lamdaTable(side, currentSpread)
        val lamdaAtTheMarketPlus : Double = lamdaTable(side, currentSpread - 1)

        val phys = repositoryHelper.getPhys(currentTime)

        strategies = strategies ++ getStrategies(strategies,
            isAggressive = false,
            isBidOrder,
            lamdaAtTheMarket,
            phys,
            currentHoldingInventory)

        strategies = strategies ++ getStrategies(strategies,
            isAggressive = true,
            isBidOrder,
            lamdaAtTheMarketPlus,
            phys,
            currentHoldingInventory)

        val bestStrategy = mathHelper.getMax(strategies)

        (new Order(bestStrategy._2, bestStrategy._1), strategies(bestStrategy))
    }
}
