package com.marketmaker.formula

/**
 * Created by wacharint on 5/19/15.
 */

import com.marketmaker.helper.{Configuration, RepositoryHelper}
import com.marketmaker.math._
import com.marketmaker.repositories.{Order, Strategy, Phy}

trait FormulaCalculatorTrait extends MarketParameters {

    val mathHelper = new MathHelper

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

    protected def valueOfMarketOrder(phyBeforeTheOrderMatch : Double,
                                        phyWhenTheOrderMatch : Double,
                                        orderSize : Int)
                                    (implicit currentSpread : Byte) : Double = {

        phyBeforeTheOrderMatch - phyWhenTheOrderMatch - ( currentSpread * tickSize * orderSize / 2 )
    }

    protected def valueOfInventoryPunishment(currentInventory : Int) : Double = {

        riskAverseLevel * currentInventory * currentInventory
    }

    def addPhyAtTerminal(implicit currentSpread : Byte, databaseName : String, databaseSaveInterval : Short)  = {

        for( inv <- -maximumNumberOfContract to maximumNumberOfContract) {

            RepositoryHelper.addPhy(time = 0, inv, - math.abs(inv) * currentSpread * tickSize / 2)
        }

        RepositoryHelper.forceUpdate
    }
}

class FormulaCalculator extends FormulaCalculatorTrait with Configuration {

    def supBuyLimitOrder(currentHoldingInventory : Int)
                        (implicit currentSpread : Byte, currentTime : Int) : (Order,Double) = {
        supLimitOrder(currentHoldingInventory, isBidOrder = true)
    }

    def supSellLimitOrder(currentHoldingInventory : Int)
                        (implicit currentSpread : Byte, currentTime : Int) : (Order,Double) = {
        supLimitOrder(currentHoldingInventory, isBidOrder = false)
    }

    private def supLimitOrder(currentHoldingInventory : Int, isBidOrder : Boolean)
                     (implicit currentSpread : Byte, currentTime : Int) : (Order,Double) = {

        val side = if(isBidOrder) "BID" else "ASK"
        var interestedTimeAndInventory = Seq[(Int,Int)]()
        var strategies = Map[(Int,Int), Double]()
        val lamdaAtTheMarket : Double = lamdaTable(side, currentSpread)
        val lamdaAtTheMarketPlus : Double = lamdaTable(side, currentSpread - 1)

        for(targetInventory <- -maximumNumberOfContract to maximumNumberOfContract) {

            interestedTimeAndInventory = interestedTimeAndInventory ++ Seq[(Int,Int)]((currentTime, targetInventory))
        }

        val phys = RepositoryHelper.getStrategies(interestedTimeAndInventory)

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

    private def getStrategies(strategies : Map[(Int,Int) , Double],
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
}
