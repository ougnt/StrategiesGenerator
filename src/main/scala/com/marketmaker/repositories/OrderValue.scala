package com.marketmaker.repositories

/**
 * Created by wacharint on 5/28/15.
 */
class OrderValue(timeOfValue : Int,
                 inventoryOfValue : Int,
                 spreadOfValue : Byte,
                 initialLimitOrderValue : Double,
                 initialMarketOrderValue : Double,
                 initialLimitBuyType : Int,
                 initialLimitBuySize : Int,
                 initialLimitSellType : Int,
                 initialLimitSellSize : Int,
                 initialMarketType : Int,
                 initialMarketSize : Int) {

    val time = timeOfValue
    val inventory = inventoryOfValue
    val spread = spreadOfValue
    val limitOrderStrategyValue = initialLimitOrderValue
    val marketOrderStrategyValue = initialMarketOrderValue
    val limitBuyType = initialLimitBuyType
    val limitBuySize = initialLimitBuySize
    val limitSellType = initialLimitSellType
    val limitSellSize = initialLimitSellSize
    val marketType = initialMarketType
    val marketSize = initialMarketSize
}
