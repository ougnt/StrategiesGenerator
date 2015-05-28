package com.marketmaker.repositories

/**
 * Created by wacharint on 5/28/15.
 */
class OrderValue(timeOfValue : Int, inventoryOfValue : Int, spreadOfValue : Byte, initialLimitOrderValue : Double, initialMarketOrderValue : Double) {

    val time = timeOfValue
    val inventory = inventoryOfValue
    val spread = spreadOfValue
    val limitOrderStrategyValue = initialLimitOrderValue
    val marketOrderStrategyValue = initialMarketOrderValue
}
