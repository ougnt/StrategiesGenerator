package com.marketmaker.repositories

/**
 * Created by wacharint on 5/20/15.
 */
class Strategy {

    var bidLimitORder : Order = null
    var askLimitOrder : Order = null
    var marketOrder : Order = null

}

class Order(size : Int, position : Int) {

    var orderSize : Int = size
    var orderPosition : Int = position
}

object Strategy {

    lazy val MarketBuyOrder = 1
    lazy val MarketSellOrder = 2
    lazy val LimitBuyOrderAtTheMarket = 3
    lazy val LimitBuyOrderAtTheMarketPlusOneSpread = 4
    lazy val LimitSellOrderAtTheMarket = 5
    lazy val LimitSellOrderAtTheMarketMinusOneSpread = 6
}
