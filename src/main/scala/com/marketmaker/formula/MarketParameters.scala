package com.marketmaker.formula

/**
 * Created by wacharint on 5/19/15.
 */
trait MarketParameters {

    val tickSize = 0.1
    val decimalPoint = 8
    val riskAverseLevel = 0.00001
    val maximumNumberOfContract = 10

    val spreadTransitionMatrix = Map(
        (1,1) -> 0.0, (1,2) -> 0.0, (1,3) -> 0.0,
        (2,1) -> 0.0, (2,2) -> 0.0, (2,3) -> 0.0,
        (3,1) -> 0.0, (3,2) -> 0.0, (3,3) -> 0.0
    )

    val lamdaTable = Map(
        ("BID",0) -> 1.0 , ("BID",1) -> 0.3 , ("BID",2) -> 0.2 , ("BID",3) -> 0.1,
        ("ASK",0) -> 1.0 , ("ASK",1) -> 0.3 , ("ASK",2) -> 0.2 , ("ASK",3) -> 0.1
    )
}
