package com.marketmaker.formula

/**
 * Created by wacharint on 5/19/15.
 */
trait MarketParameters {

    val tickSize = 0.1
    val decimalPoint = 8
    val riskAverseLevel = 0.00001
    val maximumNumberOfContract = 10
    val marketClockInterval = 500

    val spreadTransitionMatrix = Map(
        (1,1) -> 0.87473904, (1,2) -> 0.08752803, (1,3) -> 0.03773293,
        (2,1) -> 0.20283551, (2,2) -> 0.73497664, (2,3) -> 0.06218785,
        (3,1) -> 0.08581477, (3,2) -> 0.11957796, (3,3) -> 0.79460727
    )

    val lamdaTable = Map(
        ("BID",0) -> 1.0, ("BID",1) -> 0.31821784, ("BID",2) -> 0.15232896, ("BID",3) -> 0.10654222,
        ("ASK",0) -> 1.0, ("ASK",1) -> 0.25398433, ("ASK",2) -> 0.12151096, ("ASK",3) -> 0.04741569
    )
}
