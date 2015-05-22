package test

/**
 * Created by wacharint on 5/19/15.
 */
trait TestObservedValue {

    val tickSize = 0.1
    val decimalPoint = 8
    val riskAverseLevel = 0.00001
    val maximumNumberOfContract = 10

    val spreadTransitionMatrix = Map(
        (1,1) -> 0.4, (1,2) -> 0.3, (1,3) -> 0.3,
        (2,1) -> 0.3, (2,2) -> 0.4, (2,3) -> 0.3,
        (3,1) -> 0.3, (3,2) -> 0.3, (3,3) -> 0.4
    )
}
