package com.marketmaker.formula

/**
 * Created by wacharint on 5/19/15.
 */
trait ObservedVariable {

    val spreadTransitionMatrix = Map(
        (1,1) -> 0.0, (1,2) -> 0.0, (1,3) -> 0.0,
        (2,1) -> 0.0, (2,2) -> 0.0, (2,3) -> 0.0,
        (3,1) -> 0.0, (3,2) -> 0.0, (3,3) -> 0.0
    )
}
