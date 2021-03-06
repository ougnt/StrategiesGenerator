package test

import com.marketmaker.repositories.{MarketMakerStrategy, OrderValue}
import org.specs2.matcher.Matcher
import org.specs2.mutable.Specification
import org.specs2.specification.core.Fragments

/**
 * Created by wacharint on 5/27/15.
 */
trait BaseSpec extends Specification {
    override def map(fs : => Fragments) =
    {
        sequential ^ fs
    }


    def beOrderValue(expected : OrderValue) : Matcher[OrderValue] = (src : OrderValue) =>  (

        src.time == expected.time &&
            src.inventory == expected.inventory &&
            src.spread == expected.spread &&
            src.limitOrderStrategyValue == expected.limitOrderStrategyValue &&
            src.marketOrderStrategyValue == expected.marketOrderStrategyValue &&
            src.limitBuyType == expected.limitBuyType &&
            src.limitBuySize == expected.limitBuySize &&
            src.limitSellType == expected.limitSellType &&
            src.limitSellSize == expected.limitSellSize &&
            src.marketType == expected.marketType &&
            src.marketSize == expected.marketSize ,
        """The order valus are matced""",
        """The order value are not matched"""
        )

    def beMarketMakerStrategy(expected : MarketMakerStrategy) : Matcher[MarketMakerStrategy] = (src : MarketMakerStrategy) => {

        src.time == expected.time &&
            src.inventory == expected.inventory &&
            src.spread == expected.spread &&
            src.limitBuyOrderType == expected.limitBuyOrderType &&
            src.limitBuyOrderSize == expected.limitBuyOrderSize &&
            src.limitSellOrderType == expected.limitSellOrderType &&
            src.limitSellOrderSize == expected.limitSellOrderSize &&
            src.marketOrderType == expected.marketOrderType &&
            src.marketOrderSize == expected.marketOrderSize &&
            src.feeStructure == expected.feeStructure
    }
}
