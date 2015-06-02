package com.marketmaker.helper

import java.sql.{ResultSet, DriverManager, Connection}
import java.util.Date

import com.marketmaker.repositories.{MarketMakerStrategy, OrderValue, Phy}

/**
 * Created by wacharint on 5/17/15.
 */
class RepositoryHelper {

    val TimeAtMaturity = 300000
    var orderPhyQueue = Seq[Phy]()
    var orderOrderValueQueue = Seq[OrderValue]()
    var orderMarketMakerStrategyQueue = Seq[MarketMakerStrategy]()
    var connection : Option[Connection] = None
    var isUpdating = false

    def addPhy(phy : Phy)(implicit databaseName : String, databaseSavedInterval : Short) {

        addPhy(phy.time, phy.inv, phy.spread, phy.value)
    }

    def addPhy(time : Int , inv : Int, spread : Byte, value : Double)(implicit databaseName : String, databaseSavedInterval : Short) {

        if(orderPhyQueue.find( p => p.time == time && p.spread == spread && p.inv == inv).isEmpty) {
            while(isUpdating){ Thread.sleep(10)}
            orderPhyQueue = orderPhyQueue ++ Seq[Phy](new Phy(time, inv, spread, value))

            if(orderPhyQueue.size >= databaseSavedInterval) {
                addOrderPhysToDatabase
                orderPhyQueue = Seq[Phy]()
            }
        }
    }

    def addOrderValue(orderValue : OrderValue)(implicit databaseName : String, databaseSavedInterval : Short) {

        if(orderOrderValueQueue.find( p => p.time == orderValue.time && p.spread == orderValue.spread && p.inventory == orderValue.inventory).isEmpty) {
            while(isUpdating){ Thread.sleep(10)}
            orderOrderValueQueue = orderOrderValueQueue ++ Seq[OrderValue](orderValue)

            if(orderOrderValueQueue.size >= databaseSavedInterval) {
                addOrderOrderValueToDatabase
                orderOrderValueQueue = Seq[OrderValue]()
            }
        }
    }

    def addMarketMakerStrategy(strategy : MarketMakerStrategy)(implicit databaseName : String, databaseSavedInterval : Short) {

        if(orderMarketMakerStrategyQueue.find( p => p.time == strategy.time && p.spread == strategy.spread && p.inventory == strategy.inventory).isEmpty) {
            while(isUpdating){ Thread.sleep(10)}
            orderMarketMakerStrategyQueue = orderMarketMakerStrategyQueue ++ Seq[MarketMakerStrategy](strategy)

            if(orderMarketMakerStrategyQueue.size >= databaseSavedInterval) {
                addOrderMarketMakerStrategyToDatabase
                orderMarketMakerStrategyQueue = Seq[MarketMakerStrategy]()
            }
        }
    }

    def forceUpdatePhyTable(implicit databaseName : String) = {

        if(orderPhyQueue.size != 0) {
            while(isUpdating){ Thread.sleep(10)}
            addOrderPhysToDatabase
            orderPhyQueue = Seq[Phy]()
        }
    }

    def forceUpdateOrderValueTable(implicit databaseName : String) = {

        if(orderOrderValueQueue.size != 0) {
            while(isUpdating){ Thread.sleep(10)}
            addOrderOrderValueToDatabase
            orderOrderValueQueue = Seq[OrderValue]()
        }
    }

    def addOrderPhysToDatabase(implicit databaseName : String): Unit = {

        isUpdating = true

        if(connection == None) {
            connect
        }

        orderPhyQueue = orderPhyQueue.distinct

        val statement = connection.get.createStatement()

        statement.execute("BEGIN TRANSACTION")

        var query =
            """INSERT INTO phy """.stripMargin

        orderPhyQueue.foreach(order => query = query +
            """SELECT %s,%s,%s,%s UNION """.stripMargin.format(order.time, order.inv, order.spread, order.value))

        query = query.replaceAll("UNION[ \n\r\t]*$","")

        val sizeOfTheQueue = orderPhyQueue.size
        val updatedRow = statement.executeUpdate(query)

        statement.execute("COMMIT TRANSACTION")
        isUpdating = false
    }

    def addOrderOrderValueToDatabase(implicit databaseName : String): Unit = {

        isUpdating = true

        if(connection == None) {
            connect
        }

        orderPhyQueue = orderPhyQueue.distinct

        val statement = connection.get.createStatement()

        var query =
            """INSERT INTO order_value """.stripMargin

        orderOrderValueQueue.foreach(order => query = query +
            """SELECT %s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s UNION """.stripMargin.format(order.time,
                order.inventory,
                order.spread,
                order.limitOrderStrategyValue,
                order.marketOrderStrategyValue,
                order.limitBuyType,
                order.limitBuySize,
                order.limitSellType,
                order.limitSellSize,
                order.marketType,
                order.marketSize))

        query = query.replaceAll("UNION[ \n\r\t]*$","")

        val sizeOfTheQueue = orderOrderValueQueue.size
        val updatedRow = statement.executeUpdate(query)

        isUpdating = false

        if(sizeOfTheQueue != updatedRow) {

            throw new RuntimeException("The updated rows is not equal to the number of order in the queue.")
        }
    }

    def addOrderMarketMakerStrategyToDatabase(implicit databaseName : String): Unit = {

        isUpdating = true

        if(connection == None) {
            connect
        }

        orderPhyQueue = orderPhyQueue.distinct

        val statement = connection.get.createStatement()

        var query =
            """INSERT OR REPLACE INTO market_maker_strategy """.stripMargin

        orderMarketMakerStrategyQueue.foreach(order => query = query +
            """SELECT %s,%s,%s,%s,%s,%s,%s,%s,%s,%s UNION """.stripMargin.format(order.time,
                order.spread,
                order.inventory,
                order.limitBuyOrderType,
                order.limitBuyOrderSize,
                order.limitSellOrderType,
                order.limitSellOrderSize,
                order.marketOrderType,
                order.marketOrderSize,
                order.feeStructure))

        query = query.replaceAll("UNION[ \n\r\t]*$","")

        val sizeOfTheQueue = orderMarketMakerStrategyQueue.size
        val updatedRow = statement.executeUpdate(query)

        isUpdating = false

        if(sizeOfTheQueue != updatedRow) {

            throw new RuntimeException("The updated rows is not equal to the number of order in the queue.")
        }
    }

    def getPhys(timeAndInvPairs : Seq[(Int,Int,Byte)])
               (implicit databaseName : String) : Seq[Phy] = {

        if(connection == None) {
            connect
        }

        val statement = connection.get.createStatement()

        var query =
            """SELECT   *
              |FROM     phy
              |WHERE    (inventory = 999999999 )
              |
            """.stripMargin

        timeAndInvPairs.foreach(pair => query = query +
                                                """ OR   (time = %s and inventory = %s and spread = %s ) """.format(pair._1, pair._2, pair._3))

        val resultSet = statement.executeQuery(query)
        var ret = Seq[Phy]()

        while(resultSet.next())
        {
            ret = ret ++ Seq[Phy](
                new Phy(resultSet.getInt("time"), resultSet.getInt("inventory"), resultSet.getByte("spread"), resultSet.getDouble("phy"))
            )
        }

        ret.distinct
    }

    def getPhys(time : Int)
               (implicit databaseName : String) : Seq[Phy] = {

        if(connection == None) {
            connect
        }

        val statement = connection.get.createStatement()

        val query =
            """SELECT   *
              |FROM     phy
              |WHERE    time = %s
            """.format(time).stripMargin

        val resultSet = statement.executeQuery(query)
        var ret = Seq[Phy]()

        while(resultSet.next())
        {
            ret = ret ++ Seq[Phy](
                new Phy(resultSet.getInt("time"), resultSet.getInt("inventory"), resultSet.getByte("spread"), resultSet.getDouble("phy"))
            )
        }

        ret
    }

    def getPhys()
               (implicit databaseName : String) : Seq[Phy] = {

        if(connection == None) {
            connect
        }

        val statement = connection.get.createStatement()

        val query =
            "SELECT   * FROM     phy"

        val resultSet = statement.executeQuery(query)
        var ret = Seq[Phy]()

        while(resultSet.next())
        {
            ret = ret ++ Seq[Phy](
                new Phy(resultSet.getInt("time"), resultSet.getInt("inventory"), resultSet.getByte("spread"), resultSet.getDouble("phy"))
            )
        }

        ret
    }

    def getOrderValue(timeAndInvPairs : Seq[(Int,Int,Byte)])
               (implicit databaseName : String) : Seq[OrderValue] = {

        if(connection == None) {
            connect
        }

        val statement = connection.get.createStatement()

        var query =
            """SELECT   *
              |FROM     order_value
              |WHERE    (inventory = 999999999 )
              |
            """.stripMargin

        timeAndInvPairs.foreach(pair => query = query +
            """ OR   (time = %s and inventory = %s and spread = %s ) """.format(pair._1, pair._2, pair._3))

        val resultSet = statement.executeQuery(query)
        var ret = Seq[OrderValue]()

        while(resultSet.next())
        {
            ret = ret ++ Seq[OrderValue](
                new OrderValue(resultSet.getInt("time"),
                    resultSet.getInt("inventory"),
                    resultSet.getByte("spread"),
                    resultSet.getDouble("limit_order_strategy_value"),
                    resultSet.getDouble("market_order_strategy_value"),
                    resultSet.getInt("limit_buy_type"),
                    resultSet.getInt("limit_buy_size"),
                    resultSet.getInt("limit_sell_type"),
                    resultSet.getInt("limit_sell_size"),
                    resultSet.getInt("market_type"),
                    resultSet.getInt("market_size"))
            )
        }

        ret.distinct
    }

    def getOrderValue(time : Int)
               (implicit databaseName : String) : Seq[OrderValue] = {

        if(connection == None) {
            connect
        }

        val statement = connection.get.createStatement()

        val query =
            """SELECT   *
              |FROM     order_value
              |WHERE    time = %s
            """.format(time).stripMargin

        val resultSet = statement.executeQuery(query)
        var ret = Seq[OrderValue]()

        while(resultSet.next())
        {
            ret = ret ++ Seq[OrderValue](
                new OrderValue(resultSet.getInt("time"),
                    resultSet.getInt("inventory"),
                    resultSet.getByte("spread"),
                    resultSet.getDouble("limit_order_strategy_value"),
                    resultSet.getDouble("market_order_strategy_value"),
                    resultSet.getInt("limit_buy_type"),
                    resultSet.getInt("limit_buy_size"),
                    resultSet.getInt("limit_sell_type"),
                    resultSet.getInt("limit_sell_size"),
                    resultSet.getInt("market_type"),
                    resultSet.getInt("market_size"))
            )
        }

        ret.distinct
    }

    def getOrderValue()
               (implicit databaseName : String) : Seq[OrderValue] = {

        if(connection == None) {
            connect
        }

        val statement = connection.get.createStatement()

        val query =
            "SELECT   * FROM     order_value"

        val resultSet = statement.executeQuery(query)
        var ret = Seq[OrderValue]()

        while(resultSet.next())
        {
            ret = ret ++ Seq[OrderValue](
                new OrderValue(resultSet.getInt("time"),
                    resultSet.getInt("inventory"),
                    resultSet.getByte("spread"),
                    resultSet.getDouble("limit_order_strategy_value"),
                    resultSet.getDouble("market_order_strategy_value"),
                    resultSet.getInt("limit_buy_type"),
                    resultSet.getInt("limit_buy_size"),
                    resultSet.getInt("limit_sell_type"),
                    resultSet.getInt("limit_sell_size"),
                    resultSet.getInt("market_type"),
                    resultSet.getInt("market_size"))
            )
        }

        ret.distinct
    }

    def getMarketMakerStrategy()
                     (implicit databaseName : String) : Seq[MarketMakerStrategy] = {

        if(connection == None) {
            connect
        }

        val statement = connection.get.createStatement()

        val query =
            "SELECT   * FROM     market_maker_strategy"

        val resultSet = statement.executeQuery(query)
        var ret = Seq[MarketMakerStrategy]()

        while(resultSet.next())
        {
            ret = ret ++ Seq[MarketMakerStrategy](
                new MarketMakerStrategy(){
                    time = resultSet.getInt("time")
                    inventory =resultSet.getInt("inventory")
                    spread = resultSet.getByte("spread")
                    limitBuyOrderType = resultSet.getInt("limit_buy_order_type")
                    limitBuyOrderSize = resultSet.getInt("limit_buy_order_Size")
                    limitSellOrderType = resultSet.getInt("limit_sell_order_type")
                    limitSellOrderSize = resultSet.getInt("limit_sell_order_Size")
                    marketOrderType = resultSet.getInt("market_order_type")
                    marketOrderSize = resultSet.getInt("market_order_Size")
                    feeStructure = resultSet.getInt("fee_structure")}
            )
        }

        ret.distinct
    }

    def deleteAllPhy(implicit databaseName : String) = {

        if(connection == None) {
            connect
        }

        val statement = connection.get.createStatement()

        val query = """DELETE FROM phy"""

        statement.executeUpdate(query)
    }

    def deleteAllOrderValue(implicit databaseName : String) = {

        if(connection == None) {
            connect
        }

        val statement = connection.get.createStatement()

        val query = """DELETE FROM order_value"""

        statement.executeUpdate(query)
    }

    def deleteAllMarketMakerStrategy(implicit databaseName : String) = {

        if(connection == None) {
            connect
        }

        val statement = connection.get.createStatement()

        val query = """DELETE FROM market_maker_strategy"""

        statement.executeUpdate(query)
    }

    private def connect(implicit databaseName : String) = {

        val url = "jdbc:sqlite:%s".format(databaseName)

        Class.forName("org.sqlite.JDBC")
        connection = if(connection.isEmpty || connection.get.isClosed) Option(DriverManager.getConnection(url)) else connection
    }
}


