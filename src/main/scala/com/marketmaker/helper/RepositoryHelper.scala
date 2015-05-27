package com.marketmaker.helper

import java.sql.{ResultSet, DriverManager, Connection}

import com.marketmaker.repositories.Phy

/**
 * Created by wacharint on 5/17/15.
 */
class RepositoryHelper {

    val TimeAtMaturity = 300000
    var orderPhyQueue = Seq[Phy]()
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

    def forceUpdate(implicit databaseName : String) = {

        if(orderPhyQueue.size != 0) {
            while(isUpdating){ Thread.sleep(10)}
            addOrderPhysToDatabase
            orderPhyQueue = Seq[Phy]()
        }
    }

    def addOrderPhysToDatabase(implicit databaseName : String): Unit = {

        isUpdating = true

        if(connection == None) {
            connect
        }

        orderPhyQueue = orderPhyQueue.distinct

        val statement = connection.get.createStatement()

        var query =
            """INSERT OR REPLACE INTO phy """.stripMargin

        orderPhyQueue.foreach(order => query = query +
            """SELECT %s,%s,%s,%s UNION """.stripMargin.format(order.time, order.inv, order.spread, order.value))

        query = query.replaceAll("UNION[ \n\r\t]*$","")

        val sizeOfTheQueue = orderPhyQueue.size
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

    def deleteAllPhy(implicit databaseName : String) = {

        if(connection == None) {
            connect
        }

        val statement = connection.get.createStatement()

        val query = """DELETE FROM phy"""

        statement.executeUpdate(query)
    }

    private def connect(implicit databaseName : String) = {

        val url = "jdbc:sqlite:%s".format(databaseName)

        Class.forName("org.sqlite.JDBC")
        connection = if(connection.isEmpty || connection.get.isClosed) Option(DriverManager.getConnection(url)) else connection
    }
}


