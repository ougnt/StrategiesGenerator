package com.marketmaker.helper

import java.sql.{ResultSet, DriverManager, Connection}

import com.marketmaker.repositories.Phy

/**
 * Created by wacharint on 5/17/15.
 */
object Helper {

    val TimeAtMaturity = 300000
    var orderPhyQueue = Seq[Phy]()
    var connection : Option[Connection] = None

    def addPhy(time : Int , inv : Int, value : Double)(implicit databaseName : String, databaseSavedInterval : Int) {

        orderPhyQueue = orderPhyQueue ++ Seq[Phy](new Phy(time, inv, value))

        if(orderPhyQueue.size >= databaseSavedInterval) {
            addOrderPhysToDatabase
            orderPhyQueue = Seq[Phy]()
        }
    }

    def addOrderPhysToDatabase(implicit databaseName : String): Unit = {

        if(connection == None) {
            connect
        }

        val statement = connection.get.createStatement()

        var query =
            """INSERT OR REPLACE INTO phy """.stripMargin

        orderPhyQueue.foreach(order => query = query +
            """SELECT %s,%s,%s UNION """.stripMargin.format(order.time, order.inv, order.value))

        query = query.replaceAll("UNION[ \n\r\t]*$","")

        statement.execute(query)
    }

    def getStrategies(timeAndInvPairs : Seq[
  (Int,Int)])(implicit databaseName : String) : Seq[Phy] = {

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
                                                """ OR   (time = %s and inventory = %s ) """.format(pair._1, pair._2))

        val resultSet = statement.executeQuery(query)
        var ret = Seq[Phy]()

        while(resultSet.next())
        {
            ret = ret ++ Seq[Phy](
                new Phy(resultSet.getInt("time"), resultSet.getInt("inventory"), resultSet.getDouble("phy"))
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


