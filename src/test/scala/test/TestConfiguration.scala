package test

/**
 * Created by wacharint on 5/17/15.
 */
trait TestConfiguration {

    implicit val databaseName = """/Users/wacharint/Desktop/MarketStrategiesTest.db"""
    implicit var databaseSavedInterval : Short = 3
}
