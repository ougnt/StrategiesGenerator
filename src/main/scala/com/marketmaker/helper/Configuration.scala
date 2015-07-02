package com.marketmaker.helper

/**
 * Created by wacharint on 5/17/15.
 */
trait Configuration {
    implicit val databaseName = """/Users/wacharint/Desktop/MarketStrategies.db"""
    implicit val databaseSavedInterval : Short = 500
    var counter = 0
}
