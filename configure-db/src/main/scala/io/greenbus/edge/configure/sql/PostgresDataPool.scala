package io.greenbus.edge.configure.sql

import javax.sql.DataSource

object PostgresDataPool {

  def dataSource(sqlSettings: SqlSettings): DataSource = {
    val pool = new org.apache.commons.dbcp.BasicDataSource
    pool.setDriverClassName("org.postgresql.Driver")
    pool.setUrl(sqlSettings.url)
    pool.setUsername(sqlSettings.user)
    pool.setPassword(sqlSettings.password)
    pool.setMaxActive(sqlSettings.poolMaxActive)
    pool
  }
}
