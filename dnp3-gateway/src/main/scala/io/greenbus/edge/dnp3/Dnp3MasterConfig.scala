/**
 * Copyright 2011-2017 Green Energy Corp.
 *
 * Licensed to Green Energy Corp (www.greenenergycorp.com) under one or more
 * contributor license agreements. See the NOTICE file distributed with this
 * work for additional information regarding copyright ownership. Green Energy
 * Corp licenses this file to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */
package io.greenbus.edge.dnp3

import io.greenbus.edge.dnp3.config.model.{ AppLayer, DNP3Gateway, LinkLayer, Master }
import org.totalgrid.dnp3._

object Dnp3MasterConfig {
  def load(config: DNP3Gateway): Dnp3MasterConfig = {

    val filter = FilterLevel.LEV_INFO
    val address = config.client.host
    val port = config.client.port
    val retryMs = {
      val fromConfig = config.client.retryMs
      if (fromConfig <= 0) 5000 else fromConfig
    }

    Dnp3MasterConfig(loadMaster(config.master), filter, address, port, retryMs)
  }

  def loadMaster(config: Master): MasterStackConfig = {
    val master = new MasterStackConfig
    master.setMaster(loadMasterConfig(config, config.stack.appLayer.maxFragSize))
    master.setApp(loadAppLayer(config.stack.appLayer))
    master.setLink(loadLinkLayer(config.stack.linkLayer))
    master
  }

  def loadMasterConfig(config: Master, fragSize: Int): MasterConfig = {
    val mcfg = new MasterConfig
    mcfg.setAllowTimeSync(config.masterSettings.allowTimeSync)
    mcfg.setTaskRetryRate(config.masterSettings.taskRetryMs)
    mcfg.setIntegrityRate(config.masterSettings.integrityPeriodMs)

    mcfg.setDoUnsolOnStartup(config.unsol.doTask)
    mcfg.setEnableUnsol(config.unsol.enable)

    var unsolClass = 0
    if (config.unsol.enableClass1) unsolClass = unsolClass | PointClass.PC_CLASS_1.swigValue
    if (config.unsol.enableClass2) unsolClass = unsolClass | PointClass.PC_CLASS_2.swigValue
    if (config.unsol.enableClass3) unsolClass = unsolClass | PointClass.PC_CLASS_3.swigValue
    mcfg.setUnsolClassMask(unsolClass)

    mcfg.setFragSize(fragSize)

    config.scanList.foreach { scan =>
      var point_class = PointClass.PC_CLASS_0.swigValue
      if (scan.enableClass1) point_class = point_class | PointClass.PC_CLASS_1.swigValue
      if (scan.enableClass2) point_class = point_class | PointClass.PC_CLASS_2.swigValue
      if (scan.enableClass3) point_class = point_class | PointClass.PC_CLASS_3.swigValue
      mcfg.AddExceptionScan(point_class, scan.periodMs)
    }

    mcfg
  }

  def loadLinkLayer(config: LinkLayer): LinkConfig = {
    val cfg = new LinkConfig(config.isMaster, config.userConfirmations)
    cfg.setNumRetry(config.numRetries)
    cfg.setRemoteAddr(config.remoteAddress)
    cfg.setLocalAddr(config.localAddress)
    cfg.setTimeout(config.ackTimeoutMs)
    cfg
  }

  def loadAppLayer(config: AppLayer): AppConfig = {
    val cfg = new AppConfig
    cfg.setFragSize(config.maxFragSize)
    cfg.setRspTimeout(config.timeoutMs)
    cfg
  }
}
case class Dnp3MasterConfig(stack: MasterStackConfig, logLevel: FilterLevel, address: String, port: Int, retryMs: Long)
