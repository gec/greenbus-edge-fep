package io.greenbus.edge.configure.dyn

import io.greenbus.edge.api._
import io.greenbus.edge.configure.endpoint.{ModuleConfiguration, ModuleConfigurer}
import io.greenbus.edge.thread.CallMarshaller
import scala.collection.mutable

import scala.concurrent.Promise


/*

gateway (component) boots up
- asks for all for component/node

a) subscribe component/node for modules, then individual kv subscribes for modules
b) subscribe to component/node active set, get module -> file
c) subscribe to component/node for modules, active set is module -> hash, then http get


 */

class ConfigurationTable(eventThread: CallMarshaller, endpointId: EndpointId, producerService: ProducerService) extends ModuleConfigurer {

  //private val subscribedMap = mutable.Map.empty[]

  eventThread.marshal {
    init()
  }

  private def init(): Unit = {
    val builder = producerService.endpointBuilder(endpointId)

    val setHandle = builder.keyValueDynamicSet("configuration", new DynamicDataKey {
      def subscribed(path: Path): Unit = {
        eventThread.marshal(onSubscribed(path))
      }

      def unsubscribed(path: Path): Unit = {
        eventThread.marshal(onUnsubscribed(path))
      }
    })

    val endpointHandle = builder.build()
  }

  private def onSubscribed(path: Path): Unit = {

  }

  private def onUnsubscribed(path: Path): Unit = {

  }

  def updateModule(module: String, config: ModuleConfiguration, promise: Promise[Boolean]): Unit = {

  }

  def removeModule(module: String, promise: Promise[Boolean]): Unit = {

  }
}
