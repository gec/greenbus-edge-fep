package io.greenbus.edge.dnp3

import io.greenbus.edge.tag._
import org.totalgrid.dnp3._

class Adapter {

}


case class Dnp3MasterConfig(stack: MasterStackConfig, logLevel: FilterLevel, address: String, port: Int, retryMs: Long)

case class ConnectionOpts(address: String, port: Int, retryMs: Option[Long])
case class DnpMasterConfiguration(
                                 connection: ConnectionOpts
                                 )

trait ReaderContext {
  def context: String
  def field(name: String): ReaderContext
  def structField(tag: String, name: String): ReaderContext
}

object ValueUtils {

  def descName(v: ValueElement): String = {
    v.getClass.getSimpleName
  }

  def toFieldMap(struct: VTuple): Map[String, ValueElement] = {
    struct.value.flatMap {
      case field: TaggedField => Some(field.name, field.value)
      case _ => None
    }.toMap
  }

  /*def structSuffices(data: Map[String, ValueElement], schema: StructSchema): Either[String, Boolean] = {
    schema.fields.forall { fd =>
      data.get(fd.fieldName) match {
        case None => Left(s"missing ${fd.fieldName}")
        case Right =>
      }

    }
  }*/

  def readFieldSubStruct[A](fieldName: String, map: Map[String, ValueElement], tag: String, read: (VTuple, ReaderContext) => Either[String, A], ctx: ReaderContext): Either[String, A] = {

    def matchV(elem: ValueElement): Either[String, A] = {
      elem match {
        case v: VTuple => read(v, ctx.structField(tag, fieldName))
        case _ => Left(s"${ctx.context} error: expected boolean value, saw: ${descName(elem)}")
      }
    }

    map.get(fieldName) match {
      case None => Left(s"${ctx.context} error: expected field '$fieldName'")
      case Some(elem) =>
        elem match {
          case v: TaggedValue => matchV(v.value)
          case other => matchV(other)
        }
    }
  }

  def readField[A](fieldName: String, map: Map[String, ValueElement], read: (ValueElement, ReaderContext) => Either[String, A], ctx: ReaderContext): Either[String, A] = {
    map.get(fieldName) match {
      case None => Left(s"${ctx.context} error: expected field '$fieldName'")
      case Some(elem) => read(elem, ctx.field(fieldName))
    }
  }

  /*def readBoolField(fieldName: String, map: Map[String, ValueElement], ctx: ReaderContext): Either[String, Boolean] = {
    map.get(fieldName) match {
      case None => Left(s"${ctx.context} error: expected field $fieldName")
      case Some(elem) => readBool()
    }
  }*/
  def readBool(elem: ValueElement, ctx: ReaderContext): Either[String, Boolean] = {
    elem match {
      case v: VBool => Right(v.value)
      case _ => Left(s"${ctx.context} error: expected boolean value, saw: ${descName(elem)}")
    }
  }
  def readInt(elem: ValueElement, ctx: ReaderContext): Either[String, Int] = {
    elem match {
      case v: IntegerValue => Right(v.toInt)
      case _ => Left(s"{ctx.context} error: expected integer value, saw: ${descName(elem)}")
    }
  }
  def readLong(elem: ValueElement, ctx: ReaderContext): Either[String, Long] = {
    elem match {
      case v: IntegerValue => Right(v.toLong)
      case _ => Left(s"{ctx.context} error: expected integer value, saw: ${descName(elem)}")
    }
  }
}

trait StructSchema {
  def tag: String
  def fields: IndexedSeq[FieldDef]
  def map: Map[String, FieldDef]
}

abstract class StructSchemaDef(tagName: String) extends StructSchema {
  private var fieldSeq = Vector.empty[FieldDef]
  private var fieldMap = Map.empty[String, FieldDef]

  def tag: String = tagName

  protected def field(name: String, typ: VType): FieldDef = {
    val fd = FieldDef(name, typ)
    fieldSeq :+= fd
    fieldMap += (name -> fd)
    fd
  }

  def fields: IndexedSeq[FieldDef] = fieldSeq
  def map: Map[String, FieldDef] = fieldMap
  def vtype: VType = VTExtType(tagName, VTTuple(fieldSeq))
}

object Dnp3ConfigSchema {

  object LinkConfig extends StructSchemaDef("LinkLayer") {
    val isMaster = field("isMaster", VTBool)
    val localAddress = field("localAddress", VTUInt32)
    val remoteAddress = field("remoteAddress", VTUInt32)
    val userConfirmations = field("userConfirmations", VTBool)
    val ackTimeoutMs = field("userConfirmations", VTUInt64)
    val numRetries = field("numRetries", VTUInt32)
  }

  object AppConfig extends StructSchemaDef("AppLayer") {
    val timeoutMs = field("timeoutMs", VTUInt64)
    val maxFragSize = field("maxFragSize", VTUInt32)
    val numRetries = field("numRetries", VTUInt32)
  }

  object StackConfigSchema extends StructSchemaDef("Stack") {
    val linkLayer = field("linkLayer", LinkConfig.vtype)
    val appLayer = field("appLayer", AppConfig.vtype)
  }

  object MasterSettings extends StructSchemaDef("MasterSettings") {
    val allowTimeSync = field("allowTimeSync", VTBool)
    val timeoutMs = field("taskRetryMs", VTUInt64)
    val integrityPeriodMs = field("integrityPeriodMs", VTUInt64)
  }

  object ScanList extends StructSchemaDef("ScanList") {
    val enableClass1 = field("enableClass1", VTBool)
    val enableClass2 = field("enableClass2", VTBool)
    val enableClass3 = field("enableClass3", VTBool)
    val periodMs = field("periodMs", VTUInt64)
  }

  object Unsol extends StructSchemaDef("Unsol") {
    val doTask = field("doTask", VTBool)
    val enable = field("enable", VTBool)
    val enableClass1 = field("enableClass1", VTBool)
    val enableClass2 = field("enableClass2", VTBool)
    val enableClass3 = field("enableClass3", VTBool)
  }

  object Master extends StructSchemaDef("Master") {
    val stack = field("stack", StackConfigSchema.vtype)
    val masterSettings = field("masterSettings", MasterSettings.vtype)
    val scanList = field("scanList", VTList(ScanList.vtype))
    val unsol = field("unsol", Unsol.vtype)
  }
}

object Configurer {
  import Dnp3ConfigSchema._
  import ValueUtils._

  def configureLinkLayer(data: VTuple, ctx: ReaderContext): Either[String, LinkConfig] = {
    val fieldMap = ValueUtils.toFieldMap(data)

    for {
      isMaster <- readField(LinkConfig.isMaster.fieldName, fieldMap, readBool, ctx)
      localAddress <- readField(LinkConfig.isMaster.fieldName, fieldMap, readInt, ctx)
      remoteAddress <- readField(LinkConfig.isMaster.fieldName, fieldMap, readInt, ctx)
      userConfirmations <- readField(LinkConfig.isMaster.fieldName, fieldMap, readBool, ctx)
      ackTimeoutMs <- readField(LinkConfig.isMaster.fieldName, fieldMap, readLong, ctx)
      numRetries <- readField(LinkConfig.isMaster.fieldName, fieldMap, readInt, ctx)
    } yield {
      val cfg = new LinkConfig(isMaster, userConfirmations)
      cfg.setLocalAddr(localAddress)
      cfg.setRemoteAddr(remoteAddress)
      cfg.setTimeout(ackTimeoutMs)
      cfg.setNumRetry(numRetries)
      cfg
    }
  }

  def configureAppLayer(data: VTuple, ctx: ReaderContext): Either[String, AppConfig] = {
    val fieldMap = ValueUtils.toFieldMap(data)

    for {
      timeoutMs <- readField(AppConfig.timeoutMs.fieldName, fieldMap, readLong, ctx)
      maxFragSize <- readField(AppConfig.maxFragSize.fieldName, fieldMap, readInt, ctx)
      numRetries <- readField(AppConfig.numRetries.fieldName, fieldMap, readInt, ctx)
    } yield {

      val cfg = new AppConfig
      cfg.setFragSize(maxFragSize)
      cfg.setRspTimeout(timeoutMs)
      cfg.setNumRetry(numRetries)
      cfg
    }
  }

  //def configure(data: VTuple, ctx: ReaderContext): Either[]

  def loadStackConfig(data: VTuple, ctx: ReaderContext): Either[String, MasterStackConfig] = {
    val fieldMap = ValueUtils.toFieldMap(data)

    for {
      lc <- readFieldSubStruct(StackConfigSchema.linkLayer.fieldName, fieldMap, LinkConfig.tag, configureLinkLayer, ctx)
      ac <- readFieldSubStruct(StackConfigSchema.appLayer.fieldName, fieldMap, AppConfig.tag, configureAppLayer, ctx)
    } yield {
      val config = new MasterStackConfig
      //config.setMaster(configure(xml, xml.getStack.getAppLayer.getMaxFragSize))
      config.setApp(ac)
      config.setLink(lc)
      config
    }
  }


  /*

  private def configure(xml: Master, fragSize: Int): MasterConfig = {
    val cfg = new MasterConfig
    cfg.setAllowTimeSync(xml.getMasterSettings.isAllowTimeSync)
    cfg.setTaskRetryRate(xml.getMasterSettings.getTaskRetryMS)
    cfg.setIntegrityRate(xml.getMasterSettings.getIntegrityPeriodMS)

    cfg.setDoUnsolOnStartup(xml.getUnsol.isDoTask)
    cfg.setEnableUnsol(xml.getUnsol.isEnable)

    var unsol_class = 0
    if (xml.getUnsol.isClass1) unsol_class = unsol_class | PointClass.PC_CLASS_1.swigValue
    if (xml.getUnsol.isClass2) unsol_class = unsol_class | PointClass.PC_CLASS_2.swigValue
    if (xml.getUnsol.isClass3) unsol_class = unsol_class | PointClass.PC_CLASS_3.swigValue
    cfg.setUnsolClassMask(unsol_class)

    cfg.setFragSize(fragSize)

    xml.getScanList.getExceptionScan.foreach { scan =>
      var point_class = PointClass.PC_CLASS_0.swigValue
      if (scan.isClass1) point_class = point_class | PointClass.PC_CLASS_1.swigValue
      if (scan.isClass2) point_class = point_class | PointClass.PC_CLASS_2.swigValue
      if (scan.isClass3) point_class = point_class | PointClass.PC_CLASS_3.swigValue
      cfg.AddExceptionScan(point_class, scan.getPeriodMS)
    }

  def loadConfig(xml: Master): MasterStackConfig = {
    val config = new MasterStackConfig
    config.setMaster(configure(xml, xml.getStack.getAppLayer.getMaxFragSize))
    config.setApp(configure(xml.getStack.getAppLayer))
    config.setLink(configure(xml.getStack.getLinkLayer))
    config
  }

  def configure(xml: AppLayer): AppConfig = {
    val cfg = new AppConfig
    cfg.setFragSize(xml.getMaxFragSize)
    cfg.setRspTimeout(xml.getTimeoutMS)
    cfg
  }


    cfg
  }
   */
}

/*case class LinkLayerConfig(
                          isMaster: Boolean,
                          localAddress: Int,
                          remoteAddress: Int,
                          userConfirmations: Boolean,
                          ackTimeoutMs: Int,
                          numRetries: Int)*/

//case class

case class StackConfig()

/*
public static final FilterLevel LEV_EVENT = new FilterLevel("LEV_EVENT", javadnp3JNI.LEV_EVENT_get());
    public static final FilterLevel LEV_ERROR = new FilterLevel("LEV_ERROR", javadnp3JNI.LEV_ERROR_get());
    public static final FilterLevel LEV_WARNING = new FilterLevel("LEV_WARNING", javadnp3JNI.LEV_WARNING_get());
    public static final FilterLevel LEV_INFO = new FilterLevel("LEV_INFO", javadnp3JNI.LEV_INFO_get());
    public static final FilterLevel LEV_INTERPRET = new FilterLevel("LEV_INTERPRET", javadnp3JNI.LEV_INTERPRET_get());
    public static final FilterLevel LEV_COMM = new FilterLevel("LEV_COMM", javadnp3JNI.LEV_COMM_get());
    public static final FilterLevel LEV_DEBUG = new FilterLevel("LEV_DEBUG", javadnp3JNI.LEV_DEBUG_get());
 */

/*
object MasterXmlConfig {

  def loadConfig(xml: DNPMasterEndpoint): (Dnp3MasterConfig, IndexMapping, Map[String, OutputMapping]) = {

    val filter = Option(xml.getLog)
      .flatMap(log => Option(log.getFilter))
      .map(XmlToProtoTranslations.translateFilterLevel)
      .getOrElse(FilterLevel.LEV_WARNING)

    if (xml.getMaster == null) {
      throw new IllegalArgumentException("Configuration must include master configuration")
    }

    val master = loadConfig(xml.getMaster)

    val address = Option(xml.getTCPClient).flatMap(client => Option(client.getAddress)).getOrElse(throw new IllegalArgumentException("Configuration must include address"))
    val port = Option(xml.getTCPClient).flatMap(client => Option(client.getPort)).getOrElse(throw new IllegalArgumentException("Configuration must include port"))
    val retryMsOpt = Option(xml.getTCPClient).map(client => client.getOpenRetryMS)

    val retryMs = retryMsOpt match {
      case None | Some(0) => 5000
      case Some(v) => v
    }

    if (xml.getIndexMapping == null) {
      throw new IllegalArgumentException("Configuration must include mapping")
    }

    val (indexMapping, controlMapping) = loadMapping(xml.getIndexMapping)

    (Dnp3MasterConfig(master, filter, address, port, retryMs), indexMapping, controlMapping)
  }

  def loadMapping(xml: io.greenbus.dnp3.xml.IndexMapping): (io.greenbus.dnp3.master.IndexMapping, Map[String, OutputMapping]) = {

    val binaries = Option(xml.getBinaries).map(_.getMapping.toSeq.map(m => (m.getIndex.toLong, m.getName))).getOrElse(Nil).toMap
    val analogs = Option(xml.getAnalogs).map(_.getMapping.toSeq.map(m => (m.getIndex.toLong, m.getName))).getOrElse(Nil).toMap
    val counters = Option(xml.getCounters).map(_.getMapping.toSeq.map(m => (m.getIndex.toLong, m.getName))).getOrElse(Nil).toMap
    val controlStatuses = Option(xml.getControlStatuses).map(_.getMapping.toSeq.map(m => (m.getIndex.toLong, m.getName))).getOrElse(Nil).toMap
    val setpointStatuses = Option(xml.getSetpointStatuses).map(_.getMapping.toSeq.map(m => (m.getIndex.toLong, m.getName))).getOrElse(Nil).toMap

    val controls: Seq[(String, ControlMapping)] =
      Option(xml.getControls).map(_.getMapping.toSeq.map(m => (m.getName, loadControl(m)))).getOrElse(Nil)

    val setpoints: Seq[(String, SetpointMapping)] =
      Option(xml.getSetpoints).map(_.getMapping.toSeq.map(m => (m.getName, loadSetpoint(m)))).getOrElse(Nil)

    val controlMap = (controls ++ setpoints).toMap

    val indexMap = io.greenbus.dnp3.master.IndexMapping(binaries, analogs, counters, controlStatuses, setpointStatuses)

    (indexMap, controlMap)
  }

  def loadSetpoint(mapping: io.greenbus.dnp3.xml.IndexMapping.Setpoints.Mapping): SetpointMapping = {
    val isDirectOperate = Option(mapping.getFunction).exists(_ == FunctionType.DIRECT_OPERATE)
    SetpointMapping(mapping.getIndex.toInt, isDirectOperate)

  }

  def loadControl(mapping: io.greenbus.dnp3.xml.IndexMapping.Controls.Mapping): ControlMapping = {
    val controlOpts: Option[ControlOptions] = Option(mapping.getControlOptions)
    val count = controlOpts.flatMap(opts => Option(opts.getCount)).map(_.toShort)
    val onTime = controlOpts.flatMap(opts => Option(opts.getOnTime)).map(_.toLong)
    val offTime = controlOpts.flatMap(opts => Option(opts.getOffTime)).map(_.toLong)

    val typ = controlOpts.flatMap(opts => Option(opts.getType)).map(translateCommandType).getOrElse(ControlCode.CC_PULSE)

    val isDirectOperate = Option(mapping.getFunction).exists(_ == FunctionType.DIRECT_OPERATE)

    ControlMapping(mapping.getIndex.toInt, typ, count, onTime, offTime, isDirectOperate)
  }

  def translateCommandType(c: ControlType) = c match {
    case ControlType.LATCH_ON => ControlCode.CC_LATCH_ON
    case ControlType.LATCH_OFF => ControlCode.CC_LATCH_OFF
    case ControlType.PULSE => ControlCode.CC_PULSE
    case ControlType.PULSE_CLOSE => ControlCode.CC_PULSE_CLOSE
    case ControlType.PULSE_TRIP => ControlCode.CC_PULSE_TRIP
    case _ => throw new IllegalArgumentException("Invalid Command code")
  }

  def loadConfig(xml: Master): MasterStackConfig = {
    val config = new MasterStackConfig
    config.setMaster(configure(xml, xml.getStack.getAppLayer.getMaxFragSize))
    config.setApp(configure(xml.getStack.getAppLayer))
    config.setLink(configure(xml.getStack.getLinkLayer))
    config
  }

  def configure(xml: LinkLayer): LinkConfig = {
    val cfg = new LinkConfig(xml.isIsMaster, xml.isUseConfirmations)
    cfg.setNumRetry(xml.getNumRetries)
    cfg.setRemoteAddr(xml.getRemoteAddress)
    cfg.setLocalAddr(xml.getLocalAddress)
    cfg.setTimeout(xml.getAckTimeoutMS)
    cfg
  }

  def configure(xml: AppLayer): AppConfig = {
    val cfg = new AppConfig
    cfg.setFragSize(xml.getMaxFragSize)
    cfg.setRspTimeout(xml.getTimeoutMS)
    cfg
  }

  private def configure(xml: Master, fragSize: Int): MasterConfig = {
    val cfg = new MasterConfig
    cfg.setAllowTimeSync(xml.getMasterSettings.isAllowTimeSync)
    cfg.setTaskRetryRate(xml.getMasterSettings.getTaskRetryMS)
    cfg.setIntegrityRate(xml.getMasterSettings.getIntegrityPeriodMS)

    cfg.setDoUnsolOnStartup(xml.getUnsol.isDoTask)
    cfg.setEnableUnsol(xml.getUnsol.isEnable)

    var unsol_class = 0
    if (xml.getUnsol.isClass1) unsol_class = unsol_class | PointClass.PC_CLASS_1.swigValue
    if (xml.getUnsol.isClass2) unsol_class = unsol_class | PointClass.PC_CLASS_2.swigValue
    if (xml.getUnsol.isClass3) unsol_class = unsol_class | PointClass.PC_CLASS_3.swigValue
    cfg.setUnsolClassMask(unsol_class)

    cfg.setFragSize(fragSize)

    xml.getScanList.getExceptionScan.foreach { scan =>
      var point_class = PointClass.PC_CLASS_0.swigValue
      if (scan.isClass1) point_class = point_class | PointClass.PC_CLASS_1.swigValue
      if (scan.isClass2) point_class = point_class | PointClass.PC_CLASS_2.swigValue
      if (scan.isClass3) point_class = point_class | PointClass.PC_CLASS_3.swigValue
      cfg.AddExceptionScan(point_class, scan.getPeriodMS)
    }

    cfg
  }
}

class Dnp3XmlConfigurer {
  import MasterXmlConfig._

  private val jaxbContext = JAXBContext.newInstance("io.greenbus.dnp3.xml")

  def readMasterConfig(bytes: Array[Byte]): (Dnp3MasterConfig, io.greenbus.dnp3.master.IndexMapping, Map[String, OutputMapping]) = {
    val um = jaxbContext.createUnmarshaller
    val is = new ByteArrayInputStream(bytes)

    val obj = um.unmarshal(is)
    val xml = obj.asInstanceOf[DNPMasterEndpoint]

    loadConfig(xml)
  }
}

 */

/*
class Dnp3Manager {
  private val stackManager = new StackManager
  private val logAdapter = new LogAdapter

  stackManager.AddLogHook(logAdapter)

  private var observerMap = Map.empty[ModelUUID, (String, String, MeasAdapter, StackAdapter)]

  def addMaster(uuid: ModelUUID, name: String, config: Dnp3MasterConfig, mapping: IndexMapping, controls: Map[String, OutputMapping], measObserver: MeasurementObserver, stackObserver: StackObserver): CommandAdapter = {

    removeMaster(uuid)

    val portName = s"$name-${config.address}:${config.port}"

    val settings = new PhysLayerSettings(config.logLevel, config.retryMs)

    stackManager.AddTCPClient(portName, settings, config.address, config.port)

    val measAdapter = new MeasAdapter(mapping, measObserver.accept)

    val stackAdapter = new StackAdapter(stackObserver)

    config.stack.getMaster.setMpObserver(stackAdapter)

    val commandAcceptor = stackManager.AddMaster(portName, name, config.logLevel, measAdapter, config.stack)

    observerMap += ((uuid, (name, portName, measAdapter, stackAdapter)))

    new CommandAdapter(commandAcceptor, controls)
  }

  def removeMaster(uuid: ModelUUID) {
    observerMap.get(uuid).foreach {
      case (name, portName, _, _) =>
        stackManager.RemoveStack(name)
        stackManager.RemovePort(portName)
        observerMap -= uuid
    }
  }

  def shutdown() {
    stackManager.Shutdown()
  }
}*/
