package xyz.hyperreal.config

import com.typesafe.config.ConfigValueType._
import com.typesafe.config.{ConfigFactory, ConfigObject}

import scala.jdk.CollectionConverters._

abstract class ConfigNode extends Dynamic {
  val path: String
  val asString: String
  val value: Any

  def selectDynamic(segment: String): ConfigNode

  def getInt: Option[Int]

  def int: Int

  def getDouble: Option[Double]

  def double: Double

  def getString: Option[String]

  def string: String

  def getBoolean: Option[Boolean]

  def boolean: Boolean

  def getList: Option[List[Any]]

  def list: List[Any]

  override def toString: String = s"$path ($asString)"
}

class ConfigUndefined(val path: String) extends ConfigNode with Dynamic {
  val asString = "undefined"
  val value: Null = null

  private def undefined = sys.error(s"path $path is undefined")

  def selectDynamic(segment: String): ConfigUndefined = this

  def getInt: Option[Int] = None

  def int: Int = undefined

  def getDouble: Option[Double] = None

  def double: Double = undefined

  def getString: Option[String] = None

  def string: String = undefined

  def getBoolean: Option[Boolean] = None

  def boolean: Boolean = undefined

  def getList: Option[List[Any]] = None

  def list: List[Any] = undefined
}

class ConfigBranch(val path: String, obj: ConfigObject, val value: Map[String, AnyRef], val asString: String)
    extends ConfigNode {
  def selectDynamic(segment: String): ConfigNode = {
    val c = obj.get(segment)

    def primitive(typ: String) = new ConfigLeaf(s"$path.$segment", c.unwrapped, s"$typ: ${c.unwrapped}")

    if (c eq null) new ConfigUndefined(s"$path.$segment")
    else
      c.valueType match {
        case OBJECT =>
          new ConfigBranch(s"$path.$segment",
                           c.asInstanceOf[ConfigObject],
                           c.asInstanceOf[ConfigObject].unwrapped.asScala.toMap,
                           s"object: $c")
        case LIST =>
          new ConfigLeaf(s"$path.$segment", c.asInstanceOf[ConfigObject].unwrapped.asScala.toList, s"list: $c")
        case BOOLEAN                                    => primitive("boolean")
        case NUMBER if c.unwrapped.isInstanceOf[Double] => primitive("double")
        case NUMBER                                     => primitive("integer")
        case STRING                                     => primitive("string")
        case NULL                                       => primitive("null")
      }
  }

  private def notPrimitive = sys.error(s"path $path is an object not a value")

  def getInt: Option[Int] = notPrimitive

  def int: Int = notPrimitive

  def getDouble: Option[Double] = notPrimitive

  def double: Double = notPrimitive

  def getString: Option[String] = notPrimitive

  def string: String = notPrimitive

  def getBoolean: Option[Boolean] = notPrimitive

  def boolean: Boolean = notPrimitive

  def getList: Option[List[Any]] = notPrimitive

  def list: List[Any] = notPrimitive

}

class ConfigLeaf(val path: String, val value: Any, val asString: String) extends ConfigNode {
  def selectDynamic(segment: String): Nothing = sys.error(s"there's no $segment: path $path is a value not an object")

  def getInt: Option[Int] = Some(int)

  def int: Int = value.asInstanceOf[Int]

  def getDouble: Option[Double] = Some(double)

  def double: Double = value.asInstanceOf[Double]

  def getString: Option[String] = Some(string)

  def string: String = value.asInstanceOf[String]

  def getBoolean: Option[Boolean] = Some(boolean)

  def boolean: Boolean = value.asInstanceOf[Boolean]

  def getList: Option[List[Any]] = Some(list)

  def list: List[Any] = value.asInstanceOf[List[Any]]
}

object root
    extends ConfigBranch("<root>",
                         ConfigFactory.load.root,
                         ConfigFactory.load.root.unwrapped().asScala.toMap,
                         s"object ${ConfigFactory.load.root}")
