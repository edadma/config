package xyz.hyperreal.config

import com.typesafe.config.ConfigValueType._
import com.typesafe.config.{ConfigFactory, ConfigObject}

import scala.jdk.CollectionConverters._

abstract class ConfigNode extends Dynamic {
  val path: String
  protected val asString: String
  val value: Any

  def selectDynamic(segment: String): ConfigNode

  def int: Option[Int]

  def double: Option[Double]

  def number: Option[Number]

  def string: Option[String]

  def boolean: Option[Boolean]

  def list: Option[List[Any]]

  override def toString: String = s"$path ($asString)"
}

class ConfigUndefined(val path: String) extends ConfigNode with Dynamic {
  protected val asString = "undefined"
  val value: Null = null

  def selectDynamic(segment: String): ConfigUndefined = this

  def int: Option[Int] = None

  def double: Option[Double] = None

  def number: Option[Number] = None

  def string: Option[String] = None

  def boolean: Option[Boolean] = None

  def list: Option[List[Any]] = None
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

  def int: Option[Int] = notPrimitive

  def double: Option[Double] = notPrimitive

  def number: Option[Number] = notPrimitive

  def string: Option[String] = notPrimitive

  def boolean: Option[Boolean] = notPrimitive

  def list: Option[List[Any]] = notPrimitive

}

class ConfigLeaf(val path: String, val value: Any, val asString: String) extends ConfigNode {
  def selectDynamic(segment: String): Nothing = sys.error(s"there's no $segment: path $path is a value not an object")

  def int: Option[Int] = Some(value.asInstanceOf[Int])

  def double: Option[Double] = Some(value.asInstanceOf[Double])

  def number: Option[Number] = Some(value.asInstanceOf[Number])

  def string: Option[String] = Some(value.asInstanceOf[String])

  def boolean: Option[Boolean] = Some(value.asInstanceOf[Boolean])

  def list: Option[List[Any]] = Some(value.asInstanceOf[List[Any]])
}

object root
    extends ConfigBranch("<root>",
                         ConfigFactory.load.root,
                         ConfigFactory.load.root.unwrapped().asScala.toMap,
                         s"object ${ConfigFactory.load.root}")
