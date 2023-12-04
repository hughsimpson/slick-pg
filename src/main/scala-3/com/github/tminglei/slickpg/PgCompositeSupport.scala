package com.github.tminglei.slickpg

import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.{Tag => TTag}

import scala.reflect.{ClassTag, classTag}
import composite.Struct
import slick.jdbc.{PositionedResult, PostgresProfile}
import slick.jdbc.SetParameter

import scala.deriving.*
import scala.compiletime.{error, erasedValue, summonInline}
import utils.PgTokenHelper._
import utils.TypeConverters._
import utils.RegisteredTypeConverter

case class Args(args: Any*)

sealed trait TokenConverter[T] {
  def fromToken(token: Token): T

  def toToken(value: T): Token
}


//case class SimpleConverter(fromString: (String => T), ToString: (T => String), level: Int) extends TokenConverter[T] {
//  def fromToken(token: Token): Any =
//    if (token == Null) null else fromString(getString(token, level))
//
//  def toToken(value: Any): Token =
//    if (value == null) Null else Chunk(ToString(value))
//}

trait PgCompositeSupport extends utils.PgCommonJdbcTypes with array.PgArrayJdbcTypes { driver: PostgresProfile =>

  protected lazy val emptyMembersAsNull = true

  //---
  def createCompositeJdbcType[T <: Struct](sqlTypeName: String, cl: ClassLoader = getClass.getClassLoader): GenericJdbcType[T] =
    throw new UnsupportedOperationException("Composite support is unimplemented for scala 3")

  def createCompositeArrayJdbcType[T <: Struct](sqlTypeName: String, cl: ClassLoader = getClass.getClassLoader)(implicit ev: TTag[T], tag: ClassTag[T]): AdvancedArrayJdbcType[T] =
    throw new UnsupportedOperationException("Composite support is unimplemented for scala 3")

  /// Plain SQL support
  def nextComposite[T <: Struct](r: PositionedResult, cl: ClassLoader = getClass.getClassLoader)(implicit ev: TTag[T], tag: ClassTag[T]): Option[T] =
    throw new UnsupportedOperationException("Composite support is unimplemented for scala 3")
  def nextCompositeArray[T <: Struct](r: PositionedResult, cl: ClassLoader = getClass.getClassLoader)(implicit ev: TTag[T], tag: ClassTag[T]): Option[Seq[T]] =
    throw new UnsupportedOperationException("Composite support is unimplemented for scala 3")

  def createCompositeSetParameter[T <: Struct](sqlTypeName: String, cl: ClassLoader = getClass.getClassLoader)(implicit ev: TTag[T], tag: ClassTag[T]): SetParameter[T] =
    throw new UnsupportedOperationException("Composite support is unimplemented for scala 3")

  def createCompositeOptionSetParameter[T <: Struct](sqlTypeName: String, cl: ClassLoader = getClass.getClassLoader)(implicit ev: TTag[T], tag: ClassTag[T]): SetParameter[Option[T]] =
    throw new UnsupportedOperationException("Composite support is unimplemented for scala 3")
  def createCompositeArraySetParameter[T <: Struct](sqlTypeName: String, cl: ClassLoader = getClass.getClassLoader)(implicit ev: TTag[T], tag: ClassTag[T]): SetParameter[Seq[T]]  = {
    throw new UnsupportedOperationException("Composite support is unimplemented for scala 3")
  }
  def createCompositeOptionArraySetParameter[T <: Struct](sqlTypeName: String, cl: ClassLoader = getClass.getClassLoader)(implicit ev: TTag[T], tag: ClassTag[T]): SetParameter[Option[Seq[T]]] =
    throw new UnsupportedOperationException("Composite support is unimplemented for scala 3")
}

class PgCompositeSupportUtils(cl: ClassLoader, emptyMembersAsNull: Boolean) {
  import scala.deriving.*
  import scala.compiletime.{error, erasedValue, summonInline}

  implicit def baseConverter[T](using fromString: RegisteredTypeConverter[String, T], toString: RegisteredTypeConverter[T, String]): TokenConverter[T] = new TokenConverter[T] {
    def fromToken(token: Token): T = if (token == Null) null.asInstanceOf[T] else fromString.convert(getString(token, level))

    def toToken(value: T): Token = if (value == null) Null else Chunk(toString.convert(value))
  }

  inline def summonInstances[T, Elems <: Tuple]: List[TokenConverter[?]] = {
    inline erasedValue[Elems] match {
      case _: (elem *: elems) => deriveOrSummon[T, elem] :: summonInstances[T, elems]
      case _: EmptyTuple => Nil
    }
  }

  inline def deriveOrSummon[T, Elem]: TokenConverter[Elem] = {
    inline erasedValue[Elem] match {
      case _: T => deriveRec[T, Elem]
      case _ => summonInline[TokenConverter[Elem]]
    }
  }

  inline def deriveRec[T, Elem]: TokenConverter[Elem] = {
    inline erasedValue[T] match {
      case _: Elem => error("infinite recursive derivation")
      case _ => derived[Elem](using summonInline[Mirror.Of[Elem]]) // recursive derivation
    }
  }

  def convertProduct[T](p: Mirror.ProductOf[T], elems: => List[TokenConverter[?]]): TokenConverter[T] =
    new TokenConverter[T] {
      def fromToken(token: Token): T =
        if (token == Null) null
        else {
          val args =
            getChildren(token)
              .zip(elems)
              .map { case (token, converter) => converter.fromToken(token) }
          p.fromProduct(Args(args:_*))
        }

      def toToken(value: T): Token =
        if (value == null) Null
        else {
          val tokens = value.productIterator.zip(elems).map({
            case (value, converter) => converter.toToken(value)
          })
          val members = Open("(") +: tokens :+ Close(")")
          GroupToken(members)
        }
    }

  inline def derived[T <: Struct](using m: Mirror.Of[T]): TokenConverter[T] = {
    lazy val elemInstances = summonInstances[T, m.MirroredElemTypes]
    inline m match
      case p: Mirror.ProductOf[T] => convertProduct(p, elemInstances)
  }

//  def mkTokenConverter(theType: u.Type, level: Int = -1)(implicit ev: u.TypeTag[String]): TokenConverter = ???
}
