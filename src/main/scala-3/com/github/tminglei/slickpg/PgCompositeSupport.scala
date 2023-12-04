package com.github.tminglei.slickpg

import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.{Tag => TTag}

import scala.reflect.{ClassTag, classTag}
import composite.Struct
import slick.jdbc.{PositionedResult, PostgresProfile}
import slick.jdbc.SetParameter

import scala.deriving.*
import scala.compiletime.{error, erasedValue, summonInline}

trait PgCompositeSupport extends utils.PgCommonJdbcTypes with array.PgArrayJdbcTypes { driver: PostgresProfile =>

  protected lazy val emptyMembersAsNull = true

  //---
  def createCompositeJdbcType[T <: Struct](sqlTypeName: String, cl: ClassLoader = getClass.getClassLoader)(implicit ev: TTag[T], tag: ClassTag[T]): GenericJdbcType[T] =
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

//  def mkTokenConverter(theType: u.Type, level: Int = -1)(implicit ev: u.TypeTag[String]): TokenConverter = ???
}
