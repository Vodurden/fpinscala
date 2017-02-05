package fpinscala.parsing

import language.higherKinds
import scala.util.matching.Regex

import fpinscala.testing._

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  // Primatives
  implicit def string(s: String): Parser[String]
  def succeed[A](a: A): Parser[A]
  def regex(r: Regex): Parser[String]
  def slice[A](p: Parser[A]): Parser[String]
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]
  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]
  def not[A](p: Parser[A]): Parser[A]

  def map[A,B](p: Parser[A])(f: A => B): Parser[B] =
    p.flatMap(a => succeed(f(a)))

  def map2[A,B,C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = for {
    a <- p1
    b <- p2
  } yield f(a, b)

  def product[A,B](p1: Parser[A], p2: => Parser[B]): Parser[(A,B)] = for {
    a <- p1
    b <- p2
  } yield (a, b)

  def sequence[A](parsers: List[Parser[A]]): Parser[List[A]] =
    parsers.foldRight(succeed(List[A]()))((p, acc) => map2(p, acc)(_ :: _))

  def digit[A](p: Parser[A]): Parser[Double] =
    p.slice.map(v => v.toDouble)

  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    sequence(List.range(0, n).map(_ => p))

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def between[A,B,C](lhs: Parser[A], rhs: Parser[B])(body: Parser[C]): Parser[C] = for {
    _ <- lhs
    data <- body
    _ <- rhs
  } yield data

  def delimited[A, B](entry: Parser[A], delimiter: Parser[B]): Parser[List[A]] =
    entry.flatMap(value =>
      delimiter.flatMap(d => delimited(entry, delimiter)).or(succeed(List(value)))
    ).or(succeed(Nil))

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String]
    = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def not: Parser[A] = self.not(p)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def map2[B,C](p2: Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def slice: Parser[String] = self.slice(p)
    def digit: Parser[Double] = self.digit(p)
    def char(c: Char): Parser[Char] = self.char(c)
    def many: Parser[List[A]] = self.many(p)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapIdentityLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def orCommutative[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      equal(or(p1, p2), or(p2, p1))(in)

    def orAssociative[A](p1: Parser[A], p2: Parser[A], p3: Parser[A])(in: Gen[String]): Prop =
      equal(p1 | (p2 | p3), (p1 | p2) | p3)(in)

    // a | b == b | a
    // (a | b) | c == a | (b | c)

    // map(p)(a => a) == p

    // run(char(c))(c.toString) == Right(c)
    // run(char(s))(s.toString) == Right(s)
    // run(listOfN(n, c))(c.repeat(n)) == Right(c.repeat(n))
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON
}

object JsonParser {
  def parser[Err, Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    lazy val jValue: Parser[JSON] = jString | jNumber | jObject | jArray | jBoolean | jNull

    lazy val jString: Parser[JSON.JString] = for {
      _ <- many(char(' ')) | char('"')
      s <- not(char('"')).many.slice
      _ <- char('"')
    } yield JSON.JString(s)

    lazy val jNumber: Parser[JSON] = regex("[0-9]+".r).digit.map(d => JSON.JNumber(d))

    lazy val jObject: Parser[JSON] = {
      val entry: Parser[(String, JSON)] = for {
        key <- jString
        _ <- char(':')
        value <- jValue
      } yield (key.get, value)

      val entries: Parser[List[(String, JSON)]] = delimited(entry, char(','))
      between(char('{'), char('}'))(entries.map(l => JSON.JObject(l.toMap)))
    }

    lazy val jArray: Parser[JSON] = {
      val arrayValue = delimited(jValue, char(','))
      val arrayValues = between(char('['), char(']'))(arrayValue)

      arrayValues.map(l => JSON.JArray(l.toIndexedSeq))
    }

    lazy val jBoolean: Parser[JSON] = ("true" | "false").map {
      case "true" => JSON.JBool(true)
      case "false" => JSON.JBool(false)
    }

    lazy val jNull: Parser[JSON] = string("null").map(_ => JSON.JNull)

    jValue
  }
}

object Parsers {
  def jsonParser[Err, Parser[+_]](P: Parsers[Parser]): Parser[String] = {
    import P._
    val spaces = char(' ').many.slice
    spaces
  }
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}
