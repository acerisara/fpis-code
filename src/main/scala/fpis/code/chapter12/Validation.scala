package fpis.code.chapter12

import java.time.LocalDate
import java.time.format.DateTimeFormatter

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector())
    extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

case class WebForm(name: String, birthdate: LocalDate, phoneNumber: String)

object Validation {

  def validationApplicative[E]
      : Applicative[({ type f[x] = Validation[E, x] })#f] = new Applicative[
    ({
      type f[x] = Validation[E, x]
    })#f
  ] {
    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(
        f: (A, B) => C
    ): Validation[E, C] = (fa, fb) match {
      case (Success(a), Success(b)) => Success(f(a, b))
      case (Failure(h1, t1), Failure(h2, t2)) =>
        Failure(h1, t1 ++ Vector(h2) ++ t2)
      case (_, Failure(h, t)) => Failure(h, t)
      case (Failure(h, t), _) => Failure(h, t)
    }

    override def unit[A](a: => A): Validation[E, A] = Success(a)
  }

  def validName(name: String): Validation[String, String] =
    if (name != "") Success(name) else Failure("Name cannot be empty")

  def validBirthdate(birthdate: String): Validation[String, LocalDate] =
    try {
      Success(
        LocalDate.parse(birthdate, DateTimeFormatter.ofPattern("yyyy-MM-dd"))
      )
    } catch {
      case _: Exception => Failure("Birthdate must be in the form yyyy-MM-dd")
    }

  def validPhone(phoneNumber: String): Validation[String, String] =
    if (phoneNumber.matches("[0-9]{10}")) Success(phoneNumber)
    else Failure("Phone number must be 10 digits")

  def validateWebForm(
      name: String,
      birthdate: String,
      phone: String
  ): Validation[String, WebForm] =
    validationApplicative[String].map3(
      validName(name),
      validBirthdate(birthdate),
      validPhone(phone)
    )(WebForm)

}
