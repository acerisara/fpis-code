package fpis.code.chapter12

import fpis.code.chapter12.Validation.validateWebForm
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.junit.JUnitRunner

import java.time.LocalDate

@RunWith(classOf[JUnitRunner])
class ValidationTest extends AnyFunSuite {

  test("create a web form only if all validations are successful") {
    validateWebForm("Andrea", "1999-01-01", "1111122222") should be(
      Success(WebForm("Andrea", LocalDate.parse("1999-01-01"), "1111122222"))
    )
  }

  test("collect all validation errors") {
    validateWebForm("", "01-01-1999", "") should be(
      Failure(
        "Name cannot be empty",
        Vector(
          "Birthdate must be in the form yyyy-MM-dd",
          "Phone number must be 10 digits"
        )
      )
    )
  }

}
