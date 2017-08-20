package com.example

import org.scalatest._

class OptionSpec extends FlatSpec with Matchers {
  import SpecHelpers._
  import com.example._

  "Option" should "be mappable" in {
    val opt: Option[Int] = Some(10)
    opt.map(a => a * 10) shouldBe Some(100)

    val opt2: Option[Int] = None
    opt2.map(a => a * 10) shouldBe None
  }

  "Option" should "be obtainable with getOrElse" in {
    Some("Hello!") getOrElse "Good Bye!" shouldBe "Hello!"
    None getOrElse "Good Bye!" shouldBe "Good Bye!"
  }

  "Option" should "be flatMappable" in {
    val opt: Option[Int] = Some(10)
    val opt2: Option[Int] = Some(20)

    val result =
      opt flatMap { a =>
        opt2 map { b =>
          a + b
        }
      }

    //val result =
      //for {
        //a ←  opt
        //b ←  opt2
      //} yield a + b

    result shouldBe Some(30)
  }

  "Option" should "be recoverable" in {
    val none: Option[Double] = None

    Some(1.0) orElse Some(2.0) shouldBe Some(1.0)

    none orElse Some(2.0) shouldBe Some(2.0)

    none orElse none shouldBe none
  }

  "Option" should "be filterable" in {

    val opt = Some(2)

    opt filter isPair shouldBe Some(2)
    opt filter greaterThan(1) shouldBe Some(2)
    opt filter greaterThan(2) shouldBe None
  }

  object SpecHelpers {
    def isPair(x: Int) = x % 2 == 0
    def greaterThan(n: Int)(x: Int) = x > n
  }
}
