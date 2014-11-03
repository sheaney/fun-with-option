package com.example

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] =
    this match {
      case Some(a) => Some(f(a))
      case None    => None
    }

  def getOrElse[B >: A](default: => B): B =
    this match {
      case Some(a) => a
      case None    => default
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this map f getOrElse None

  def orElse[B >: A](op: => Option[B]): Option[B] =
    op flatMap (b => Some(this getOrElse b))

  def filter(p: A => Boolean): Option[A] =
    this flatMap (a => if (p(a)) Some(a) else None)
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
