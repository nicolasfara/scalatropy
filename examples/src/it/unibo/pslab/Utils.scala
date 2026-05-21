package it.unibo.pslab

import cats.effect.std.Console

def log[F[_]: Console, A](basic: String)(param: A): F[Unit] =
  F.println(basic + param.toString())
