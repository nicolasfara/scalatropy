package it.unibo.pslab.matmul

import scala.util.Random

import upickle.default.ReadWriter

case class Matrix[T](values: List[List[T]]) derives ReadWriter:
  val rows: Int = values.length
  val cols: Int = if rows > 0 then values.head.length else 0
  def rowSlice(start: Int, end: Int): Matrix[T] = Matrix(values.slice(start, end))
  def show: String = values.map(_.map(_.toString).mkString("  ")).mkString("\n")

object Matrix:
  def draw(rows: Int = 10, cols: Int = 10): Matrix[Double] = Matrix(List.fill(rows, cols)(Random().nextDouble() * 10))

  def product(rows: Int, cols: Int): Matrix[Double] =
    Matrix(List.tabulate(rows, cols)((i, j) => ((i + 1) * (j + 1)).toDouble))

case class MatrixChunk[T](startRow: Int, endRow: Int, subMatrix: Matrix[T]) derives ReadWriter

case class Vec[T](values: List[T]) derives ReadWriter:
  def show: String = values.map(_.toString).mkString("[", ", ", "]")

object Vec:
  def draw(size: Int): Vec[Double] = Vec(List.fill(size)(Random().nextDouble() * 10))

  def naturals(size: Int): Vec[Double] = Vec(List.tabulate(size)(i => (i + 1).toDouble))

case class VecChunk[T](startRow: Int, endRow: Int, values: Vec[T]) derives ReadWriter

object LinearAlgebra:

  extension (matrix: Matrix[Double])
    infix def *(vector: Vec[Double]): Vec[Double] =
      Vec(matrix.values.map(_.zip(vector.values).map { case (a, b) => a * b }.sum))
