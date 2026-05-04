package it.unibo.pslab

trait Device1
trait Device2

def test1(): Unit =
  myDSL.myProgramConfiguration:
    configure[Device1](())
    configure[Device2](())

// def test2(): Unit =
//   myProgramConfiguration:
//     configure[Device1](())
