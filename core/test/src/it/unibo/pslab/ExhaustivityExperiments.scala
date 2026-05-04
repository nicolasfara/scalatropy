package it.unibo.pslab

import scala.quoted.*

// Runtime scope — collects configured types
class ConfigScope:
  def configure[D](config: => Unit): Unit =
    ()

def configure[D](config: => Unit)(using scope: ConfigScope): Unit =
  scope.configure(config)

opaque type MyDSL = String

def myDSL: MyDSL = "my-dsl"

extension (dsl: MyDSL)

  // Macro checks the block statically
  inline def myProgramConfiguration(
      inline block: ConfigScope ?=> Unit,
  ): Unit = ${ programConfigurationImpl('block) }

def programConfigurationImpl(
    block: Expr[ConfigScope ?=> Unit],
)(using Quotes): Expr[Unit] =
  import quotes.reflect.*

  val required = Set("Device1", "Device2")
  val configured = collectConfiguredTypes(block.asTerm) // walk the AST

  val missing = required -- configured
  if missing.nonEmpty then report.errorAndAbort(s"Missing configurations for: ${missing.mkString(", ")}")

  '{
    given scope: ConfigScope = new ConfigScope()
    $block(using scope)
  }

def collectConfiguredTypes(using Quotes)(term: quotes.reflect.Term): Set[String] =
  import quotes.reflect.*
  // Walk AST looking for `configure[X](...)` calls and extract type arg names
  val found = collection.mutable.Set[String]()
  new TreeAccumulator[Unit]:
    def foldTree(acc: Unit, tree: Tree)(owner: Symbol): Unit =
      tree match
        case Inlined(_, bindings, body) => foldTree(acc, body)(owner)

        // Double-Apply: configure[D](config)(using scope)
        // Inner Apply carries the type args, outer Apply carries the implicit scope
        case Apply(inner @ Apply(TypeApply(fun, List(tpt)), _), _) if fun.symbol.name == "configure" =>
          found += tpt.tpe.typeSymbol.name
          foldOverTree(acc, tree)(owner)

        case _ => foldOverTree(acc, tree)(owner)
  .foldTree((), term)(Symbol.spliceOwner)
  found.toSet
