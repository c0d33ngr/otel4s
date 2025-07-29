/*
 * Copyright 2024 Typelevel
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.typelevel.otel4s.scalafix

import scalafix.v1._

import scala.meta._

class V0_5_0Rewrites extends SemanticRule("V0_5_0Rewrites") {

  private object selectors {
    def oteljava(packages: String*): Term.Ref =
      otel4s("oteljava" :: packages.toList: _*)

    def otel4s(packages: String*): Term.Ref =
      make("org" :: "typelevel" :: "otel4s" :: packages.toList: _*)

    def make(packages: String*): Term.Ref =
      packages.tail.foldLeft(Term.Name(packages.head): Term.Ref) { case (selector, pkg) =>
        Term.Select(selector, Term.Name(pkg))
      }
  }

  private object imports {
    val java = SymbolMatcher.exact("org/typelevel/otel4s/java/")
    val metrics = SymbolMatcher.exact("org/typelevel/otel4s/java/metrics/")
    val context = SymbolMatcher.exact("org/typelevel/otel4s/java/context/")
    val trace = SymbolMatcher.exact("org/typelevel/otel4s/java/trace/")
    val instances = SymbolMatcher.exact("org/typelevel/otel4s/java/instances.")
    val localInstances = SymbolMatcher.exact("org/typelevel/otel4s/instances/local.")
  }

  override def fix(implicit doc: SemanticDocument): Patch = {
    val LocalForIoLocal_M = SymbolMatcher.exact(
      "org/typelevel/otel4s/java/instances.localForIoLocal()."
    )

    val LocalForIOLocal_M = SymbolMatcher.exact(
      "org/typelevel/otel4s/instances/local.localForIOLocal()."
    )

    doc.tree.collect {
      case importer @ Importer(ref, _) if imports.java.matches(ref) =>
        val next = importer.copy(ref = selectors.oteljava())
        Patch.replaceTree(importer, next.toString())

      case importer @ Importer(ref, _) if imports.metrics.matches(ref) =>
        val next = importer.copy(ref = selectors.oteljava("metrics"))
        Patch.replaceTree(importer, next.toString())

      case importer @ Importer(ref, _) if imports.context.matches(ref) =>
        val next = importer.copy(ref = selectors.oteljava("context"))
        Patch.replaceTree(importer, next.toString())

      case importer @ Importer(ref, _) if imports.trace.matches(ref) =>
        val next = importer.copy(ref = selectors.oteljava("trace"))
        Patch.replaceTree(importer, next.toString())

      case importer @ Importer(ref, imp) if imports.instances.matches(ref) =>
        val importees = imp.map {
          case Importee.Name(Name("localForIoLocal")) =>
            Importee.Name(Name("localForIOLocal"))

          case Importee.Rename(Name("localForIoLocal"), rename) =>
            Importee.Rename(Name("localForIOLocal"), rename)

          case other => other
        }

        val next = Importer(selectors.otel4s("instances", "local"), importees)
        Patch.replaceTree(importer, next.toString())

      case importer @ Importer(ref, imp) if imports.localInstances.matches(ref) =>
        val filteredImportees = imp.filter {
          case Importee.Name(Name("localForIOLocal")) => false
          case Importee.Rename(Name("localForIOLocal"), _) => false
          case _ => true
        }

        if (filteredImportees.isEmpty) {
          Patch.removeImportee(importer)
        } else {
          val next = Importer(ref, filteredImportees)
          Patch.replaceTree(importer, next.toString())
        }

      case t @ LocalForIoLocal_M(_: Term.Name) =>
        Patch.replaceTree(t, "local.asLocal[F]")

      // Replace localForIOLocal function calls with IOLocal#asLocal
      case t @ Term.Apply(LocalForIOLocal_M(_), args) =>
        // For localForIOLocal(implicit1, implicit2, ioLocal), replace with ioLocal.asLocal[F]
        args.lastOption match {
          case Some(ioLocalArg) =>
            Patch.replaceTree(t, s"${ioLocalArg}.asLocal[F]")
          case None =>
            Patch.empty
        }

      case t @ LocalForIOLocal_M(_: Term.Name) =>
        // For bare localForIOLocal references, replace with local.asLocal[F]
        Patch.replaceTree(t, "local.asLocal[F]")

      // meter ops
      case MeterOps.Counter(patch) =>
        patch

      case MeterOps.Histogram(patch) =>
        patch

      case MeterOps.UpDownCounter(patch) =>
        patch

      case MeterOps.ObservableGauge(patch) =>
        patch

      case MeterOps.ObservableCounter(patch) =>
        patch

      case MeterOps.ObservableUpDownCounter(patch) =>
        patch

    }.asPatch + Patch.replaceSymbols(
      "org.typelevel.otel4s.trace.Status" -> "org.typelevel.otel4s.trace.StatusCode"
    )
  }

  private object MeterOps {
    abstract class MeterOpMatcher(termName: String, tpe: String) {
      private val matcher = SymbolMatcher.exact(
        s"org/typelevel/otel4s/metrics/Meter#$termName()."
      )

      def unapply(term: Term)(implicit doc: SemanticDocument): Option[Patch] = {
        term match {
          case a @ Term.Apply.After_4_6_0(select, arg) if matcher.matches(a) =>
            val rewrite = Term.Apply(
              Term.ApplyType(select, Type.ArgClause(List(Type.Name(tpe)))),
              arg
            )

            Some(Patch.replaceTree(a, rewrite.toString()))

          case _ =>
            None
        }
      }
    }

    object Counter extends MeterOpMatcher("counter", "Long")
    object Histogram extends MeterOpMatcher("histogram", "Double")
    object UpDownCounter extends MeterOpMatcher("upDownCounter", "Long")
    object ObservableGauge extends MeterOpMatcher("observableGauge", "Double")
    object ObservableCounter extends MeterOpMatcher("observableCounter", "Long")
    object ObservableUpDownCounter extends MeterOpMatcher("observableUpDownCounter", "Long")
  }

}
