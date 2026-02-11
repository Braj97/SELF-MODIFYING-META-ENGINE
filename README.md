# SELF-MODIFYING-META-ENGINE
Scala
import scala.util.Random
import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._

// =====================================
// Expression Algebra System
// =====================================

sealed trait Expr {
  def eval(x: Double): Double
}

case class Const(value: Double) extends Expr {
  def eval(x: Double): Double = value
}

case class Variable() extends Expr {
  def eval(x: Double): Double = x
}

case class Add(a: Expr, b: Expr) extends Expr {
  def eval(x: Double): Double = a.eval(x) + b.eval(x)
}

case class Sub(a: Expr, b: Expr) extends Expr {
  def eval(x: Double): Double = a.eval(x) - b.eval(x)
}

case class Mul(a: Expr, b: Expr) extends Expr {
  def eval(x: Double): Double = a.eval(x) * b.eval(x)
}

case class Div(a: Expr, b: Expr) extends Expr {
  def eval(x: Double): Double = {
    val denominator = b.eval(x)
    if (denominator == 0) 0 else a.eval(x) / denominator
  }
}

case class Sin(e: Expr) extends Expr {
  def eval(x: Double): Double = math.sin(e.eval(x))
}

case class Cos(e: Expr) extends Expr {
  def eval(x: Double): Double = math.cos(e.eval(x))
}

// =====================================
// Expression Generator
// =====================================

object ExpressionFactory {

  def randomExpr(depth: Int): Expr = {
    if (depth == 0) {
      if (Random.nextBoolean())
        Const(Random.nextDouble() * 10)
      else
        Variable()
    } else {
      Random.nextInt(6) match {
        case 0 => Add(randomExpr(depth - 1), randomExpr(depth - 1))
        case 1 => Sub(randomExpr(depth - 1), randomExpr(depth - 1))
        case 2 => Mul(randomExpr(depth - 1), randomExpr(depth - 1))
        case 3 => Div(randomExpr(depth - 1), randomExpr(depth - 1))
        case 4 => Sin(randomExpr(depth - 1))
        case _ => Cos(randomExpr(depth - 1))
      }
    }
  }
}

// =====================================
// Functional Mutation Engine
// =====================================

object Mutator {

  def mutate(expr: Expr, probability: Double = 0.1): Expr = {
    if (Random.nextDouble() < probability)
      ExpressionFactory.randomExpr(2)
    else
      expr match {
        case Add(a, b) => Add(mutate(a), mutate(b))
        case Sub(a, b) => Sub(mutate(a), mutate(b))
        case Mul(a, b) => Mul(mutate(a), mutate(b))
        case Div(a, b) => Div(mutate(a), mutate(b))
        case Sin(e)    => Sin(mutate(e))
        case Cos(e)    => Cos(mutate(e))
        case other     => other
      }
  }
}

// =====================================
// Fitness Evaluator
// =====================================

object Fitness {

  // Target function: x^2 + 3x + 1
  def target(x: Double): Double = x * x + 3 * x + 1

  def score(expr: Expr): Double = {
    val testPoints = (-10 to 10).map(_.toDouble)

    val error = testPoints.par.map { x =>
      val diff = expr.eval(x) - target(x)
      diff * diff
    }.sum

    error
  }
}

// =====================================
// Evolution Engine
// =====================================

object EvolutionEngine {

  val populationSize = 50
  val generations = 50

  def evolve(): Expr = {

    var population = Vector.fill(populationSize)(
      ExpressionFactory.randomExpr(3)
    )

    for (gen <- 1 to generations) {

      val scored = population.map(expr => (expr, Fitness.score(expr)))

      val sorted = scored.sortBy(_._2)

      println(s"Generation: $gen | Best Error: ${sorted.head._2}")

      val survivors = sorted.take(10).map(_._1)

      val newPopulation = survivors.flatMap { expr =>
        Seq(
          expr,
          Mutator.mutate(expr, 0.2),
          Mutator.mutate(expr, 0.3),
          Mutator.mutate(expr, 0.4),
          ExpressionFactory.randomExpr(3)
        )
      }

      population = newPopulation.take(populationSize).toVector
    }

    population.minBy(Fitness.score)
  }
}

// =====================================
// Main Application
// =====================================

object MetaEngine {

  def main(args: Array[String]): Unit = {

    println("=== SELF EVOLVING META ENGINE ===\n")

    val best = EvolutionEngine.evolve()

    println("\nBest Evolved Expression Performance:")
    println("-------------------------------------")

    for (x <- -5 to 5) {
      val result = best.eval(x)
      println(f"x = $x%2d  =>  $result%2.4f")
    }

    println("\nEngine Completed.")
  }
}
# OUTPUT
https://i.supaimg.com/3f67040b-ffde-4cd4-9020-6a86a2ebeec4.png
