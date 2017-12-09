package constraints

import javax.swing.FocusManager

import cafesat.api.Formulas._
import cafesat.api.FormulaBuilder._
import cafesat.api.Solver._

/**
 * This component implements a constraint solver for assigning time slots to volunteers
 * for various tasks at a festival. A task may require more than one volunteer,
 * and a volunteer can take a limited number of tasks
 */
object Balelec {

  import Arithmetic._

  case class Volunteer(name: String) {
    override def toString = name
  }

  /**
   * A task is represented by its name and
   * its capacity, i.e. the exact number of people
   * required to complete it.
   */
  case class Task(name: String, capacity: Int) {
    override def toString = name
  }

  /**
   * This function schedules volunteers to tasks.
   * It takes as input a list of volunteers and a list of tasks.
   * The `availability` map contains mappings from volunteers to the
   * tasks they are available for.
   * A volunteer can be assigned to several tasks, but only
   * up to a maximum number of task specified by the `maxWorkload` parameter.
   * It is ok to not assign a volunteer to any task.
   *
   * The return value is a list of volunteers assigned to each task. The function only
   * returns a complete valid assignment, if no such assignment exists then the
   * function returns None.
   */
  def schedule(
    volunteers: List[Volunteer],
    tasks: List[Task],
    availability: Map[Volunteer, List[Task]],
    maxWorkload: Int
  ): Option[Map[Task, List[Volunteer]]] = {

    val propVariables: Map[(Volunteer, Task), PropVar] =
    volunteers.flatMap({ case b@Volunteer(name) =>
      tasks.map(s => (b, s) -> propVar(name))
    }).toMap


    val availableTasks: Seq[Formula] = {
      volunteers map ( vol => {
        (for{ t <- tasks if availability(vol).contains(t)} yield propVariables((vol,t))).foldLeft[Formula](false)((acc, el) => acc || el)})
    }

    val workLoad: Seq[Formula] = {
      def mostWorkLoad(workLoad: Int): Seq[Formula] = volunteers map (vol => atMostMaxTrue(tasks map (t => propVariables(vol, t)), workLoad))
      mostWorkLoad(maxWorkload)
    }

    val capSatTask: Seq[Formula]={
      def bijection(ns: List[Formula], x: Int): Formula ={
        val (r,c) = countPositiveBits(ns)
        lessEquals(r,int2binary(x)) && lessEquals(int2binary(x),r) && and(c.toSeq:_*)
      }
      tasks map (t =>
      {val proVar2 = for{ volu <- volunteers if availability(volu).contains(t)}yield propVariables(volu,t)
        bijection(proVar2,t.capacity)
    })

}
    //3.Combining all the constraints together
    val allConstraints: Seq[Formula] =
    availableTasks ++ workLoad ++ capSatTask

    val res = solveForSatisfiability(and(allConstraints: _*))

    res.map(model => tasks.map(t => (t,volunteers.filter(vol => model(propVariables((vol, t)))))).toMap)



  }

  /**
   * This function takes a list of constraint, and returns a
   * constraint that is true if and only if at most max
   * of them are true.
   */
  def atMostMaxTrue(ns: List[Formula], max: Int): Formula = {
    val (r, c) = countPositiveBits(ns)
    lessEquals(r, int2binary(max)) && and(c.toSeq:_*)
  }

  /**
   * This function counts the number of positive bits in a number.
   * 
   * It takes a list of formulas, and returns a pair.
   * The first element of the pair is a list of formulas representing the number
   * of ones in `ns`.
   * The second element is a set of additional constraints that have been gathered along
   * the way. Hint: see `adder` for understanding how to use additional constraints
   */
  def countPositiveBits(ns: List[Formula]): (List[Formula], Set[Formula]) = {
    ns.foldLeft((List[Formula](false), Set[Formula]())) { case ((tmpSum, tmpAcc), n) =>
      val (r, c) = adder(tmpSum, List(n))
      (r, tmpAcc ++ c)
    }
  }

}
