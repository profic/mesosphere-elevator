import scala.collection.mutable.ListBuffer

sealed abstract class Direction

case object Up extends Direction
case object Down extends Direction
case object Idle extends Direction

// For the sake of simplicity it will be a trait implemented by the Intention,
// which is an Order in mind of the Resident
trait PickupRequest {
  def at: Int
  def direction: Direction
}

/**
  * Goal describe demand to achieve certain floor
  *
  * @param to Target floor
  * @param dir Reserved direction
  * @param score Schedule score counted for this goal
  */
case class Goal(to: Int, dir: Direction, score: Double) {
  def same(r: PickupRequest): Boolean = r.at == to && r.direction == dir
  def same(g: Goal): Boolean = g.to == to && g.dir == dir
}

case class Order(oid: Int, from: Int, to: Int, time: Int) extends PickupRequest {
  require(from != to)

  override def at = from

  override def direction: Direction = {
    if (to > from) {
      Up
    } else {
      Down
    }
  }
}

trait ElevatorState {
  var pos: Int = 0
  var orders: List[Order] = List()

  def free = orders.isEmpty

  def distance(to: Int): Int = {
    math.abs(to - pos)
  }

  def distance(to: Goal): Int = {
    distance(to.to)
  }

  def distance(to: PickupRequest): Int = {
    distance(to.at)
  }

  def distance(to: Order): Int = {
    if (orders.contains(to)) {
      distance(to.to)
    } else {
      distance(to.at)
    }
  }
}

trait ElevatorController {
  var tasks: List[Goal] = List()
  var prev: Option[Goal] = None

  def goal = tasks.headOption
  def idle = direction == Idle
  def floor: Int

  def step(): Unit

  def stop() = tasks = List()

  def direction: Direction = direction(tasks.headOption)

  def direction(goal: Option[Goal]): Direction = {
    goal.map({ t =>
      if (t.to - floor > 0) {
        Up
      } else if (t.to - floor < 0) {
        Down
      } else {
        Idle
      }
    }).getOrElse(Idle)
  }

  def describeIntention = {
    direction match {
      case Idle => "Idle"
      case x => s"going $x/$tasks"
    }
  }
}

class Elevator(id: Int) extends ElevatorState with ElevatorController {
  override def floor = pos

  private[this] def goal(to: Order): Goal = {
    def goal(to: PickupRequest): Goal = Goal(to.at, to.direction, distance(to))
    if (orders.contains(to)) {
      Goal(to.to, to.direction, distance(to.to))
    } else {
      goal(to.asInstanceOf[PickupRequest])
    }
  }

  override def step(): Unit = {
    // If there are requests inside the elevator is busy
    // If it's idle in that case, go for the closest
    def selectNextStep =
      if (tasks.isEmpty) {
        orders.sortBy(distance).headOption.map({ o => List(goal(o))}).getOrElse(List())
      } else {
        tasks
      }

    tasks = selectNextStep

    if (direction == Up) {
      pos += 1
    } else if (direction == Down) {
      pos -= 1
    }

    // Server all this floor orders - this is ideal world
    val takeoff = orders.count(_.to == pos)
    orders = orders.filter(_.to != pos)

    if (direction == Idle && tasks.nonEmpty) {
      prev = Option(tasks.head)
      tasks = tasks.tail
    }

    if (takeoff > 0 && free && idle) {
      this.prev = None
    }
  }

  override def toString = s"Elevator #$id at $pos, $describeIntention carry $orders"
}

object Elevator {
  private[this] var lastElevatorId = 0

  def next = {
    lastElevatorId += 1
    new Elevator(lastElevatorId)
  }

  def apply() = next
}

trait ControlSystem {
  // TODO Should provide Id, Orders, Tasks
  type ControlSystemStatus = Unit

  def connect(to: Seq[Elevator]): Unit
  def pickup(o: PickupRequest): Unit
  def status: ControlSystemStatus
  def score(e: Elevator)(g: PickupRequest): Option[Goal]
}

abstract class ControlSystemBase extends ControlSystem with TimeMachine {
  // For the sake of simplicity we keep it here not to repeat this in Building
  var queue = new ListBuffer[PickupRequest]()

  // The control system is directly connected to the elevators controllers
  // Or keeps their state in memory
  var elevators: Seq[Elevator] = List()

  override def connect(to: Seq[Elevator]) = elevators = to
  override def pickup(o: PickupRequest) = queue += o
  override def status: ControlSystemStatus = {
    println(s"Queue $queue")

    for (e <- elevators) {
      println(e)
    }
  }

  def goals: Iterable[PickupRequest] = queue.groupBy(_.at).flatMap({ a =>
    a._2.groupBy(_.direction).map({b => b._2.head})
  })

  def assignedGoals: Iterable[PickupRequest] = elevators.flatMap(_.orders).groupBy(_.to).flatMap({ a =>
    a._2.groupBy(_.direction).map({b => b._2.head})
  })

  def unassignedGoals: Iterable[PickupRequest] = goals.toSet -- assignedGoals

  def take(e: Elevator): Unit = {
    queue.find(_.at == e.pos).take(1).foreach({
      o => take(e, o.direction)
    })
  }

  def take(e: Elevator, d: Direction): Unit = {
    queue
      .filter({ t => t.at == e.pos && t.direction == d })
      .foreach({ o =>
        e.orders = e.orders.+:(o.asInstanceOf[Order])
        queue -= o
    })
  }

  override def step() = {
    // Pickups
    for (e <- elevators) {
      // Idle
      if (e.free && e.idle) {
        if (e.prev.nonEmpty) {
          take(e, e.prev.get.dir)
        } else {
          take(e)
        }
      }

      // On the way
      if (!e.free && e.direction != Idle) {
        take(e, e.direction)
      }
    }

    // Assign
    if (queue.nonEmpty) {
      var free = elevators.filter(_.free)
      var scores = free.flatMap({ e => queue.map({ r => (e, score(e)(r))})})
      // Do not touch none
      val skip = free.filter({ e => scores.exists({ pair => e == pair._1 && pair._2.isEmpty})})
      free = free.filterNot(skip.contains(_))
      scores = scores.filterNot({ pair => pair._2.isEmpty || skip.exists({_.tasks.exists(pair._2.get.same)}) })
      // Assign best score greedy
      while (scores.nonEmpty && free.nonEmpty) {
        val next = scores.minBy(_._2.get.score)
        next._1.tasks = List(next._2.get)
        free = free.filter(_ != next._1)
        scores = scores.filterNot({ pair => pair._1 == next._1 || pair._2.get.same(next._2.get)})
      }
      // Stop left
      free.foreach(_.stop())
    }
  }
}

// First-come First-served implementation of ControlSystem
class FCFS extends ControlSystemBase {
  override def score(e: Elevator)(g: PickupRequest): Option[Goal] = {
    if (!e.idle) {
      // In FCFS never reschedule elevators until they are totally Idle
      None
    } else {
      Option(Goal(g.at, g.direction, e.distance(g)))
    }
  }
}

// Nearest Car
class NearestCar extends ControlSystemBase {
  def distance(e: Elevator)(g: PickupRequest) = {
    if (e.direction == g.direction || e.direction == Idle) {
      e.distance(g)
    } else {
      // Changing direction costs more
      1 + e.distance(g)
    }
  }

  override def score(e: Elevator)(g: PickupRequest): Option[Goal] = {
    Option(Goal(g.at, g.direction, distance(e)(g)))
  }
}

trait TimeMachine {
  var clock = 0

  def step() = clock += 1
}

class Building(floors: Int, elevators: Int, residents: Int) extends TimeMachine {
  private[this] var oid = 0

  val incomingSpeed = 5
  val r = scala.util.Random

  val property = 1 to elevators map { _ => Elevator() }
  val controller: ControlSystemBase = {
    val c = new NearestCar
    c.connect(property)
    c
  }

  override def step() = {
    clock += 1

    // Incoming residents
    val incoming = r.nextInt(incomingSpeed)
    if (incoming > 0) {
      val pr = newPickupRequest
      println(s"Incoming (PickupRequest at ${pr.at}, ${pr.direction}})")
      controller.pickup(pr)
    }

    // Tick controller
    controller.step()

    // Tick elevators
    property.foreach(_.step())

    status()
  }

  def newPickupRequest: PickupRequest = newOrder

  def newOrder: Order = {
    var from = r.nextInt(floors)
    var to = r.nextInt(floors)
    while (from == to) {
      from = r.nextInt(floors)
      to = r.nextInt(floors)
    }
    newOrder(from, to)
  }

  def newOrder(from: Int, to: Int): Order = {
    oid += 1
    Order(oid, from, to, clock)
  }

  def status() = {
    println(s"Building clock #$clock")
    controller.status
  }
}

object Main extends App {
  val floors = 16
  val elevators = 5
  val residents = 100
  val time = 100

  val universe = new Building(floors, elevators, residents).asInstanceOf[TimeMachine]
  for (time <- 1 to time) {
    universe.step
  }
}