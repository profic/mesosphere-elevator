import scala.collection.mutable.ListBuffer

sealed abstract class Direction

case object Up extends Direction
case object Down extends Direction
case object Idle extends Direction

case class PickupRequest(at: Int, direction: Direction)

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

object Goal {
  def apply(r: PickupRequest, distance: PickupRequest => Double): Goal = new Goal(r.at, r.direction, distance(r))
}

case class Order(oid: Int, from: Int, to: Int, time: Int) {
  require(from != to)

  def pickupFrom: PickupRequest = PickupRequest(from, direction)
  def pickupTo: PickupRequest = PickupRequest(to, direction)

  def direction: Direction = {
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

  var tasks: Seq[Goal] = List()
  var prev: Option[Goal] = None

  def free = orders.isEmpty

  def goal = tasks.headOption
  def idle = direction == Idle
  def floor: Int = pos

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
      distance(to.pickupTo)
    } else {
      distance(to.pickupFrom)
    }
  }
}

trait ElevatorController {
  def state: ElevatorState

  def selectPath: Seq[Goal] = {
    if (state.tasks.isEmpty) {
      state.orders.sortBy(state.distance).headOption.map({ o => List(goal(o)) }).getOrElse(List())
    } else {
      state.tasks
    }
  }

  def goal(to: Order): Goal = {
    if (state.orders.contains(to)) {
      Goal(to.pickupTo, state.distance)
    } else {
      Goal(to.pickupFrom, state.distance)
    }
  }

  def step(): Unit = {
    state.tasks = selectPath

    if (state.direction == Up) {
      state.pos += 1
    } else if (state.direction == Down) {
      state.pos -= 1
    }

    if (state.direction == Idle && state.tasks.nonEmpty) {
      state.prev = Option(state.tasks.head)
      state.tasks = state.tasks.tail
    }
  }
}

case class ElevatorSnapshot(id: Int, state: ElevatorState) extends ElevatorState {
  pos = state.pos
  orders = state.orders
  tasks = state.tasks
  prev = state.prev
}

class Elevator(id: Int) extends ElevatorState with ElevatorController {
  override def state = this

  private[this] def describeIntention = {
    direction match {
      case Idle => "Idle"
      case x => s"going $x/$tasks"
    }
  }

  def snapshot: ElevatorSnapshot = ElevatorSnapshot(id, state)

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
  def pickup(o: PickupRequest): Unit
  def release(o: PickupRequest): Unit

  def update(to: ElevatorSnapshot): Unit

  def score(e: ElevatorSnapshot)(g: PickupRequest): Option[Goal]
}

abstract class ControlSystemBase extends ControlSystem {
  val state = scala.collection.mutable.Map[Int, ElevatorSnapshot]()
  val goals = scala.collection.mutable.Set[PickupRequest]()

  var clock = 1
  var solutionClock = 0
  var solution = scala.collection.immutable.Map[(Int, PickupRequest), Goal]()

  override def pickup(o: PickupRequest) = {
    if (!goals.contains(o)) {
      clock += 1
      goals += o
    }
  }

  // TODO call this from building on completions
  override def release(o: PickupRequest) = {
    if (goals.contains(o)) {
      clock += 1
      goals -= o
    }
  }

  override def update(to: ElevatorSnapshot) = {
    state(to.id) = to
  }

  override def score(e: ElevatorSnapshot)(g: PickupRequest) = {
    if (solutionClock != clock) {
      solutionClock = solve
    }

    solution get (e.id, g)
  }

  private[this] def solve: Int = {
    ??? // TODO

    clock
  }

  /*
  def goals: Iterable[PickupRequest] = queue.groupBy(_.at).flatMap({ a =>
    a._2.groupBy(_.direction).map({b => b._2.head})
  })

  def assignedGoals: Iterable[PickupRequest] = elevators.flatMap(_.orders).groupBy(_.to).flatMap({ a =>
    a._2.groupBy(_.direction).map({b => b._2.head})
  })

  def unassignedGoals: Iterable[PickupRequest] = goals.toSet -- assignedGoals

  override def step() = {
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
  }  */
}

// First-come First-served implementation of ControlSystem
class FCFS extends ControlSystemBase {
  override def score(e: ElevatorSnapshot)(g: PickupRequest): Option[Goal] = {
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
  def distance(e: ElevatorSnapshot)(g: PickupRequest) = {
    if (e.direction == g.direction || e.direction == Idle) {
      e.distance(g)
    } else {
      // Changing direction costs more
      1 + e.distance(g)
    }
  }

  override def score(e: ElevatorSnapshot)(g: PickupRequest): Option[Goal] = {
    Option(Goal(g.at, g.direction, distance(e)(g)))
  }
}

trait TimeMachine {
  var clock = 0

  def step() = clock += 1
}

class Building(floors: Int, size: Int, residents: Int) extends TimeMachine {
  private[this] var oid = 0

  val incomingSpeed = 5
  val r = scala.util.Random

  val elevators = 1 to size map { _ => Elevator() }

  val controller: ControlSystem = new NearestCar

  var queue = new ListBuffer[Order]()

  def take(e: Elevator): Unit = {
    queue.find(_.from == e.pos).take(1).foreach({
      o => take(e, o.direction)
    })
  }

  def take(e: Elevator, d: Direction): Unit = {
    queue
      .filter({ t => t.from == e.pos && t.direction == d })
      .foreach({ o =>
        e.orders = e.orders.+:(o)
        queue -= o
    })
  }

  override def step() = {
    def pickup() = {
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
    }

    def incoming() = {
      val incoming = r.nextInt(incomingSpeed)
      if (incoming > 0) {
        val pr = newPickupRequest
        println(s"Incoming (PickupRequest at ${pr.at}, ${pr.direction}})")
        controller.pickup(pr)
      }
      println(s"Queue $queue")
    }

    def takeoff() = {
      for (e <- elevators) {
        // Server all this floor orders - this is ideal world
        val takeoff = e.orders.count(_.to == e.pos)
        e.orders = e.orders.filter(_.to != e.pos)

        if (takeoff > 0 && e.free && e.idle) {
          e.prev = None
        }
      }
    }

    clock += 1

    println(s"Building clock #$clock")

    pickup()

    // Simplify distributed nature here with strict ordering of following 2 events
    elevators.map(_.snapshot).foreach(controller.update)
    incoming()

    elevators.foreach(_.step())
    takeoff()

    show()
  }

  def newPickupRequest: PickupRequest = newOrder.pickupFrom

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

  def show() = {
    for (e <- elevators) {
      println(e)
    }
  }
}

object Main extends App {
  val floors = 16
  val elevators = 5
  val residents = 100
  val time = 100

  val universe = new Building(floors, elevators, residents).asInstanceOf[TimeMachine]
  for (time <- 1 to time) {
    universe.step()
  }
}