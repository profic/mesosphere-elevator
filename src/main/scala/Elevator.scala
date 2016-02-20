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
  def distance(g: Goal): Int = math.abs(g.to - to)
}

object Goal {
  def apply(distance: PickupRequest => Int)(r: PickupRequest): Goal = new Goal(r.at, r.direction, distance(r))
  def apply(r: PickupRequest, score: Double): Goal = new Goal(r.at, r.direction, score)
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

  def distance(to: Int): Int = math.abs(to - pos)

  def distance(to: Goal): Int = distance(to.to)

  def distance(to: PickupRequest): Int = distance(to.at)

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
      Goal(state.distance)(to.pickupTo)
    } else {
      Goal(state.distance)(to.pickupFrom)
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
      state.prev = state.tasks.headOption
      state.tasks = state.tasks.tail
    }
  }

  def stop() = state.tasks = List()
}

case class ElevatorSnapshot(id: Int, state: ElevatorState) extends ElevatorState {
  pos = state.pos
  orders = state.orders
  tasks = state.tasks
  prev = state.prev
}

class Elevator(id: Int) extends ElevatorState with ElevatorController {
  override def state = this

  def Id = id

  def snapshot: ElevatorSnapshot = ElevatorSnapshot(id, state)

  private[this] def describeIntention = {
    direction match {
      case Idle => "Idle"
      case x => s"going $x/$tasks"
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
  def pickup(o: PickupRequest): Unit
  def release(o: PickupRequest): Unit

  def update(to: ElevatorSnapshot): Unit
  def score(id: Int)(g: PickupRequest): Option[Double]
}

abstract class ControlSystemBase(floors: Int) extends ControlSystem {
  val state = scala.collection.mutable.Map[Int, ElevatorSnapshot]()
  val goals = scala.collection.mutable.Set[PickupRequest]()

  var clock = 1
  var solutionClock = 0

  var solution = scala.collection.immutable.Map[(Int, PickupRequest), Double]()

  override def pickup(o: PickupRequest) = {
    if (!goals.contains(o)) {
      println(s"Pickup $o")
      clock += 1
      goals += o
    }
  }

  override def release(o: PickupRequest) = {
    if (goals.contains(o)) {
      println(s"Release $o")
      clock += 1
      goals -= o
    }
  }

  override def update(to: ElevatorSnapshot) = {
    state(to.id) = to
  }

  def score(e: ElevatorSnapshot)(g: PickupRequest): Option[Double]

  override def score(id: Int)(g: PickupRequest) = {
    if (solutionClock < clock) {
      println(s"Solution at #$clock")
      solution = solve
      solutionClock = clock
    }

    solution get (id, g)
  }

  private[this] def solve: scala.collection.immutable.Map[(Int, PickupRequest), Double] = {
    if (goals.nonEmpty) {
      state.values
        .filter(_.free)
        .flatMap({ e => goals.map({ r => ((e.id, r), score(e)(r)) }) })
        .filter(_._2.nonEmpty)
        .map({ a => (a._1, a._2.get) })
        .toMap
    } else {
      Map()
    }
  }

  def assignedGoals: Iterable[PickupRequest] = state.values.flatMap(_.orders).map(_.pickupTo)
    .groupBy(_.at).flatMap({ a => a._2.groupBy(_.direction).map({b => b._2.head}) })

  def unassignedGoals: Iterable[PickupRequest] = goals.toSet -- assignedGoals
}

// First-come First-served implementation of ControlSystem
class FCFS(floors: Int) extends ControlSystemBase(floors) {
  override def score(e: ElevatorSnapshot)(g: PickupRequest): Option[Double] = {
    if (!e.idle) {
      // In FCFS never reschedule elevators until they are totally Idle
      None
    } else {
      Option(1 + floors - e.distance(g))
    }
  }
}

// Nearest Car
class NearestCar(floors: Int) extends ControlSystemBase(floors) {
  def distance(e: ElevatorSnapshot)(g: PickupRequest) = {
    if (e.direction == g.direction || e.direction == Idle) {
      e.distance(g)
    } else {
      // Changing direction costs more
      1 + e.distance(g)
    }
  }

  override def score(e: ElevatorSnapshot)(g: PickupRequest): Option[Double] = {
    Option(1 + floors - distance(e)(g))
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

  val controller: ControlSystem = algorithm

  var queue = new ListBuffer[Order]()

  def algorithm = new NearestCar(floors)

  def take(e: Elevator): Unit = {
    // Most angry men wins
    queue
      .filter(_.from == e.pos)
      .groupBy(_.direction)
      .reduceOption({ (a, b) => if (a._2.length > b._2.length) a else b })
      .foreach({ a => take(e, a._1) })
  }

  def take(e: Elevator, d: Direction): Unit = {
    controller.release(PickupRequest(e.pos, d))
    queue
      .filter({ t => t.from == e.pos && t.direction == d })
      .foreach({ o =>
        e.orders :+= o
        queue -= o
      })
  }

  def goals: Iterable[PickupRequest] = queue.map(_.pickupFrom).groupBy(_.at).flatMap({ a =>
    a._2.groupBy(_.direction).map({b => b._2.head})
  })

  override def step() = {
    def pickup() = {
      for (e <- elevators) {
        // Idle
        if (e.free && e.idle) {
          if (e.prev.nonEmpty) {
            take(e, e.prev.get.dir)
          }

          if (e.free) {
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
        val o = newOrder
        queue += o
        controller.pickup(o.pickupFrom)
      }
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

    def schedule() = {
      def filterSchedulingSet(free: Seq[Elevator], skip: Seq[Elevator], scores: Seq[(Elevator, Option[Goal])]):
        (Seq[Elevator], Seq[(Elevator, Option[Goal])]) = {

        val skipGoals: Seq[Goal] = skip.flatMap(_.selectPath)

        ( free.filterNot(skip.contains), scores.filterNot({ pair =>
          skip.contains(pair._1) || pair._2.isEmpty || skipGoals.exists(pair._2.get.same) }) )
      }

      def schedulingSet: (Seq[Elevator], Seq[(Elevator, Option[Goal])]) = {
        val free: Seq[Elevator] = elevators.filter(_.free)
        val scores: Seq[(Elevator, Option[Goal])] = free
          .flatMap({ elevator =>
            goals.map({ pickup =>
              (elevator, controller.score(elevator.Id)(pickup).map({ s => Goal(pickup, s)})) })})

        // Skip elevators scheduler asks to backoff from
        filterSchedulingSet(free,  scores.filter(_._2.isEmpty).groupBy(_._1).keys.toList, scores)
      }

      var space = schedulingSet

      // Assign best score greedy
      while (space._1.nonEmpty && space._2.nonEmpty) {
        val (free, scores) = space

        val best = scores.maxBy(_._2.get.score)
        best._1.tasks = List(best._2.get)

        space = filterSchedulingSet(free, List(best._1), scores)
      }

      // Rest idle
      space._1.foreach(_.stop())
    }

    clock += 1

    println(s"Building clock #$clock")

    incoming()
    elevators.map(_.snapshot).foreach(controller.update)

    pickup()
    schedule()
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
    println(s"Queue $queue")
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