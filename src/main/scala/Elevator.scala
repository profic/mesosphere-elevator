import scala.collection.mutable.ListBuffer

sealed abstract class Direction

case object Up extends Direction
case object Down extends Direction
case object Idle extends Direction

/**
  * Goal describe demand to achieve certain floor
  *
  * @param to Target floor
  * @param dir Reserved direction
  * @param score Schedule score counted for this goal
  */
case class Goal(to: Int, dir: Direction, score: Double)

// For the sake of simplicity it will be a trait implemented by the Intention,
// which is an Order in mind of the Resident
trait PickupRequest {
  def at: Int
  def direction: Direction
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

  def goal(to: PickupRequest): Goal = Goal(to.at, to.direction, distance(to))

  def goal(to: Order): Goal = {
    if (orders.contains(to)) {
      Goal(to.to, to.direction, distance(to.to))
    } else {
      goal(to.asInstanceOf[PickupRequest])
    }
  }
}

trait ElevatorController {
  var tasks: List[Goal] = List()
  var prev: Option[Goal] = None

  def next = tasks.headOption
  def idle = direction == Idle
  def floor: Int

  def step(): Unit

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

  override def step(): Unit = {
    // Server all this floor orders - this is ideal world
    val takeoff = orders.count(_.to == pos)
    orders = orders.filter(_.to != pos)

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
  def step(): Unit
}

abstract class ControlSystemBase extends ControlSystem {
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

  // TODO: goals weight here could be wait time metric
  def goals: Iterable[Goal] = queue.groupBy(_.at).flatMap({ a =>
    a._2.groupBy(_.direction).map({b => Goal(b._2.head.at, b._1, 0.0)})
  })

  // TODO: goals weight here could be wait time metric
  def assignedGoals: Iterable[Goal] = elevators.flatMap({ e => e.orders.map(e.goal) ++ e.tasks }).groupBy(_.to).flatMap({ a =>
    a._2.groupBy(_.dir).map({b => Goal(b._2.head.to, b._1, 0.0)})
  })

  def unassignedGoals: Iterable[Goal] = goals.toSet -- assignedGoals
}

// First-come First-served implementation of ControlSystem
class FCFS extends ControlSystemBase {
  var pending = new ListBuffer[PickupRequest]()

  override def step() = {
    // Server pending order
    // This algorithm so bad, that it do not allow to take ALL the orders at
    // this floor in order not to break FCFS guarantee
    elevators.filter({ e => e.idle && e.free }).foreach({ e =>
      pending.filter(_.at == e.pos).foreach({ o =>
        e.orders = e.orders.+:(o.asInstanceOf[Order])
        pending.remove(pending.indexOf(o))
      })
    })

    // Assign tasks in FIFO
    // As this is FCFS we schedule only free lifts to maintain rest ordering
    // TODO: to better serve (worse) FCFS shuffle the free lifts
    var free = elevators.filter({ e => e.idle && e.free })
    while (queue.nonEmpty && free.nonEmpty) {
      // Send lifts after an order
      val task = queue.remove(0)
      val elevator = free.head
      if (elevator.pos != task.at) {
        // Serve pickup
        elevator.tasks = List(elevator.goal(task))
      }
      free = free.tail
      pending += task
    }
  }

  override def status: ControlSystemStatus = {
    println(s"Pending $pending")
    super.status
  }
}

// Greedy algorithm and:
// 1. Serve closest free lift
// 2. Get people on the way
class Greedy extends ControlSystemBase {
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
      // TODO: Count scores
      val i = unassignedGoals.iterator
      var free = elevators.filter({ e => e.free && e.idle })
      while (i.hasNext && free.nonEmpty) {
        val goal = i.next

        free = free.sortBy(_.distance(goal.to))

        val e = free.head
        if (e.pos != goal.to) {
          e.tasks = List(Goal(goal.to, goal.dir, e.distance(goal)))
        }
        free = free.tail
      }
    }

    // TODO: use instead of greedy closest traveling, greedy up to down or scored version
  }
}

trait TimeMachine {
  var clock = 0

  def step = clock += 1
}

class Building(floors: Int, elevators: Int, residents: Int) extends TimeMachine {
  private[this] var oid = 0

  val incomingSpeed = 5
  val r = scala.util.Random

  val property = 1 to elevators map { _ => Elevator() }
  val controller: ControlSystem = {
    // val c = new FCFS
    val c = new Greedy
    c.connect(property)
    c
  }

  override def step = {
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