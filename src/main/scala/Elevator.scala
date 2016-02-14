import scala.collection.mutable.ListBuffer

sealed abstract class Direction

case object Up extends Direction
case object Down extends Direction
case object Idle extends Direction

case class Task(to: Int)

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

class Elevator(id: Int) {
  var pos: Int = 0
  var orders: List[Order] = List()
  var tasks: List[Task] = List()

  def next = tasks.headOption
  def idle = tasks.isEmpty
  def free = orders.isEmpty

  def distance(to: Int): Int = {
    math.abs(to - pos)
  }

  def direction: Direction = {
    tasks.headOption.map({ t =>
      if (t.to - pos > 0) {
        Up
      } else if (t.to - pos < 0) {
        Down
      } else {
        Idle
      }
    }).getOrElse(Idle)
  }

  def step: Option[Task] = {
    tasks.headOption.flatMap({ t =>
      if (t.to - pos > 0) {
        pos += 1
        None
      } else if (t.to - pos < 0) {
        pos -= 1
        None
      } else {
        tasks = tasks.tail
        // Server all this floor orders
        orders = orders.filter(_.to != pos)
        Option(t)
      }
    })
  }

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
  // TODO Should provide Id, Orders, Tasks
  type ControlSystemStatus = Unit

  // For the sake of simplicity we keep it here not to repeat this in Building
  var queue = new ListBuffer[PickupRequest]()

  // The control system is directly connected to the elevators controllers
  // Or keeps their state in memory
  var elevators: Seq[Elevator] = List()

  // System directly connected to the elevators controller via bus
  def connect(to: Seq[Elevator]) = elevators = to
  def pickup(o: PickupRequest) = queue += o
  def status: ControlSystemStatus = {
    println(s"Queue ${queue}")

    for (e <- elevators) {
      println(e)
    }
  }
  def step
}

// First-come First-served implementation of ControlSystem
class FCFS extends ControlSystem {
  var pending = new ListBuffer[PickupRequest]()

  override def step = {
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
        elevator.tasks = List(Task(task.at))
      }
      free = free.tail
      pending += task
    }

    // Lift with orders
    elevators.filter({ e => e.idle && !e.free }).foreach({ e =>
      e.tasks = List(Task(e.orders.head.to))
    })
  }

  override def status: ControlSystemStatus = {
    println(s"Pending ${pending}")
    super.status
  }
}

// Keep ordering, but:
// 1. Serve closest free lift
// 2. Get people on the way
class Improved extends ControlSystem {
  var pending = new ListBuffer[PickupRequest]()

  override def step = {
    // Pickup destination floor
    elevators.filter({ e => e.idle && e.free }).foreach({ e =>
      pending.filter(_.at == e.pos).foreach({ o =>
        e.orders = e.orders.+:(o.asInstanceOf[Order])
        pending.remove(pending.indexOf(o))

        // Take floor with it
        val constraint = e.orders.head.direction
        val move = queue.filter(_.at == e.pos)
        move.foreach({ o =>
          if (constraint == o.direction) {
            e.orders = e.orders.+:(o.asInstanceOf[Order])
            queue -= o
          }
        })
      })
    })

    // Pickup any guys on the way
    elevators.filterNot({ e => e.free }).foreach({ e =>
      val move = queue.filter(_.at == e.pos)
      val constraint = e.orders.head.direction
      move.foreach({ o =>
        if (constraint == o.direction) {
          e.orders = e.orders.+:(o.asInstanceOf[Order])
          queue -= o
        }
      })
    })

    // Send all same floor Pickups to the pending, must keep same direction
    for (task <- pending) {
      queue.filter({ t => t.at == task.at && t.direction == task.direction }).foreach({ o =>
        pending += o
        queue -= o
      })
    }

    // Assign PickUps to free elevators
    var free = elevators.filter({ e => e.idle && e.free })
    while (queue.nonEmpty && free.nonEmpty) {
      // Send lifts after an order
      val task = queue.remove(0)

      free = elevators.filter({ e => e.idle && e.free }).sortWith({(a, b) => a.distance(task.at) < b.distance(task.at)})
      val e = free.head
      if (e.pos != task.at) {
        // Serve pickup
        e.tasks = List(Task(task.at))
        // Send all same floor Pickups to the pending, must keep same direction
        queue.filter({ t => t.at == task.at && t.direction == task.direction}).foreach({ o =>
          pending += o
          queue -= o
        })
      }

      free = free.tail
      pending += task
    }

    // Lift with orders
    elevators.filter({ e => !e.free }).foreach({ e =>
      // Greedy: go for closest distance
      e.tasks = List(Task(e.orders.sortWith({
        (a, b) => math.abs(a.to - e.pos) < math.abs(b.to - e.pos)
      }).head.to))
    })
  }

  override def status: ControlSystemStatus = {
    println(s"Pending ${pending}")
    super.status
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
    val c = new Improved
    c.connect(property)
    c
  }

  override def step = {
    clock += 1

    // Incoming residents
    if (controller.queue.length < residents) {
      val incoming = r.nextInt(incomingSpeed)
      if (incoming > 0) {
        val pr = newPickupRequest
        println(s"Incoming (PickupRequest at ${pr.at}, ${pr.direction}})")
        controller.pickup(pr)
      }
    }

    // Tick controller
    controller.step

    // Tick elevators
    property.foreach(_.step)

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