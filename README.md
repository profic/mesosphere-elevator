# mesosphere-elevator

## Problem

Improve simple first-come first-served (FCFS) scheduler for multiple elevators system. 

## Building

```bash
project/sbt
run
```

## Description

The problem is equivalent to the [time dependent traveling salesman problem](https://en.wikipedia.org/wiki/Travelling_salesman_problem).

Improvements of the elevators schedulers are concentrated on:

- minimizing expected wait time
- minimizing expected travel time
- minimizing queues
- maximizing constraints satisfaction
- maximizing throughput

In real life, there also a lot more of physical constraints:

- every action costs
- multiple types of calls (internal and external)
- waste energy, speed, acceleration at any point in time
- doors open, close, wait
- there is strict max wait/serve time constraint for a single request
- elevator can't cancel moves - change move direction if is observed by someone (transfer someone)
- incoming stream is non-homogeneous stochastic, but with morning to evening patterns

For single elevator it's proven the problem is NP-hard.

- proved by Seckinger and Koehler for 1 elevator without capacity constraints:
  [Online Synthesis of Elevator Controls as a Planning Problem (in German),
   German Workshop on Planning and Configuration (PUK), 1999.](https://user.enterpriselab.ch/~takoehle/publications/elev/seckinger-koehler.pdf)
- [AI Planning for Destination Control in Elevators](https://user.enterpriselab.ch/~takoehle/publications/elev/elev.html)

## Solution

The modeling system consists of the: time machine, building with controller (scheduler) and property (elevators).
Residents came and issue a `PickupRequest`. When `Elevator` pickups the resident or a group, those pickups requests
transform to `Orders`. `ControlSystem` watch for all incoming requests placing them into the `queue` and trying to serve 
them assigning `Tasks` to the elevators, putting them into the `pending` mode. Depending on the algorithm of the
scheduler, system decides on priorities and what order requests to server.

In this distributed system Parent controller is a Master of it's child elevators controllers. Schedule system receives
elevators updates and pickups, and evaluate scores and goals. Weighted scores are used to schedule goals to the elevators
in active greedy manner. Reevaluation of the state happening every change.

```scala
trait ControlSystem {
  def connect(to: Seq[Elevator]) <-- should be Update
  def pickup(o: PickupRequest)
  def status: ControlSystemStatus
  def score(e: Elevator)(g: PickupRequest): Option[Goal]
}
```

Two schedulers are implemented:

- `FCFS`: the original first come first served does not try reevaluate scoring solution under changing circumstances.
  
- `Nearest Car`: greedy algorithm - not best among the class of approximating algorithms for the salesman problem,
  but it's a way better then FCFS as it tries to minimize costs doing recalculations each system state change in live.

Another possible improvements to the greedy algorithm are:

- keep elevators uniformly distributed among the floors when they are idle to try to make wait time for the incoming residents
  less in the case if it's uniformly distributed. or try to keep them closer to the patterns in the stream.
 
- consider busy elevators and their future positions where they are moving to count - maybe they are cheaper to pickup
  the request than count on free ones.

- add max wait time constraint

## Good algorithms to use here

- Branch and Bound  method
- Christofides algorithm for the TSP
- Ant Colony Optimization

## To read

- Elevator Traffic Handbook: Theory and Practice
- Elevator Scheduling by James Dong and Qasim Zafar
- Constructing a Scheduling Algorithm For Multidirectional Elevators (JOAKIM EDLUND, FREDRIK BERNTSSON)
- Decision-Theoretic Group Elevator Scheduling (Daniel Nikovski Matthew Brand)
- G.C. Barney and S.M. dos Santos, Elevator Traffic Analysis Design and Control, Peter Peregrinus Ltd, London, UK,
  Second Edition.
- Time dependent traveling salesman problem
- Ant Colony Optimization for single car scheduling of elevator systems with full information
- M. Brand and D. Nikovski, �Optimal Parking in Group Elevator Control,� Proceedings of the 2004 IEEE International
  Conference on Robotics & Automation (2004) 1002-1008.
- D. Nikovski and M. Brand, �Marginalizing Out Future Passengers in Group Elevator Control,� Proceedings of the
Nineteenth Conference on Uncertainty in Artificial Intelligence (2003) 443-450.
- D. Nikovski and M. Brand, �Decision-theoretic group elevator scheduling,� 13th International Conference on Automated
Planning and Scheduling (2003).
- D. Nikovski and M. Brand, �Exact Calculation of Expected Waiting Times for Group Elevator Control,� IEEE
Transportation Automation Control 49(10) pp. 1820-1823.
- IBM, �Smarter Buildings Survey,� Apr. 29 2010.
- T. Strang anad C. Bauer, �Context-Aware Elevator Scheduling,� 21st International Conference on Advanced Information
Networking and Applications Workshops (2007) vol. 2 pp. 276-281.
- National Elevator Industry, Inc. A �Step by Step� Guide.