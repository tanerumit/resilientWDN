from enum import IntEnum
from typing import Dict, List, Union

import casadi as ca

import numpy as np

import pandas as pd


def _calc_head_loss(v, length, diameter):
    return 0.015 * length / diameter * v**2 / (2 * 9.81)

def _get_head_loss_coeffs(length, diameter, vs=[0, 0.1, 0.2, 0.4, 1.0, 2.0]):
    if diameter > 2.0:
        raise ValueError("Diameter should be in m")

    v_points = np.array([0, 0.1, 0.2, 0.4, 1.0, 2.0])

    # Calculate head
    dh = np.array([_calc_head_loss(p, length, diameter) for p in v_points])

    area = diameter ** 2 * 3.1415 / 4.0
    q_points = v_points * area

    # Calculate line coefficients for inequality constraints (using the discharge)
    a = (dh[1:] - dh[:-1]) / (q_points[1:] - q_points[:-1])
    b = dh[:-1] - a * q_points[:-1]

    return a, b

def _get_head_loss_coeff_linear(length, diameter, v=1.0):
    dh = _calc_head_loss(v, length, diameter)

    area = diameter ** 2 * 3.1415 / 4.0
    q = v * area
    a = dh / q

    return a


class Node:
    min_demand_head = 10.0

    def __init__(self, id, type, weight, demand, elevation, max_supply_head):
        self.id = id
        self.type = type
        self.weight = weight
        self.elevation = elevation

        self.q_bounds = None
        self.h_bounds = (elevation, np.inf)
        self.objective = 0.0
        self.q_control = None
        self.q_balance = []
        self.demand = demand

        self.head = ca.SX.sym(f"h_node_{id}")

        # Negative flow is into the node, positive is out of the node
        if demand < 0:
            raise ValueError(f"Demand of node {id} is negative. Flow rate should always be positive")

        if type == 'none' and not demand == 0:
            raise ValueError(f"Demand of node {id} should be zero as it is of type 'none'")

        if demand > 0:
            self.q_control = ca.SX.sym(f"q_node_{id}_control")

            if type == 'supply':
                self.q_bounds = (-demand, 0.0)
                self.h_bounds = (elevation, elevation + max_supply_head)
            elif type == 'demand':
                self.q_bounds = (0.0, demand)
                self.h_bounds = (elevation + self.min_demand_head, np.inf)
                self.objective = demand - self.q_control

            self.q_balance.append(self.q_control)


class Pipe:
    # Positive flow is into the pipe, negative is out of the pipe

    id: str
    start: int
    end: int
    length: float
    diameter: float

    max_velocity = 10.0  # m/s

    def __init__(self, id, start, end, length, diameter):
        self.id = id
        self.start = start
        self.end = end
        self.length = length
        self.diameter = diameter

        self.q_start = ca.SX.sym(f"q_pipe_{id}_start")
        self.q_end = ca.SX.sym(f"q_pipe_{id}_end")

        self.h_start = ca.SX.sym(f"h_pipe_{id}_start")
        self.h_end = ca.SX.sym(f"h_pipe_{id}_end")
        self.head_loss = ca.SX.sym(f"h_pipe_{id}_head_loss")

        self.flow_dir = ca.SX.sym(f"flowdir_pipe_{id}")

    @property
    def area(self):
        return (self.diameter / 1000) ** 2 * 3.1415 / 4.0

    @property
    def q_bounds(self):
        return (-1 * self.area * self.max_velocity, self.area * self.max_velocity)


class HeadLossOption(IntEnum):
    Q_ONLY = 1
    LINEAR = 2
    NONLINEAR = 3


def calculate_network(nodes: Union[str, pd.DataFrame, Dict[str, List]] = 'nodes.csv',
                      pipes: Union[str, pd.DataFrame, Dict[str, List]] = 'pipes.csv',
                      penalty_order: float = 1.0,
                      head_loss_option: HeadLossOption = HeadLossOption.Q_ONLY):

    # Parse input data
    if isinstance(nodes, str):
        nodes_df = pd.read_csv(nodes, sep='\t', index_col=0)
    elif isinstance(nodes, dict):
        nodes_df = pd.DataFrame(nodes).set_index('node_id')
    elif isinstance(nodes, pd.DataFrame):
        nodes_df = nodes
    else:
        raise TypeError("Unknown type '{}' for argument 'nodes'".format(type(nodes)))

    if isinstance(pipes, str):
        pipes_df = pd.read_csv(pipes, sep='\t', index_col=0)
    elif isinstance(pipes, dict):
        pipes_df = pd.DataFrame(pipes).set_index('pipe_id')
    elif isinstance(pipes, pd.DataFrame):
        pipes_df = pipes
    else:
        raise TypeError("Unknown type '{}' for argument 'pipes'".format(type(pipes)))

    # Convert to dict of Node/Pipe instances
    nodes = {}

    for k, v in nodes_df.to_dict('index').items():
        nodes[k] = Node(k, **v)

    pipes = {}

    for k, v in pipes_df.to_dict('index').items():
        pipes[k] = Pipe(k, **v)

    # Convention is that positive is into a branch and negative is out of a
    # branch. For nodes, that means that their demand has to be signed
    # accordingly (as if a branch was attached)
    equations = []

    node_balance = {n: 0 for n in nodes}

    for p in pipes.values():
        equations.append((p.q_start + p.q_end, 0.0, 0.0))

        # Continuity equations
        nodes[p.start].q_balance.append(p.q_start)
        nodes[p.end].q_balance.append(p.q_end)

        if head_loss_option >= HeadLossOption.LINEAR:
            # Head at node equal to head at pipe
            equations.append((nodes[p.start].head - p.h_start, 0.0, 0.0))
            equations.append((nodes[p.end].head - p.h_end, 0.0, 0.0))

            # Head loss definition is h_start - h_end. In other words, a positive
            # discharge means a positive head _loss_
            # TODO: We do not actually need the head loss symbol.
            equations.append((p.head_loss - (p.h_start - p.h_end), 0.0, 0.0))

            if head_loss_option == HeadLossOption.LINEAR:
                a = _get_head_loss_coeff_linear(p.length, p.diameter / 1000)
                equations.append((p.head_loss - a * p.q_start, 0.0, 0.0))

            elif head_loss_option == HeadLossOption.NONLINEAR:
                # Flow direction is positive --> flow_dir is 1
                # Flow direction is negative --> flow dir is 0
                max_discharge = p.max_velocity * p.area
                equations.append((p.q_start - p.flow_dir * max_discharge, -np.inf, 0.0))
                equations.append((p.q_start + (1 - p.flow_dir) * max_discharge, 0.0, np.inf))

                for a, b in zip(*_get_head_loss_coeffs(p.length, p.diameter / 1000)):
                    equations.append((p.head_loss - (a * p.q_start + b - (1 - p.flow_dir) * max_discharge), 0.0, np.inf))
                    equations.append((-p.head_loss - (a * -p.q_start + b - p.flow_dir * max_discharge), 0.0, np.inf))

    for n in nodes.values():
        equations.append((sum(n.q_balance), 0.0, 0.0))

    # Build the state vector with the appropriate bounds
    x = []
    lbx = []
    ubx = []
    discrete = []

    for n in nodes.values():
        if n.q_control is not None:
            x.append(n.q_control)
            lb, ub = n.q_bounds
            lbx.append(lb)
            ubx.append(ub)
            discrete.append(False)

    if head_loss_option >= HeadLossOption.LINEAR:
        for n in nodes.values():
            if n.head is not None:
                x.append(n.head)
                lb, ub = n.h_bounds
                lbx.append(lb)
                ubx.append(ub)
                discrete.append(False)

    for p in pipes.values():
        x.append(p.q_start)
        lb, ub = p.q_bounds
        lbx.append(lb)
        ubx.append(ub)
        discrete.append(False)

        x.append(p.q_end)
        lbx.append(lb)
        ubx.append(ub)
        discrete.append(False)

        if head_loss_option >= HeadLossOption.LINEAR:
            x.append(p.h_start)
            lbx.append(0.0)
            ubx.append(np.inf)
            discrete.append(False)

            x.append(p.h_end)
            lbx.append(0.0)
            ubx.append(np.inf)
            discrete.append(False)

            x.append(p.head_loss)
            lbx.append(-np.inf)
            ubx.append(np.inf)
            discrete.append(False)

        if head_loss_option == HeadLossOption.NONLINEAR:
            x.append(p.flow_dir)
            lbx.append(0.0)
            ubx.append(1.0)
            discrete.append(True)

    # Build the indices for easy lookup
    index_to_name = {i: v.name() for i, v in enumerate(x)}

    # Build the constraints
    g, lbg, ubg = zip(*equations)

    # Build the objective
    f = 0.0
    for n in nodes.values():
        f += n.objective**penalty_order

    # Construct the qp, and solve
    qp = {'f': f, 'g': ca.vertcat(*g), 'x': ca.vertcat(*x)}

    if head_loss_option <= HeadLossOption.LINEAR:
        print(head_loss_option)
        print(int(head_loss_option))
        if penalty_order == 1.0:
            solver = ca.qpsol('qp', 'clp', qp)
        else:
            solver = ca.nlpsol('qp', 'ipopt', qp, {"discrete": discrete})
    else:
        if penalty_order != 1.0:
            raise Exception("Mixed-integer solving requires penalty order of 1.0 (for now)")
        solver = ca.qpsol('qp', 'cbc', qp, {"discrete": discrete})

    ret = solver(lbx=lbx, ubx=ubx, lbg=lbg, ubg=ubg)
    x_solved = np.array(ret['x']).ravel()
    results = {index_to_name[i]: x_solved[i] for i in range(len(x_solved))}

    shortage_dict = {}
    flow_dict = {}
    total_shortage = 0.0
    max_shortage = 0.0

    for n in nodes.values():
        if n.type == 'demand':
            if n.q_control is not None:
                q = results[n.q_control.name()]
                short = n.demand - q

                shortage_dict[n.id] = short
                total_shortage += short
                max_shortage = max(max_shortage, short)
            else:
                shortage_dict[n.id] = 0.0

    for p in pipes.values():
        flow_dict[p.id] = results[p.q_start.name()]

    return total_shortage, shortage_dict, flow_dict


#if __name__ == "__main__":
#    print(calculate_network('nodes.csv', 'pipes.csv', 1.0, 3))
