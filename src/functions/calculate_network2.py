

import casadi as ca
import numpy as np
import pandas as pd

from enum import IntEnum
from typing import Dict, List, Union


"""
# Taken from Richmond EPANET model, curve 1123
default_pump = np.array([[0, 88.0],
                         [2.78, 87.0],
                         [5.56, 84.0],
                         [8.53, 76.0],
                         [11.11, 63.0],
                         [13.89, 47.0],
                         [19, 0.0]])

scaled_default_pump = np.vstack((default_pump[:, 0] / max(default_pump[:, 0]),
                                 default_pump[:, 1] / max(default_pump[:, 1]))).transpose()

xs = scaled_default_pump[:, 0]
ys = scaled_default_pump[:, 1]

a = (ys[1:] - ys[:-1])/(xs[1:] - xs[:-1])
b = ys[1:] - a * xs[1:]

line_coefficients = list(zip(a, b))
"""

PUMP_LINE_COEFFICIENTS = [(-0.07766514061478083, 1.0),
                          (-0.23299542184434252, 1.0227272727272727),
                          (-0.5815733088460364, 1.1247321701867157),
                          (-1.0879140239605356, 1.3520525017618041),
                          (-1.2426422498364942, 1.442527795945062),
                          (-1.9858566091442804, 1.9858566091442804)]


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
    min_demand_dh = 10.0

    def __init__(self, id, type, weight, discharge, elevation, max_supply_head, disuse, name=None, use_pump_curve=False, **kwargs):
        self.id = id
        self.type = type
        self.weight = weight
        self.elevation = elevation

        self.q_bounds = None
        self.h_bounds = (elevation, np.inf)
        self.objective = 0.0
        self.q_control = None
        self.q_balance = []
        self.discharge = discharge

        self.disuse = disuse
        self.name = name
        self.use_pump_curve = bool(use_pump_curve) if not np.isnan(use_pump_curve) else False

        self.head = ca.SX.sym(f"h_node_{id}")

        # Negative flow is into the node, positive is out of the node
        if discharge < 0:
            raise ValueError(f"Discharge of node {id} is negative. Flow rate should always be positive")

        if type == 'none' and not discharge == 0:
            raise ValueError(f"Discharge of node {id} should be zero as it is of type 'none'")

        if discharge > 0:
            self.q_control = ca.SX.sym(f"q_node_{id}_control")

            if type == 'supply':
                self.q_bounds = (-discharge, 0.0)
                self.h_bounds = (elevation, elevation + max_supply_head)

                if self.use_pump_curve:
                    # NOTE: This is just for feasilibity! In reality, they can
                    # be fixed speed pumps, and should always operate on their
                    # rated rpm line. The inequality means that we assume that
                    # any discharge can always be delivered at an equal or
                    # higher head (up until the head at the rated speed)
                    dh = self.head - elevation
                    self.pump_qh_constraints = [(dh - (a * -self.q_control / discharge + b) * max_supply_head, -np.inf, 0.0) for a, b in PUMP_LINE_COEFFICIENTS]

            elif type == 'demand':
                self.q_bounds = (0.0, discharge)
                self.h_bounds = (elevation, np.inf)
                self.objective = discharge - self.q_control

                # When the pressure at the node is below the minimum demand pressure (say
                # self.min_demand_dh / 10.0), the discharge drops off to zero. This can be
                # done either quadratically, or a lineair approximation thereof.
                dh = (self.head - elevation)
                self.qh_constraints_quadratic = [(dh / self.min_demand_dh - (self.q_control / discharge) ** 2, 0.0, np.inf)]

                """
                xs = np.array([0.0, 0.1, 0.25, 0.5, 0.75, 1.0])
                ys = xpoints ** 2

                a = (ys[1:] - ys[:-1])/(xs[1:] - xs[:-1])
                b = ys[1:] - a * xs[1:]

                line_coefficients = list(zip(a, b))
                """
                line_coefficients = [(0.1, 0.0),
                                     (0.35, -0.025),
                                     (0.75, -0.125),
                                     (1.25, -0.375),
                                     (1.75, -0.75)]

                self.qh_constraints_linear= [(dh / self.min_demand_dh - (a * (self.q_control / discharge) + b), 0.0, np.inf) for a, b in line_coefficients]

            self.q_balance.append(self.q_control)

        if self.disuse:
            self.q_bounds = (0.0, 0.0)


class Pipe:
    # Positive flow is into the pipe, negative is out of the pipe

    id: str
    start: int
    end: int
    length: float
    diameter: float

    max_velocity = 10.0  # m/s

    def __init__(self, id, start, end, length, diameter, disuse, flowdir, q_max=np.inf, has_booster=False, booster_hmax=0.0, **kwargs):
        self.id = id
        self.start = start
        self.end = end
        self.length = length
        self.diameter = diameter
        self.allowed_flow_direction = flowdir
        self.has_booster = bool(has_booster) if not np.isnan(has_booster) else False

        self.q_start = ca.SX.sym(f"q_pipe_{id}_start")
        self.q_end = ca.SX.sym(f"q_pipe_{id}_end")
        self.q_max = np.inf if np.isnan(q_max) else q_max

        self.h_start = ca.SX.sym(f"h_pipe_{id}_start")
        self.h_end = ca.SX.sym(f"h_pipe_{id}_end")
        self.head_loss = ca.SX.sym(f"h_pipe_{id}_head_loss")

        self.flow_dir_sym = ca.SX.sym(f"flowdir_pipe_{id}")

        self.disuse = disuse

        if self.has_booster:
            if not np.isfinite(booster_hmax) or not np.isfinite(self.q_max):
                raise Exception("If the pipe contains a booster, both `booster_hmax` and `q_max` need to be set")

            if booster_hmax < 0:
                raise Exception("Booster head should be positive")

            if self.q_max < 0:
                raise Exception("Pipe maximum discharge should be positive when using booster")

            # NOTE: This is just for feasilibity! In reality, they can
            # be fixed speed pumps, and should always operate on their
            # rated rpm line. The inequality means that we assume that
            # any discharge can always be delivered at an equal or
            # higher head (up until the head at the rated speed)
            dh = self.booster_head = ca.SX.sym("h_pipe_booster_head")
            self.pump_qh_constraints = [(dh - (a * -self.q_start / self.q_max + b) * booster_hmax, -np.inf, 0.0) for
                                        a, b in PUMP_LINE_COEFFICIENTS]
        else:
            self.booster_head = 0.0

    @property
    def area(self):
        return (self.diameter / 1000) ** 2 * 3.1415 / 4.0

    @property
    def q_bounds(self):
        max_q = None

        if self.disuse:
            return (0.0, 0.0)
        elif self.q_max:
            max_q = self.q_max
        else:
            max_q = self.area * self.max_velocity

        if self.allowed_flow_direction == 0:
            return (-max_q, max_q)
        elif self.allowed_flow_direction == -1:
            return (-max_q, 0.0)
        elif self.allowed_flow_direction == 1:
            return (0.0, max_q)
        else:
            raise ValueError("Allowed flow direction of pipe should be -1, 0, or 1, not {}".format(self.allowed_flow_direction))


class HeadLossOption(IntEnum):
    Q_ONLY = 1
    LINEAR = 2
    NONLINEAR = 3


def calculate_network(nodes: pd.DataFrame,
                      pipes: pd.DataFrame,
                      penalty_order: float = 1.0,
                      head_loss_option: HeadLossOption = HeadLossOption.Q_ONLY):

    if head_loss_option not in HeadLossOption.__members__.values():
        raise Exception(f"Head loss option '{head_loss_option}' does not exist")

    pipes.set_index('id', inplace=True)
    nodes.set_index('id', inplace=True)

    nodes_df = nodes
    pipes_df = pipes

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
            # If a pipe has a booster, the booster is located at the start. The head
            # loss is then the difference between the downstream head of the booster,
            # and the end of the pipe.
            # TODO: We do not actually need the head loss symbol.
            equations.append((p.head_loss - (p.h_start + p.booster_head - p.h_end), 0.0, 0.0))
            if p.has_booster:
                equations.extend(p.pump_qh_constraints)

            if head_loss_option == HeadLossOption.LINEAR:
                a = _get_head_loss_coeff_linear(p.length, p.diameter / 1000)
                equations.append((p.head_loss - a * p.q_start, 0.0, 0.0))

            elif head_loss_option == HeadLossOption.NONLINEAR:
                # Flow direction is positive --> flow_dir_sym is 1
                # Flow direction is negative --> flow_dir_sym is 0
                max_discharge = p.max_velocity * p.area
                head_loss_coeffs = list(zip(*_get_head_loss_coeffs(p.length, p.diameter / 1000)))
                max_head_loss = head_loss_coeffs[-1][0] * p.max_velocity + head_loss_coeffs[-1][1]

                equations.append((p.q_start - p.flow_dir_sym * max_discharge, -np.inf, 0.0))
                equations.append((p.q_start + (1 - p.flow_dir_sym) * max_discharge, 0.0, np.inf))

                for a, b in head_loss_coeffs:
                    equations.append((p.head_loss - (a * p.q_start + b) + (1 - p.flow_dir_sym) * max_head_loss, 0.0, np.inf))
                    equations.append((-p.head_loss - (a * -p.q_start + b) + p.flow_dir_sym * max_head_loss, 0.0, np.inf))

    # Whether to use a quadratic drop-off, or a linear apprixation thereof for the Node discharge
    node_quadratic = None
    if head_loss_option <= HeadLossOption.LINEAR:
        if penalty_order == 1.0:
            node_quadratic = False
        else:
            node_quadratic = True
    else:
        node_quadratic = False

    for n in nodes.values():
        equations.append((sum(n.q_balance), 0.0, 0.0))

        if head_loss_option > HeadLossOption.Q_ONLY:
            if n.type == 'demand':
                if node_quadratic:
                    equations.extend(n.qh_constraints_quadratic)
                else:
                    equations.extend(n.qh_constraints_linear)
            elif n.type == 'supply' and n.use_pump_curve:
                equations.extend(n.pump_qh_constraints)

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

            if p.has_booster:
                x.append(p.booster_head)
                lbx.append(-np.inf)
                ubx.append(np.inf)
                discrete.append(False)

        if head_loss_option == HeadLossOption.NONLINEAR:
            x.append(p.flow_dir_sym)
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

    cbc_options = {'cbc': {'slog': 0, 'log': 0}}
    ipopt_options = {'print_time': 0,
                     'ipopt': {
                         'print_level': 0,
                         'sb': 'yes',
                    }}

    if head_loss_option <= HeadLossOption.LINEAR:
        if penalty_order == 1.0:
            # We use cbc instead of clp, because we can make it shut up (and
            # Clp cannot really be)
            solver = ca.qpsol('qp', 'cbc', qp, {"discrete": discrete, **cbc_options})
        else:
            # 'sb' = 'yes' disable the license header. Very much an obscure
            # 'and undocumented option.
            solver = ca.nlpsol('qp', 'ipopt', qp, {"discrete": discrete, **ipopt_options})
    else:
        if penalty_order != 1.0:
            raise Exception("Mixed-integer solving requires penalty order of 1.0 (for now)")
        solver = ca.qpsol('qp', 'cbc', qp, {"discrete": discrete, **cbc_options})

    ret = solver(lbx=lbx, ubx=ubx, lbg=lbg, ubg=ubg)
    x_solved = np.array(ret['x']).ravel()
    results = {index_to_name[i]: x_solved[i] for i in range(len(x_solved))}

    discharge_dict = {}
    flow_dict = {}
    total_shortage = 0.0
    max_shortage = 0.0

    for n in nodes.values():
        if n.q_control is not None:
            q = results[n.q_control.name()]
            if n.type == 'supply':
                q = -q
            else:
                total_shortage += n.discharge - results[n.q_control.name()]
            discharge_dict[n.id] = q
        else:
            discharge_dict[n.id] = 0.0

    for p in pipes.values():
        flow_dict[p.id] = results[p.q_start.name()]

    return total_shortage, discharge_dict, flow_dict


#if __name__ == "__main__":
#    print(calculate_network(pd.read_csv('nodes.csv'), pd.read_csv('pipes.csv'), 1.0, 3))
