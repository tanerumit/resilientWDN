#!/usr/bin/env python
# coding: utf-8

# In[38]:

import casadi as ca
import numpy as np
import pandas as pd

# In[39]:

from dataclasses import dataclass

# In[68]:

class Node:
    def __init__(self, id, type, weight, demand):
        self.id = id
        self.type = type
        self.weight = weight

        self.bounds = None
        self.objective = 0.0
        self.q_control = None
        self.q_balance = []
        self.demand = demand

        # Negative flow is into the node, positive is out of the node
        if demand < 0:
            raise ValueError(f"Demand of node {id} is negative. Flow rate should always be positive")

        if type == 'none' and not demand == 0:
            raise ValueError(f"Demand of node {id} should be zero as it is of type 'none'")

        if demand > 0:
            self.q_control = ca.SX.sym(f"node_{id}_control")

            if type == 'supply':
                self.bounds = (-demand, 0.0)
            elif type == 'demand':
                self.bounds = (0.0, demand)
                self.objective = demand - self.q_control

            self.q_balance.append(self.q_control)

class Pipe:
    # Positive flow is into the pipe, negative is out of the pipe

    id: str
    start: int
    end: int
    length: float
    diameter: float

    def __init__(self, id, start, end, length, diameter):
        self.id = id
        self.start = start
        self.end = end
        self.length = length
        self.diameter = diameter

        self.q_start = ca.SX.sym(f"pipe_{id}_start")
        self.q_end = ca.SX.sym(f"pipe_{id}_end")

# In[69]:

nodes = {}

for k, v in pd.read_csv('nodes.csv', sep='\t', index_col=0).to_dict('index').items():
    nodes[k] = Node(k, **v)


# In[70]:

pipes = {}

for k, v in pd.read_csv('pipes.csv', sep='\t', index_col=0).to_dict('index').items():
    pipes[k] = Pipe(k, **v)


# In[71]:

# Convention is that positive is into a branch and negative is out of a branch
# For nodes, that means that their demand has to be signed accordingly (as if a branch was attached)
equations = []

node_balance = {n: 0 for n in nodes}

for p in pipes.values():
    
    equations.append(p.q_start + p.q_end)
    nodes[p.start].q_balance.append(p.q_start)
    nodes[p.end].q_balance.append(p.q_end)

for n in nodes.values():
    equations.append(sum(n.q_balance))

# In[72]:

# Build the state vector with the appropriate bounds
x = []
lbx = []
ubx = []

for n in nodes.values():
    if n.q_control is not None:
        x.append(n.q_control)
        lb, ub = n.bounds
        lbx.append(lb)
        ubx.append(ub)

for p in pipes.values():
    x.append(p.q_start)
    lbx.append(-np.inf)
    ubx.append(np.inf)

    x.append(p.q_end)
    lbx.append(-np.inf)
    ubx.append(np.inf)


# In[103]:


# Build the constraints
g = equations
lbg = [0.0] * len(g)
ubg = lbg.copy()

# Build the objective
f = 0.0
for n in nodes.values():
    f += n.objective


# In[104]:


# Construct the qp, and solver
qp = {'f': f, 'g': ca.vertcat(*g), 'x': ca.vertcat(*x)}

solver = ca.qpsol('qp', 'clp', qp, {})


# In[105]:


results = solver(lbx=lbx, ubx=ubx, lbg=lbg, ubg=ubg)


# In[112]:

totalShortage = []
maxShortage = []

total_shortage = 0
max_shortage = 0

i = 0
for n in nodes.values():
    if n.q_control is not None:
        #print("Node", n.id, n.type, n.demand, results['x'][i])

        if n.type == 'demand':
            short = n.demand - results['x'][i]
            total_shortage += short
            max_shortage = max(max_shortage, short)
            
        i += 1

    
#print()
#print("Total shortage", total_shortage)
#print("Max shortage", max_shortage)


# In[ ]:




