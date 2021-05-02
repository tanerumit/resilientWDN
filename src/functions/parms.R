edev.change = 0.0005    
tdev.change = 0.01 
temp.change = 0.04        
price.change = 0.01         
pop.change  = 0.01  
dnetwork.change = 0.01 
wqual.change = 0.005 
edev.elasticity = 1.0        
price.elasticity = -0.2       
temp.elasticity = 0.03      
dom.peak.factor = 1 
ind.peak.factor = 1 
year.sim = 2021        
year.ref = 2021         
nodes.data = nodes_scn        
pipes.data = pipes_scn 
global.output = TRUE



nodes.data = out$nodes
pipes.data = out$pipes
node.fill.var = "rel"
node.size.var = "discharge"
edge.color.var = "usage"
edge.size.var = "diameter"
background.map = mapFriesland
edge.color.threshold = 95
