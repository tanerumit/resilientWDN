

library(igraph)






plot(G, layout = layout.fruchterman.reingold, 
     main = G$name,
     vertex.label = V(G)$name,
     vertex.size = 15,
     vertex.color= V(G)$color,
     vertex.frame.color= "white",
     vertex.label.color = "white",
     vertex.label.family = "sans",
     edge.width=E(G)$weight, 
     edge.color=E(G)$color)




# create function
line_function <- function(mydata     = dat,
                          xinput     = "x", # note the defaults are
                          yinput     = "y", # strings here
                          aes_group  = NULL,
                          aes_color  = NULL,
                          ...) {
  
  ggplot(mydata, # this should be the argument, not the global variable dat
         # now we create the aes binding with aes_string
         aes_string(x = xinput,
                    y = yinput,
                    group = aes_group,
                    color = aes_color)) +
    geom_line()
}


xdata = 0:20
ydata = 20:40
zdata = c(rep(1,10),rep(0,11))
dat <- data.frame(xdata, ydata)

# test the function
line_test_p <- line_function(
  mydata = dat,
  xinput = "xdata", # note the strings
  yinput = "ydata"
)

# test the function again with explicit NULL inputs
line_test2_p <- line_function(mydata    = dat,
                              xinput    = "xdata", # and strings here
                              yinput    = "ydata",
                              aes_group = NULL,
                              aes_color = "zdata")

line_test2_p

