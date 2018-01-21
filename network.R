### Network Visualization Using Airport Transit Data ###

# Objective: Review inbound/outbound air traffic to determine degree to which DTW airport is connected to other major US airports. 
# Create visualizations to help understand where flights originate and terminate and determine how active is DTW airport.

# import libraries for two approaches to network visualization
library('igraph') 
library('visNetwork')

# import data - two separate CSVs
# a node file containing airport information
# an edge file containing flight information
nodes <- read.csv("C:/Data/Dataset3-Airlines-NODES.csv", header=T, as.is=T)
edges <- read.csv("C:/Data/Dataset3-Airlines-EDGES.csv", header=T, as.is=T)

# examine data
head(nodes)
str(nodes)
head(edges)
str(edges)

### First Approach - igraph
net <- graph_from_data_frame(d=edges, vertices=nodes, directed=T) 
net # view structure

plot(net)
# plot is very hard to read, many nodes overlap

# clean up plot
net <- simplify(net, remove.multiple = F, remove.loops = T) 
plot(net, edge.arrow.size=.4,vertex.label=NA)

# test a few layouts to find a better visual
# circle layout
l1 <- layout_in_circle(net)
plot(net, layout=l1)

# line layout
l2 <- cbind(1:vcount(net), c(1, vcount(net):2))
plot(net, layout=l2)

# fructerman layout
l3 <- layout_with_fr(net)
plot(net, layout=l3)

# kamada kawai
l4 <- layout_with_kk(net)
plot(net, layout=l4)

# large graph layout
l5 <- layout_with_lgl(net)
plot(net, layout=l5)

# all of the above visuals are very crowded and hard to read
# we will try trimming the data set to look at only the airports with many
# connections and assume smaller ones tell less of the story

tab <- table(edges$Source)
big.id <- names(tab)[tab>10]
airports <- nodes[nodes$ID %in% big.id,]
flights  <- edges[edges$Source %in% big.id & 
                    edges$Target %in% big.id, ]

net <- graph_from_data_frame(d=flights, vertices=airports, directed=T)
plot(net) #easier to see, but could use improvement

# let's retry the layouts used for the whole dataset
l1 <- layout_in_circle(net)
plot(net, layout=l1) # circle layout
l2 <- cbind(1:vcount(net), c(1, vcount(net):2))
plot(net, layout=l2) # line layout
l3 <- layout_with_fr(net)
plot(net, layout=l3) # fructerman layout
l4 <- layout_with_kk(net)
plot(net, layout=l4) # kamada kawai
l5 <- layout_with_lgl(net)
plot(net, layout=l5) # large graph layout

# do some formatting
V(net)$size <- 10
V(net)$frame.color <- "black"
V(net)$color <- "grey"
E(net)$arrow.mode <- 0

#change arrow size:
E(net)$arrow.size <- .2
E(net)$width <- 1+E(net)$weight/12
plot(net, layout=layout_with_lgl)

# plot with nodes labeled with airport code
plot(net, vertex.label=V(net)$Code, 
     vertex.label.font=2, vertex.label.color="blue",
     vertex.label.cex=.5, edge.color="gray85", layout=layout_with_lgl)

# change node size to reflect airports with more connecting flights
deg <- degree(net, mode="all")
V(net)$size <- deg
plot(net, vertex.label=V(net)$Code, 
     vertex.label.font=2, vertex.label.color="blue",
     vertex.label.cex=.5, edge.color="gray85", layout=layout_with_lgl)

# we can now see which airports have the most connecting flights
# but it is still hard to see where the connecting flights go
# let's see if we can focus on a few of the larger airports

dtw_f  <- flights[flights$Source==50 | flights$Target==50, ]
dtw <- graph_from_data_frame(d=dtw_f, vertices=airports, directed=T)

V(dtw)$size <- 10
V(dtw)$frame.color <- "black"
V(dtw)$color <- "grey"
deg <- degree(dtw, mode="all")
V(dtw)$size <- deg
E(dtw)$arrow.mode <- 0
E(dtw)$arrow.size <- .2
E(dtw)$width <- 1+E(net)$weight/12

plot(dtw, vertex.label=V(dtw)$Code, 
     vertex.label.font=2, vertex.label.color="blue",
     vertex.label.cex=.5, edge.color="gray85", main="Flights To/From Detroit")

# Let's try to visualize just the flights to/from Detroit within the network

# Use the incident function to highlight specific nodes
dtw_flights <- incident(net,  V(net)[Code=="DTW"], mode="all")

# change color so flights to/from Detroit stand out
ecol <- rep("gray80", ecount(net))
ecol[dtw_flights] <- "orange"
vcol <- rep("grey80", vcount(net))
vcol[V(net)$Code=="DTW"] <- "gold"
deg <- degree(net, mode="all")
V(net)$size <- deg*.75

plot(net, vertex.color=vcol, edge.color=ecol, vertex.label=V(dtw)$Code, 
     vertex.label.font=2, vertex.label.color="blue",
     vertex.label.cex=.5, edge.color="gray85")

# Now let's see where those flights go to and come from
connections <- neighbors(net, V(net)[Code=="DTW"], mode="all")

# Set colors to plot the neighbors:
vcol[connections] <- "#ff9d00"
plot(net, vertex.color=vcol, edge.color=ecol, vertex.label=V(dtw)$Code, 
     vertex.label.font=2, vertex.label.color="blue",
     vertex.label.cex=.5, edge.color="gray85")

# flights that come into/out of Detroit come from almost all other cities
# with the exception of Portland, OR (PDX)


library('maps')
library('geosphere')

a2 = flights[flights$Source==50 | flights$Target==50, ]

map("state", col="grey80", fill=TRUE, bg="white", lwd=0.1) 
points(x=a2$longitude, y=a2$latitude, pch=19, col="blue")


### Network visualization in visNetwork
visNetwork(airports, flights, width="100%", height="1000px")

airports$shape <- "dot"  
airports$shadow <- FALSE # Nodes will drop shadow
airports$label <- airports$Code # Node label
airports$size <- airports$Visits*.5# Node size
airports$borderWidth <- 2 # Node border width


airports$color.border <- "black"
airports$color.highlight.background <- "orange"
airports$color.highlight.border <- "red"

flights$width <- 1+flights$Freq/6 # line width

visNetwork(airports, flights)

links$color <- "gray"    # line color  
links$arrows <- "middle" # arrows: 'from', 'to', or 'middle'
links$smooth <- FALSE    # should the edges be curved?
links$shadow <- FALSE    # edge shadow

visNetwork(airports, flights, width="100%", height="1000px")





net <- simplify(net, remove.multiple = F, remove.loops = T) 
plot(net, edge.arrow.size=.4,vertex.label=NA)

net.bg <- sample_pa(80) 
V(net.bg)$size <- 8
V(net.bg)$frame.color <- "black"
V(net.bg)$color <- "grey"
V(net.bg)$label <- "" 
E(net.bg)$arrow.mode <- 0

l <- layout_with_kk(net.bg)
plot(net.bg, layout=l)

visNetwork(airports, flights, width="100%", height="1000px")

airports$shape <- "dot"  
airports$shadow <- FALSE # Nodes will drop shadow
airports$label <- nodes$Code # Node label
airports$size <- (nodes$Visits/10)# Node size
airports$borderWidth <- 2 # Node border width

airports$color.border <- "black"
airports$color.highlight.background <- "orange"
airports$color.highlight.border <- "darkred"



visNetwork(airports, flights)