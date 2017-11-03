context('hit and run functions')

constr <- list(
  constr = rbind(
  c(10/3, -53/15, 2),
  c(-10/3, 53/15, -2),
  c(1,0,0),
  c(-1,0,0),
  c(0,1,0),
  c(0,-1,0),
  c(0,0,1),
  c(0,0,-1)
),
dir=rep('<=', 8),
rhs=c(1,-1,1,0,1,0,1,0)
)
state <- har.init(constr)
result <- har.run(state, n.samples=1000)
samples <- result$samples

plot3d(samples) #show 3d plane
