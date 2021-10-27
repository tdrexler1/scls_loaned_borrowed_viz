library(UserNetR)
library(statnet)
old.par <- par(no.readonly=T)

data("TCnetworks")
TCcnt <- TCnetworks$TCcnt
TCcoll <- TCnetworks$TCcoll 
TCdiss <- TCnetworks$TCdiss
TCdist <- TCnetworks$TCdist
summary(TCdiss, print.adj = F)
components(TCdiss)
gden(TCdiss)
centralization(TCdiss, betweenness, mode = 'graph')

deg <- degree(TCdiss, gmode = 'graph')
lvl <- TCdiss %v% 'agency_lvl'
plot(TCdiss, usearrows = F, displaylabels = T, vertex.cex = log(deg), vertex.col = lvl+1, 
     label.pos = 3, label.cex = 0.7, edge.lwd = 0.5, edge.col = 'grey75')
legend("bottomleft", legend = c("Local", "State", "National"), col = 2:4, pch = 19, pt.cex = 1.5)

library(ergm)
DSmod0 <- ergm(TCdiss ~ edges, control = control.ergm(seed=40))
class(DSmod0)
summary(DSmod0)
plogis(coef(DSmod0))

scatter.smooth(TCdiss %v% 'tob_yrs', degree(TCdiss, gmode = 'graph'), 
               xlab = 'Years of Tobacco Experience', ylab = 'Degree')

DSmod1 <- ergm(TCdiss ~ edges + nodefactor('lead_agency') + nodecov('tob_yrs'), control = control.ergm(seed = 40))
summary(DSmod1)

p_edg <- coef(DSmod1)[1]
p_yrs <- coef(DSmod1)[3]
plogis(p_edg + 5*p_yrs + 10*p_yrs)

mixingmatrix(TCdiss, 'agency_lvl')
mixingmatrix(TCdiss, 'agency_cat')
?TCnetworks

DSmod2a <- ergm(TCdiss ~ edges + nodecov('tob_yrs') + nodematch('agency_lvl'), control = control.ergm(seed = 40))
summary(DSmod2a)

DSmod2b <- ergm(TCdiss ~ edges + nodecov('tob_yrs') + nodematch('agency_lvl', diff = TRUE), control = control.ergm(seed = 40))
summary(DSmod2b)

DSmod2c <- ergm(TCdiss ~ edges + nodecov('tob_yrs') + nodemix('agency_lvl', base = 1), control = control.ergm(seed = 40))
summary(DSmod2c)

as.sociomatrix(TCdist, attrname = 'distance')[1:5, 1:5]
as.sociomatrix(TCcnt, attrname = 'contact')[1:5, 1:5]
DSmod3 <- ergm(TCdiss ~ edges + nodecov('tob_yrs') + nodematch('agency_lvl', diff = T) +
                 edgecov(TCdist, attr = 'distance') + edgecov(TCcnt, attr = 'contact'),
               control = control.ergm(seed = 40))
summary(DSmod3)

DSmod4 <- ergm(TCdiss ~ edges + nodecov('tob_yrs') + nodematch('agency_lvl', diff = T) +
                 edgecov(TCdist, attr = 'distance') + edgecov(TCcnt, attr = 'contact') + 
                 gwesp(0.7, fixed = T), control = control.ergm(seed = 40))
summary(DSmod4)

(prd_prob1 <- plogis(-6.31 + 2*1*0.099 + 1.52 + 4*1.042 + 0.858*(0.50^4)) )
(prd_prob2 <- plogis(-6.31 + 2*5*0.099 + 1*1.042 + 0.858*(0.50^4)) )

DSmod.fit <- gof(DSmod4, GOF = ~ distance + espartners + degree + triadcensus, 
                 burnin = 1e+5, interval = 1e+5)
summary(DSmod.fit)
DSmod.fit

par(mfrow=c(2,2))
plot(DSmod.fit, cex.axis = 1.6, cex.label = 1.6)

mcmc.diagnostics(DSmod4)

sim4 <- simulate(DSmod4, nsim = 1, seed = 569)
summary(sim4, print.adj = F)

par(mfrow = c(1,2), mar = c(0,0,2,0))
lvlobs <- TCdiss %v% 'agency_lvl'
plot(TCdiss, usearrows = F, vertex.col = lvl + 1, edge.lwd = 0.5, edge.col = 'grey75',
     main = "Observed TC network")
lvl4 <- sim4 %v% 'agency_lvl'
plot(sim4, usearrows = F, vertex.col = lvl4 + 1, edge.lwd = 0.5, edge.col = 'grey75',
     main = "Simulated network - Model 4")