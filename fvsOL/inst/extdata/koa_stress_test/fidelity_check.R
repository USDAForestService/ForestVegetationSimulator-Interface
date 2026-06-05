# fidelity_check.R  --  base R, no packages.
# Evaluates the FVS-HI HiGy.R component equations VERBATIM (function bodies and
# 'site' coefficients copied exactly from fvsOL/inst/extdata/HiGy.R) over an
# input grid, and writes predictions to CSV. A companion Python step evaluates
# koa_equations.LineageA on the same grid; the two are diffed to prove the
# Python port reproduces the actual R code to machine precision.

# ---- HiGy.R 'site' coefficients (verbatim) --------------------------------
P_ht  <- list(a0=19.832, a1=0.106, b=0.044, c=0.863, g1=-0.198, g2=0.479)
P_hcb <- list(b0=0.1684, b1=1.0146, b2=-0.376, b3=-0.0078, b4=-0.3734, b5=-0.221)
P_dd  <- list(b0=-2.4704737,b1=0.2072221,b2=-0.0159616,b3=-0.0016893,b4=-0.2972574,
            b5=-0.4470330,b6=-0.0158403,b7=0.0188938,b8=0.4530166)
P_dh  <- list(b0=-3.382162,b1=0.272454,b2=-0.105319,b3=-0.000829,b4=-0.071718,
            b5=-1.483889,b6=0.033035,b7=0.017887,b8=0.433224)
P_sv  <- list(b0=18.133,b1=0.199,b2=-5.718,b3=7.640,b4=15.678,b5=-3.396,b6=3.039,b7=-25.102)

# ---- VERBATIM function bodies from HiGy.R ----------------------------------
pred_ht <- function(dbh, ba, bal, qmd, byi, a0,a1,b,c,g1,g2){
  rdbh <- dbh/qmd
  ht.intercept <- ifelse(byi %in% c(NA,0), a0, a0 + a1*byi/100)
  pmax(ht.intercept*(1-exp(-b*dbh))^c*exp(g1*log(ba+1)+g2*rdbh), 1.37)
}
pred_hcb <- function(dbh, ht, bal, ba, byi, b0,b1,b2,b3,b4,b5){
  eta <- b0 + b1*sqrt(ht/100) + b2*log(pmax(ht/pmax(dbh,0.1),0.5)) +
         b3*sqrt(bal*ba+1) + b4*log(ba+1) + b5*log(pmax(byi,1)/100)
  hcb <- ht/(1+exp(-eta)); pmin(pmax(hcb,0), 0.95*ht)
}
ddbh <- function(dbh,bal,ba,cr,byi,planted,b0,b1,b2,b3,b4,b5,b6,b7,b8){
  cf <- 1.026
  x <- exp(b0 + b1*log(dbh+1) + b2*dbh + b3*bal^2/log(dbh+5) + b4*log(bal+1) +
           b5*log(pmax(cr,0.01)) + b6*sqrt(pmax(ba*dbh,0)) +
           b7*planted*pmin(dbh,40) + b8*log(pmax(byi,1)))*cf
  pmin(pmax(x,0),4)
}
dht <- function(dbh,ht,bal,ba,cr,byi,planted,b0,b1,b2,b3,b4,b5,b6,b7,b8){
  cf <- 1.030
  x <- exp(b0 + b1*log(ht+1) + b2*ht + b3*bal^2/log(ht+5) + b4*log(bal+1) +
           b5*log(pmax(cr,0.01)) + b6*sqrt(pmax(ba*ht,0)) +
           b7*sqrt(planted*pmin(ht,20)) + b8*log(pmax(byi,1)))*cf
  pmin(pmax(x,0),2)
}
surv_prob <- function(dbh,ht,cr,r.ht,byi,b0,b1,b2,b3,b4,b5,b6,b7){
  ht <- pmax(ht,0.1); dbh <- pmax(dbh,0.1)
  s <- exp(-exp((b0 + b1*ht + b2*log(ht) + b3*r.ht +
                 b4*log(pmax(pmin(cr,0.99),0.01)) + b5*log(ht/dbh) +
                 b6*log(pmax(byi,1)/100) + b7*(byi/1000))))
  pmin(pmax(s,0),1)
}

# ---- grid ------------------------------------------------------------------
set.seed(1)
g <- expand.grid(dbh=c(5,15,30,50,80), ht=c(3,8,15,22), bal=c(0,5,20),
                 baph=c(2,15,40), cr=c(0.3,0.6,0.9), byi=c(100,264,450),
                 planted=c(0,1))
g$qmd <- pmax(g$dbh*0.9, 5); g$rht <- pmin(g$ht/22, 1)
g$HT  <- with(g, pred_ht(dbh,baph,bal,qmd,byi, P_ht$a0,P_ht$a1,P_ht$b,P_ht$c,P_ht$g1,P_ht$g2))
g$HCB <- with(g, pred_hcb(dbh,ht,bal,baph,byi, P_hcb$b0,P_hcb$b1,P_hcb$b2,P_hcb$b3,P_hcb$b4,P_hcb$b5))
g$DDBH<- with(g, ddbh(dbh,bal,baph,cr,byi,planted, P_dd$b0,P_dd$b1,P_dd$b2,P_dd$b3,P_dd$b4,P_dd$b5,P_dd$b6,P_dd$b7,P_dd$b8))
g$DHT <- with(g, dht(dbh,ht,bal,baph,cr,byi,planted, P_dh$b0,P_dh$b1,P_dh$b2,P_dh$b3,P_dh$b4,P_dh$b5,P_dh$b6,P_dh$b7,P_dh$b8))
g$SURV<- with(g, surv_prob(dbh,ht,cr,rht,byi, P_sv$b0,P_sv$b1,P_sv$b2,P_sv$b3,P_sv$b4,P_sv$b5,P_sv$b6,P_sv$b7))
out <- "/sessions/elegant-lucid-cannon/mnt/outputs/koa_harness/results"
dir.create(out, showWarnings=FALSE, recursive=TRUE)
write.csv(g, file.path(out,"fidelity_R_HiGy.csv"), row.names=FALSE)
cat("R verbatim HiGy.R predictions written:", nrow(g), "grid points\n")
