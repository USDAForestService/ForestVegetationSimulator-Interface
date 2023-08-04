svsTree <- function(tree,treeform)
{
  # data in the treeform (inforation form Bob's documentation): 
  # Sp Species
  # TrCl tree class
  # CrCl crown class
  # PlFrm plant form or plant type, Bits 1-4 as follows:
  #  0=single leader form, 1=multiple leader with strong central,
  #  2=multiple leader with weak central, 3=no central. 
  #  Bits 5-8: foliage damage, bits 9-12 branch damage. 
  #  Foliage damage codes are: 0=no damage, 1=top 1/3, 2=top 2/3, 
  #    3=bot 1/3, 4=bot 2/3, 5=entire crown. 
  #  Branch damage information: 0=no damage, 1=top 1/3 dead branches,
  #    2=top 2/3, 3=bot 1/3, 4=bot 2/3, 5=entire crown 
  # Nbrchs for single leader forms, number of branches, for multi leader
  #  this is the number of heavy branches
  # NWorls for single leader forms, number of whorls, for multi leader
  #  it is the number of leaves
  # BrBase Location of the branching point relative to base of live crown
  #  (used on multiple leader trees).                            
  # BrAngle Angular increment between branches and leaf structures
  # LoX the portion of the crown width for the lower radius
  # LoY the portion of the crown length for the lower radius
  # HiX the portion of the crown width for the upper radius
  # HiY the poriton of the crown length for the upper radius
  # BaseUp branch insertation angle for branches near the bottom
  # TopUp branch insertation angle for branches near the top
  # StemC Stem color
  # BrCol Branch color
  # FlCol1 Foliage color for 75%           
  # FlCol2 Foliage color for 25%
  # SampHt Ht of sample trees used by SVS tree designer
  # SampCR Crown radio ... ditto
  # SampCDia Crown diameter ... ditto                                               
  # SampScale Object scaling, not used here
  
  #    CURRENT TREE CLASSES: from FVS svout.f
  #     0 - GREEN TREE (STANDING OR RECENTLY CUT TREE)
  #    90 - NEW WESTWIDE PINE BEETLE KILL, OFF-GREEN
  #    91 - 1 YEAR OLD WWPB KILL, RED TREE
  #    92 - 2 YEAR OLD WWPB KILL, FADING TREE
  #    94 - SNAG
  #    95 - RED AND GREEN TREE (BURNT 99) NOT USED YET
  #    96 - GREY SNAG (BURNT 94) - OLDER BURNED TREE
  #    97 - GREY TREE (BURNT 98 OR 99) - RECENTLY BURNED TREE
  #    98 - RED TREE (RECENTLY DEAD STANDING OR DOWN TREE)
  #    99 - WILD CARD, WE DON'T USE THIS CODE...IT IS A GREEN
  #         TREE (STANDING OR RECENTLY CUT TREE)
                                                   
  colors=c(rgb(210,  66, 14, maxColorValue=255),
           rgb(163, 117,  0, maxColorValue=255),
           rgb(119,  42, 24, maxColorValue=255),
           rgb( 98,  98, 98, maxColorValue=255),
           rgb(112, 153,  0, maxColorValue=255),
           rgb(  0,  86, 26, maxColorValue=255),
           rgb( 20,  66, 42, maxColorValue=255),
           rgb(  0,  76,  0, maxColorValue=255),
           rgb( 62,  45, 45, maxColorValue=255),
           rgb( 98,  18,  0, maxColorValue=255),
           rgb( 88,  55, 57, maxColorValue=255),
           rgb( 52, 149, 64, maxColorValue=255),
           rgb(  0,  58, 44, maxColorValue=255),
           rgb( 90,  64, 38, maxColorValue=255),
           rgb(115,  82,  0, maxColorValue=255),
           rgb(137, 137,  0, maxColorValue=255),
           rgb( 69,  72, 72, maxColorValue=255),
           rgb( 86,  64, 16, maxColorValue=255),
           rgb(  0, 107,  0, maxColorValue=255),
           rgb( 76,  46,  0, maxColorValue=255))
           
  if (any(is.na(tree))) return(NULL)
  if (tree$DBH == 0) return(NULL)                                                             
  tree$DBH = tree$DBH/12
  CL = tree$Ht*tree$Cr1
  fallangle=tree$Fang
  HCB = tree$Ht*(1-tree$Cr1)
  ttcl = if (tree$TrCl == 0) 99 else tree$TrCl  
  tr = subset(treeform,Sp == tree$sp & TrCl == ttcl)
  tr = as.list(tr[1,])
  tltslp = diff(c(tr$BaseUp,tr$TopUp))
  tree$crowncolor= c(colors[tr$FlCol1+1],colors[tr$FlCol2+1])  
  tree$crowncolor= tree$crowncolor[!duplicated(tree$crowncolor)]
  tree$stemcolor = colors[tr$StemC+1]
                                                                                            
  if (tree$Cr1 && tree$Crd1 && HCB && CL) 
  {
    branchList = list()
    # single leader
    if (tr$PlFrm == 0)
    {
      # single leader, then ignore the nwhorl and nbran data from treeform
      nwhorl = tr$Nwhorls
      nbran  = tr$Nbrchs  # total number
      # limit the number of branches to 4 per foot of crown length
      if (nbran > 4*CL) nbran = 4*CL
      if (nwhorl == 0) nwhorl = nbran
      nwhorl = ceiling(nwhorl)
      if (nwhorl > 0) 
      {
        nbran = max(3,floor(nbran/nwhorl))  # branches per whorl
        xtap = c(HCB,HCB+CL*tr$LoY,HCB+CL*tr$HiY,tree$Ht)
        ytap = c(0,tree$Crd1*tr$LoX,tree$Crd1*tr$HiX,0) 
        distfun <- approxfun(xtap,ytap,rule=2,ties="ordered")
        rsc = runif(nwhorl)*min(1/nwhorl,.05)
        z <- rep((seq(0,1,length=nwhorl)+rsc),each=nbran)
        z[z>1] = 1
        # tlt in degrees.
        tlt <- tr$BaseUp + tltslp*z
        # tlt in slope proprtion
        tlt <- tan(tlt*pi/180)
        z <- HCB+CL*z
        angs <- rep(seq(0,2*pi,length=nbran),nwhorl)        
        startang = runif(nwhorl*nbran)*2*pi               
        angs = angs+startang  
        r <- tree$Crd1/2
        ll <- distfun(z)
        x <- tree$Xloc + ll*cos(angs)
        y <- tree$Yloc + ll*sin(angs)
        ll <- ll * tlt * .5
        ans = cbind(x,y,z)
        for (row in 1:nrow(ans)) 
        {
          lin = rbind(c(x=tree$Xloc,y=tree$Yloc,ans[row,3]),ans[row,])
          lin[1,3] = lin[1,3]-ll[row]
          lin[2,3] = lin[2,3]+ll[row]
          lin[lin[,3] > tree$Ht,3] = tree$Ht
          branchList[[row]] = lin
        }
        branchList = branchList[!duplicated(branchList)]
        tree$branches = list()
        if (fallangle==0)
        {
          tree$baseht = 0
          tree$tip = c(tree$Xloc,tree$Yloc,tree$Ht)                 
          if (length(branchList))
          {
            if (length(tree$crowncolor) > 1)
            {
              c1 = sort(sample.int(n=length(branchList),size=floor(length(branchList)*.75)))
              tree$branches[[1]] = do.call(rbind,branchList[ c1])
              tree$branches[[2]] = do.call(rbind,branchList[-c1]) 
            } else {  
              tree$branches[[1]] = do.call(rbind,branchList)
            }                                                    
          }
        } else {    
          tree$baseht=tree$DBH/2
          tree$branches[[1]] = do.call(rbind,branchList)   
          tree=fellTree(fallangle,tree)
        }
      } else {
        if (fallangle==0)
        {
          tree$baseht = 0
          tree$tip = c(tree$Xloc,tree$Yloc,tree$Ht)
        } else {
          tree$baseht=tree$DBH/2
          tree=fellTree(fallangle,tree)
        }
      }
    } else {     # multi leader  2=multiple leader with weak central
      nbran = tr$Nwhorl
      nleaves = tr$Nbrchs
      if (nbran == 0)
      {
cat ("svsTree, multiple leader, nbran == 0, tree not drawn.\n")
        return(NULL)
      }
      xtap = c(HCB,HCB+CL*tr$LoY,HCB+CL*tr$HiY,tree$Ht)
      ytap = c(0,tree$Crd1*tr$LoX,tree$Crd1*tr$HiX,0) 
      distfun <- approxfun(xtap,ytap,rule=2,ties="ordered")
      rsc = runif(nbran)*min(1/nbran,.05)
      z <- rep(seq(0,1,length=nbran)+rsc)
      z[z>1] = 1
      # tlt in degrees.
      tlt <- tr$BaseUp + tltslp*z
      # tlt in slope proprtion
      tlt <- tan(tlt*pi/180)
      z <- HCB+CL*z
      angs <- seq(0,2*pi,length=nbran)
      startang = runif(nbran)*.5*pi               
      angs = angs+startang  
      r <- tree$Crd1/2
      ll <- distfun(z)*.7  #just make them shorter.
      x <- tree$Xloc + ll*cos(angs)
      y <- tree$Yloc + ll*sin(angs)
      ll <- ll * tlt * .5
      ans = as.matrix(cbind(x,y,z),ncol=3)
      for (row in 1:nrow(ans)) 
      {
        lin = rbind(c(x=tree$Xloc,y=tree$Yloc,ans[row,3]),ans[row,])
        lin[1,3] = lin[1,3]-ll[row]
        lin[2,3] = lin[2,3]+ll[row]
        #branches can not be taller than the tree
        lin[lin[,3] > tree$Ht,3] = tree$Ht  
        branchList[[row]] = lin
      }
      branchList = branchList[!duplicated(branchList)]
      tree$branches = list()
      tree$branches[[1]] = do.call(rbind,branchList)  
      tree$leaves = list()     
      # draw the leaves if there are some.
      if (nleaves>0)
      {
        #adjust the leaf count
        lm3 = nleaves/(tr$SampHt*tr$SampCR*tr$SampCDia)
        nleaves = max(5,floor(lm3*CL*tree$Crd1*5)) # 5 comes from experimentation
        angs = runif(nleaves)*2*pi              
        z = runif(nleaves)
        z = HCB+CL*z
        ll <- distfun(z)*runif(nleaves)
        x <- tree$Xloc + ll*cos(angs)
        y <- tree$Yloc + ll*sin(angs)
        lvs = as.matrix(cbind(x,y,z),ncol=3)
        if (length(tree$crowncolor) > 1 && nleaves >= 5)
        {
          n1 = floor(nleaves)*.75
          tree$leaves[[1]] = lvs[1:n1,,drop=FALSE]
          tree$leaves[[2]] = lvs[(n1+1):nleaves,,drop=FALSE]
        } else tree$leaves[[1]] = lvs
      }
      # fell the tree if indicated  
      tree$baseht = 0
      if (fallangle>0) 
      {
        tree$baseht=tree$DBH/2
        tr=try(fellTree(fallangle,tree))
        tree = tr
      } else {
        tree$baseht = 0
        tree$tip = c(tree$Xloc,tree$Yloc,tree$Ht)
      }
    }
  }
  tree
}

fellTree <- function (fallangle,tree)
{
  if (fallangle == 0) return(tree)
  fallangle=(fallangle*pi/180)
  sinfa = sin(fallangle)
  cosfa = cos(fallangle)
  tx90 = matrix(c(1,0,0,0,0,-1,0,1,0),nrow=3,byrow=TRUE)
  ty90 = matrix(c(0,0,1,0,1,0,-1,0,0),nrow=3,byrow=TRUE)
  tzfa = matrix(c(cosfa,-sinfa,0,sinfa,cosfa,0,0,0,1),nrow=3,byrow=TRUE)  
  tree$tip = c(tree$Xloc+tree$Ht*cosfa,tree$Yloc+tree$Ht*sinfa,0)
  if (length(tree$branches))                           
  {
    down = tree$branches[[1]]
    if (nrow(down) == 0) next
    down[,1] =  (tree$branches[[1]][,2]-tree$Yloc)
    down[,2] =   tree$branches[[1]][,3]
    down[,3] = -(tree$branches[[1]][,1]-tree$Xloc) 
    down = t(apply(down,1,function(x)
      {      
        tt = as.vector(tzfa %*% (ty90 %*% (tx90 %*% x))[,1])
        c(tt[1]+tree$Xloc,tt[2]+tree$Yloc,tt[3])                  
      }))
    del = down[,3]<0
    if (any(del))
    {
      even = seq(2,length(del),2)
      del = even[del[even]]
      down = down[-c(del-1,del),]
      tree$branches[[1]] = down
    }
  }
  if (length(tree$leaves)) 
  {
    for (ilv in 1:length(tree$leaves))
    {
      down = tree$leaves[[ilv]]
      if (nrow(down) == 0) next
      down[,1] =  (tree$leaves[[ilv]][,2]-tree$Yloc)
      down[,2] =   tree$leaves[[ilv]][,3]
      down[,3] = -(tree$leaves[[ilv]][,1]-tree$Xloc) 
      down = t(apply(down,1,function(x)
        {      
          tt = as.vector(tzfa %*% (ty90 %*% (tx90 %*% x))[,1])
          c(tt[1]+tree$Xloc,tt[2]+tree$Yloc,tt[3])                  
        }))
      del = down[,3]<0
      if (any(del)) down = down[!del,]
      tree$leaves[[ilv]] = down
    }
  }
  tree
}

displayTrees <- function (drawnTrees)
{ 
  #draw the trunks
  alltr = list()
  for (tree in drawnTrees)                             
  { 
    if (is.null(tree$baseht)) next                                                  
    line = rbind(c(tree$Xloc,tree$Yloc,tree$baseht),tree$tip) 
    alltr[[tree$stemcolor]] = if (is.null(alltr[[tree$stemcolor]])) line 
                              else  rbind(alltr[[tree$stemcolor]],  line)                                                             
  }
  for (col in names(alltr)) segments3d(alltr[[col]],col=col,lwd=3,alpha=1,add=TRUE)
  #make big trees cones ... this code could be much faster by making a list of all triangles. 
  for (tree in drawnTrees)                                  
  {
    if (is.null(tree$baseht)) next
    if (tree$DBH > 1) # note that dbh is in feet. 
    {
      cone3d(base=c(tree$Xloc,tree$Yloc,tree$baseht),tip=tree$tip,
                    rad=tree$DBH/2,n= 5, col=tree$stemcolor)
    }
  } 
  allbr = list()
  alllv = list()
  # draw the crowns when they are made up of branches or leaves
  # draw all the line segments of the given color
  for (tree in drawnTrees)
  {
    if (length(tree$branches))
    { 
      ic = 0
      cols = if (length(tree$leaves)) tree$stemcolor else tree$crowncolor
      for (col in cols)
      { 
        ic = ic+1
        if (length(tree$branches) < ic) break
        allbr[[col]] = if (is.null(allbr[[col]])) tree$branches[[ic]] else 
                       rbind(allbr[[col]],tree$branches[[ic]]) 
      }
    }
    if (length(tree$leaves))
    {
      ic = 0
      for (col in tree$crowncolor)
      { 
        ic = ic+1
        alllv[[col]] = if (is.null(alllv[[col]])) tree$leaves[[ic]] else 
                       rbind(alllv[[col]],tree$leaves[[ic]]) 
      }
    }
  }
  for (col in names(allbr)) segments3d(allbr[[col]],col=col, lwd=1,alpha=1,add=TRUE)
  for (col in names(alllv))   points3d(alllv[[col]],col=col,cex=.5,alpha=1,add=TRUE,pch=".") #17)
}

cone3d <- function(base=c(0,0,0),tip=c(0,0,1),rad=1,n=8,draw.base=TRUE,
                   trans = par3d("userMatrix"), ...) {
  ax <- tip-base
  if (missing(trans) && !rgl.cur()) trans <- diag(4)
  ### is there a better way?
  if (ax[1]!=0) {
    p1 <- c(-ax[2]/ax[1],1,0)
    p1 <- p1/sqrt(sum(p1^2))
    if (p1[1]!=0) {
      p2 <- c(-p1[2]/p1[1],1,0)
      p2[3] <- -sum(p2*ax)
      p2 <- p2/sqrt(sum(p2^2))
    } else {
      p2 <- c(0,0,1)
    }
  } else if (ax[2]!=0) {
    p1 <- c(0,-ax[3]/ax[2],1)
    p1 <- p1/sqrt(sum(p1^2))
    if (p1[1]!=0) {
      p2 <- c(0,-p1[3]/p1[2],1)
      p2[3] <- -sum(p2*ax)
      p2 <- p2/sqrt(sum(p2^2))
    } else {
      p2 <- c(1,0,0)
    }
  } else {
    p1 <- c(0,1,0); p2 <- c(1,0,0)
  }
  degvec <- seq(0,2*pi,length=n+1)[-1]
  ecoord2 <- function(theta) 
  {
    base+rad*(cos(theta)*p1+sin(theta)*p2)
  }
  i <- rbind(1:n,c(2:n,1),rep(n+1,n))
  v <- cbind(sapply(degvec,ecoord2),tip)
  if (draw.base) 
  {
    v <- cbind(v,base)
    i.x <- rbind(c(2:n,1),1:n,rep(n+2,n))
    i <- cbind(i,i.x)
  }
  triangles3d(v[1,i],v[2,i],v[3,i],...)
}     

circle3D <- function (x0=0,y0=0,z0=0,r=1,n=60,col="gray",alpha=.5,...)
{
  theta <- seq(0, 2*pi, len=n)
  cords = cbind((cos(theta)*r) + x0,(sin(theta)*r) + y0, 0)
  polygon3d(cords,color=col,alpha=alpha,...)
  cords
}

matRotat <- function(mat,xa=0,ya=0,za=0)
{
  x = 0.01745329  # x = pi/180
  sinxa = sin(xa*x)
  sinya = sin(ya*x)
  sinza = sin(za*x)
  cosxa = cos(xa*x)
  cosya = cos(ya*x)
  cosza = cos(za*x)
  mx = matrix(c(1,0,0,0,cosxa,-sinxa,0,sinxa,cosxa),nrow=3,byrow=TRUE)
  my = matrix(c(cosya,0,sinya,0,1,0,-sinya,0,cosya),nrow=3,byrow=TRUE)
  mz = matrix(c(cosza,-sinza,0,sinza,cosza,0,0,0,1),nrow=3,byrow=TRUE)  
  rm = mx %*% my %*% mz
  mat %*% rm
}

matRotateZ180 <- function(mat,offset)
{
  mat[,1] = mat[,1]-offset
  mat[,2] = mat[,2]-offset
  mat = mat %*% diag(c(-1,-1,1))
  mat[,1] = mat[,1]+offset
  mat[,2] = mat[,2]+offset
  mat
}
  
