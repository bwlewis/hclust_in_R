# A basic (and wasteful) way to compute a full Euclidean distance matrix for
# the input matrix x. The output[i,j] position is the Euclidean distance
# between rows x[i,] and x[j,].
dis = function(x)
{
  x = as.matrix(x)
  u = apply(x*x,1,sum) %*% matrix(1.0,1,nrow(x))
  sqrt(abs(u + t(u) - 2 * x %*% t(x)))
}

# The following ordering function for the hclust plot is translated from the
# original Fortran code used by R in hclust.f. It's only needed by the plotting
# routine to avoid crossing connections.
iorder = function(m)
{
  N = nrow(m) + 1
  iorder = rep(0,N)
  iorder[1] = m[N-1,1]
  iorder[2] = m[N-1,2]
  loc = 2
  for(i in seq(N-2,1))
  {
    for(j in seq(1,loc))
    {
      if(iorder[j] == i)
      {
        iorder[j] = m[i,1]
        if(j==loc)
        {
          loc = loc + 1
          iorder[loc] = m[i,2]
        } else
        {
          loc = loc + 1
          for(k in seq(loc, j+2)) iorder[k] = iorder[k-1]
          iorder[j+1] = m[i,2]
        }
      }
    }
  }
  -iorder
}

# A simple pure-R single-linkage hierarchical clustering implementation. Can
# be used like hclust(d, "single").
hc = function(d)
{
  if(!is.matrix(d)) d = as.matrix(d)
  N = nrow(d)
  diag(d)=Inf
  n = -(1:N)                       # Tracks group membership
  m = matrix(0,nrow=N-1, ncol=2)   # hclust merge output
  h = rep(0,N-1)                   # hclust height output
  j = 1
  while(j<N)
  {
# Find smallest distance and corresponding indices
    h[j] = min(d)
    i = which(d==h[j], arr.ind=TRUE)
# If not already assigned to the same group, then...
    if(diff(n[i[1,]])!=0)
    {
# R's convention is to order each m[j,] pair as follows:
      p = n[i[1,]]
      p = p[order(abs(p))]
      m[j,] = p
# Agglomerate this pair and all previous groups they belong to
# into the current jth group:
      grp = c(i[1,], which(n %in% n[i[1,n[i[1,]]>0]]))
      n[grp] = j
      j = j + 1
    }
# Move on to the next minimum by excluding current one
    d[i] = Inf
  }
# This is the end of the clustering part.
  o = iorder(m)
  structure(list(merge = m, height = h, order = o,
        labels = rownames(d), method = "single", 
        call = match.call(), dist.method = "euclidean"), 
        class = "hclust")
}

# Recursively tally the tree depth at level i for hclust object h.
depth = function(h,i)
{
  m = h$merge
  j = 1
  if(all(m[i,]<0)) return(j)
  if(m[i,1]>0) j = j + depth(h,m[i,1])
  if(m[i,2]>0) j = j + depth(h,m[i,2])
  j
}


# Compare!
h = hclust(dist(USArrests),method="single")
h1 = hc(dis(USArrests))
plot(h)
plot(h1)
