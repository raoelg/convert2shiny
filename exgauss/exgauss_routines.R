`dexgauss` <-
  function (t, mu = 0.5, sigma = .1, lambda = 1,log=FALSE) {ld <- log(lambda)-t*lambda+mu*lambda+(lambda*sigma)^2/2 + pnorm(t,mu+lambda*sigma^2,sigma,log=TRUE); if(log) ld else exp(ld);}

`fit.exgauss` <-
  function (x, start = fit.exgauss.mom(x), ...) optim(start, function(theta, y) -sum(dexgauss(y, mu = theta[1], sigma = theta[2], lambda = theta[3], log = TRUE)), y = x, ...)

`fit.exgauss.QMLE` <-
  function(rt, p, start =fit.exgauss.mom(rt), ...)
  {
    p = sort(unique(c(0,1,p)));
    RT = sort(rt);
    N = diff(p)*length(rt);
    I = p[-c(1,length(p))]*length(rt)+ 0.5;
    Im = trunc(I);
    Ip=trunc(Im+1);
    qhat = c(-Inf,RT[Im] + (RT[Ip]-RT[Im])*(I-Im), Inf);
    f=function(theta, q, N) {
      P=pexgauss(qhat,mu=theta[1],sigma=theta[2],lambda=theta[3]);
      P[1]=0;
      P = diff(P);
      -sum(N*log(P[P>0]));
    }
    optim(start, f, q=qhat, N=N, ...)
  }



`fit.exgauss.QMLE2` <-
  function(rt, p, start =fit.exgauss.mom(rt), ...)
  {
    p = sort(unique(c(0,1,p)));
    RT = sort(rt);
    if(length(p)>length(RT))
      stop("Empty bins. Provide p of length at most ", length(RT))
    n = length(rt)
    N = diff(p)*n;
    I = p[-c(1,length(p))]*n+ 0.5;
    Im = floor(I);
    Ip = ceiling(I);
    qhat = RT[Im] + (RT[Ip]-RT[Im])*(I-Im);
    qhat = c(-Inf, qhat, Inf)
    f = function(theta, q, N)
    {
      P=pexgauss(qhat,mu=theta[1],sigma=theta[2],lambda=theta[3]);
      dP = diff(c(0,P,1));
      if(any(dP<=0))
        return(1e16)
      else
        -sum(N*log(dP));
    }
    optim(start, f, q=qhat, N=N, ...)
  }

`fit.exgauss.QMLE3` <-
  function(rt, p=seq(0,1,len=7), start=fit.exgauss.mom(rt), ...){p=sort(unique(c(0,1,p))); qhat=quantile(rt,p); N=as.vector(table(findInterval(rt,qhat)));f=function(theta,q,N){P=pexgauss(q,mu=theta[1],sigma=theta[2],lambda=theta[3]);dP=diff(c(0,P)); -sum(N*log(dP))}; optim(start,f,q=qhat,N=N,...);}

`fit.exgauss.mom` <-
  function (rt)
  {
    lmb = 1/abs(tmp <- mean((rt - (m <- mean(rt)))^3)/2)^(1/3)
    if(tmp < 0)
      warning("Negative lambda estimate ", sign(tmp)*lmb)
    lmb = if(tmp < 0)  1 else lmb
    c(mu = m - 1/lmb, sigma = if ((v <- var(rt)) > 1/lmb^2) sqrt(v -
                                                                   1/lmb^2) else 0, lambda = lmb)
  }

`fitExgauss` <-
  function(y,p=NA,nrepeat=10,
           start=function(y){st=fit.exgauss.mom(y); jitter(ifelse(st>0,st,1))},
           method='L-BFGS',
           lower=1e-5,
           control=list(factr=1e1,fnscale=1e6), ...)
  {
    keep=list(value=1e10,i=0,failed=TRUE);
    for(i in 1:nrepeat) {
      if(missing(p))
        fit = tryCatch(
          fit.exgauss(y,method=method,lower=if(pmatch(method,'L-BFGS',FALSE)) lower else -Inf,
                      start=startpar <- if(is.function(start)) start(y) else start,
                      hessian=TRUE,control=control, ...),
          error= function(e) NULL)
      else
        fit = tryCatch(
          fit.exgauss.QMLE(y,p,method=method,lower=if(pmatch(method,'L-BFGS',FALSE)) lower else -Inf,
                           start=startpar <- if(is.function(start)) start(y) else start,
                           hessian=TRUE,control=control, ...),
          error= function(e) NULL)
      if(!is.null(fit) && fit$value<keep$value && all(eigen(fit$hessian,TRUE, TRUE)$val>0)) { # keep best fit
        keep=fit;
        keep$i=i;
        keep$start = startpar;
      }
    };
    if(!is.null(keep$failed) && keep$failed)
      stop("Failed to find estimates. Please provide different starting values/function")
    keep$N = length(y);
    keep$a_par = keep$par; # alternative parameterization
    keep$a_par[3] = 1/keep$a_par[3]
    if(all(eigen(keep$hessian,TRUE,TRUE)$values>0)){
      keep$cov = solve(keep$hessian);
      keep$s.e. <- sqrt(diag(keep$cov));
      .R <- diag(c(1,1,-keep$a_par[3]^2));
      keep$a_cov = .R %*% keep$cov %*% .R;
      keep$a_s.e. = sqrt(diag(keep$a_cov));
      names(keep$a_s.e.) = names(keep$a_par);
      keep$note = "a_par, a_cov, a_s.e. are for alternative parameterization; the latter two are approximations"
    }
    else {
      warning("No proper convergence. Diagnose the problem, or try different starting values.");
      keep$cov = NA;
      keep$s.e. = NA;
    }
    keep;
  }

`pexgauss` <-
  function(t, mu=0.5, sigma=0.1, lambda=1) pnorm(t,mu,sigma) - dexgauss(t,mu,sigma,lambda)/lambda

`rexgauss` <-
  function(n,mu=0.5,sigma=.1,lambda=1) rnorm(n,mu,sigma)+rexp(n,lambda)

`valid.exgauss` <-
  function (mu, sigma, lambda) pexgauss(0, mu, sigma, lambda) > -1e-06

`qexgauss` <- function(p,mu=.5,sigma=.1,lambda=1){P=p;p=p[(not1 <- p<1) & (not0 <- p>0)]; f=function(t)pexgauss(t,mu,sigma,lambda)-p; b=a=p;v=sigma+1/lambda^2;a[]=mu-4*v;b=mu+10*v;fa=f(a);fb=f(b);c=co=b;co[]=0; while(max(abs(c-co)>1e-8)){co=c;c=(a+b)/2;fc=f(c); iac=fa*fc<0; a=ifelse(iac,a,c);fa=ifelse(iac,fa,fc); b=ifelse(iac,c,b);fb=ifelse(iac,fc,fb);};Q=P; Q[not1&not0]=(a+b)/2;Q[!not1]=Inf;Q[!not0]=-Inf; Q}

