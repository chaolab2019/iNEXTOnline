# -- data
x1 = rep(c(1:12, 14, 17, 19, 20, 21, 24, 26, 40, 57, 60, 64, 71, 77),
         c(70, 17, 4, 5, 5, 5, 5, 3, 1, 2, 3, 2, 2, 1, 2, 3, 1, 1, 1, 1, 2, 1, 1, 1, 1))

x2 = rep(c(1,2,3,4,5,6,7,8,14,42),c(84,10,4,3,5,1,2,1,1,1))

beetles <- list("SecondGrowth"=x1,"OldGrowth"=x2)

Diversity_profile <- function(x,q){
  x = x[x>0]
  n = sum(x)
  f1 = sum(x==1)
  f2 = sum(x==2)
  p1 = ifelse(f2>0,2*f2/((n-1)*f1+2*f2),ifelse(f1>0,2/((n-1)*(f1-1)+2),1))
  r <- 1:(n-1)
  Sub <- function(q){
    if(q==0){
      sum(x>0) + (n-1)/n*ifelse(f2>0, f1^2/2/f2, f1*(f1-1)/2)
    }
    else if(q==1){
      A <- sum(x/n*(digamma(n)-digamma(x)))
      B <- ifelse(f1==0|p1==1,0,f1/n*(1-p1)^(1-n)*(-log(p1)-sum((1-p1)^r/r)))
      exp(A+B)
    }else if(abs(q-round(q))==0){
      A <- sum(exp(lchoose(x,q)-lchoose(n,q)))
      #ifelse(A==0,NA,A^(1/(1-q)))
      A^(1/(1-q))
    }else {
      sort.data = sort(unique(x))
      tab = table(x)
      term = sapply(sort.data,function(z){
        k=0:(n-z)
        sum(choose(k-q,k)*exp(lchoose(n-k-1,z-1)-lchoose(n,z)))
      })
      r <- 0:(n-1)
      A = sum(tab*term)
      B = ifelse(f1==0|p1==1,0,f1/n*(1-p1)^(1-n)*(p1^(q-1)-sum(choose(q-1,r)*(p1-1)^r)))
      (A+B)^(1/(1-q))
    }
  }
  sapply(q, Sub)
}

Diversity_profile_MLE <- function(x,q){
  p <- x[x>0]/sum(x)
  Sub <- function(q){
    if(q==0) sum(p>0)
    else if(q==1) exp(-sum(p*log(p)))
    else exp(1/(1-q)*log(sum(p^q)))
  }
  sapply(q, Sub)
}

Diversity_Tsallis <- function(x,q){
  qD = Diversity_profile(x, q)
  ans = rep(0,length(qD))
  ans[which(q==1)] <- log(qD[which(q==1)])
  q1 = q[q!=1]
  ans[which(q!=1)] <- (qD[which(q!=1)]^(1-q1)-1)/(1-q1)
  return(ans)
}

Diversity_Tsallis_MLE <- function(x,q){
  qD = Diversity_profile_MLE(x,q)
  ans = rep(0,length(qD))
  ans[which(q==1)] <- log(qD[which(q==1)])
  q1 = q[q!=1]
  ans[which(q!=1)] <- (qD[which(q!=1)]^(1-q1)-1)/(1-q1)
  return(ans)
}

bootstrap_forq = function(data,B,q,conf,FUNNAME){
  data <- data[data!=0]
  n <- sum(data)
  f1 = sum(data==1); f2 = sum(data==2)
  f0 = ceiling(ifelse( f2>0, (n-1)*f1^2/n/2/f2, (n-1)*f1*(f1-1)/2/n ))
  C_hat = ifelse(f2>0, 1-f1*(n-1)*f1/n/((n-1)*f1+2*f2), 1-f1*(n-1)*(f1-1)/n/((n-1)*(f1-1)+2))
  lamda_hat = (1-C_hat)/sum((data/n)*(1-data/n)^n) 
  pi_hat = (data/n)*(1-lamda_hat*((1-data/n)^n)) 
  p_hat = c( pi_hat, rep( (1-C_hat)/f0, f0 )) 
  random = rmultinom( B, n, p_hat ) 
  #Bt_estimate <- sapply(c(1:B),function(i) FUNNAME(random[,i],q))
  Bt_estimate <- apply(random,MARGIN = 2,function(i) FUNNAME(i,q))
  estimate <- FUNNAME(data,q)
  #Interval_mean = apply( Bt_estimate, 1, mean)
  Interval_mean = rowMeans(Bt_estimate)
  Interval_sd = apply(Bt_estimate, 1, sd)
  Interval_quantileL = apply( Bt_estimate, 1, quantile, p=(1-conf)/2)
  Interval_quantileU = apply( Bt_estimate, 1, quantile, p=1-(1-conf)/2)
  Upper_bound = estimate+Interval_quantileU-Interval_mean 
  Lower_bound = estimate+Interval_quantileL-Interval_mean
  result <- cbind("estimate"=estimate,"sd"=Interval_sd,"LCL"=Lower_bound,"UCL"=Upper_bound)
  result 
}

MakeTable_Proposeprofile_nose = function(data,q){
  Diversity = Diversity_profile(data,q)
  Entropy = Diversity_Tsallis(Diversity,q)
  output = rbind(data.frame("Order.q" = q,"Target"="Diversity","Estimate"=Diversity),
                 data.frame("Order.q" = q,"Target"="Entropy","Estimate"=Entropy))
  output[,c(1,3)] = round(output[,c(1,3)],9)
  
  return(output)
}

MakeTable_Proposeprofile = function(data, B, q, conf){
  Diversity = bootstrap_forq(data, B, q, conf, Diversity_profile)
  Entropy = bootstrap_forq(data, B, q, conf, Diversity_Tsallis)
  # tmp <- Diversity_Tsallis(Diversity[,1],q)
  # Entropy = cbind("estimate"=tmp,"sd"=Diversity[,2],"LCL"=tmp-Diversity[,2],"UCL"=tmp+Diversity[,2])
  output = rbind(data.frame("Order.q" = q,"Target"="Diversity","Estimate"=Diversity[,1],"s.e."=Diversity[,2],"LCL"=Diversity[,3],"UCL"=Diversity[,4]),
                 data.frame("Order.q" = q,"Target"="Entropy","Estimate"=Entropy[,1],"s.e."=Entropy[,2],"LCL"=Entropy[,3],"UCL"=Entropy[,4]))
  output[,c(1,3,4,5,6)] = round(output[,c(1,3,4,5,6)],9)
  
  return(output)
}

MakeTable_Empericalprofile_nose = function(data,q){
  Diversity = Diversity_profile_MLE(data,q)
  Entropy = Diversity_Tsallis_MLE(Diversity,q)
  output = rbind(data.frame("Order.q" = q,"Target"="Diversity","Emperical"=Diversity),
                 data.frame("Order.q" = q,"Target"="Entropy","Emperical"=Entropy))
  output[,c(1,3)] = round(output[,c(1,3)],9)
  
  return(output)
}

MakeTable_Empericalprofile = function(data, B, q, conf){
  Diversity = bootstrap_forq( data, B,q,conf,Diversity_profile_MLE)
  Entropy = bootstrap_forq( data, B,q,conf,Diversity_Tsallis_MLE)
  # tmp <- Diversity_Tsallis(Diversity[,1],q)
  # Entropy = cbind("estimate"=tmp,"sd"=Diversity[,2],"LCL"=tmp-Diversity[,2],"UCL"=tmp+Diversity[,2])
  output = rbind(data.frame("Order.q" = q,"Target"="Diversity","Emperical"=Diversity[,1],"s.e."=Diversity[,2],"LCL"=Diversity[,3],"UCL"=Diversity[,4]),
                 data.frame("Order.q" = q,"Target"="Entropy","Emperical"=Entropy[,1],"s.e."=Entropy[,2],"LCL"=Entropy[,3],"UCL"=Entropy[,4]))
  output[,c(1,3,4,5,6)] = round(output[,c(1,3,4,5,6)],9)
  
  return(output)
}

plot_diversity_profile_nose=function(output){
  output <- filter(output,Target=="Diversity")
  if(colnames(output)[3]=="Estimate"){
    ggplot(output, aes(x=Order.q, y=Estimate, colour=Community))+
    geom_line(size=1.2) +    
    labs(x="Order q", y="Diversity")+
    theme(text=element_text(size=18))
  } else{
    ggplot(output, aes(x=Order.q, y=Emperical, colour=Community))+
      geom_line(size=1.2) +    
      labs(x="Order q", y="Diversity")+
      theme(text=element_text(size=18))
  }
  
}

plot_diversity_profile=function(output){
  output <- filter(output,Target=="Diversity")
  if(colnames(output)[3]=="Estimate"){
    ggplot(output, aes(x=Order.q, y=Estimate, colour=Community))+
    geom_line(size=1.2) +    
    labs(x="Order q", y="Diversity")+
    theme(text=element_text(size=18))+
    geom_ribbon(aes(ymin=LCL, ymax=UCL, fill=Community, colour=NULL), alpha=0.3, linetype=0)
  } else{
    ggplot(output, aes(x=Order.q, y=Emperical, colour=Community))+
      geom_line(size=1.2) +    
      labs(x="Order q", y="Diversity")+
      theme(text=element_text(size=18))+
      geom_ribbon(aes(ymin=LCL, ymax=UCL, fill=Community, colour=NULL), alpha=0.3, linetype=0)
  }
  
}

#=======incidence profile==========#
#cpp function in Diversity_profile.inc
cppFunction(
  "NumericVector qDFUN(NumericVector q,NumericVector Xi,const int n){
    const int length = q.size();
    const int Sobs = Xi.size();
    NumericVector Q(length);
    NumericVector delta(n);
    NumericVector temp(Sobs);
    for(int k=0;k<=(n-1);k++){
    for(int i = 0;i<Sobs;i++){
    temp[i] = (Xi[i]/n)*exp(Rf_lchoose(n-Xi[i],k)-Rf_lchoose(n-1,k));
    }
    delta[k] = sum(temp);
    }
    
    for(int i=0;i<length;i++){
    float temp = 0;
    for(int k=0;k<=(n-1);k++){
    temp = temp + (Rf_choose(q[i]-1,k)*pow(-1,k)*delta[k]);
    }
    Q[i] = temp;
    }
    return Q;
  }")
#cpp function in Diversity_profile_MLE.inc
cppFunction(
  "NumericVector qD_MLE(NumericVector q,NumericVector ai){
    const int length = q.size();
    const int S = ai.size();
    NumericVector Q(length);
    NumericVector temp(S);
    for(int j = 0; j<length;j++){
    for(int i = 0 ; i<S;i++){
    temp[i] = pow(ai[i],q[j]);
    }
    Q[j] = pow(sum(temp),1/(1-q[j]));
    }
    return Q;
}")
AA.inc <- function(data){
  T = data[1]
  U <- sum(data[-1])
  data = data[-1]
  Yi = data[data!=0]
  Q1 <- sum(Yi==1)
  Q2 <- sum(Yi==2)
  if(Q2>0 & Q1>0){
    A <- 2*Q2/((T-1)*Q1+2*Q2)
  }
  else if(Q2==0 & Q1>1){
    A <- 2/((T-1)*(Q1-1)+2)
  }
  else{
    A <- 1
  }
  return(A)
}

Diversity_profile.inc <- function(data,q){
  T = data[1]
  Yi = data[-1]
  Yi <- Yi[Yi!=0]
  U <- sum(Yi)
  Q1 <- sum(Yi==1)
  Q2 <- sum(Yi==2)
  Sobs <- length(Yi)
  A <- AA.inc(data)
  Q0hat <- ifelse(Q2 == 0, (T - 1) / T * Q1 * (Q1 - 1) / 2, (T - 1) / T * Q1 ^ 2/ 2 / Q2)
  B <- sapply(q,function(q) ifelse(A==1,0,(Q1/T)*(1-A)^(-T+1)*(A^(q-1)-sum(sapply(c(0:(T-1)),function(r) choose(q-1,r)*(A-1)^r)))))
  qD <- (U/T)^(q/(q-1))*(qDFUN(q,Yi,T) + B)^(1/(1-q))
  qD[which(q==0)] = Sobs+Q0hat
  yi <- Yi[Yi>=1 & Yi<=(T-1)]
  delta <- function(i){
    (yi[i]/T)*sum(1/c(yi[i]:(T-1)))
  }
  if(sum(q %in% 1)>0){
    C_ <- ifelse(A==1,0,(Q1/T)*(1-A)^(-T+1)*(-log(A)-sum(sapply(c(1:(T-1)),function(r) (1-A)^r/r))))
    qD[which(q==1)] <- exp((T/U)*( sum(sapply(c(1:length(yi)),function(i) delta(i))) + C_)+log(U/T))
  }
  return(qD)
}

Diversity_profile_MLE.inc <- function(data,q){
  Yi = data[-1]
  U = sum(Yi)
  Yi <- Yi[Yi!=0]
  ai <- Yi/U
  qD = qD_MLE(q,ai)
  qD[which(q==1)] <- exp(-sum(ai*log(ai)))
  return(qD)
}

Diversity_Tsallis.inc <- function(qD,q){
  #qD = Diversity_profile.inc(data,q)
  ans = rep(0,length(qD))
  ans[which(q==1)] <- log(qD[which(q==1)])
  q1 = q[q!=1]
  ans[which(q!=1)] <- (qD[which(q!=1)]^(1-q1)-1)/(1-q1)
  return(ans)
}

Diversity_Tsallis_MLE.inc <- function(qD,q){
  #qD = Diversity_profile_MLE.inc(data,q)
  ans = rep(0,length(qD))
  ans[which(q==1)] <- log(qD[which(q==1)])
  q1 = q[q!=1]
  ans[which(q!=1)] <- (qD[which(q!=1)]^(1-q1)-1)/(1-q1)
  return(ans)
}

bootstrap_forq.inc = function(data,B,q,conf,FUNNAME){
  T <- data[1]
  Yi <- data[-1]
  Yi <- Yi[Yi>0]
  Sobs <- sum(Yi > 0)   
  Q1 <- sum(Yi == 1) 	
  Q2 <- sum(Yi == 2) 
  Q0.hat <- ifelse(Q2 == 0, (T - 1) / T * Q1 * (Q1 - 1) / 2, (T - 1) / T * Q1 ^ 2/ 2 / Q2)	#estimation of unseen species via Chao2
  A <- ifelse(Q1>0, T*Q0.hat/(T*Q0.hat+Q1), 1)
  a <- Q1/T*A
  b <- sum(Yi / T * (1 - Yi / T) ^ T)
  w <- a / b  		
  Prob.hat <- Yi / T * (1 - w * (1 - Yi / T) ^ T)					
  Prob.hat.Unse <- rep(a/ceiling(Q0.hat), ceiling(Q0.hat))  	
  p_hat =c(Prob.hat, Prob.hat.Unse)
  random = t(sapply(1:length(p_hat), function(i){rbinom(B,T,p_hat[i])}))
  random = rbind(rep(T,B),random)
  Bt_estimate <- apply(random,MARGIN = 2,function(i) FUNNAME(i,q))
  estimate <- FUNNAME(data,q)
  Interval_mean = rowMeans(Bt_estimate)
  Interval_sd = apply(Bt_estimate, 1, sd)
  Interval_quantileL = apply( Bt_estimate, 1, quantile, p=(1-conf)/2)
  Interval_quantileU = apply( Bt_estimate, 1, quantile, p=1-(1-conf)/2)
  Upper_bound = estimate+Interval_quantileU-Interval_mean 
  Lower_bound = estimate+Interval_quantileL-Interval_mean
  result <- cbind("estimate"=estimate,"sd"=Interval_sd,"LCL"=Lower_bound,"UCL"=Upper_bound)
  result 
}

MakeTable_EmpericalDiversityprofile.inc_nose = function(data,q){
  Diversity = Diversity_profile_MLE.inc(data,q)
  Entropy = Diversity_Tsallis_MLE.inc(Diversity,q)
  output = rbind(data.frame("Order.q" = q,"Target"="Diversity","Emperical"=Diversity),
                 data.frame("Order.q" = q,"Target"="Entropy","Emperical"=Entropy))
  return(output)
}

MakeTable_Proposeprofile.inc_nose = function(data,q){
  Diversity = Diversity_profile.inc(data,q)
  Entropy = Diversity_Tsallis.inc(Diversity,q)
  output = rbind(data.frame("Order.q" = q,"Target"="Diversity","Estimate"=Diversity),
                 data.frame("Order.q" = q,"Target"="Entropy","Estimate"=Entropy))
  return(output)
}

MakeTable_EmpericalDiversityprofile.inc = function(data, B, q,conf){
  Diversity = bootstrap_forq.inc( data, B,q,conf,Diversity_profile_MLE.inc)
  #Entropy = bootstrap_forq.inc( data, B,q,conf,Diversity_Tsallis_MLE.inc)
  tmp <- Diversity_Tsallis_MLE.inc(Diversity[,1],q)
  Entropy = cbind("estimate"=tmp,"sd"=Diversity[,2],"LCL"=tmp-Diversity[,2],"UCL"=tmp+Diversity[,2])
  output = rbind(data.frame("Order.q" = q,"Target"="Diversity","Emperical"=Diversity[,1],"s.e."=Diversity[,2],"LCL"=Diversity[,3],"UCL"=Diversity[,4]),
                 data.frame("Order.q" = q,"Target"="Entropy","Emperical"=Entropy[,1],"s.e."=Entropy[,2],"LCL"=Entropy[,3],"UCL"=Entropy[,4]))
  return(output)
}

MakeTable_Proposeprofile.inc = function(data, B, q,conf){
  Diversity = bootstrap_forq.inc(data, B, q,conf,Diversity_profile.inc)
  #Entropy = bootstrap_forq.inc( data, B,q,conf,Diversity_Tsallis.inc)
  tmp <- Diversity_Tsallis.inc(Diversity[,1],q)
  Entropy = cbind("estimate"=tmp,"sd"=Diversity[,2],"LCL"=tmp-Diversity[,2],"UCL"=tmp+Diversity[,2])
  output = rbind(data.frame("Order.q" = q,"Target"="Diversity","Estimate"=Diversity[,1],"s.e."=Diversity[,2],"LCL"=Diversity[,3],"UCL"=Diversity[,4]),
                 data.frame("Order.q" = q,"Target"="Entropy","Estimate"=Entropy[,1],"s.e."=Entropy[,2],"LCL"=Entropy[,3],"UCL"=Entropy[,4]))
  return(output)
}

plot_diversity_profile.inc_nose <- function(data){
  data <- filter(data,Target=="Diversity")
  if(colnames(data)[3]=="Estimate"){
    ggplot(data, aes(x=Order.q, y=Estimate, colour=Community))+
    geom_line(size=1.2) +    
    labs(x="Order q", y="Diversity")+
    theme(text=element_text(size=18))
  } else{
    ggplot(data, aes(x=Order.q, y=Emperical, colour=Community))+
      geom_line(size=1.2) +    
      labs(x="Order q", y="Diversity")+
      theme(text=element_text(size=18))
  }
  
}

plot_diversity_profile.inc <- function(data){
  data <- filter(data,Target=="Diversity")
  if(colnames(data)[3]=="Estimate"){
    ggplot(data, aes(x=Order.q, y=Estimate, colour=Community))+
    geom_line(size=1.2) +    
    labs(x="Order q", y="Diversity")+
    theme(text=element_text(size=18))+
    geom_ribbon(aes(ymin=LCL, ymax=UCL, fill=Community, colour=NULL), alpha=0.3)
  } else{
    ggplot(data, aes(x=Order.q, y=Emperical, colour=Community))+
    geom_line(size=1.2) +    
    labs(x="Order q", y="Diversity")+
    theme(text=element_text(size=18))+
    geom_ribbon(aes(ymin=LCL, ymax=UCL, fill=Community, colour=NULL), alpha=0.3)
  }
  
}

#=======Sample Completeness Curve=========#

sample_coverage = function(freq, q, datatype = c("abundance","incidence_freq")){
  
  if(datatype=="abundance"){
    freq = freq[freq>0]
    n = sum(freq)
    f1 = sum(freq==1)
    f2 = sum(freq==2)
    A = ifelse(f2>0,2*f2/((n-1)*f1+2*f2),ifelse(f1>0,2/((n-1)*(f1-1)+2),1))
    
    c_hat = function(q){
      if (q==0){
        
        S_obs = length(freq)
        f0_hat = if ( f2 == 0 ){( (n-1)/n ) * ( f1*(f1-1)/2 )} else {( (n-1)/n ) * ( (f1^2) / (2*f2) )}
        f0_hat_star = ceiling(f0_hat)
        c_hat = S_obs / (S_obs + f0_hat_star)
        return(c_hat)
        
      } else if (q==1){  
        
        c_hat = 1 - (f1/n)*(1-A)
        return(c_hat)
        
      } else if (q==2){
        
        x = freq[freq>=2]
        c_hat = 1 - (f1/n)*( (A*(1-A))/sum( x*(x-1) / (n*(n-1)) ) )
        return(c_hat)
        
      } else {
        
        r <- 0:(n-1)
        sort.data = sort(unique(freq))
        tab = table(freq)
        term = sapply(sort.data,function(z){
          k=0:(n-z)
          sum(choose(k-q,k)*exp(lchoose(n-k-1,z-1)-lchoose(n,z)))
        })
        lambda_hat =  sum(tab*term) + ifelse(f1==0|A==1,0,f1/n*(1-A)^(1-n)*(A^(q-1)-sum(choose(q-1,r)*(A-1)^r)))
        c_hat = 1 - ((f1/n)*(A^(q-1))*(1-A)/lambda_hat)
        return(c_hat)
        
      }
    }
  } else {
    
    t = freq[1]
    freq = freq[-1]; freq = freq[freq>0]
    u = sum(freq)
    Q1 = sum(freq==1)
    Q2 = sum(freq==2)
    B = ifelse(Q2>0,2*Q2/((t-1)*Q1+2*Q2),ifelse(Q1>0,2/((t-1)*(Q1-1)+2),1))
    
    c_hat = function(q){
      if (q==0){
        
        S_obs = length(freq)
        Chao2 = S_obs + ceiling(if ( Q2 == 0 ){( (t-1)/t ) * ( Q1*(Q1-1)/2 )} else {( (t-1)/t ) * ( (Q1^2) / (2*Q2) )})
        c_hat = S_obs / Chao2
        return(c_hat)
        
      } else if (q==1){  
        
        c_hat = 1 - (Q1/u)*(1-B)
        return(c_hat)
        
      } else if (q==2){
        
        x = freq[freq>=2]
        c_hat = 1 - (t-1)*Q1*( (B*(1-B))/sum( x*(x-1) ) )
        return(c_hat)
        
      } else {
        
        r <- 0:(t-1)
        sort.data = sort(unique(freq))
        tab = table(freq)
        term = sapply(sort.data,function(z){
          k=0:(t-z)
          sum(choose(k-q,k)*exp(lchoose(t-k-1,z-1)-lchoose(t,z)))
        })
        phi_hat = sum(tab*term) + ifelse(Q1==0|B==1,0,Q1/t*(1-B)^(1-t)*(B^(q-1)-sum(choose(q-1,r)*(B-1)^r)))
        c_hat = 1 - ((Q1/t)*(B^(q-1))*(1-B)/phi_hat)
        return(c_hat)
      }
    }
    
  }
  
  sapply(q,c_hat)
  
}

bootstrap_sample = function(freq, B, datatype = c("abundance","incidence_freq")){
  
  if(datatype=="abundance"){
    
    freq = freq[freq>0]
    n = sum(freq)
    f1 = sum(freq == 1)
    f2 = sum(freq == 2)
    
    S_obs = length(freq)
    f0_hat = if ( f2 == 0 ){( (n-1)/n ) * ( f1*(f1-1)/2 )} else {( (n-1)/n ) * ( (f1^2) / (2*f2) )}
    f0_hat_star = ceiling(f0_hat)
    S_hat_Chao1 = S_obs + f0_hat_star
    
    c_hat = if ( f2 != 0 ){ 1 - (f1/n)*((n-1)*f1/(((n-1)*f1)+2*f2))
      
    } else if (f1 != 0) { 1 - (f1/n)*((n-1)*(f1-1)/(((n-1)*(f1-1))+2*f2)) } else { 1 }
    
    lambda_hat = (1-c_hat) / sum((freq/n)*(1-freq/n)^n )
    p_i_hat_obs = (freq/n) * (1-lambda_hat* (1-freq/n)^n ) 
    p_i_hat_unobs = rep( (1-c_hat)/ f0_hat_star, f0_hat_star )
    
    bootstrap_population = c(p_i_hat_obs,p_i_hat_unobs)
    bootstrap_sample = rmultinom(n=B, size=n, prob=bootstrap_population)
    return(bootstrap_sample)
    
  } else {
    
    t = freq[1]
    freq = freq[-1]; freq = freq[freq>0]
    u = sum(freq)
    Q1 = sum(freq==1)
    Q2 = sum(freq==2)
    
    S_obs = length(freq)
    Q_0_hat = if ( Q2 == 0 ){( (t-1)/t ) * ( Q1*(Q1-1)/2 )} else {( (t-1)/t ) * ( (Q1^2) / (2*Q2) )}
    Q_0_hat_star = ceiling(Q_0_hat)
    
    c_hat = if ( Q2 > 0 ){ 1 - (Q1/u)*((t-1)*Q1/(((t-1)*Q1)+2*Q2))
      
    } else { 1 - (Q1/u)*((t-1)*(Q1-1)/(((t-1)*(Q1-1))+2)) }
    
    tau_hat = (u/t) * (1-c_hat) / sum((freq/t)*(1-freq/t)^t )
    pi_i_hat_obs = (freq/t) * (1-tau_hat* (1-freq/t)^t ) 
    pi_i_hat_unobs = rep( (u/t) * (1-c_hat)/ Q_0_hat_star, Q_0_hat_star )
    
    bootstrap_population = c(1,pi_i_hat_obs,pi_i_hat_unobs)
    bootstrap_sample = sapply(1:length(bootstrap_population), function(i) rbinom(n=B, size=t, prob=bootstrap_population[i]))
    bootstrap_sample = if(B==1) {as.matrix(bootstrap_sample)} else {t(bootstrap_sample)}
    return(bootstrap_sample)
    
  }
  
  
  
}

sc_profile.nose = function(freq, q, datatype = c("abundance","incidence_freq")) {
  
  data.frame(Order.q=q, Estimate=sample_coverage(freq, q, datatype))
  
}

sc_profile = function(freq, q, B, conf, datatype = c("abundance","incidence_freq")) {
  
  bootstrap_samples = bootstrap_sample(freq, B, datatype)
  sc_bs = sapply(1:B, function(i) sample_coverage(bootstrap_samples[,i], q, datatype))
  
  LCL = sample_coverage(freq, q, datatype) - qnorm(1-(1-conf)/2)*apply(sc_bs, 1, sd); LCL[LCL<0]=0
  UCL = sample_coverage(freq, q, datatype) + qnorm(1-(1-conf)/2)*apply(sc_bs, 1, sd); UCL[UCL>1]=1
  
  data.frame(Order.q=q, Estimate=sample_coverage(freq, q, datatype), s.e.=apply(sc_bs, 1,sd), LCL=LCL, UCL=UCL)
  
}

plot_sc_profile_nose <- function(data){

      ggplot(data, aes(x=Order.q, y=Estimate, colour=Community))+
      geom_line(size=1.2) +    
      labs(x="Order q", y="sample completeness")+
      theme(text=element_text(size=18), legend.position="bottom")
  
}

plot_sc_profile <- function(data){
  
    ggplot(data, aes(x=Order.q, y=Estimate, colour=Community))+
      geom_line(size=1.2) +    
      labs(x="Order q", y="sample completeness")+
      theme(text=element_text(size=18), legend.position="bottom")+
      geom_ribbon(aes(ymin=LCL, ymax=UCL, fill=Community, colour=NULL), alpha=0.3)
  
}