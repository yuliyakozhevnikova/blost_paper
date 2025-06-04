# the function for the brms (a regular BLOST) results

# libraries
library(gsDesign)

# BLOST Function
run_blost <- function(scenario, cohort_size, alpha, spending_function, 
                      num_simulations = 1000, num_tests = 5, 
                      sim_path = "/Users/julykozhevnikova/Desktop/rds_files/sim_1000",  #CHOOSE THE DIRECTORY 
                      scen_csv_path = "/Users/julykozhevnikova/Desktop/scen_new.csv") { #CHOOSE THE DIRECTORY 
  
  #  the working directory dynamically based on scenario and cohort size
  working_dir <- paste0(sim_path, "/brms_", scenario, "_c_", cohort_size, "/yka132") #CHOOSE THE DIRECTORY
  ScenNum= scenario#CHANGE
  sim=1
  cohortsize=cohort_size
  #ensure it exists
  if (!dir.exists(working_dir)) {
    stop("The working directory does not exist: ", working_dir)
  }
  
  # set the working directory
  setwd(working_dir)
  
  # a list to store simulation data
  data_list <- list()
  
  #simulation data
  for (sim in 1:num_simulations) {
    file_path <- paste0(scenario, "_", sim, "_dat.rds")
    if (!file.exists(file_path)) next
    data_list[[sim]] <- readRDS(file_path)
  }
  
  # all loaded datasets into one (if they are of the same structure)
  fulldat <- do.call(rbind, data_list)
  
  resultmat= matrix(ncol=5,nrow=0)
  for (sim in 1:1000){ #for (sim in 1:500){ 
    
    if (file_test("-f", paste0(ScenNum,"_",sim,".rds"))==TRUE){
      
      resvec = c()
      
      resultlist = readRDS(paste0(ScenNum,"_",sim,".rds"))
      
      res = resultlist[[1]]
      prob_sup1 = length(which(res$pdEls>res$pdSls))/1000
      
      res = resultlist[[2]]
      prob_sup2 =length(which(res$pdEls>res$pdSls))/1000
      
      
      res = resultlist[[3]]
      prob_sup3 =length(which(res$pdEls>res$pdSls))/1000
      
      res = resultlist[[4]]
      prob_sup4 =length(which(res$pdEls>res$pdSls))/1000
      
      res = resultlist[[5]]
      prob_sup5 =length(which(res$pdEls>res$pdSls))/1000
      
      resvec = c(resvec,c(prob_sup1,prob_sup2,prob_sup3,prob_sup4,prob_sup5))
      
      resultmat = rbind(resultmat, resvec)
    }
  }
  
  #select the appropriate alpha spending function (OF or Pocock)
  gs_result <- if (spending_function == "OF") {
    gsDesign(k = num_tests, timing = c(0.2,0.4,0.6,0.8,1)/1, test.type = 1, sfu = "OF", alpha = alpha, beta = 0.2)
  } else if (spending_function == "Pocock") {
    gsDesign(k = num_tests, timing = c(0.2,0.4,0.6,0.8,1)/1, test.type = 1, sfu = "Pocock", alpha = alpha, beta = 0.2)
  } else {
    stop("Invalid spending function. Choose either 'OF' or 'Pocock'.")
  }
  
  # the rejection boundaries
  popbound <- pnorm(gs_result$upper$bound)
  print(popbound)
  print(dim(resultmat))
  print(dim(popbound))
  print(nrow(resultmat))
  # Calculate whether null hypothesis was rejected at any interim analysis
  rej = sapply(1:nrow(resultmat),function(i){
    
    return(resultmat[i,1]>popbound[1] ||
             resultmat[i,2]>popbound[2] ||
             resultmat[i,3]>popbound[3] ||
             resultmat[i,4]>popbound[4] ||
             resultmat[i,5]>popbound[5] 
    )
  })
  
  #the percentage of rejections
  rejection_rate <- length(which(rej)) / nrow(resultmat)
  
  # return outputs: result matrix and rejection rate
  return(list(
    rejection_rate = rejection_rate,
    result_matrix = resultmat
  ))
}


output <- run_blost(
  scenario = 9,               # scenario number
  cohort_size = 30,           # cohort size
  alpha = 0.05,               # alpha level
  spending_function = "OF",   # alpha spending function (choose "OF" or "Pocock")
  num_simulations = 1000,     # number of simulations
  num_tests = 5,              # number of interim analyses
  sim_path = "/Users/julykozhevnikova/Desktop/rds_files/sim_1000",  #CHOOSE THE DIRECTORY
  scen_csv_path = "/Users/julykozhevnikova/Desktop/scen_new.csv"    #CHOOSE THE DIRECTORY
)
output


##########
# the function for the brms-bms (a BLOST-BMS) results
# libraries
library(gsDesign)

# BLOST Function
run_blost_bms <- function(scenario, cohort_size, alpha, spending_function, 
                          num_simulations = 1000, num_tests = 5, 
                          sim_path = "/Users/julykozhevnikova/Desktop/rds_files/sim_1000", #CHOOSE THE DIRECTORY
                          scen_csv_path = "/Users/julykozhevnikova/Desktop/scen_new.csv") { #CHOOSE THE DIRECTORY
  
  # the working directory dynamically based on scenario and cohort size
  working_dir <- paste0(sim_path, "/bms_", scenario, "_c_", cohort_size, "/yka132") #CHOOSE THE DIRECTORY
  ScenNum= scenario#CHANGE
  sim=1
  cohortsize=cohort_size
  # ensure it exists
  if (!dir.exists(working_dir)) {
    stop("The working directory does not exist: ", working_dir)
  }
  
  
  setwd(working_dir)
  
  data_list <- list()
  
  #load all relevant simulation data
  for (sim in 1:num_simulations) {
    file_path <- paste0(scenario, "_", sim, "_dat.rds")
    if (!file.exists(file_path)) next
    data_list[[sim]] <- readRDS(file_path)
  }
  
  # combine all loaded datasets into one (if they are of the same structure)
  fulldat <- do.call(rbind, data_list)
  
  resultmat= matrix(ncol=5,nrow=0)
  for (sim in 1:1000){ #for (sim in 1:500){ 
    
    if (file_test("-f", paste0(ScenNum,"_",sim,".rds"))==TRUE){
      
      resvec = c()
      
      resultlist = readRDS(paste0(ScenNum,"_",sim,".rds"))
      
      res = resultlist[[1]]
      prob_sup1 = length(which(res$pdEls>res$pdSls))/1000
      
      res = resultlist[[2]]
      prob_sup2 =length(which(res$pdEls>res$pdSls))/1000
      
      
      res = resultlist[[3]]
      prob_sup3 =length(which(res$pdEls>res$pdSls))/1000
      
      res = resultlist[[4]]
      prob_sup4 =length(which(res$pdEls>res$pdSls))/1000
      
      res = resultlist[[5]]
      prob_sup5 =length(which(res$pdEls>res$pdSls))/1000
      
      resvec = c(resvec,c(prob_sup1,prob_sup2,prob_sup3,prob_sup4,prob_sup5))
      
      resultmat = rbind(resultmat, resvec)
    }
  }
  
  # select the appropriate alpha spending function (OF or Pocock)
  gs_result <- if (spending_function == "OF") {
    gsDesign(k = num_tests, timing = c(0.2,0.4,0.6,0.8,1)/1, test.type = 1, sfu = "OF", alpha = alpha, beta = 0.2)
  } else if (spending_function == "Pocock") {
    gsDesign(k = num_tests, timing = c(0.2,0.4,0.6,0.8,1)/1, test.type = 1, sfu = "Pocock", alpha = alpha, beta = 0.2)
  } else {
    stop("Invalid spending function. Choose either 'OF' or 'Pocock'.")
  }
  
  # compute the rejection boundaries
  popbound <- pnorm(gs_result$upper$bound)
  print(popbound)
  print(dim(resultmat))
  print(dim(popbound))
  print(nrow(resultmat))
  # calculate whether null hypothesis was rejected at any interim analysis
  rej = sapply(1:nrow(resultmat),function(i){
    
    return(resultmat[i,1]>popbound[1] ||
             resultmat[i,2]>popbound[2] ||
             resultmat[i,3]>popbound[3] ||
             resultmat[i,4]>popbound[4] ||
             resultmat[i,5]>popbound[5] 
    )
  })
  
  # calculate the percentage of rejections
  rejection_rate <- length(which(rej)) / nrow(resultmat)
  
  # return outputs: result matrix and rejection rate
  return(list(
    rejection_rate = rejection_rate,
    result_matrix = resultmat
  ))
}


output_bms <- run_blost_bms(
  scenario = 4,               # scenario number
  cohort_size = 10,           # cohort size
  alpha = 0.05,               # alpha level
  spending_function = "OF",   # alpha spending function (choose "OF" or "Pocock")
  num_simulations = 1000,     # number of simulations
  num_tests = 5,              # number of interim analyses
  sim_path = "/Users/julykozhevnikova/Desktop/rds_files/sim_1000",   #CHOOSE THE DIRECTORY
  scen_csv_path = "/Users/julykozhevnikova/Desktop/scen_new.csv"     #CHOOSE THE DIRECTORY
)
output_bms

# freq function

run_freq <- function(scenario, cohort_size, alpha, spending_function, num_tests) {
  scencsvpath = "/Users/julykozhevnikova/Desktop/scen_new.csv"    #CHOOSE THE DIRECTORY
  SCENNO=scenario
  ScenNum = SCENNO
  numsim = 1000 
  cohortsize= cohort_size
  
  T=10
  raneffsd = 0.1
  bE = c(0,0,1,1) #weighting factor for ordinal probability
  cohorttime = 1
  sd = 1
  numordinalE = 4 #efficacy outcome is ordinal with 4 levels
  wtime=c(rep(0,9),rep(1,1)) #weighting factor for time
  NULLCASE=FALSE
  timetransform<-log
  
  getpdest_naive<-function(realtime, D){ #D is needed to distinguish b/w E and S groups, D=1 experimental, D=0 control
    meanall=0
    varall = 0
    
    for(oo in 10:10){
      subdat = fulldat[which(fulldat$realtime <= realtime),]
      subdat = subdat[which(subdat$relativetime == oo & subdat$w ==1 & subdat$ident_num ==D),] 
      xx= length(which(subdat[which(subdat$relativetime == oo),'effoutcome']%in% c(3,4))) 
      nn=length(which(subdat[which(subdat$relativetime == oo),'effoutcome']%in% c(1,2,3,4)))
      
      pp=xx/nn
      qq=1-pp
      
      meanall = meanall + pp
      varall = varall + pp*qq/nn
    }
    
    return(list(mean=meanall,var=varall))
  }
  
  timetransformE_true <<- log
  timetransformS_true <<- log
  
  determineordinal<-function(value, thresvector){
    return(length(which(thresvector < value))+1)
  }
  
  simulatecohort<-function(vec_num_subj, beta_vec,gamma_vec, thresvec_beta,thresvec_gamma, sd, raneffsd,cohorttime,timetransform=log){
    ### vec_num_subj, vector of subjects for the S and E arms ##further, vec_num_subj=c(1,1) * cohortsize
    ### beta_vec (one value actually), coef for the E's efficacy: Y = beta_0 * log(t) + u_i + eps_y
    ### gamma_vec (one value actually), coefficient for the S's efficacy: W = gamma_0 * log(t) + u_i + eps_w
    ### thresvec_beta, thresholds for the E's efficacy's ordinal outcome
    ### thresvec_gamma, thresholds for the S's efficacy's ordinal outcome
    ### sd, random error in the ordinal model, we assume all the same for E,ScenNum
    ### raneffsd, sd of random effect in the E's ordinal model
    
    numS = vec_num_subj[1] #number of subjects for the standard arm
    numE = vec_num_subj[2] #number of subjects for the experimental arm
    
    raneffS = rnorm(numS,0,raneffsd) #random effect of subjects in the standard arm
    raneffE = rnorm(numE,0,raneffsd) #random effect of subjects in the experimental arm
    
    subjectIDE = cohorttime * 10000 + seq(1,length(raneffE)) #identificators for subjects
    subjectIDS = cohorttime * 100000 + seq(1,length(raneffS))
    
    #xxE - simulates efficacy data in the experimental setting   
    xxE= lapply(1:length(raneffE),function(i){
      raneff = raneffE[i] #random effect from above is assigned to the current object
      d = rep(0,T) 
      d2 = rep(0,T)
      t = 1:T
      m = as.matrix(cbind(sapply(t,timetransform), d, d2 )) %*% (as.matrix(beta_vec)) 
      #m - efficacy model, matrices multiplication; each row represents a time point for a specific subject
      latent = sapply(1:nrow(m),function(i){rnorm(1,m[i,1],sd = sd)}) + raneff #Y^(~i)
      observed = sapply(latent,function(i){determineordinal(i,thresvec_beta)})
      #the res of "observed" - is each entry is the ordinal category corresponding to each time point's latent efficacy score
      realt = cohorttime + t - 1
      wgt = rep(1,T) #weight
      subjid = subjectIDE[i]
      ident_num = 1
      o = data.frame(subjectID = rep(subjid,T), ident_num = ident_num, realtime = realt, relativetime = t, raneff = rep(raneff,T), effoutcome = observed, latent=latent,cohorttime = rep(cohorttime,T),w=wgt)
      return(o) 
      #return(o) involves individual df's per each subject with T rows each
    })%>%Reduce(rbind,.) # united all the df's into one df
    
    #check and ensuring all possible ordinal outcomes are presented in xxE (E setting) for efficacy
    if (length(unique(xxE$effoutcome))<numordinalE){
      toadd = setdiff(seq(1,numordinalE),unique(xxE$effoutcome)) #identifying missing categories, store them in "toadd"
      for(i in 1:length(toadd)){
        tempE = xxE[1:T,] #copies the first T rows of xxE
        tempE$relativetime=10
        tempE$w = 0.0001 #indicate a very low weight for them (since they're artificially added)
        tempE$effoutcome = toadd[i] #assigns one of the missing categories to the efficacy outcome
        xxE = rbind(xxE,tempE) #appends these rows to the original dataset
      }
    }
    
    #same for the standard arm
    xxS= lapply(1:length(raneffS),function(i){
      raneff = raneffS[i]
      d = rep(0,T)
      d2 = rep(0,T)
      t = 1:T
      m = as.matrix(cbind(sapply(t,timetransform)) ) %*% (as.matrix(gamma_vec))
      latent = sapply(1:nrow(m),function(i){rnorm(1,m[i,1],sd = sd)}) + raneff
      observed = sapply(latent,function(i){determineordinal(i,thresvec_gamma)})
      realt = cohorttime + t - 1
      subjid = subjectIDS[i]
      ident_num = 0
      wgt = rep(1,T)
      o = data.frame(subjectID = rep(subjid,T), ident_num= ident_num, realtime = realt, relativetime = t, raneff = rep(raneff,T), effoutcome = observed, latent=latent,cohorttime = rep(cohorttime,T),w=wgt)
      return(o)
    })%>%Reduce(rbind,.)
    
    if (length(unique(xxS$effoutcome))<numordinalE){
      toadd = setdiff(seq(1,numordinalE),unique(xxS$effoutcome))
      for(i in 1:length(toadd)){
        tempE = xxS[1:T,]
        tempE$w = 0.0001
        tempE$effoutcome = toadd[i]
        xxS = rbind(xxS,tempE)
      }
    }
    
    return(rbind(xxE,xxS))
  }
  
  
  #efficacy probability in the Experimental setting 
  #пE used to compute a summary measure
  geteffprobE<-function(time,thresholdvec,btime,raneffsd,timetransform){
    
    sdx=1+raneffsd
    m = btime * timetransform(time) #Yi (t) = beta_0 * log(t) + u_i + eps_y
    thres = c(-Inf,thresholdvec,Inf)
    o=sapply(2:length(thres),function(i){pnorm(thres[i],mean = m,sd = sdx) - pnorm(thres[i-1],mean = m,sd = sdx)})
    #calculates the probability that the efficacy score is between thres[i-1] and thres[i] through CDF
    return(o)
  }
  
  #same for the standard arm
  geteffprobS<-function(time,thresholdvec,btime,raneffsd,timetransform){
    
    sdx=1+raneffsd
    m = btime * timetransform(time)
    thres = c(-Inf,thresholdvec,Inf)
    o=sapply(2:length(thres),function(i){pnorm(thres[i],mean = m,sd = sdx) - pnorm(thres[i-1],mean = m,sd = sdx)})
    return(o)
    
  }
  
  computepE_true<-function(thresvec_beta,beta_vec,raneffsd,bE,wtime,timetransform=log){
    thres_est = thresvec_beta
    btime_est = beta_vec[1]
    raneffsd_est = raneffsd
    
    pd=sapply(1:1,function(i){
      probs = sapply(1:10,function(t)(sum(bE * geteffprobE(t,as.numeric(thres_est),btime_est,raneffsd_est,timetransform))))
      sum(wtime*probs)/sum(wtime)
    })
    return(pd)
  }
  
  computepS_true<-function(thresvec_gamma,gamma_vec,raneffsd,bE,wtime,timetransform=log){
    thres_est = thresvec_gamma
    btime_est = gamma_vec	
    raneffsd_est = raneffsd
    
    pd=sapply(1:1,function(i){
      probs = sapply(1:10,function(t)(sum(bE * geteffprobS(t,as.numeric(thres_est),btime_est,raneffsd_est,timetransform))))
      sum(wtime*probs)/sum(wtime)
    })
    return(pd)
  }
  
  scencsv = read.csv(scencsvpath)
  
  tempf<-function(x){
    gamma_vec = c(x)
    thresvec_gamma = c(2,3.5,4)
    computepS_true(thresvec_gamma,gamma_vec,raneffsd,bE,wtime)	-targetpdS
  }
  scencsv = read.csv(scencsvpath)
  
  targetpdS = scencsv$ps[ScenNum]
  gamma_vec = uniroot(tempf,c(0.1,2))$root
  thresvec_gamma = c(2,3.5,4)
  
  
  scencsv = read.csv(scencsvpath)
  PS = scencsv$ps[ScenNum]
  
  eff_solve<-function(pds,PS){
    
    tempf<-function(x){
      gamma_vec = c(x)
      thresvec_gamma = c(2,3.5,4)
      computepS_true(thresvec_gamma,gamma_vec,raneffsd,bE,wtime)	-PS
    }
    
    gamma_vec = uniroot(tempf,c(0.1,2))$root
    
    tempf2<-function(x){
      beta_vec = c(gamma_vec,x,0)
      thresvec_beta = c(2,3.5,4) + beta_vec[2]
      ll = computepE_true(thresvec_beta,beta_vec,raneffsd,bE,wtime) 
      ll - targetdiff
    }
    
    targetdiff =  pds
    beta_vecdose = uniroot(tempf2,c(0.05,10),extendInt = "yes")$root
    beta_vec = c(gamma_vec,beta_vecdose,0)
    thresvec_beta = c(2,3.5,4) + beta_vec[2]
    pdtrue = computepE_true(thresvec_beta,beta_vec,raneffsd,bE,wtime)
    return(list(beta_vec=beta_vec,thresvec_beta=thresvec_beta,pdtrue=pdtrue))
  }
  
  pds = scencsv[SCENNO,1] #col with pE, the first row at first
  eff_solu = eff_solve(pds,scencsv[SCENNO,2]) #scencsv[SCENNO,2] - pS value
  beta_vec = eff_solu$beta_vec
  thresvec_beta = eff_solu$thresvec_beta
  pdtrue = eff_solu$pdtrue
  print(pds) #the target value extracted from a file
  print(eff_solu$pdtrue) #the simulated value resulted from the calibration process
  
  if(NULLCASE){
    beta_vec = c(gamma_vec,0,0)
    thresvec_beta = c(2,3.5,4) 
  }
  
  resultmat= matrix(ncol=5,nrow=0)
  #for(sim in 1:500){
  for(sim in 1:numsim){
    #print(sim)
    set.seed(sim)
    vec_num_subj=c(1,1) * cohortsize
    tt=1
    cohorttime=1
    fulldat = simulatecohort(vec_num_subj, beta_vec,  gamma_vec, thresvec_beta,thresvec_gamma, sd, raneffsd,cohorttime)
    resultlist=list()
    for (tt in 1:3*T){ 
      cohorttime=tt
      if (tt == 1){
        fulldat = simulatecohort(vec_num_subj, beta_vec, gamma_vec, thresvec_beta,thresvec_gamma, sd, raneffsd,cohorttime)
      }else{
        tempdat = simulatecohort(vec_num_subj, beta_vec, gamma_vec, thresvec_beta,thresvec_gamma, sd, raneffsd,cohorttime)
        fulldat = rbind(fulldat,tempdat)
      }
    }
    subdat = fulldat[which(fulldat$realtime <= T),]
    subdat$relativetime = timetransform(subdat$relativetime)
    realtime=T
    ee= getpdest_naive(realtime,1)
    ss= getpdest_naive(realtime,0)
    prob_sup1 =  pnorm((ee$mean-ss$mean)/(sqrt(ee$var+ss$var)))
    
    subdat = fulldat[which(fulldat$realtime <= 1.5*T),]
    subdat$relativetime = timetransform(subdat$relativetime)
    realtime=1.5*T
    ee= getpdest_naive(realtime,1)
    ss= getpdest_naive(realtime,0)
    prob_sup2 =  pnorm((ee$mean-ss$mean)/(sqrt(ee$var+ss$var)))
    
    subdat = fulldat[which(fulldat$realtime <= 2*T),]
    subdat$relativetime = timetransform(subdat$relativetime)
    realtime=2*T
    ee= getpdest_naive(realtime,1)
    ss= getpdest_naive(realtime,0)
    prob_sup3 =  pnorm((ee$mean-ss$mean)/(sqrt(ee$var+ss$var)))
    
    subdat = fulldat[which(fulldat$realtime <= 2.5*T),]
    subdat$relativetime = timetransform(subdat$relativetime)
    realtime=2.5*T
    ee= getpdest_naive(realtime,1)
    ss= getpdest_naive(realtime,0)
    prob_sup4 =  pnorm((ee$mean-ss$mean)/(sqrt(ee$var+ss$var)))
    
    subdat = fulldat[which(fulldat$realtime <= 3*T),]
    subdat$relativetime = timetransform(subdat$relativetime)
    realtime=3*T
    ee= getpdest_naive(realtime,1)
    ss= getpdest_naive(realtime,0)
    prob_sup5 =  pnorm((ee$mean-ss$mean)/(sqrt(ee$var+ss$var)))
    
    resvec = c(prob_sup1,prob_sup2,prob_sup3,prob_sup4,prob_sup5)
    resultmat = rbind(resultmat, resvec)
  }
  
  
  Numtest=5 #the numbber of looks or interim analyses
  library(gsDesign)
  
  # select the appropriate alpha spending function (OF or Pocock)
  gs_result <- if (spending_function == "OF") {
    gsDesign(k = num_tests, timing = c(0.2,0.4,0.6,0.8,1)/1, test.type = 1, sfu = "OF", alpha = alpha, beta = 0.2)
  } else if (spending_function == "Pocock") {
    gsDesign(k = num_tests, timing = c(0.2,0.4,0.6,0.8,1)/1, test.type = 1, sfu = "Pocock", alpha = alpha, beta = 0.2)
  } else {
    stop("Invalid spending function. Choose either 'OF' or 'Pocock'.")
  }
  
  # compute the rejection boundaries
  popbound <- pnorm(gs_result$upper$bound)
  
  rej = sapply(1:nrow(resultmat),function(i){
    
    return(resultmat[i,1]>popbound[1] ||
             resultmat[i,2]>popbound[2] ||
             resultmat[i,3]>popbound[3] ||
             resultmat[i,4]>popbound[4] ||
             resultmat[i,5]>popbound[5] 
    )
  })
  # calculate the percentage of rejections
  rejection_rate <- length(which(rej)) / nrow(resultmat)
  
  # return outputs: result matrix and rejection rate
  return(list(
    rejection_rate = rejection_rate,
    result_matrix = resultmat
  ))
}

output_freq <- run_freq(
  scenario = 9,               # scenario number
  cohort_size = 30,           # cohort size
  alpha = 0.05,               # alpha 
  spending_function = "OF",   # alpha spending function (choose "OF" or "Pocock")
  num_tests = 5
)

#output_freq
#simulatecohort
#resultmat
#fulldat


######
#CREATING THE FINAL TABLES FROM THE PAPER
# let define the scenarios (pE, pS, Scenario)
scenarios <- data.frame(
  pE = c(0.6, 0.5, 0.4,  0.8, 0.7, 0.6, 0.5, 0.2, 0.4, 0.3),
  pS = c(0.2, 0.2, 0.2,  0.4, 0.4, 0.4, 0.4, 0.2, 0.4, 0.3),
  Scenario = c(1, 2, 3, 6, 7, 8, 9, 12, 13, 14) 
)

# parameters
alpha_levels <- c(0.1, 0.05)  # Alpha levels to test
cohort_size <- 30  # Cohort size
num_tests <- 5  # Number of interim analyses
T <- 10  # Total time for simulation
spending_function <- "OF"  # O'Brien-Fleming alpha spending function or Pocock

# initialize result storage
results <- data.frame()

#run simulations for each scenario
for (i in 1:nrow(scenarios)) {
  pE <- scenarios$pE[i]
  pS <- scenarios$pS[i]
  scenario_num <- scenarios$Scenario[i]
  
  for (alpha in alpha_levels) {
    # BLOST
    blost_output <- run_blost(
      scenario = scenario_num,
      cohort_size = cohort_size,
      alpha = alpha,
      spending_function = spending_function,
      num_tests = num_tests
    )
    
    # BLOST-BMS
    blost_bms_output <- run_blost_bms(
      scenario = scenario_num,
      cohort_size = cohort_size,
      alpha = alpha,
      spending_function = spending_function,
      num_tests = num_tests
    )
    
    # Frequentist
    freq_output <- run_freq(
      scenario = scenario_num,
      cohort_size = cohort_size,
      alpha = alpha,
      spending_function = spending_function,
      num_tests = num_tests
    )
    
    # collecting results
    results <- rbind(results, data.frame(
      pE = pE,
      pS = pS,
      Scenario = scenario_num,
      Alpha = alpha,
      BLOST = sprintf("%.1f%%", blost_output$rejection_rate * 100),
      BLOST_BMS = sprintf("%.1f%%", blost_bms_output$rejection_rate * 100),
      Frequentist = sprintf("%.1f%%", freq_output$rejection_rate * 100)
    ))
  }
}

# Reshape results for final table format
library(tidyr)
final_table <- pivot_wider(
  results,
  names_from = Alpha,
  values_from = c(BLOST, BLOST_BMS, Frequentist),
  names_glue = "{.value}_alpha{Alpha}"
)

#to CSV
#write.csv(final_table, "simulation_results.csv", row.names = FALSE)
View(final_table)



######
#SAMPLE SIZE PLOTS
#A FUNCTION TO COMPUTE SAMPLE SIZE
calculate_sample_size <- function(resultmat, popbound, cohort_size, T = 10, spending_function) {
  # Function to calculate sample size for all rows in resultmat
  numsamplesize_all <- sapply(1:nrow(resultmat), function(i) {
    # Select the appropriate alpha spending function (OF or Pocock)
    gs_result <- if (spending_function == "OF") {
      gsDesign(k = num_tests, timing = c(0.2,0.4,0.6,0.8,1)/1, test.type = 1, sfu = "OF", alpha = alpha, beta = 0.2)
    } else if (spending_function == "Pocock") {
      gsDesign(k = num_tests, timing = c(0.2,0.4,0.6,0.8,1)/1, test.type = 1, sfu = "Pocock", alpha = alpha, beta = 0.2)
    } else {
      stop("Invalid spending function. Choose either 'OF' or 'Pocock'.")
    }
    
    # compute the rejection boundaries
    popbound <- pnorm(gs_result$upper$bound)
    # determine the first boundary exceeded
    rej <- c(
      resultmat[i, 1] > popbound[1],
      resultmat[i, 2] > popbound[2],
      resultmat[i, 3] > popbound[3],
      resultmat[i, 4] > popbound[4],
      resultmat[i, 5] > popbound[5]
    ) * 1
    
    for (ii in 1:length(rej)) {
      if (is.na(rej[ii])) {
        next  # skip this iteration if `rej[ii]` is `NA`
      }
      if (rej[ii] == 1) {
        break  # exit the loop if the condition is met
      }
    }
    numgroup = ii
    
    # compute the sample size based on numgroup
    sample_size <- 2 * T * cohort_size +  # Initial stage
      2 * cohort_size * 5 * ifelse(numgroup >= 2, 1, 0) +  
      2 * cohort_size * 5 * ifelse(numgroup >= 3, 1, 0) +
      2 * cohort_size * 5 * ifelse(numgroup >= 4, 1, 0) +
      2 * cohort_size * 5 * ifelse(numgroup >= 5, 1, 0)
    
    return(sample_size)
  })
  
  return(numsamplesize_all)
}

#COMPUTING FOR EACH METHOD
# initialize result storage for sample sizes
sample_sizes <- data.frame()

#  sample size calculation for each scenario
for (i in 1:nrow(scenarios)) {
  scenario_num <- scenarios$Scenario[i]
  cohort_size <- 10  # OR change as needed
  T <- 10
  spending_function <- "OF" # OR change as needed
  alpha <- 0.05
  
  # run BLOST
  blost_output <- run_blost(
    scenario = scenario_num,
    cohort_size = cohort_size,
    alpha = alpha,
    spending_function = spending_function,
    num_tests = 5
  )
  blost_sample_size <- calculate_sample_size(blost_output$result_matrix, popbound, cohort_size, T=10, spending_function)
  
  # run BLOST-BMS
  blost_bms_output <- run_blost_bms(
    scenario = scenario_num,
    cohort_size = cohort_size,
    alpha = alpha,
    spending_function = spending_function,
    num_tests = 5
  )
  blost_bms_sample_size <- calculate_sample_size(blost_bms_output$result_matrix, popbound, cohort_size, T=10, spending_function)
  
  # run Frequentist
  freq_output <- run_freq(
    scenario = scenario_num,
    cohort_size = cohort_size,
    alpha = alpha,
    spending_function = spending_function,
    num_tests = 5
  )
  freq_sample_size <- calculate_sample_size(freq_output$result_matrix, popbound, cohort_size, T=10, spending_function)
  
  # store results
  sample_sizes <- rbind(sample_sizes, data.frame(
    Scenario = scenario_num,
    BLOST = mean(blost_sample_size),  # mean sample size across simulations
    BLOST_BMS = mean(blost_bms_sample_size),
    Frequentist = mean(freq_sample_size)
  ))
}
sample_sizes$Scenario <- 1:nrow(sample_sizes)
# results
print(sample_sizes)

library(tidyr)

# reshape the data to long format
sample_sizes_long <- sample_sizes %>%
  pivot_longer(
    cols = c(BLOST, BLOST_BMS, Frequentist),
    names_to = "Method",
    values_to = "SampleSize"
  )

ggplot(sample_sizes_long, aes(x = factor(Scenario), y = SampleSize, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge2(padding = 0.3), width = 0.7) +
  labs(
    x = "Scenarios",
    y = "Sample Size",
    fill = "Method",
    title = "Average sample size under the BLOST design, BLOST-BMS design, and Frequentist design based on the O’Brien-Fleming type spending function with a cohort size of 10"
  ) +
  scale_x_discrete(expand = expansion(add = c(0.5, 0.5))) 

#save.image("/Users/julykozhevnikova/Desktop/figure_of_10.Rdata") 

trial_duration <- sample_sizes
trial_duration[, -1] <- trial_duration[, -1] / 20 #change 20 or 60
trial_duration_long <- trial_duration %>%
  pivot_longer(
    cols = c(BLOST, BLOST_BMS, Frequentist),
    names_to = "Method",
    values_to = "SampleSize"
  )

ggplot(trial_duration_long, aes(x = factor(Scenario), y = SampleSize, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge2(padding = 0.3), width = 0.7) +
  labs(
    x = "Scenarios",
    y = "Sample Size",
    fill = "Method",
    title = "Average trial duration under the BLOST design, BLOST-BMS design, and Frequentist design based on the O’Brien-Fleming type spending function with a cohort size of 10"
  ) +
  scale_x_discrete(expand = expansion(add = c(0.5, 0.5))) 

#save.image("/Users/julykozhevnikova/Desktop/figure_ss_of_10.Rdata")



