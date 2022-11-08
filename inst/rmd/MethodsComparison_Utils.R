library(nlme)
library(data.table)
library("ggplot2");
library("plotly");
library("GGally");
#library("rmarkdown");
library("knitr");

##### Input data

#####
## Estimation functions 

#*----------------------------------* Residual variance

ResidVar = function(ModCoeff, Corr, sigmaPredict){
  ResidVar = ModCoeff^2*sigmaPredict^2*(1 - Corr^2)/Corr^2;
  return(ResidVar)
}

#*----------------------------------* Hybrid

rSD_HY = function(Beta, Gamma, meanX_S, meanX_U, sigmaX_S, sigmaZ_Sa, CorrYX, CorrXZ, n, MM, N, PixelSize, mu_HY){
  omega2 = ResidVar(Beta, CorrYX, sigmaX_S);
  sigma2_X_U = Gamma^2*sigmaZ_Sa^2/CorrXZ^2;
  Var = (Beta^2 + omega2/(n*sigmaX_S^2)) * (1 - MM)*sigma2_X_U /(MM*(N*10000/PixelSize)) + (omega2/n)*(1 + (meanX_S - meanX_U)^2/sigmaX_S^2);
  rSD = 100*sqrt(Var)/mu_HY;
  return(rSD);
}

rRMSE_HY = function(Beta, Gamma, meanX_S, meanX_U, sigmaX_S, sigmaZ_Sa, CorrYX, CorrXZ, n, MM, N, PixelSize, AutoCorrSumX,  mu_HY){
  omega2 = ResidVar(Beta, CorrYX, sigmaX_S);
  sigma2_X_U = Gamma^2*sigmaZ_Sa^2/CorrXZ^2;
  Var = (Beta^2 + omega2/(n*sigmaX_S^2)) * (1 - MM)*sigma2_X_U /(MM*(N*10000/PixelSize)) + (omega2/n)*(1 + (meanX_S - meanX_U)^2/sigmaX_S^2);
  MSE = Var + omega2/(N*10000/PixelSize) + (omega2/(N*10000/PixelSize)^2)*AutoCorrSumX;
  rRMSE = 100*sqrt(MSE)/mu_HY;
  return(rRMSE);
}

rRMSE_HY_spat = function(Beta, Gamma, meanX_S, meanX_U, sigmaX_S, sigmaZ_Sa, CorrYX, CorrXZ, n, MM, N, PixelSize, AutoCorrX_seq, mu_HY){
  omega2 = ResidVar(Beta, CorrYX, sigmaX_S);
  sigma2_X_U = Gamma^2*sigmaZ_Sa^2/CorrXZ^2;
  Var = (Beta^2 + omega2/(n*sigmaX_S^2)) * (1 - MM)*sigma2_X_U /(MM*(N*10000/PixelSize)) + (omega2/n)*(1 + (meanX_S - meanX_U)^2/sigmaX_S^2);
  AutoCorrSumX_seq = AutoCorrSum((N*10000/PixelSize), AutoCorrX_seq);
  MSE = Var + omega2/(N*10000/PixelSize) + (omega2/(N*10000/PixelSize)^2)*AutoCorrSumX_seq;
  rRMSE = 100*sqrt(MSE)/mu_HY;
  return(rRMSE);
}

rRMSE_HY_spat2 = function(Beta, Gamma, meanX_S, meanX_U, sigmaX_S, sigmaZ_Sa, CorrYX, CorrXZ, n, MM, N_seq, PixelSize, AutoCorrX, mu_HY){
  omega2 = ResidVar(Beta, CorrYX, sigmaX_S);
  sigma2_X_U = Gamma^2*sigmaZ_Sa^2/CorrXZ^2;
  Var = (Beta^2 + omega2/(n*sigmaX_S^2)) * (1 - MM)*sigma2_X_U /(MM*(N_seq*10000/PixelSize)) + (omega2/n)*(1 + (meanX_S - meanX_U)^2/sigmaX_S^2);
  AutoCorrSumX_seq2 = AutoCorrSum2((N_seq*10000/PixelSize), AutoCorrX);
  MSE = Var + omega2/(N_seq*10000/PixelSize) + (omega2/(N_seq*10000/PixelSize)^2)*AutoCorrSumX_seq2;
  rRMSE = 100*sqrt(MSE)/mu_HY;
  return(rRMSE);
}
#*----------------------------------* Conventional Model-Based 

rSD_MB = function(Alpha, meanZ_S, meanZ_U, sigmaZ_S, CorrYZ, n, mu_MB){
  delta2 = ResidVar(Alpha, CorrYZ, sigmaZ_S);
  Var = (delta2/n)*(1 + (meanZ_S - meanZ_U)^2/sigmaZ_S^2);
  rSD = 100*sqrt(Var)/mu_MB;
  return(rSD);
}

rRMSE_MB = function(Alpha, meanZ_S, meanZ_U, sigmaZ_S, CorrYZ, n, N, PixelSize, AutoCorrSumZ, mu_MB){
  delta2 = ResidVar(Alpha, CorrYZ, sigmaZ_S);
  Var = (delta2/n)*(1 + (meanZ_S - meanZ_U)^2/sigmaZ_S^2);
  MSE = Var + delta2/(N*10000/PixelSize) + (delta2/(N*10000/PixelSize)^2)*AutoCorrSumZ;
  rRMSE = 100*sqrt(MSE)/mu_MB;
  return(rRMSE);
}

rRMSE_MB_spat = function(Alpha, meanZ_S, meanZ_U, sigmaZ_S, CorrYZ, n, N, PixelSize, AutoCorrZ_seq, mu_MB){
  delta2 = ResidVar(Alpha, CorrYZ, sigmaZ_S);
  Var = (delta2/n)*(1 + (meanZ_S - meanZ_U)^2/sigmaZ_S^2);
  AutoCorrSumZ_seq = AutoCorrSum((N*10000/PixelSize), AutoCorrZ_seq);
  MSE = Var + delta2/(N*10000/PixelSize) + (delta2/(N*10000/PixelSize)^2)*AutoCorrSumZ_seq;
  rRMSE = 100*sqrt(MSE)/mu_MB;
  return(rRMSE);
}

rRMSE_MB_spat2 = function(Alpha, meanZ_S, meanZ_U, sigmaZ_S, CorrYZ, n, N_seq, PixelSize, AutoCorrZ, mu_MB){
  delta2 = ResidVar(Alpha, CorrYZ, sigmaZ_S);
  Var = (delta2/n)*(1 + (meanZ_S - meanZ_U)^2/sigmaZ_S^2);
  AutoCorrSumZ_seq2 = AutoCorrSum2((N_seq*10000/PixelSize), AutoCorrZ);
  MSE = Var + delta2/(N_seq*10000/PixelSize) + (delta2/(N_seq*10000/PixelSize)^2)*AutoCorrSumZ_seq2;
  rRMSE = 100*sqrt(MSE)/mu_MB;
  return(rRMSE);
}

#*-----------------------------------* Hierarchical Model-Based 

rSD_HMB = function(Beta, Gamma, meanX_S, meanX_Sa, meanZ_Sa, meanZ_U, sigmaX_S, sigmaZ_Sa, CorrYX, CorrXZ, n, MM, EnLargeFactor, N, PixelSize, mu_HMB){
  omega2 = ResidVar(Beta, CorrYX, sigmaX_S);
  theta2 = ResidVar(Gamma, CorrXZ, sigmaZ_Sa);
  Ex_lambda2 = (Beta^2 + omega2/(n*sigmaX_S^2))*theta2;
  Var = Ex_lambda2/(EnLargeFactor*MM*(N*10000/PixelSize)) * (1 + (meanZ_Sa - meanZ_U)^2/sigmaZ_Sa^2) +
    (omega2/n) * (1 + (meanX_S - meanX_Sa + Gamma*(meanZ_U - meanZ_Sa))^2/sigmaX_S^2);
  rSD = 100*sqrt(Var)/mu_HMB;
  return(rSD);
}

rRMSE_HMB = function(Beta, Gamma, meanX_S, meanX_Sa, meanZ_Sa, meanZ_U, sigmaX_S, sigmaZ_Sa, CorrYX, CorrXZ, n, MM, EnLargeFactor, N, PixelSize, AutoCorrSumX, AutoCorrSumZ_Sa, mu_HMB){
  omega2 = ResidVar(Beta, CorrYX, sigmaX_S);
  theta2 = ResidVar(Gamma, CorrXZ, sigmaZ_Sa);
  Ex_lambda2 = (Beta^2 + omega2/(n*sigmaX_S^2))*theta2;
  lambda2 = (Beta^2)*theta2;
  Var = Ex_lambda2/(EnLargeFactor*MM*(N*10000/PixelSize)) * (1 + (meanZ_Sa - meanZ_U)^2/sigmaZ_Sa^2) +
    (omega2/n) * (1 + (meanX_S - meanX_Sa + Gamma*(meanZ_U - meanZ_Sa))^2/sigmaX_S^2);
  MSE = Var + lambda2/(N*10000/PixelSize) + (lambda2/(N*10000/PixelSize)^2)*AutoCorrSumZ_Sa + 
               omega2/(N*10000/PixelSize) + (omega2/(N*10000/PixelSize)^2)*AutoCorrSumX;
  rRMSE = 100*sqrt(MSE)/mu_HMB;  
  return(rRMSE);
}

rRMSE_HMB_spat = function(Beta, Gamma, meanX_S, meanX_Sa, meanZ_Sa, meanZ_U, sigmaX_S, sigmaZ_Sa, CorrYX, CorrXZ, n, MM, EnLargeFactor, N, PixelSize, AutoCorrX_seq, AutoCorrZa_seq, mu_HMB){
  omega2 = ResidVar(Beta, CorrYX, sigmaX_S);
  theta2 = ResidVar(Gamma, CorrXZ, sigmaZ_Sa);
  Ex_lambda2 = (Beta^2 + omega2/(n*sigmaX_S^2))*theta2;
  lambda2 = (Beta^2)*theta2;
  Var = Ex_lambda2/(EnLargeFactor*MM*(N*10000/PixelSize)) * (1 + (meanZ_Sa - meanZ_U)^2/sigmaZ_Sa^2) +
    (omega2/n) * (1 + (meanX_S - meanX_Sa + Gamma*(meanZ_U - meanZ_Sa))^2/sigmaX_S^2);
  
  AutoCorrSumZ_Sa_seq = AutoCorrSum((N*10000/PixelSize), AutoCorrZa_seq);
  AutoCorrSumX_seq    = AutoCorrSum((N*10000/PixelSize), AutoCorrX_seq);
  
  MSE = Var + lambda2/(N*10000/PixelSize) + (lambda2/(N*10000/PixelSize)^2)*AutoCorrSumZ_Sa_seq + 
               omega2/(N*10000/PixelSize) + (omega2/(N*10000/PixelSize)^2)*AutoCorrSumX_seq;
  rRMSE = 100*sqrt(MSE)/mu_HMB;  
  return(rRMSE);
}

rRMSE_HMB_spat2 = function(Beta, Gamma, meanX_S, meanX_Sa, meanZ_Sa, meanZ_U, sigmaX_S, sigmaZ_Sa, CorrYX, CorrXZ, n, MM, EnLargeFactor, N_seq, PixelSize, AutoCorrX, AutoCorrZ_Sa, mu_HMB){
  omega2 = ResidVar(Beta, CorrYX, sigmaX_S);
  theta2 = ResidVar(Gamma, CorrXZ, sigmaZ_Sa);
  Ex_lambda2 = (Beta^2 + omega2/(n*sigmaX_S^2))*theta2;
  lambda2 = (Beta^2)*theta2;
  Var = Ex_lambda2/(EnLargeFactor*MM*(N_seq*10000/PixelSize)) * (1 + (meanZ_Sa - meanZ_U)^2/sigmaZ_Sa^2) +
    (omega2/n) * (1 + (meanX_S - meanX_Sa + Gamma*(meanZ_U - meanZ_Sa))^2/sigmaX_S^2);
  
  AutoCorrSumX_seq2 = AutoCorrSum2((N_seq*10000/PixelSize), AutoCorrX);
  AutoCorrSumZ_Sa_seq2 = AutoCorrSum2((N_seq*10000/PixelSize), AutoCorrZ_Sa);
  
  MSE = Var + lambda2/(N_seq*10000/PixelSize) + (lambda2/(N_seq*10000/PixelSize)^2)*AutoCorrSumZ_Sa_seq2 + 
    omega2/(N_seq*10000/PixelSize) + (omega2/(N_seq*10000/PixelSize)^2)*AutoCorrSumX_seq2;
  rRMSE = 100*sqrt(MSE)/mu_HMB;  
  return(rRMSE);
}

### For the effect of disparity factor

#*----------------------------------* Hybrid 

rSD_HY_DF = function(Beta, Gamma, meanY_S, sigmaX_S, sigmaZ_Sa, CorrYX, CorrXZ, n, MM, N, PixelSize, DF_X){
  omega2 = ResidVar(Beta, CorrYX, sigmaX_S);
  sigma2_X_U = Gamma^2*sigmaZ_Sa^2/CorrXZ^2;
  Var = (Beta^2 + omega2/(n*sigmaX_S^2)) * (1 - MM)*sigma2_X_U /(MM*(N*10000/PixelSize)) + (omega2/n)*(1 + DF_X^2);
  rSD = 100*sqrt(Var)/(meanY_S + DF_X*abs(Beta)*sigmaX_S);
  return(rSD);
}

rRMSE_HY_DF = function(Beta, Gamma, meanY_S, sigmaX_S, sigmaZ_Sa, CorrYX, CorrXZ, n, MM, N, PixelSize, DF_X, AutoCorrSumX){
  omega2 = ResidVar(Beta, CorrYX, sigmaX_S);
  sigma2_X_U = Gamma^2*sigmaZ_Sa^2/CorrXZ^2;
  Var = (Beta^2 + omega2/(n*sigmaX_S^2)) * (1 - MM)*sigma2_X_U /(MM*(N*10000/PixelSize)) + (omega2/n)*(1 + DF_X^2);
  MSE = Var + omega2/(N*10000/PixelSize) + (omega2/(N*10000/PixelSize)^2)*AutoCorrSumX;
  rRMSE = 100*sqrt(MSE)/(meanY_S + DF_X*abs(Beta)*sigmaX_S);
  return(rRMSE);
}

#*----------------------------------*Conventional Model-Based

rSD_MB_DF = function(Alpha, meanY_S, sigmaZ_S, CorrYZ, n, DF_Z){
  delta2 = ResidVar(Alpha, CorrYZ, sigmaZ_S);
  Var = (delta2/n)*(1 + DF_Z^2);
  rSD = 100*sqrt(Var)/(meanY_S + DF_Z*abs(Alpha)*sigmaZ_S);
  return(rSD);
}

rRMSE_MB_DF = function(Alpha, meanY_S, sigmaZ_S, CorrYZ, n, N, PixelSize, DF_Z, AutoCorrSumZ){
  delta2 = ResidVar(Alpha, CorrYZ, sigmaZ_S);
  Var = (delta2/n)*(1 + DF_Z^2);
  MSE = Var + delta2/(N*10000/PixelSize) + (delta2/(N*10000/PixelSize)^2)*AutoCorrSumZ;
  rRMSE = 100*sqrt(MSE)/(meanY_S + DF_Z*abs(Alpha)*sigmaZ_S);
  return(rRMSE);
}

#*-----------------------------------* Hierarchical Model-Based 

rSD_HMB_DF = function(Beta, Gamma, meanX_S, meanX_Sa, meanZ_Sa, meanZ_U, sigmaX_S, sigmaZ_Sa, CorrYX, CorrXZ, n, MM, EnLargeFactor, N, PixelSize, DF_X, DF_Z){
  omega2 = ResidVar(Beta, CorrYX, sigmaX_S);
  theta2 = ResidVar(Gamma, CorrXZ, sigmaZ_Sa);
  Ex_lambda2 = (Beta^2 + omega2/(n*sigmaX_S^2))*theta2;
  Var = Ex_lambda2/(EnLargeFactor*MM*(N*10000/PixelSize)) * (1 + DF_Z^2) +
    (omega2/n) * (1 + (DF_X + abs(Gamma)*sigmaZ_Sa*DF_Z/sigmaX_S)^2);
  rSD = 100*sqrt(Var)/(meanY_S + (DF_X*sigmaX_S + DF_Z*abs(Gamma)*sigmaZ_Sa)*abs(Beta));
  return(rSD);
}

rRMSE_HMB_DF = function(Beta, Gamma, meanX_S, meanX_Sa, meanZ_Sa, meanZ_U, sigmaX_S, sigmaZ_Sa, CorrYX, CorrXZ, n, MM, EnLargeFactor, N, PixelSize, DF_X, DF_Z, AutoCorrSumX, AutoCorrSumZ_Sa){
  omega2 = ResidVar(Beta, CorrYX, sigmaX_S);
  theta2 = ResidVar(Gamma, CorrXZ, sigmaZ_Sa);
  Ex_lambda2 = (Beta^2 + omega2/(n*sigmaX_S^2))*theta2;
  lambda2 = (Beta^2)*theta2;
  Var = Ex_lambda2/(EnLargeFactor*MM*(N*10000/PixelSize)) * (1 + DF_Z^2) +
    (omega2/n) * (1 + (DF_X + abs(Gamma)*sigmaZ_Sa*DF_Z/sigmaX_S)^2);
  MSE = Var + lambda2/(N*10000/PixelSize) + (lambda2/(N*10000/PixelSize)^2)*AutoCorrSumZ_Sa + 
               omega2/(N*10000/PixelSize) + (omega2/(N*10000/PixelSize)^2)*AutoCorrSumX;
  rRMSE = 100*sqrt(MSE)/(meanY_S + (DF_X*sigmaX_S + DF_Z*abs(Gamma)*sigmaZ_Sa)*abs(Beta));  
  return(rRMSE);
}

#*---------------------------*


### For the comparison between hybrid and hierarchical model-based inferential modes 

rRMSE_HY_ratio = function(Beta, Gamma, meanY_S, sigmaX_S, sigmaZ_Sa, CorrYX, CorrXZ, n, MM, N, PixelSize, DF_X, AutoCorrSumX){
  omega2 = ResidVar(Beta, CorrYX, sigmaX_S);
  sigma2_X_U = Gamma^2*sigmaZ_Sa^2/CorrXZ^2;
  Var = (Beta^2 + omega2/(n*sigmaX_S^2)) * (1 - MM)*sigma2_X_U /(MM*(N*10000/PixelSize)) + (omega2/n)*(1 + DF_X^2);
  MSE = Var + omega2/(N*10000/PixelSize) + (omega2/(N*10000/PixelSize)^2)*AutoCorrSumX;
  Fcontr = (omega2/n)*(1 + DF_X^2) + omega2/(N*10000/PixelSize) + (omega2/(N*10000/PixelSize)^2)*AutoCorrSumX;
  ratio_HY = 100 * Fcontr/MSE;
  return(ratio_HY);
}

rRMSE_HMB_ratio = function(Beta, Gamma, meanX_S, meanX_Sa, meanZ_Sa, meanZ_U, sigmaX_S, sigmaZ_Sa, CorrYX, CorrXZ, n, MM, EnLargeFactor, N, PixelSize, DF_X, DF_Z, AutoCorrSumX, AutoCorrSumZ_Sa){
  omega2 = ResidVar(Beta, CorrYX, sigmaX_S);
  theta2 = ResidVar(Gamma, CorrXZ, sigmaZ_Sa);
  Ex_lambda2 = (Beta^2 + omega2/(n*sigmaX_S^2))*theta2;
  lambda2 = (Beta^2)*theta2;
  Var = Ex_lambda2/(EnLargeFactor*MM*(N*10000/PixelSize)) * (1 + DF_Z^2) +
    (omega2/n) * (1 + (DF_X + abs(Gamma)*sigmaZ_Sa*DF_Z/sigmaX_S)^2);
  MSE = Var + lambda2/(N*10000/PixelSize) + (lambda2/(N*10000/PixelSize)^2)*AutoCorrSumZ_Sa + 
    omega2/(N*10000/PixelSize) + (omega2/(N*10000/PixelSize)^2)*AutoCorrSumX;
  Fcontr = (omega2/n) * (1 + (DF_X + abs(Gamma)*sigmaZ_Sa*DF_Z/sigmaX_S)^2) + omega2/(N*10000/PixelSize) + (omega2/(N*10000/PixelSize)^2)*AutoCorrSumX;
  ratio_HMB = 100 * Fcontr/MSE;  
  return(ratio_HMB);
}
#*---------------------------*

