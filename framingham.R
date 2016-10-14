# Framingham risk functions

framingham=function(Sex=c("M","F"), Age, BMI, Smoke=c("Y","N"), DM=c("Y","N"),SBP, Meds=c("Y","N"), Totalchol, HDL , method=c("Office","Full"))
{
  #  John W Pickering 2016, October 2016. john.pickering @ otago.ac.nz (c) Creative Commons Attribution-Share Alike 4.0 International
  #  use at own risk (I checked against some data I had, but not guaranteed)
  #  See: https://www.framinghamheartstudy.org/risk-functions/cardiovascular-disease/10-year-risk.php
  #  Note:  cholesterol is in mg/dL.  To convert from mmol/L divide by 0.02586
  
  # Converstions
  age <- log(Age)
  bmi <- log(BMI)
  sbp <- log(SBP)  
  
  sex = ifelse(Sex=="M",1,0)
  smoke <- ifelse(Smoke=="Y",1,0)
  dm <- ifelse(DM=="Y",1,0)
  meds <- ifelse(Meds=="Y",1,0)
  nomeds <- ifelse(Meds=="N",1,0)
  
  sbp_meds <- sbp*meds
  sbp_nomeds<-sbp*nomeds
  
  df <- data.frame(sex,age,bmi,smoke,sbp_meds,sbp_nomeds,dm)
  n <- dim(df)[1]
  r<-matrix(nrow=n,ncol=1)
  
  # Method and calculation
  
  if(method=="Full")
  {
    df$totalchol <- log(Totalchol)
    df$hdl <- log(HDL)
    attach(df)
    for (i in 1: n)
    {
      ifelse(sex[i]==0,
             r[i] <- (1-0.95012^(exp(((2.32888*age[i])+(1.20904*totalchol[i])+(-0.70833*hdl[i])+(2.76157*sbp_nomeds[i])+(2.82263*sbp_meds[i])+(0.52873*smoke[i])+(0.69154*dm[i]))-26.1931)))*100,
             r[i] <- (1-0.88936^(exp(((3.06117*age[i])+(1.12370*totalchol[i])+(-0.93263*hdl[i])+(1.93303*sbp_nomeds[i])+(1.99881*sbp_meds[i])+(.65451*smoke[i])+(.57367*dm[i]))-23.9802)))*100 )
    }
  }  
  
  if(method=="Office")
  {
    attach(df)
    for (i in 1: n)
    {
      ifelse(sex[i]==0,
             r[i] <- (1-0.94833^(exp(((2.72107*age[i])+(0.51125*bmi[i])+(2.81291*sbp_nomeds[i])+(2.88267*sbp_meds[i])+(0.61868*smoke[i])+(.77763*dm[i]))-26.0145)))*100,
             r[i] <- (1-0.88431^(exp(((3.11296*age[i])+(.79277*bmi[i])+(1.85508*sbp_nomeds[i])+(1.92672*sbp_meds[i])+(.70953*smoke[i])+(.53160*dm[i]))-23.9388)))*100  )
    }
  }  
  return(r)
}