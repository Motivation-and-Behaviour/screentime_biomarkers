make_table1 <-function(scored_data){
  require(gtsummary)
  
  
  scored_data %>% select(age,sex,indig,ses,st_total,accmvpa,accsed,cardio_index,diet,sexualmaturity,vo2,waistcm,waist2height,bmiz,
                             bodyfat,bpsys,bpdia,fastingtime,glucose,trigly,cholesttotal,
                             cholesttotalhdl,cholestnonhdl,phospholipids,apolipa1,apolipb,ApoBA1_ratio,
                             glycoprotein) %>% 
    tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} / {N} ({p}%)"
    )
  )

  # Select the correct variables
  # Make wide by wave
  # Change the format of the continuous
  
}
