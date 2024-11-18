load(file = "~/jointcenteR/data/acs22.Rda")

tenTable <- acs22 |> 
  tab(ten, wgtp)

tenTable_unwgt <- acs22 |> 
  tab(ten)

tenDisTable <- acs22 |> 
  tab2(ten, dis, wgtp) 

tenDisTable_unwgt <- acs22 |> 
  tab2(ten, dis) 

regionTenDisTable <- acs22 |> 
  tab3(region, ten, dis, wgtp)

regionTenDisTable_unwgt <- acs22 |> 
  tab3(region, ten, dis)

medInc <- acs22 |> 
  wgtd_med(hincp, wgtp)

medInc_unwgt <- acs22 |> 
  wgtd_med(hincp)

medIncTen <- acs22 |> 
  wgtd_med2(ten, hincp, wgtp)

medIncTen_unwgt <- acs22 |> 
  wgtd_med2(ten, hincp)
