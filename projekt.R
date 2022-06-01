library(dplyr)
library(rootSolve)

dane=read.csv("dane_dochody.csv")
dane=filter(dane, (dane$doch_praca !=0 | dane$doch_gielda != 0 | dane$doch_dg !=0))


dane=mutate(dane, zost1=dane$doch_praca-10000)
dane=mutate(dane, pod_20=ifelse(dane$zost1>60000,60000*0.2,
                                ifelse(dane$zost1 %in% 0:60000,dane$zost1*0.2,0)))
dane=mutate(dane, zost2=dane$zost1-60000)
dane=mutate(dane, pod_30=ifelse(dane$zost2>0,dane$zost2*0.3,0))
dane=mutate(dane, pod_dg=ifelse(dane$doch_dg>0,dane$doch_dg*0.2,0))
dane=mutate(dane, pod_gielda=ifelse(dane$doch_gielda>0,dane$doch_gielda*0.2,0))
dane=mutate(dane, suma_pod=dane$pod_20+dane$pod_30+dane$pod_dg+dane$pod_gielda)
dane=mutate(dane, podst_opod=ifelse(dane$doch_praca>0,dane$doch_praca+
                                      (dane$pod_dg+dane$pod_gielda)*5,(dane$pod_dg+dane$pod_gielda)*5))
dane=mutate(dane, ulga=ifelse(dane$pod_20+dane$pod_30<=1000,dane$pod_20+dane$pod_30,1000))

dochody=sum(dane$suma_pod)
dochody

kosztulgi=sum(dane$ulga)

funkcja=function(x){
  y=(dane$podst_opod-x)*0.4
  wynik=ifelse(y>0,y,0)
  wynik2=sum(wynik)-kosztulgi
  return(wynik2)
}

poziom=multiroot(funkcja,c(0))
poziom=poziom$root
poziom=round(poziom,0)
poziom

dane=mutate(dane, doch_brutto = dane$doch_praca+dane$doch_dg+dane$doch_gielda)
dane=mutate(dane, danina=ifelse((dane$podst_opod-poziom)*0.4>0,(dane$podst_opod-poziom)*0.4,0))
dane=mutate(dane, doch_net_przed_reforma = dane$doch_brutto-dane$suma_pod)
dane=mutate(dane, doch_net_po_reformie = dane$doch_brutto-dane$suma_pod+dane$ulga-dane$danina)
dane=mutate(dane, zmiana_doch_rozp = (dane$doch_net_po_reformie-dane$doch_net_przed_reforma)/dane$doch_net_przed_reforma)

dane=filter(dane, dane$zmiana_doch_rozp<quantile(dane$zmiana_doch_rozp,0.98), dane$zmiana_doch_rozp>quantile(dane$zmiana_doch_rozp,0.02))

dane=filter(dane, dane$doch_brutto<quantile(dane$doch_brutto,0.98), dane$doch_brutto>quantile(dane$doch_brutto,0.02))

wsp.kor= cor(dane$doch_brutto,dane$zmiana_doch_rozp)
wsp.kor
plot(dane$doch_brutto,dane$zmiana_doch_rozp, col = ifelse(dane$doch_brutto>poziom,'brown','darkgreen'),
     xlab="Dochód brutto (PLN)", ylab= "Zmiana dochodu rozporządzalnego (proc.)",panel.first=grid(), yaxt="n")
axis(2, at=pretty(dane$zmiana_doch_rozp), lab=paste0(pretty(dane$zmiana_doch_rozp) * 100, "%"), las=TRUE)

