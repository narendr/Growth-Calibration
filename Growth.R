#####Code for the paper - "Secular Fertility Decline and Rising Educational Achievements"####
setwd("Users/narendra/Documents/Growth")
source("seqm.R")
source("seqa.R")

#Initiating parameter values 
alpha= 0.48
alpha=0.275
lambda= 1.95
phi=.55
theta=1/8
aa=3.5
rho= .39
eps= .5
z=1
top = 7.25
top=5.15
mmm=.25

rental= .22
rental=.23823957

mu=.8
mu=.35
mu=.049
mu=.04

aa=3.5
aa=2.5
aa=1.5
aa=1.55

phi=0.55
lambda=1-phi

psi = 0.67
psi =0.66

beta = .058
beta = 0.18
beta = 0.72


#this section determines parameter values in order to produce #xss=1, tauss=.4
#-----------------------------------------------------------------
tauss = 15.25/40
babyboom = 0.0001
bb = 0.01
sigmabb= 0.5

a	=(tauss+theta- (lambda-phi)/lambda)*lambda/phi
a	=1-(1-phi)*(1-theta-tauss)/(phi*(1-psi*(1-theta-tauss)))

#-----------------------------------------------------------------
#This is the time frame we have
time.g	=151+40+10+10



#in this section I interpolate the mortality rates, infant #mortality and young adult mortality #between decade #observations.
avgyrscen	=rep(1,time.g)
ybddt		=rep(0, time.g)
yrsyng		=ybddt
avgyrscen[1]	=.8
avgyrscen[11]	=1.14
avgyrscen[21]	=1.39466
avgyrscen[31]	=1.75724
avgyrscen[41]	=2.11909
avgyrscen[51]	=2.54839
avgyrscen[61]	=3.12597 
avgyrscen[71]	=3.68313
avgyrscen[81]	=4.29430
avgyrscen[91]	=4.70799
avgyrscen[101]	=5.01069
avgyrscen[111]	=5.429808
avgyrscen[121]	=6.231236 
avgyrscen[131]	=7.182373 
avgyrscen[141]	=8.367956 
avgyrscen[151]	=9.323303 
avgyrscen[161]	=10.45722
avgyrscen[171]	=10.92471
avgyrscen[181]	=12.00664 
avgyrscen[191]	=12.75106
avgyrscen[201]	=13.34753 
avgyrscen[207]	=13.51486


ttt=seq(1800,2010,by=10)-1800+1
avgyrscen[ttt]= c( 4.7254301, 4.7972045,4.8332352, 4.8455465, 4.8549275, 4.7541878, 5.3162118, 6.0109716, 6.9203863,7.5397313,7.9128752,8.1569193,8.4908204,8.9161787,9.4596848,9.9184973,10.359731,11.122081, 11.958886,12.682659,13.11261,13.749803)

avgyrscendata=avgyrscen
yrsyng[1:40]=2.062716/seqm(1.0246^40,1/1.0246,40)

yrsyng[41:50]=c(2.293345,2.375107,2.462705,2.556652,2.650258,2.740834,2.8293,2.912426,2.999845,3.094354)
yrsyng[51:60]=c(3.16615,3.261022,3.36899,3.492126,3.635024,3.792304,3.903104,3.966645,3.97858,3.938965)
yrsyng[61:70]=c(3.896726,3.887982,3.890068,3.906586,3.957826,4.025879,4.100533,4.177644,4.252215,4.323626)
yrsyng[71:80]=c(4.407205,4.48664,4.555547,4.627087,4.680271,4.744117,4.804007,4.862964,4.914862,4.964221)
yrsyng[81:90]=c(5.011275,5.071714,5.137857,5.208179,5.305478,5.398335,5.491214,5.578674,5.641364,5.707055)
yrsyng[91:100]=c(5.7569685,.805818,5.860713,5.910832,5.968033,6.021489,6.09936,6.176828,6.25072,6.339121)
yrsyng[101:110]=c(6.415377,6.521668,6.630264,6.734369,6.855792,6.987334,7.127425,7.282687,7.400363,7.478918)
yrsyng[111:120]=c(7.609027,7.790784,7.997145,8.21703,8.391328,8.526427,8.642937,8.72476,8.800191,8.873804)
yrsyng[121:130]=c(8.994333,9.159093,9.344339,9.551019,9.724909,9.909011,10.07398,10.21892,10.36937,10.48265)
yrsyng[131:140]=c(10.58341,10.66764,10.7373,10.79216,10.8506,10.83159,10.81246,10.78387,10.76744,10.85603)
yrsyng[141:150]=c(10.94664,11.05236,11.14336,11.21225,11.30241,11.30555,11.32157,11.34702,11.36985,11.47608)
yrsyng[151:160]=c(11.59355,11.71505,11.85769,12.024,12.15235,12.41189,12.6352,12.82543,13.02144,13.06543)
yrsyng[161:170]=c(13.15455,13.2579,13.34557,13.40421,13.41647,13.52215,13.63457,13.76994,13.88545,13.902)
yrsyng[171:180]=c(13.90531,13.89396,13.93286,14.0405,14.19901,14.09318,13.9472,13.72949,13.43446,13.44185)
yrsyng[181:190]=c(13.46359,13.49496,13.52907,13.56378,13.57513,13.57055,13.58761,13.61506,13.66573,13.71923)
yrsyng[191:201]=c(13.73191,13.79368,13.85356,13.93315,13.99556,14.0569,14.1159,14.16445,14.22032,14.27422,14.3273)
yrsyng[202:207]=(yrsyng[201]^(1-seq(1/6,1/6,6)))*14.468^seq(1/6,1,1/6)



ybddt[1]=2688.5648
ybddt[11]=2594.7565 
ybddt[21]=3298.0646 
ybddt[31]=3530.1094 
ybddt[41]=4008.4610 
ybddt[51]=4368.6159 
ybddt[61]=5182.8298 
ybddt[71]=6424.4154 
ybddt[81]=6996.5403
ybddt[91]=7729.0220 
ybddt[101]=9095.0914 
ybddt[111]=10572.4221 
ybddt[121]=12002.8038 
ybddt[131]=13171.2295 
ybddt[141]=15084.0394
ybddt[151]=20333.2096 
ybddt[161]=24693.2744 
ybddt[171]=30128.5953 
ybddt[181]=34107.0568 
ybddt[191]=40542.7664
ybddt[201]=47431.8
ybddt[207]=52389.39

ybddt[1]=4728.308
ybddt[11]=5063.959 
ybddt[21]=5577.795 
ybddt[31]=6202.604 
ybddt[41]=6540.734 
ybddt[51]=6843.705 
ybddt[61]=8216.696 
ybddt[71]=9211.468
ybddt[81]=11092.16 
ybddt[91]=11083.32 
ybddt[101]=13041.96 
ybddt[111]=15160.37 
ybddt[121]=17213.41 
ybddt[131]=18887.08 
ybddt[141]=21629.86 
ybddt[151]=29159.51 
ybddt[161]=35412.41 
ybddt[171]=43204.89 
ybddt[181]=48911.54 
ybddt[191]=56375.21 
ybddt[201]=69291.03
ybddt[211]=76578.09


ybddt=ybddt/1.469664037853951
ybddtyrly=ybddt

yrsyng[1]=2.4342 
yrsyng[11]=3.2342 
yrsyng[21]=4.0342 
yrsyng[31]=4.8479512
yrsyng[41]=4.69110168
yrsyng[51]=5.78972226
yrsyng[61]=6.52682174
yrsyng[71]=7.81538542
yrsyng[81]=8.18448332
yrsyng[91]=8.28970514
yrsyng[101]=8.43018866
yrsyng[111]=8.79052172
yrsyng[121]=9.49281392
yrsyng[131]=10.18329722
yrsyng[141]=10.53706274
yrsyng[151]=11.03954564
yrsyng[161]=12.33675584
yrsyng[171]=12.60462608
yrsyng[181]=13.17645992
yrsyng[191]=13.74242
yrsyng[201]=14.0888

yrsyng[1]=4.8642 
yrsyng[11]=4.8642
yrsyng[21]=4.8642
yrsyng[31]=4.8779512 
yrsyng[41]=4.6911017 
yrsyng[51]=5.7897223
yrsyng[61]=6.5268217
yrsyng[71]=7.8153854
yrsyng[81]=8.1844833
yrsyng[91]=8.2897051
yrsyng[101]=8.4301887
yrsyng[111]=8.7905217
yrsyng[121]=9.4928139
yrsyng[131]=10.183297
yrsyng[141]=10.537063  
yrsyng[151]=11.039546  
yrsyng[161]=12.336756 
yrsyng[171]=12.758
yrsyng[181]=13.532
yrsyng[191]=14.051
yrsyng[201]=14.597
yrsyng[211]=14.597

yrsyngyrly=yrsyng


interpup=seq(.1,.9,by=.1)
interpdn=1-interpup
 
for(qq in seq(1, time.g-9, by=10))
{
	mm=qq +seq(1,9,1)
    infant[mm]=(infant[qq]^interpdn)*(infant[qq+10]^interpup)
    infanta[mm]=(infanta[qq]^interpdn)*(infanta[qq+10]^interpup)
    deltat[mm]=(deltat[qq]^interpdn)*(deltat[qq+10]^interpup)
    deltaa[mm]=(deltaa[qq]^interpdn)*(deltaa[qq+10]^interpup)
    avgyrscen[mm]=(avgyrscen[qq]^interpdn)*(avgyrscen[qq+10]^interpup)
    ybddtyrly[mm]=(ybddt[qq]^interpdn)*(ybddt[qq+10]^interpup)
    yrsyngyrly[mm]=(yrsyng[qq]^interpdn)*(yrsyng[qq+10]^interpup)
       
       }


# in this section I add the young adult mortality rate with the infant mortality rate in order to #let the model have the highest rate of death to precautionate   
for (nnn in (1:211))
{
    
    delta[nnn]	=  deltat[nnn] + infant[nnn]/3
    
   
}

tfrabefore	=c(8.34,8.22,7.8,7.2,6.66)
tfra		=c(5.82,5.52,5.01,4.65,4.11,3.90,3.804,3.537,2.676,2.3,3.446,3.314,1.969,1.815,2.1,2.1)
tfra		=c(tfra, 2.1*rep(1, time.g-16))
chbornyr	=c(1840, 1845, 1850, 1855, 1860 ,1865,1870 ,1875 ,1880 ,1885 ,1890 ,1895 ,1900, 1902, 			1904,1905, 1907, 1909, 1910, 1912, 1914, 1915, 1917, 1919, 1920, 1921, 1922, 1923, 				1924, 1925, 1926, 1927, 1928, 1929, 1930, 1931, 1932, 1933, 1934)
chbornyr	=c (chbornyr,1935 , 1936,1937,1938,1939,1940,1941,1942,1943,1944, 1945,1946, 1947,1948, 			1949,1950,1951,1952,1953,1954, 	1955,1956,	1957,1958,1959,1960,1961, 1962,1963,1964,			1965, 1966, 1967)
chbornyr=	c(chbornyr,1968,1969,1970,1971,1972,1973,1974,1975,1977,1978,1979,1980,1982,1983,1984, 			1985,1987,1988,1989,1990)
chbornyr	=chbornyr+20

chborn		= c (5.49,5.364,5.266,5.218,5.076,4.744,4.383,3.781,3.323, 3.314,3.215,3.087, 2.822, 			2.937, 2.610,2.492,2.352,2.436,2.364, 2.401,2.366,2.402,2.514,2.569,2.603 ,	2.596, 				2.694,2.63, 2.832,2.856,2.948,2.941,2.989,3.082)
chborn		=c(chborn,3.152,3.206,3.256,3.236,3.249,3.281,3.232,3.266,3.212,3.108, 3.105, 2.97, 			2.855,2.788, 2.678,2.572,2.452,2.329,2.280,2.270,2.167,2.101, 2.13, 2.092,2.094, 				2.087,2.100, 2.020,2.002)
chborn		=c (chborn, 2.025, 2.048, 2.048, 2.074, 2.050, 2.053, 2.016, 2.01, 2.015, 2.01, 2.01, 			2.01, 2.01, 2.01, 2.01,2.01,2.01,2.01,2.01,2.01,2.01,2.01,2.01,2.01,2.06,2.08,2,2,2)
year		= seq(from=1800, to= 2011, by=1)
parentbrthyr=rep(0, 2010) 
cheverborn	=parentbrthyr
for (qqq in (1:92))
{
    parentbrthyr[chbornyr[qqq]]=1
    cheverborn[chbornyr[qqq]]=chborn[qqq]    
}


parentbrthyr[1810]=1
parentbrthyr[1820]=1 
parentbrthyr[1830]=1 
parentbrthyr[1840]=1

#Actual Fertility Values
cheverborn[1800]= 7.04 
cheverborn[1810]=6.92 
cheverborn[1820]=6.73 
cheverborn[1830]=6.55 
cheverborn[1840]=6.14 
cheverborn[1850]=5.42


cheverborn[1800]= 8.242113*2.575/3 
cheverborn[1810]=8.038542*2.575/3 
cheverborn[1820]=7.681432*2.625/3 
cheverborn[1830]=7.040518*2.75/3 
cheverborn[1840]=6.728356*2.75/3 
cheverborn[1850]=5.953719*5.75/6


cheverborn[1800]=7.752507 
cheverborn[1810]=7.648624 
cheverborn[1820]=7.174476 
cheverborn[1830]=6.811343 
cheverborn[1840]=6.407971 
cheverborn[1850]=5.836418 
cheverborn[1860]=5.57963 
cheverborn[1870]=5.124645 
cheverborn[1880]=4.836077


#-----------------------------------------------------------------------------------------------
	

#THIS SECTION SETS INITIAL HUMAN CAPITAL for THE SIMULATION run!
h		=rep(1,time.g+20)
h[1]	=5.45*1.47/1.012^10 
h[11]	=5.45*1.4 
h[21]	=(h[11]*h[31])^.5 
h[31]	=7.3

husa	=rep(0, time.g)
h[1:11]	=h[1]*seqm(1,1.001,11)
h[12:21]=h[11]	/ seqm(1/1.004,1/1.004,10) 




x=h
tau=h 
s=h 
consump=h 
eulert=h 
indx=rep(0,time.g)
tt=1
consump=h 
tol=1e-9
xmin=.5 
xx=seqa(xmin,(1/theta-xmin)/1000,1000) 
sx=rep(0,1000) 
taux=sx 
cx=sx 
uhat= matrix(0, nrow = 1000, ncol= time.g)





zzzz=1
mmmm=seqa(1/3.5,2.5/3.5/10,11)
mmmm=c(mmmm,rep(1,3))
mmmm=c(mmmm, 1-seqa(.05,.075/1.11,10))
r=rep(1, time.g)
r=rental*rep(1, time.g)

mum	=.075	* rep(1, time.g)
mum	=.085	* rep(1, time.g)
muavg=mum	*rep(1, time.g)/time.g
#muavg=mum'*ones(gen,1)/gen

kappa=rep(1, time.g)
seqa_0.1_0.1_10= seq(0.1,by=0.1,length.out=10)
interp_up= seqa_0.1_0.1_10
interp_dn= 1- interp_up 
kappa[1:10]=(.55^interp_dn)*(.64^interp_up)
kappa[11:20]=(.64^interp_dn)*(.66^interp_up)
kappa[21:30]=(.64^interp_dn)*(.75^interp_up)
kappa[31:40]=(.75^interp_dn)*(.64^interp_up)
kappa[41:50]=(.62^interp_dn)*(.62^interp_up)
kappa[51:60]=(.62^interp_dn)*(.60^interp_up)

#kappa is interpolated different after the 61st period
interp_up1= seq(0.2,by=0.2,length.out=5) 
interp_dn1=	1-interp_up1
kappa[61:65]=(.60^interp_dn1)*(.62^ interp_up1)
kappa[66:70]=(.62^interp_dn1)*(.65^interp_up1)

#kappa is interpolated with the initial geometric interpolation
kappa[71:80]=(0.65^interp_dn)*(0.77^interp_up)
kappa[81:90]=(.72^interp_dn)*(.95^interp_up)
kappa[91:100]=(1.0^interp_dn)*(1.07^interp_up)
kappa[101:110]=(1.07^interp_dn)*(1.15^interp_up)
kappa[111:120]=(1.15^interp_dn)*(1.40^interp_up)

#Notice that kappa is interpolated with interp_up1 
kappa[121:125]=(1.41^interp_up1)*(1.34^interp_dn1)
kappa[126:130]=c(1.34,1.31,1.24,1.18,1.16)
kappa[131:140]=c(1.15,1.12,1.07,1.04,1.00,.96,.96,.96,.96,.96)
#@(1.65^(1-seqa(1/10,1/10,10))).*(1.15^(seqa(1/10,1/10,10))); @

kappa[141:145]=(.77^interp_dn1)*(.62^interp_up1)
kappa[146:150]=(.57^interp_dn1)*(.60^interp_up1)
kappa[151:155]=(.58^interp_dn1)*(.92^interp_up1) 
kappa[156:160]=(1.00^interp_dn1)*(1.20^interp_up1) 
kappa[161:165]=(1.23^interp_dn1)*(1.27^interp_up1) 
kappa[166:170]=(1.27^interp_dn1)*(1.35^interp_up1) 
kappa[171:181]=(1.35^(1-seqa(1/11,1/11,11)))*(1.365^(seqa(1/11,1/11,11)))
kappa[182:191]=(1.37^(1-seqa(1/10,1/10,10)))*(1.253^(seqa(1/10,1/10,10)))
kappa[192:201]=(1.253^(1-seqa(1/10,1/10,10)))*(1.225^(seqa(1/10,1/10,10)))

kappa=c((.55^interp_dn)*(.55^interp_up), kappa[1:201])

#Calibrated rental values
rental	=.23823957

r[1:10]=rental*.15*rep(1,10)
r[11:20]=rental*rep(1,10)*.15
r[21:30]=rental*rep(1,10)*.15
r[31]=rental*.14
r[41]=rental*.14
r[46]=rental*.14
r[51]=rental*.14 
r[56]=rental*.14 
r[61]=rental*.10 
r[66]=rental*.10
r[71]=rental*.10 
r[76]=rental*.10
r[81]=rental*.09 
r[86]=rental*.09 
r[91]=rental*.11 
r[101]=rental*.22
r[111]=rental*.11 
r[121]=rental*.11
r[131]=rental*.22 
r[136]=rental*.22 
r[141]=rental*.14 
r[146]=rental*.14
r[150]=rental*.15
r[151]=rental*.05
r[156]=rental*.0225 
r[161]=rental*.0235 
r[166]=rental*.125
r[171]=rental*.22 
r[176]=rental*.22 
r[181]=rental*.20 
r[191]=rental*.165 
r[201]=rental*.10 
r[211]=rental*.075 

interpup_r=seq(from=0, by=.2, length.out=5)
interpdn_r=1-interpup_r
interpup_r1=seq(from=0, by=.1, length.out=10)
r[21:30]=(r[21]^(1-seqa(0,1/10,10)))*(r[31]^seqa(0,1/10,10))
r[31:40]=(r[31]^(1-seqa(0,1/10,10)))*(r[41]^seqa(0,1/10,10))
r[41:45]=(r[41]^interpdn_r)*(r[46]^interpup_r)
r[46:50]=(r[46]^interpdn_r)*(r[51]^interpup_r)
r[51:55]=(r[51]^interpdn_r)*(r[56]^interpup_r)
r[56:60]=(r[56]^interpdn_r)*(r[61]^interpup_r)
r[61:65]=(r[61]^interpdn_r)*(r[66]^interpup_r)
r[66:70]=(r[66]^interpdn_r)*(r[71]^interpup_r)
r[71:75]=(r[71]^interpdn_r)*(r[76]^interpup_r)
r[76:80]=(r[76]^interpdn_r)*(r[81]^interpup_r)
r[81:85]=(r[81]^interpdn_r)*(r[86]^interpup_r)
r[86:90]=(r[86]^interpdn_r)*(r[91]^interpup_r)
r[91:100]=(r[91]^(1-seqa(0,.1,10)))*(r[101]^seqa(0,.1,10))
r[101:110]=(r[101]^(1-seqa(0,1/10,10)))*(r[111]^seqa(0,1/10,10))
r[111:120]=(r[111]^(1-seqa(0,1/10,10)))*(r[121]^seqa(0,1/10,10))
r[121:130]=(r[121]^(1-seqa(0,1/10,10)))*(r[131]^seqa(0,1/10,10))
r[131:135]=(r[131]^(1-seqa(0,1/5,5)))*(r[136]^seqa(0,1/5,5))
r[137:140]=((r[136]*1)^(1-seqa(0,1/4,4)))*(r[141]^seqa(0,1/4,4))
r[141:145]=(r[141]^(1-seqa(0,1/5,5)))*(r[146]^seqa(0,1/5,5))
r[146:150]=(r[146]^(1-seqa(0,1/5,5)))*(r[150]^seqa(0,1/5,5))
r[151:155]=(r[151]^(1-seqa(0,1/5,5)))*(r[156]^seqa(0,1/5,5))
r[156:160]=(r[156]^(1-seqa(0,1/5,5)))*(r[161]^seqa(0,1/5,5))
r[161:165]=(r[161]^(1-seqa(0,1/5,5)))*(r[166]^seqa(0,1/5,5))
r[166:170]=(r[166]^(1-seqa(0,1/5,5)))*(r[171]^seqa(0,1/5,5))
r[171:175]=(r[171]^(1-seqa(0,1/5,5)))*(r[176]^seqa(0,1/5,5))
r[176:180]=(r[176]^(1-seqa(0,1/5,5)))*(r[181]^seqa(0,1/5,5))
r[181:190]=(r[181]^(1-seqa(0,1/10,10)))*(r[191]^seqa(0,1/10,10))
r[191:200]=(r[191]^(1-seqa(0,1/10,10)))*(r[201]^seqa(0,1/10,10))
r[201:211]=(r[201]^(1-seqa(0,1/11,11)))*(r[211]^seqa(0,1/11,11))


#Frontier Human Capital Values- Human capital values for the US
husa[1:100]=c(7.1106403,7.1177510,7.1248687,7.1319936,7.1391256,7.1462647,7.1534110,7.1605644,7.1677250,7.1748927,7.1820676,7.2107958,7.2396390,7.2685976,7.2976720,7.3268627,7.3561701,7.3855948,7.4151372,7.4447977,7.8531724,7.8622039,7.8712458,7.8802982,7.8868404,7.8959107,7.9049916,7.9140829,7.9231848,7.9322972,7.9376429,7.9767681,8.0161165,8.0530936,8.0928719,8.1328792,8.1704565,8.2109045,8.2488830,8.2897790,8.7630747,8.7830302,8.8001300,8.8172831,8.8316666,8.8489219,8.8662317,8.8835964,8.9010166,8.9184925,8.9934198,9.1045367,9.2170815,9.3280654,9.4434773,9.5603769,9.6756323,9.7955185,9.9137032,10.036658,10.633050,10.661500,10.690315,10.719241,10.744843,10.770123,10.799361,10.828714,10.854273,10.883840,10.981424,11.127388,11.271294,11.413460,11.565437,11.715254,11.867589,12.021491,12.173486,12.336093,13.103661,13.148585,13.198965,13.244618,13.291213,13.337510,13.383767,13.435300,13.477131,13.529119,13.688851,13.909990,14.129797,14.343093,14.575452,14.781457,14.991229,15.197536,15.407961,15.632456)
  
husa[101:207]=c(16.639163,16.709635,16.787227,16.858985,16.932126,17.005070,17.070979,17.150918,17.218656,17.299572,17.518671,17.832025,18.145058,18.451124,18.783195,19.082762,19.379774,19.682443,19.982618,20.311776,21.639781,21.768740,21.909321,22.044777,22.172857,22.278521,22.375891,22.480076,22.581552,22.688308,23.014376,23.482987,23.954342,24.404771,24.907449,25.354408,25.800330,26.256222,26.711290,27.207778,29.054498,29.243877,29.428956,29.628152,29.796874,29.935559,30.063205,30.200138,30.355842,30.496741,30.951213,31.586971,32.226780,32.838777,33.495782,34.123314,34.731960,35.310970,35.972580,36.683970,39.140039,39.333159,39.586239,39.856892,40.086202,40.303682,40.523343,40.720740,41.020399,41.328503,41.937154,42.874623,43.818110,44.724166,45.664716,46.594526,47.452819,48.243063,49.176125,50.179046,53.527378,53.794253,54.181521,54.561295,54.848881,55.244631,55.538348,55.802133,56.206860,56.623824,57.571325,58.915819,60.224971,61.484256,62.741014,63.905731,65.098123,66.198246,67.495835,68.890834,73.399645,73.722367,74.209476,74.754487,75.103787,75.600543,76.027874)
 
 husa[1:100]=c(7.1106403,7.1177510,7.1248687,7.1319936,7.1391256,7.1462647,7.1534110,7.1605644,7.1677250,7.1748927,7.1820676,7.2107958,7.2396390,7.2685976,7.2976720,7.3268627,7.3561701,7.3855948,7.4151372,7.4447977,8.4150925,8.4739555,8.5305137,8.5874476,8.6475142,8.7052254,8.7633199,8.8218001,8.8835021,8.9427807,8.9595356,9.0116241,9.0611384,9.1138780,9.1669613,9.2173986,9.2711496,9.3252534,9.3766360,9.4314248,10.728709,10.833160,10.938863,11.045650,11.157088,11.262261,11.372357,11.479625,11.595645,11.705089,11.721998,11.785219,11.840891,11.900692,11.965067,12.021714,12.082594,12.148155,12.205800,12.267790,14.020689,14.182745,14.341782,14.502685,14.675541,14.835296,15.001990,15.165415,15.346568,15.513881,15.551168,15.644183,15.733153,15.827785,15.928690,16.019491,16.110044,16.213053,16.305687,16.404311,18.819843,19.044388,19.265062,19.488426,19.728041,19.950235,20.181951,20.409459,20.669191,20.902441,20.960633,21.110889,21.247636,21.392326,21.545808,21.685936,21.826031,21.983323,22.126876,22.278884)
            
 husa[101:211]=c(25.637147,25.934514,26.226543,26.522177,26.839870,27.133811,27.440662,27.741757,28.086612,28.395449,28.457572,28.670232,28.878392,29.084784,29.303698,29.491020,29.692675,29.918348,30.125967,30.330787,34.998069,35.467814,35.916345,36.373996,36.866428,37.288151,37.707752,38.120433,38.594300,38.996157,39.122373,39.432644,39.737573,40.040942,40.362453,40.641364,40.940958,41.274655,41.556664,41.863111,48.460281,49.135275,49.746967,50.407479,51.080736,51.656069,52.228541,52.791520,53.439327,53.987534,54.189007,54.646033,55.096412,55.545386,56.02031,56.496005,56.755800,57.242933,57.796462,58.353235,67.649367,68.601471,69.536206,70.536402,71.478818,72.493707,73.045088,73.853229,74.890949,75.793546,76.122350,76.869492,77.556406,78.288528,78.958733,79.720240,80.108777,80.818774,81.623218,82.432987,95.818782,97.143471,98.391835,99.742712,100.94918,102.36793,103.17298,104.26214,105.75698,107.06372,107.57243,108.63043,109.60399,110.64214,111.59351,112.67842, 113.23698,114.25078,115.39900,116.55572,135.88934,137.82710,139.65912,141.63929,143.41705,145.49916,146.57767,148.19390,150.38961,152.32093,152.97613)
 

#This section sets tolerance
cnt=1 
innererror=10
innertol=1e-8

while (innererror>innertol){
tt=1
w=0.23755548
p[91:94]=1 * rep(1,4)
p[110:114]=1.1 * rep(1,5)
p[27:39]=c(1.1,1.25,1.5,2.25,3.75,3.25,2.75,2.25,1.75,1.5,1.35,1.15,1.05)
p[67:75]=c(1.05,1.1,1.125,1.2,1.35,1.15,1.07,1.05,1.025)
p[141:170]=c(1.05*rep(1,10),rep(1,9),.95*rep(1,11))
p[171:180]=.9*rep(1,10)
w=.22312651
p[1:time.g]=1.00*rep(1,time.g)
ravg= mean(r)


#Calibrated taste parameters
mjm=c((1.55^(1-seqa(1/40,1/40,40)))*(1.37^seq(1/40,1/40,40)),(1.35^(1-seqa(1/30,1/30,30)))*(1.33^seqa(1/30,1/30,30)),(1.33^(1-seqa(1/35,1/35,35)))*(1.32^seqa(1/35,1/35,35)),(1.31^(1-seqa(1/30,1/30,30)))*(.9^seqa(1/30,1/30,30)),.9,.825,.77,.74,.71,.67,.63,.585,.55,.5,.48,.42,.40,.39,.34,.4*rep(1,time.g-150))

alpha=0.275
rhorho= rep(1,1000)
while (tt< time.g+1)
for(tt in (1:time.g))
{
    wwww[tt]=w
    mu=mum[tt]
    mmm=mjm[tt]
    jest=1 
    
    while(jest<1001){
    			err=10
            xmin=a/(1-delta[tt]) + .01 
            xx=seqa(xmin,((1/theta)-xmin)/1000,1000)
            taumin=0
            taumax=((1/xx[jest])-theta)/kappa[tt]
            
            taux[jest]=.5 * (taumin+taumax)
           
            
            while (err>tol){
            	
           		rho=min(.5*(taux[jest]/.38125)^.03125,.5)
           		          		
           		 euler=-kappa[tt]*alpha*(zzzz[tt]/r[tt]^(1-psi))^phi*psi^(psi*phi)*(1-psi)^(phi*(1-psi))*(xx[jest]/p[tt])^(psi*phi)*(1/					xx[jest]-theta-kappa[tt]*taux[jest])^(phi-1)*((1-delta[tt])*xx[jest]-a)^lambda + 														caplambda*mu*aa^phi*taux[jest]^(mu*phi-1)
             
          			
             if (euler < 0){ 
             taumax=taux[jest]
             taux[jest]=.5*(taumin+taumax)
             }
             else {
             	taumin=taux[jest]
             	taux[jest]=.5*(taumin+taumax)
             }
             doublchek=taumax-taumin
             err=min(abs(euler),doublchek)
         		}
            		        		
        	jest=jest+1
        	rhorho[jest]=min(.5*(taux[jest]/.38125)^.03125,.5)
        	
		}
        
        
        sx=(1-psi)*h[tt]/r[tt]*(1/xx-theta-kappa[tt]*taux)
        cx=psi/(1-psi)*sx*xx*r[tt]/p[tt]
       
       

uhat[,tt]=alpha*((cx^psi)*sx^(1-psi))^phi*((1-delta[tt])*xx-a)^lambda+caplambda*((1/zzzz[tt])*aa*h[tt]*taux^mu)^phi-	beta*delta[tt]^mmm/(xx*(1-delta[tt])-a)/(1-delta[tt])^top
    	
        jj=which.max(uhat[,tt])
        print (jj)
        umax[tt]=uhat[jj,tt]
        tau[tt]=taux[jj]
        cc[tt]=cx[jj]
        indx[tt]=jj
        x[tt]=xx[jj]
        s[tt]=sx[jj]
        h[tt+20]=aa*(hbar[tt]^rhohat[tt])*(h[tt]^(1-rhohat[tt]))*tau[tt]^mu
        popu[tt+20]=popu[tt]*x[tt]*(1-deltaa[tt]-infanta[tt])

}



#@   this part of the program presents the balanced growth values of #consumption share, housing, fertility, and human capital investment #@
tfr = 2*x

#@   this section calculates a back of the envelope average years of #schooling of 20 - 60 aged population    @


jjj=59
yavg=rep(0,time.g)
sklavg=rep(0,time.g) 
hvar=rep(0,time.g)
havg=rep(1,time.g)
el=rep(0,time.g)
sec=rep(0,time.g)
hi=rep(0,time.g)
inlf=rep(0,time.g)

aaaaa=1

while (aaaaa<time.g+1){
    if (tau[aaaaa]<.1){
    			el[aaaaa]=.60
    }		else if (tau[aaaaa]<.3){
    			sec[aaaaa]=.82
    }		else  {   
    			hi[aaaaa]=.91
    }
aaaaa=aaaaa+1
}

jjj=time.g + 20
q=rep(0, time.g) 
mkt=q
tfp=q
while (jjj>0){
    q[jjj]=max(h[jjj])
    gmma1=.35
    gmma2=105
    abc=40 
    zyz=-30
    
    q[jjj]=exp((gmma2*h[jjj]+zyz)^gmma1)/(abc+exp((gmma2*h[jjj]+zyz)^gmma1))
    if (jjj<141){
    		omega=1.00
    }		else {
    	   omega=1.00
    }
     #logb(x, base = exp(1))

    mkt[jjj]=min(max(-(omega-1)/log(q[jjj]),1),50000000000)
    tfp[jjj]=exp(-(omega-1))*mkt[jjj]^(omega-1)
    
    jjj=jjj-1
}


jjj=59
inlf=rep(0,time.g)
#while (jjj<time.g+1){
for (jjj in (59:time.g)){
    inlf[jjj]=round(40*(tau[jjj]+theta),0)
    lastin = 40 - inlf[jjj]
    appliedmort=c(seqm(.975^(58-39),1/.975,(58-39)),rep(1,lastin))
    workappliedmort=c(seqm(.975^20,1/.975,20),rep(1,20))
   if (jjj>120){
       appliedmort=c(seqm(.955^(58-39),1/.955,(58-39)),rep(1,lastin))        			
        workappliedmort=c(seqm(.955^20,1/.955,20),rep(1,20))
        
    	if (jjj>120){
        appliedmort=c(seqm(.965^(58-39),1/.965,(58-39)),rep(1,lastin))        			
        workappliedmort=c(seqm(.965^20,1/.965,20),rep(1,20))
        }
        
    	else if (jjj>190){   
        appliedmort=c(seqm(.985^(58-39),1/.985,(58-39)),rep(1,lastin))
         workappliedmort=c(seqm(.985^20,1/.985,20),rep(1,20))
        }
                	
        }
       
      sklavg[jjj]=40*(t((el[(jjj-58):(jjj-inlf[jjj])] + sec[(jjj-58):(jjj-inlf[jjj])] + hi[(jjj-58):(jjj-inlf[jjj])]) * tau[(jjj-58):(jjj-inlf[jjj])])) %*% (popu[(jjj-58):(jjj-inlf[jjj])]*appliedmort)/(t((el[(jjj-58):(jjj-inlf[jjj])]+sec[(jjj-58):(jjj-inlf[jjj])]+hi[(jjj-58):(jjj-inlf[jjj])]) *(popu[(jjj-58):(jjj-inlf[jjj])]*appliedmort)) %*% rep(1,58-inlf[jjj]+1))
      
      
      yavg[jjj]=(t((el[(jjj-39):jjj]+sec[(jjj-39):jjj]+hi[(jjj-39):jjj])*(h[(jjj-39):jjj]*tfp[(jjj-39):jjj])))%*%(popu[(jjj-39):jjj]*workappliedmort)/(t((el[(jjj-39):jjj]+sec[(jjj-39):jjj]+hi[(jjj-39):jjj])*(popu[(jjj-39):jjj]*workappliedmort)) %*% rep(1,40))
    
}

jjj=59
tautemp=tau[1]/seqm(1.001,1.0016,60)
ptemp=popu[1]/seqm(1.010,1.010,60)
while (jjj>round(40*(tau[jjj]+theta),0)){
    inlf[jjj]=round(40*(tau[jjj]+theta),0)
    
   sklavg[jjj]=40*((t((el[1:(jjj-inlf[jjj])]+sec[1:(jjj-inlf[jjj])]+hi[1:(jjj-inlf[jjj])])*tau[1:(jjj-inlf[jjj])])%*%popu[1:(jjj-inlf[jjj])])+t(tautemp[1:(60-jjj)])%*%ptemp[1:(60-jjj)])/ 
((t((el[1:(jjj-inlf[jjj])]+sec[1:(jjj-inlf[jjj])]+hi[1:(jjj-inlf[jjj])]) *popu[1:(jjj-inlf[jjj])]) %*% rep(1,(jjj-inlf[jjj]))) +  t(ptemp[1:(60-jjj)])%*%rep(1,(60-jjj)))

jjj=jjj-1

}


jjj=round(40*(tau[jjj]+theta),0)
while (jjj>0){
    sklavg[jjj]=sklavg[jjj+1]/1.00125
    havg[jjj]=havg[jjj+1]/1.005
    hvar[jjj]=0
jjj=jjj-1
}


jjj=65
while (jjj>39){
	
	
	
yavg[jjj]=(t((el[(jjj-39):jjj]+sec[(jjj-39):jjj]+hi[(jjj-39):jjj]) * (h[(jjj-39):jjj] *tfp[(jjj-39):jjj])) %*% popu[(jjj-39):jjj])/(t((el[(jjj-39):jjj] + sec[(jjj-39):jjj]+hi[(jjj-39):jjj]) * popu[(jjj-39):jjj]) %*% rep(1,40))

jjj=jjj-1
}

htemp=h[1]/seqm(1.0125,1.0125,44) 
ptemp=popu[1]/seqm(1.0155,1.0155,44)
tfptemp=exp(-(omega-1))/seqm(1.001,1.001,44)

while (jjj>0){
yavg[jjj]=((t((el[1:jjj]+sec[1:jjj]+hi[1:jjj]) * (h[1:jjj] * tfp[1:jjj]))) %*% popu[1:jjj]+(t(tfptemp[1:(45-jjj)] * htemp[1:(45-jjj)])) %*% ptemp[1:(45-jjj)]) / (t((el[1:jjj]+sec[1:jjj]+hi[1:jjj])*popu[1:jjj]) %*% rep(1,jjj) + t(ptemp[1:(45-jjj)])%*%rep(1,(45-jjj)))
 
jjj=jjj-1
}

#@   this section calculates the average value of time varying parameters in order to produce the base model responses   @
 
 ravg=t(r[1:time.g]) %*% rep(1, time.g)/time.g
 muavg=t(mum) %*% rep(1, time.g)/time.g
 wavg=t(wwww) %*% rep(1, time.g)/time.g
 pavg=t(p) %*% rep(1, time.g)/time.g
 kappaavg=t(kappa) %*% rep(1, time.g)/time.g
 #@   0.23823957      0.053460695       0.22312651        #1.0024607        1.0880890  @
 #@   ravg            muavg             wavg              pavg             #kappaavg   @
 
 easterlin=(cc[1:time.g]^psi * s[1:time.g]^(1-psi))
 #@   631.17921   @
 
 zprime = tfp[seqa(1,1,time.g)] / tfp[seqa(21,1,time.g)]
 #/* #kappa[111:201,1]~year[111:201,1]~40*tau[111:201,1]~sklavg[111:#201,1]~avgyrscen[111:201,1]; */
 innererror = (t((zzzz-zprime)) %*% (zzzz-zprime))^.5
 #/* (zzzz-zprime)|innererror;    */
 #@innererror=sumc(abs(zzzz-zprime));@
 #qqqq=cheverborn[1800:2010,.].>zeros(gen,1);
 #tfrmean=sumc(((tfr[1:gen,1]-cheverborn[1800:2010,1]).*qqqq))/sumc(qqqq);
 #sklmean=sumc(sklavg[1:gen,1]-avgyrscen)/gen;
 cnt=cnt+1
 zzzz=zprime

 }

 
 bbbbb.df= as.data.frame(year)
 bbbbb.df$delta= delta
 bbbbb.df$deltaa=deltaa[1:time.g]
 bbbbb.df$deltat=deltat[1:time.g]
 bbbbb.df$infanta=infanta[1:time.g]
 
