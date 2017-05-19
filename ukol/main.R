# MP-SPI 2017 ukol 2
# zdrojovy kod
# autori:
#   lorenpe2 - 107 - Lorenc Petr
#   liutoole - 107 - Liutova Oleksandra
#################################################################
# NASTAVENI PROMENNYCH K, L
K=nchar('Petr') # |jmeno|, K = 4
L=nchar('Lorenc') # |prijmeni|, L = 6
#################################################################
n = 20;
alpha = 0.01;
x = rnorm(n, mean=10.5, sd=1.3);

hypothesisTest = t.test(x, mu=10, conf.level = 1-alpha);
print(hypothesisTest); # Printing of the result is useful if you execute a script file


#quantile = qt(probability, degreesOfFreedom)
#criticalValue = qt(probability, degreesOfFreedom, lower.tail = FALSE)

# oboustranna alternativa na 99% -> 1% rozdelit na obe strany
quantile = qt(0.995, n-1)

#Stred interavlu tedy prumer
xmean = mean(x);
#odchylka
stdDev = sd(x);
sqrtn = sqrt(n);

# z prednasky 17/46
intv = quantile*stdDev/sqrtn

confint = c(xmean - intv, xmean + intv)
print(confint)

t.test(x, mu=10, conf.level = 1-alpha);
print("Nas interval je")
print(confint)
print("Nas prumer je")
print(xmean)

in_interval <- function(x, interval){
  stopifnot(length(interval) == 2L)
  interval[1] < x & x < interval[2]
}
mu = 10
in_interval(mu, confint)

# z prednasky 17/46
testT = (mean(x) - 10)/sqrt(var(x)/n)
intertvalT=c(-qt(.995, n-1), +qt(.995, n-1))
in_interval(testT, intertvalT)
print(testT)
print(intertvalT)
# True takze nezamitame

greater=t.test(x, mu=10, alternative = "greater", conf.level = 1-alpha);
less=t.test(x, mu=10, alternative = "less", conf.level = 1-alpha);
print(greater)
print(less)

#zamitnuti je silnejsi (da nam aspone jakou informaci) tak volime vetsi 
#protoze apriori vime ze hodnota je 10.5 a my testujeme proti 10 tak
#hypoteza je ze to je 10 a testuje oproti alternative ze to je vice nez 10 
#(tj pokud zamitneme nulovou hypotezu tak dokazeme alternativu)
criticalValue <- qt(.99,n-1);
xmean <- mean(x);
stdDev <- sd(x);
sqrtn <- sqrt(n);
intv <- criticalValue*stdDev/sqrtn

# z prednasky 17/66
confint = c(xmean - intv, Inf  )
print(greater)
print(confint) # pro kontrolu
in_interval(mu, confint)
#true -> H0 nezamitame

testT = (mean(x) - 10)/sqrt(var(x)/(n-1))
nint=c(-qt(.990, n-1), Inf)
print(nint)
in_interval(testT, nint)
#true ->H0 nezamitame .

#===============================================

n = 20;
alpha = 0.01
x = rnorm(n, mean=10, sd=1)
error = rnorm(n, mean=0.5, sd=0.8306624)
y = x + error

print(t.test(x, y=y, paired = TRUE, alternative = "less", conf.level = 1-alpha))
#zamitame hypotezu HO protoze vysel interval (-Inf -0.1216135) a tam nula nepatri, pravdepodobnost chyby je 1 procento

#b
diff = x - y
# prevedeno na priklad vyse s jednovyberovym t-testem
t.test(diff, mu=0, alternative = "less", conf.level = 1-alpha);
# H0 ze diff ma prumer v 0 zamitame - shoduje se s vysledkem vyse

#2 II
n1 = 20;
n2 = 25;
alpha = 0.01
x=rnorm(n1, mean=10, sd=1.3)
y=rnorm(n2, mean=11.25, sd=1.3)

t.test(x, y=y, paired = FALSE, var.equal = TRUE, conf.level = 1-alpha)
# H0 ze maji stejne rozplyly nezamitame s pravdepodobnosti 99%


print(t.test(x, y=y, paired = FALSE, var.equal = TRUE, conf.level = 1-alpha, alternative="less"))
# H0 ze maji stejne rozplyly nezamitame ve prospech alternativy ze prumer rozdilu je mensi nez 0 s moznosti chyby na 99%



# b - Pomocí vzorcu z prednásky spoctete testovací statistiku 't' a stupne volnosti 'df' (degrees of freedom)
# prednaska 18 slide 28
n=length(x)
m=length(y)
df=m+n-2

sx= var(x)
sy= var(y)
sxy2 = ((sx*(n-1)) + (sy*(m-1))) / df
sxy = sqrt(sxy2)
print(sxy)

#a nyni se pustimne do velkeho T
# prednaska 18 slide 29
T= ( (mean(x)-mean(y)) / (sxy*sqrt((1/n)+(1/m)) ) )
print(T)

p_value = pt(T, df)
print(p_value)
print(p_value < alpha)
# p_value je vetsi nez alpha, tudiz hypotezu h0 nezmitame

#2 III

# a
n1 = 20;
n2 = 25;
alpha = 0.01
x=rnorm(n1, mean=10, sd=1.3)
y=rnorm(n2, mean=11.28, sd=1.2)

t.test(x, y=y, paired = FALSE, var.equal = FALSE, conf.level = 1-alpha)
# H0 ze maji stejne rozplyly zamitame ve prospech alternativy

#opakovat jako v predchozim bode ale s tim rozdilem ze rozplyt1 se nerovna rozptyl2
t.test(x, y=y, paired = FALSE, var.equal = FALSE, conf.level = 1-alpha, alternative="less")
# H0 ze maji stejne rozplyly zamitame ve prospech alternativy ze prumer rozdilu je mensi nez 0 s moznosti chyby na 99%

#b

sx = var(x)
sy = var(y)
# prednaska 18 slide 29 dole pro sigma1 != sigma2
upper_part =  ((sx/n1 + sy/n2)^2)
down_part = (((sx/n1)^2 / (n1-1)) + ((sy/n2)^2 / (n2-1)))
df = upper_part / down_part
sxy = sqrt(sx/n1 + sy/n2)
T = (mean(x) - mean(y))/sxy

p_value = pt(T, df)

print(df)
print(T)
print(p_value)
print(p_value < alpha)
# p_value je mensi nez alpha, tudiz testu verime a shoduje se s hodnotou z t testu ??


#3
#I
sequenceLength = 2000000;
x = runif(sequenceLength, 0, 100)
print(system.time(sort(x)))
#II
sampleSize = L*40;
time1 = time2 = numeric(sampleSize); # Declare an array
for(i in 1:sampleSize){
  x = runif(sequenceLength, 0, 100); # Generate the sequence to be sorted
  # Measure sort times. The user-space time is at system.time(...)[1]
  # Inside system.time we must use x1 <- value and not x = value. The latter syntax is reserved for parameters.
  time1[i] = system.time(x1 <- sort(x, method = "quick"),  gcFirst = TRUE)[1]; # Singletonuv quicksort
  time2[i] = system.time(x2 <- sort(x, method = "shell"), gcFirst = TRUE)[1]; # varianta Sedgewickovy verze
}

#jako Ho bereme ze bezi stejnou dobu
#jako Ha volime ze Singletonuv quick sort je o neco rychlejsí tj time1 - time2 bude mensi nez 0
diff=time1-time2
alfa=K/100

print(t.test(diff, mu=0, alternative = "less", conf.level = 1-alfa))
# H0 zamitame ve prospech alternativy ze Singletonuv quicksort je rychlejsi s pravdepodobnosti chyby 4%
# jako alternativu jsme zvolili to cemu vice verime (nasi pracovni hypotezu)

#III
for(i in 1:sampleSize){
  x = runif(sequenceLength, 0, 100); # Generate the sequence to be sorted
  time1[i] = system.time(x1 <- sort(x, method = "quick"),  gcFirst = TRUE)[1];
}
sampleSize2 = L*35;
for(i in 1:sampleSize2){
  x = runif(sequenceLength, 0, 100); # Generate the sequence to be sorted
  time2[i] = system.time(x2 <- sort(x, method = "shell"), gcFirst = TRUE)[1];
}
print(t.test(time1, y=time2, paired = FALSE, var.equal = FALSE, conf.level = 1-alfa, , alternative = "less"))
# H0 zamitame ve prospech alternativy ze Singletonuv quicksort je rychlejsi s pravdepodobnosti chyby 4%
# jako alternativu jsme zvolili to cemu vice verime (nasi pracovni hypotezu)