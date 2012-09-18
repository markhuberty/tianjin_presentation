## Code to plot the per-capita emissions levels
## against per-capita GDP

library(foreign)
library(gdata)
library(reshape)
library(ggplot2)
## Need a library here for read.xls


## load the data
setwd("~/Documents/Academia/Conferences/beijing_ndrc_Sept2012/presentation")

## Emissions from the CDIAC
em <- read.csv("./data/nation.1751_2007.csv", header=TRUE,
               na.strings="."
               )
em <- em[,c("nation", "year", "pc.em", "tot.em")]


## Per-capita GDP in current USD from the world bank
pcgdp <- read.csv("./data/pcgdp_real_2000usd_wb.csv",
                  header=TRUE
                  )
pcgdp <- pcgdp[,-2]
pcgdp <- melt(pcgdp, id.var="nation")

names(pcgdp) <- c("country", "year", "pcgdp")
pcgdp$year <- as.integer(gsub("X", "", pcgdp$year))



## Energy use per capita from the world bank
## in kg oil equivalent per person
eng <- read.csv("./data/wb_eng_per_capita_kgoeq.csv", header=TRUE)
eng <- melt(eng, id.var=c("Country.Name"))
names(eng) <- c("country", "year", "eng.pc.kgoe")
eng$year <- as.integer(gsub("X", "", eng$year))

em$nation <- tolower(em$nation)
em$nation[em$nation == "china (mainland)"] <- "china"
em$nation[em$nation == "united states of america"] <- "united states"

pcgdp$country <- tolower(pcgdp$country)
eng$country <- tolower(eng$country)

pcgdp.eng <- merge(eng,
                   pcgdp,
                   by.x=c("country", "year"),
                   by.y=c("country", "year")
                   )

df <- merge(em,
            pcgdp.eng,
            by.x=c("nation", "year"),
            by.y=c("country", "year"),
            )

df.list <- split(df, df$year)

cor.year <- sapply(df.list, function(x){

  cor.out <- cor.test(x$pcgdp, x$pc.em, use="pairwise.complete.obs")

  out <- c(cor.out$estimate, cor.out$p.value)

  return(out)
})



df.sub <- na.omit(df)
df.sub <- df.sub[df.sub$pcgdp > 0 & df.sub$pc.em > 0, ]
df.sub <- df.sub[order(df.sub$year),]

df.sub$pcgdp.quintile <- cut2(df.sub$pcgdp, g=5, levels.mean=TRUE)

em.gdp.plot <- ggplot(df.sub,
                      aes(x=pcgdp,
                          y=pc.em,
                          color=year,
                          group=year
                          )
                      ) +
  #geom_point(size=1, alpha=0.5)  +
  geom_path(aes(group=nation), color="darkblue", alpha=0.5) + 
  scale_x_log10("Per-capita GDP (2000 USD)") +
  scale_y_log10("Carbon emissions (MT per capita)") +
  stat_smooth(method = "loess", alpha=0.1) ## +


pdf("./figures/cor_pc_em_gdp.pdf")
print(em.gdp.plot)
dev.off()

eng.gdp.plot <- ggplot(df.sub[df.sub$year > 1970,],
                      aes(x=pcgdp,
                          y=eng.pc.kgoe,
                          color=year,
                          group=year
                          )
                      ) +
  geom_path(aes(group=nation), color="darkblue", alpha=0.5) + 
  scale_x_log10("Per-capita GDP (2000 USD)") +
  scale_y_log10("Energy consumption (kg oil eq. per capita)") +
  stat_smooth(method = "loess", alpha=0.1)##  +

pdf("./figures/cor_pc_eng_gdp.pdf")
print(eng.gdp.plot)
dev.off()

eng.em.plot <- ggplot(df.sub[df.sub$year > 1970,],
                      aes(x=eng.pc.kgoe,
                          y=pc.em,
                          color=log10(pcgdp),
                          group=year
                          )
                      ) +
  geom_path(aes(group=nation), size=2, alpha=0.5) + 
  scale_x_continuous("Per-capita energy consumption (kg. oil equiv.)") +
  scale_y_continuous("Carbon emissions (MT per capita)") +
  scale_colour_gradient("Log GDP per capita") 
 
print(eng.em.plot)

capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s,1,1)),
                  {s <- substring(s,2); if(strict) tolower(s) else s},
                             sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

countries.of.interest <- c("iceland", "china", "united states",
                           "denmark", "brazil", "india")
df.sub$is.iceland <- ifelse(df.sub$nation == "iceland", 1, 0)
df.sub$is.china <- ifelse(df.sub$nation == "china", 1, 0)
df.sub$is.us <- ifelse(df.sub$nation == "united states", 1, 0)
df.sub$is.denmark <- ifelse(df.sub$nation=="denmark", 1, 0)
df.sub$countries.of.interest <-
  ifelse(df.sub$nation %in% countries.of.interest,
         1,
         0
         )
df.sub$country.names.of.interest <-
  ifelse(df.sub$countries.of.interest == 1 &
         df.sub$year==1990,
         capwords(df.sub$nation),
         ""
         )
         


## Plot the emissions-energy consumption correlation
## by country since 1970; highlight countries of interest
eng.em.plot <- ggplot(df.sub[df.sub$year > 1970,],
                      aes(x=eng.pc.kgoe,
                          y=pc.em,
                          color=log10(pcgdp),
                          group=year
                          )
                      ) +
  geom_path(aes(group=nation),
            size=2,
            alph=0.5
            ) +
  scale_x_continuous("Per-capita energy consumption (kg. oil equiv.)") +
  scale_y_continuous("Carbon emissions (MT per capita)") +
  scale_colour_gradient("Log GDP per capita (USD 2000)", low="yellow", high="darkblue") +
  scale_alpha(legend=FALSE)

pdf("./figures/cor_em_eng_all.pdf")
print(eng.em.plot)
dev.off()

eng.em.plot <- ggplot(df.sub[df.sub$year > 1970,],
                      aes(x=eng.pc.kgoe,
                          y=pc.em,
                          color=log10(pcgdp),
                          group=year
                          )
                      ) +
  geom_path(aes(group=nation,
                alpha=countries.of.interest
                ),
            size=2
            ) +
  geom_text(aes(label=country.names.of.interest), color="black",
            hjust=0, vjust=0
            ) + 
  scale_x_continuous("Per-capita energy consumption (kg. oil equiv.)") +
  scale_y_continuous("Carbon emissions (MT per capita)") +
  scale_colour_gradient("Log GDP per capita (USD 2000)", low="yellow", high="darkblue") +
  scale_alpha(legend=FALSE)

pdf("./figures/cor_em_eng_bycountry.pdf")
print(eng.em.plot)
dev.off()




## And the map version
library(maps)
library(maptools)
library(sp)
library(RColorBrewer)

dk.wind.shp <- readShapePoly("./data/DK_45m_s50/dk_45m_s50.shp")
dk.wind.shp$LEGEND <- as.factor(c("500+", "500-400",
                                  "350-400", "300-350",
                                  "250-300", "200-250",
                                  "0-200"
                                  )
                                )


dk.wind.shp$LEGEND <- reorder.factor(dk.wind.shp$LEGEND,
                                     c(1,2,3,4,5,7,6)
                                     )

my.palette <- brewer.pal(7, "PuBu")


pdf("./figures/dk_wind_potential_45m.pdf")
spplot(dk.wind.shp,
       "LEGEND",
       col.regions=my.palette,
       cuts=6,
       col="transparent",
       xlim=c(8,12)## ,
       ## legendEntries=as.character(dk.wind.shp$LEGEND),
       ## fill=brewer.pal(7, "PuBu")## ,
       ## colorkey=FALSE
       )
dev.off()

#######



## Some china-specific code per Mark Levine's
## comments at the Paris IEA conference
## His point: GDP and Energy consumption
## growth not 1:1 correlated


test <- foreach(x=countries.of.interest, .combine=rbind) %do% {
  df.country <- df.sub[df.sub$nation ==  x,]
  df.country$eng.pc.norm <- df.country$eng.pc.kgoe /
    df.country$eng.pc.kgoe[df.country$year == 1981]
  df.country$pc.em.norm <-
    df.country$pc.em / df.country$pc.em[df.country$year == 1981]
  df.country$pcgdp.norm <-
    df.country$pcgdp / df.country$pcgdp[df.country$year == 1981]
  df.country$tot.em.norm <-
    df.country$tot.em / df.country$tot.em[df.country$year == 1981]

  return(df.country)

}  

## df.china$em.tot.norm <-
##   df.china$pcgdp / df.china$pcgdp[df.china$year == 1981]

df.china <- melt(df.china[,c("nation", "year", "pc.em.norm",
                             "pcgdp.norm", "eng.pc.norm", )],
                 id.var=c("nation", "year")
                 )
levels(df.china$variable) <- c("Per capita emissions",
                               "Per capita GDP",
                               "Per capita energy consumption"
                               )

plot.china.norm <- ggplot(df.china,
                          aes(x=year,
                              y=value,
                              group=variable,
                              colour=variable
                              )
                          ) +
  geom_line() +
  scale_x_continuous("Year") +
  scale_y_continuous("Per-capita measures, (1981 = 1)") +
  scale_colour_brewer("Meaures", palette="Dark2") +
  facet_wrap( ~ nation, scales="free")

print(plot.china.norm)


plot(df.china$eng.pc.norm ~ df.china$year,
     xlab="Year",
     ylab="Per-capita measure, 1981 = 1",
     main="China\n Per-capita wealth, emissions, and energy consumption",
     ylim=c(1,10)
     )
points(y=df.china$pc.em.norm, x=df.china$year,
       pch=2)
points(y=df.china$pcgdp.norm, x=df.china$year,
       pch=3)
legend("topleft",
       legend=c("Energy", "Emissions", "Real GDP"),
       pch=c(1,2,3)
       )
