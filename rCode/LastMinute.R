# Load necessary libraries
library(rjson)
library(data.table)

# set your working directory - change as needed\

wd = "C:/Users/L.GonzalezMorales/Documents/GitHub/FIS4SDGs/"

dataSubDir = "data/unsd/2019.Q2.G.01/"

setwd(wd)


# load functions
source('rCode/f_read.tab2dataTable.r')
source('rCode/f_writeTable2tab.r')


#----------------------------------
# Read data
#----------------------------------

rc_countries <- read.tab2dataTable("globalResources/UNRC.txt")

metadata <- fromJSON(file="globalResources/metadata.json")


#---------------------------------
# Explore metadata
#---------------------------------
names(metadata[[1]])
names(metadata[[1]]$targets[[1]])
names(metadata[[1]]$targets[[1]]$indicators[[1]])
names(metadata[[1]]$targets[[1]]$indicators[[1]]$series[[1]])


#==========================
# get availability for each series
#=========================

g = metadata[[2]]

t = g$targets[[2]]

i = t$indicators[[2]]

s = i$series[[2]]

counter = 0
#options(warn=2)  -- to check what happens when there are warnings... use warn=1 afterwards
for( g in metadata) {
  goalCode = g$code
  goalLabel = g$labelEN
  goalDesc = g$descEN
  goalHex = g$hex
  goalRGB = g$rgb
  goalColorScheme = g$colorScheme
  for(t in g$targets){

    targetCode = t$code
    targetLabel = t$labelEN
    targetDesc = t$descEN

    for (i in t$indicators){

      indicatorCode = i$code
      indicatorReference = i$reference
      indicatorLabel = i$labelEN
      indicatorDesc = i$descEN
      indicatorTier = i$tier

      for (s in i$series){


        seriesCode = s$code
        seriesDesc = s$description
        seriesTags = s$tags
        release = s$release

        # Read csv file (if it exists):

        csvFile = paste("csv_Indicator_", indicatorReference, "_Series_", seriesCode, ".csv", sep="")
        print(csvFile)

        list.f = list.files(dataSubDir, pattern = "^csv", full.names = TRUE, ignore.case = TRUE)
        this.file = paste(dataSubDir,csvFile,sep="")
        fileFound = this.file %in% list.f

        if (fileFound){

          # Read csv file:

          data = read.tab2dataTable(paste(dataSubDir, csvFile, sep=""))


          # Keep only records pertaining to 130 RC countries:
          dataCT = data[data$geoAreaCode %in% rc_countries$refAreaCode, ]

          # Get latest year availalbe for each country in this series (slice with maximum timeliness)
          dataCT_max = dataCT[!is.na(latest_year),max(latest_year), by=geoAreaCode]
          names(dataCT_max) <- c( "geoAreaCode","LatestYear")

          # Calculate 'median timeliness' across countries for this series:
          median.latest.year =  median(dataCT_max$LatestYear, na.rm = TRUE)

          # Calculate number of RC countries that have data for this indicator
          n.recent = nrow(dataCT[latest_year > 2014,])
          n.old = nrow(dataCT[latest_year <= 2014,])
          n.na = nrow(dataCT[is.na(latest_year),])

          # Calculate distribution of timeliness by year for this series:
          h.1y.breaks = seq(1990,2020,1)
          h.1y.labels = paste("f",seq(1990,2020,1)[1:length(h.1y.breaks)-1],sep="")
          h.1y = hist(dataCT_max$LatestYear, breaks = h.1y.breaks-0.5, plot = FALSE)

          # Calculate distribution of timeliness by 5-year intervals for this series:
          h.5y.breaks = seq(1990,2020,5)
          h.5y.labels = paste("f",
                              h.5y.breaks[1:length(h.5y.breaks)-1],
                              "to",
                              h.5y.breaks[2:length(h.5y.breaks)]-1,
                              sep = "")
          h.5y = hist(dataCT_max$LatestYear, breaks = h.5y.breaks - 0.5, plot = FALSE)

          # Calculate number of countries with timeliness in 2015-2017 and 2018-2019
          h.recent.breaks = c(2015,2018,2020)
          h.recent.labels = paste("f",
                                  h.recent.breaks[1:length(h.recent.breaks)-1],
                                  "to",
                                  h.recent.breaks[2:length(h.recent.breaks)]-1,
                                  sep = "")
          h.recent = hist(dataCT_max$LatestYear[dataCT_max$LatestYear>min(h.recent.breaks)-0.5],
                          breaks = h.recent.breaks - 0.5,
                          plot = FALSE)

          # merge together counts for 1-year, 5-year and "recent" time intervals:

          counts = c(h.1y$counts, h.5y$counts, h.recent$counts)

          labels = c(h.1y.labels, h.5y.labels, h.recent.labels)

          test <- data.table(matrix(counts, nrow = 1))

          colnames(test) <- labels

          # calculate number of countries without any data available:
          test[, noData := 130 - (f1990to1994 + f1995to1999 + f2000to2004 + f2005to2009 + f2010to2014 + f2015to2019)]

          # add a column for 'median timeliness'
          test[, medianYear := median.latest.year]
          test[, n.recent := n.recent]
          test[, n.old := n.old]
          test[, n.na := n.na]

          test$noData
          test$medianYear

          # add columns for goal,target, indicator and series metadata
          test[, goalLabel:=goalLabel]
          test[, goalLabel:=goalLabel]
          test[, goalLabel:=goalLabel]
          test[, goalDesc:=goalDesc]
          test[, targetLabel:=targetLabel]
          test[, targetDesc := targetDesc]
          test[, indicatorLabel := indicatorLabel]
          test[, indicatorDesc := indicatorDesc]
          test[, indicatorTier := indicatorTier]
          test[, seriesCode := seriesCode]
          test[, seriesDesc:= seriesDesc]
          test[, goalHex := goalHex]
          test[, goalColorScheme1 := goalColorScheme[1]]
          test[, goalColorScheme2 := goalColorScheme[2]]
          test[, goalColorScheme3 := goalColorScheme[3]]
          test[, goalColorScheme4 := goalColorScheme[4]]
          test[, goalColorScheme5 := goalColorScheme[5]]
          test[, goalColorScheme6 := goalColorScheme[6]]
          test[, goalColorScheme7 := goalColorScheme[7]]
          test[, goalColorScheme8 := goalColorScheme[8]]

          if(counter == 0){
            availability <- test
          } else {
            availability <- rbind(availability, test)
          }

          counter =+ 1

        }

      }

    }

  }

}



#--------------------------------------------------
# Availability for RC countries
#--------------------------------------------------

availability <- data.table(availability, key="indicatorLabel")
test = availability[,list( noData = min(noData)), by = indicatorLabel ]

test = unique(merge(test, availability,
             by = c("indicatorLabel", "noData"),
             all.x = TRUE))

writeTable2tab(availability, "RC_Countries_Availability.txt")

writeTable2tab(test, "RC_Countries_MostAvailable.txt")

#---->>>>  Here it is necessary to manually select which series among most available <<<<----

#--------------------------------------------------
# List of all indicators
#--------------------------------------------------

v.goalLabel = NULL
v.goalDesc = NULL
v.goalHex = NULL
v.targetLabel = NULL
v.targetDesc = NULL
v.indicatorLabel = NULL
v.indicatorDesc = NULL
v.indicatorTier = NULL

for( g in metadata) {
  goalCode = g$code
  goalLabel = g$labelEN
  goalDesc = g$descEN
  goalHex = g$hex
  goalRGB = g$rgb
  goalColorScheme = g$colorScheme
  for(t in g$targets){

    targetCode = t$code
    targetLabel = t$labelEN
    targetDesc = t$descEN

    for (i in t$indicators){

      indicatorCode = i$code
      indicatorReference = i$reference
      indicatorLabel = i$labelEN
      indicatorDesc = i$descEN
      indicatorTier = i$tier

      print(i$reference)
      print(i$tier)

      v.goalLabel = c(v.goalLabel, goalLabel)
      v.goalDesc= c(v.goalDesc, goalDesc)

      v.goalHex = c(v.goalHex, goalHex)
      v.targetLabel = c(v.targetLabel, targetLabel)
      v.targetDesc = c(v.targetDesc, targetDesc)
      v.indicatorLabel = c(v.indicatorLabel, indicatorLabel)
      v.indicatorDesc = c(v.indicatorDesc, indicatorDesc)
      v.indicatorTier = c(v.indicatorTier, indicatorTier)
    }
  }
}

all.indicators = data.table(
  goalLabel = v.goalLabel,
  goalDesc = v.goalDesc,
  goalHex = paste("#",v.goalHex,sep = ""),
  targetLabel = v.targetLabel,
  targetDesc = v.targetDesc,
  indicatorLabel = v.indicatorLabel,
  indicatorDesc = v.indicatorDesc,
  indicatorTier = v.indicatorTier
)

#-------------------------------------------------------
# >>> this file is manually edited version of "RC_Countries_MostAvailable.txt" (column 'selected')

availability.selected = read.tab2dataTable("data/unsd/availability/Availability RC countries - selected series - 20190725b.txt")

availability.selected[, c("goalLabel","goalDesc",
                          "targetLabel", "targetDesc",
                          "indicatorDesc", "indicatorTier" ):=NULL]


#availability.selected[, c("goalHex",
#                          "goalColorScheme1","goalColorScheme2","goalColorScheme3",
#                          "goalColorScheme4","goalColorScheme5","goalColorScheme6",
#                          "goalColorScheme7","goalColorScheme8" ):=NULL]

availability.all = merge(all.indicators,
                          availability.selected[selected==1, ],
                          by = "indicatorLabel",
                          all = TRUE)

names(availability.all)

availability.all[is.na(noData), noData:= 130]

availability.all[, Data:= 130-noData]

availability.all[is.na(n.recent), n.recent := 0]
availability.all[is.na(n.old), n.old := 0]
availability.all[is.na(n.na), n.na := 0]


#==============================================================

TiersByGoal = dcast(availability.all, indicatorTier~goalLabel, value.var='Data', length)

TimelinessByGoal = dcast(availability.all, medianYear~goalLabel, value.var='Data', length)
names(TimelinessByGoal) = gsub(" ", "", names(TimelinessByGoal), fixed = TRUE)


availability.all[,s.recent := n.recent/(n.recent + n.old + n.na)]
availability.all[,s.old := n.old/(n.recent + n.old + n.na)]
availability.all[,s.na := n.na/(n.recent + n.old + n.na)]

availability.all[is.na(s.na),s.na := 1]
availability.all[s.na == 1 ,s.recent := 0]
availability.all[s.na == 1 ,s.old := 0]

availability.all[,s.recent] +
availability.all[,s.old] +
availability.all[,s.na]



# more than half is covered, and most are recent


coverageByGoal.recent = availability.all[s.na < 0.5 & s.recent > s.old, .N, by = goalLabel]
names(coverageByGoal.recent) = c("Goal", "recentCoverage")

# more than half is covered, but most are old

coverageByGoal.old = availability.all[s.na < 0.5  & s.recent <= s.old, .N, by = goalLabel]
names(coverageByGoal.old) = c("Goal", "oldCoverage")

# less than half is covered
coverageByGoal.na = availability.all[s.na>=0.5, .N, by = goalLabel]
names(coverageByGoal.na) = c("Goal", "noCoverage")

coverageByGoal = merge(merge(coverageByGoal.recent, coverageByGoal.old, by="Goal", all = TRUE),
                       coverageByGoal.na, by ="Goal", all = TRUE)



writeTable2tab(TiersByGoal, "TiersByGoal.txt")
writeTable2tab(TimelinessByGoal, "TimelinessByGoal.txt")
writeTable2tab(coverageByGoal, "coverageByGoal.txt")


for( g in metadata) {


  goalLabel2 = g$labelEN


  g.color = paste("#",g$hex, sep = "")

  x.label = TimelinessByGoal$medianYear

  x.label[is.na(x.label)] = 'n.a.'

  g.column = names(TimelinessByGoal)==gsub(" ", "", g$labelEN)

  g.x = TimelinessByGoal[,..g.column]

  svg(filename=paste("data/unsd/availability/timeliness_",g$labelEN,".svg", sep = ""),
      width=8,
      height=4)


barplot(unlist(g.x),
        width = 1,
        space = 0.1,
        col = c("#999999", rep(g.color, length(unlist(g.x))-1)),
        names.arg = x.label,
        cex.axis = 0.7,
        cex.names = 0.5,
        cex.lab = 0.7,
        cex.sub = 0.5,
        main = g$labelEN,
        sub = "*Median of most recent available year for each indicator",
        xlab = "Median timeliness*",
        ylab = "Number of indicators",
        border = NA)


cat("finisthed ",g$labelEN,"\n")

dev.off()

}
#==============================================================
plot(availability.all$Data,
     availability.all$medianYear,
     pch = 19
)

col2rgb("#08a0ff")

g = metadata[[3]]

for( g in metadata) {

    goalLabel2 = g$labelEN

    x.goal = availability.all[availability.all$goalLabel == goalLabel2,]
    x.goal = x.goal[nrow(x.goal):1,]

    n = nrow(x.goal)

    pages = ceiling(n/15)
    pages

    pagesize = ceiling(nrow(x.goal)/pages)
    pagesize

    x.colors = paste("#",x.goal[selected==1,list("ffffff",
                                       goalColorScheme1,
                                       goalColorScheme2,
                                       goalColorScheme3,
                                       goalColorScheme4,
                                       goalColorScheme5,
                                       goalColorScheme6,
                                       goalColorScheme7)
                                ][1,], sep = "")[8:1]

    for(p in 1:pages){

      page_select = rep(FALSE, n)
      page_select[ pagesize*(p-1)+(1:pagesize)] = TRUE
      page_select = page_select[1:n]

      print(page_select)

      svg(filename=paste("data/unsd/availability/",goalLabel2,"_",p,".svg", sep = ""),
          width=8,
          height=10)

      x = x.goal[page_select,list(f2018to2019,
                                  f2015to2017,
                                  f2010to2014,
                                  f2005to2009,
                                  f2000to2004,
                                  f1995to1999,
                                  f1990to1994,
                                  noData)]

      print(x)

      x.names =  x.goal[page_select,indicatorLabel]

      x.legend = c("2018 to 2019",
                   "2015 to 2017",
                   "2010 to 2014",
                   "2005 to 2009",
                   "2000 to 2004",
                   "1995 to 1999",
                   "1990 to 1994",
                   "No data")

      x.names.long = paste(x.names, x.goal[page_select,seriesDesc], sep=": ")

      x.medians = x.goal[page_select,medianYear]

      par(mar=c(8.1, 12.1, 5.1, 2.1), mgp=c(3, 1, 0), las=0)

      x.plot <- barplot(t(as.matrix(x)),
                        width = 1,
                        space = 0.25,
                        names.arg = x.names,
                        horiz = TRUE,
                        density = c( -1, -1, -1, -1, -1, -1,-1, 30),
                        las = 1,
                        col = x.colors,
                        border = NA,
                        xlim = c(0,170),
                        xlab = "Number of RC countries",
                        cex.axis = 0.8,
                        cex.names = 0.8,
                        cex = 0.8,
                        main = paste(goalLabel2, ": Data availability", sep = "" ),
                        axes = FALSE,
                        xpd = FALSE)


      text(y= x.plot-.25, x=140, pos=3,  x.medians, cex = 0.8, col = "#444444")

      mtext( "Median                  \ntimeliness:                  ",
             side = 3,
             line = 0,
             adj = 1,
             cex = 0.8)


      text(y= x.plot-.25, x=165, pos=3,  x.goal[page_select,indicatorTier], cex = 0.8, col = "#444444")

      mtext( "Tier: ",
             side = 3,
             line = 0,
             adj = 1,
             cex = 0.8)
             #y= max(x.plot)+0.75, x=140, pos=3, , cex = 0.8, col = "#444444")

      #mtext(text, side = 3, line = 0, outer = FALSE, at = NA,
      #      adj = NA, padj = NA, cex = NA, col = NA, font = NA, ...)

      axis(1, at = seq(0, 130, 5),
           labels = seq(0, 130, 5), tcl = -0.75, las = 2,
           cex.axis = 0.6, lwd.ticks = 0.25)


      legend("bottom", inset=c(0,-0.2 ), legend=x.legend, cex = 0.6,
             fill = x.colors, border = NA, horiz = TRUE,  xpd = TRUE, box.lwd = NA )

      #legend("bottom", inset=c(0,-0.1 ), legend=c(paste("---0.1---", nrow(x),"columns")), cex = 0.4,
      #       fill = x.colors[1], border = NA, horiz = TRUE,  xpd = TRUE, box.lwd = NA)
      #legend("bottom", inset=c(0,-0.2 ), legend=c(paste("---0.2---", nrow(x),"columns")), cex = 0.4,
      #       fill = x.colors[1], border = NA, horiz = TRUE,  xpd = TRUE, box.lwd = NA)
      #legend("bottom", inset=c(0,-0.3 ), legend=c(paste("---0.3---", nrow(x),"columns")), cex = 0.4,
      #       fill = x.colors[1], border = NA, horiz = TRUE,  xpd = TRUE, box.lwd = NA)
      #legend("bottom", inset=c(0,-0.4 ), legend=c(paste("---0.4---", nrow(x),"columns")), cex = 0.4,
      #       fill = x.colors[1], border = NA, horiz = TRUE,  xpd = TRUE, box.lwd = NA)
      #legend("bottom", inset=c(0,-0.5 ), legend=c(paste("---0.5---", nrow(x),"columns")), cex = 0.4,
      #       fill = x.colors[1], border = NA, horiz = TRUE,  xpd = TRUE, box.lwd = NA)

      lines(c(65,65),c(-2,max(x.plot)+1.2),lty = 2, col = "#333333", lwd = 1)
      print(c(0,nrow(x)*1.2 +max(x.plot)/nrow(x)*2))

      cat("finisthed ",goalLabel2,"\n")

      dev.off()

  }

}






