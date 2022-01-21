data = read.csv("C:/Users/MostafaMohamedIsmail/Desktop/master.csv", sep =",")
cleanedData=subset(data,select=-c(country.year,HDI.for.year))
median(cleanedData$suicides_no)
cleanedData=rename(cleanedData, "country"="ï..country","suicides(/100k.pop)"="suicides.100k.pop",
                   "gdp.peryear($)" = "gdp_for_year....","gdp.percapita($)" = "gdp_per_capita....")
cleanedData=mutate_at(cleanedData,c("country","sex", "age", "generation"), as.factor)
cleanedData=mutate(cleanedData,generation = factor(generation, levels = c("G.I. Generation", "Silent", "Boomers", "Generation X",
                                                                          "Millenials","Generation Z")))
cleanedData=mutate(cleanedData,age = factor(age, levels = c("5-14 years",
                                                            "15-24 years",
                                                            "25-34 years",
                                                            "35-54 years",
                                                            "55-74 years",
                                                            "75+ years")))
cleanedData=mutate(cleanedData,`gdp.peryear($)` = as.numeric(gsub(",","", `gdp.peryear($)`,fixed = T)))
library(countrycode)
cleanedData$continent <- countrycode(sourcevar = cleanedData$country,origin = "country.name",destination = "continent")
south_america <- c('Argentina', 'Brazil', 'Chile', 'Colombia', 'Ecuador', 'Guyana', 'Paraguay', 'Suriname', 'Uruguay')
cleanedData$continent[cleanedData$country %in% south_america] <- 'South America'
cleanedData$continent[cleanedData$continent=='Americas'] <- 'North America'

################################################################################
custom_theme <- hc_theme(
  colors = c('#5CACEE', 'green', 'red'),
  chart = list(
    backgroundColor = '#FAFAFA',
    plotBorderColor = "black"),
  xAxis = list(
    gridLineColor = "E5E5E5",
    labels = list(style = list(color = "#333333")),
    lineColor = "#E5E5E5",
    minorGridLineColor = "#E5E5E5",
    tickColor = "#E5E5E5",
    title = list(style = list(color = "#333333"))),
  yAxis = list(
    gridLineColor = "#E5E5E5",
    labels = list(style = list(color = "#333333")),
    lineColor = "#E5E5E5",
    minorGridLineColor = "#E5E5E5",
    tickColor = "#E5E5E5",
    tickWidth = 1,
    title = list(style = list(color = "#333333"))),
  title = list(style = list(color = '#333333', fontFamily = "Lato")),
  subtitle = list(style = list(color = '#666666', fontFamily = "Lato")),
  legend = list(
    itemStyle = list(color = "#333333"),
    itemHoverStyle = list(color = "#FFF"),
    itemHiddenStyle = list(color = "#606063")),
  credits = list(style = list(color = "#666")),
  itemHoverStyle = list(color = 'gray'))

sex_color <- c("#EE6AA7", "#87CEEB") # baby blue & pink

##########################################################################
###Q1

pie_sex <- cleanedData %>%
  select(sex, suicides_no, population) %>%
  group_by(sex) %>%
  summarise(sum_suicides = round((sum(suicides_no)/sum(population))*100000, 2))

# Create pie chart for sex.
highchart() %>%
  hc_add_series(pie_sex, hcaes(x = sex, y = sum_suicides, color = sex_color), type = "pie") %>%
  hc_tooltip(borderWidth = 1.5, headerFormat = "", pointFormat = paste("Gender:
  <b>{point.sex} ({point.percentage:.1f}%)</b> <br> Suicides per 100K: <b>{point.y}</b>")) %>%
  hc_title(text = "<b>Worldwide suicides by Gender</b>", style = (list(fontSize = '30px'))) %>%
  hc_subtitle(text = "1985-2016", style = (list(fontSize = '30px'))) %>%
  hc_plotOptions(pie = list(dataLabels = list(distance = 5, style = list(fontSize = 10)), size = 500)) %>%
  hc_add_theme(custom_theme)

########################################################################
worldsex <- cleanedData %>%
  group_by(sex) %>%
  summarise(sum_suicides = sum(`suicides(/100k.pop)`)) %>%
  arrange(-sum_suicides)
worldsex
y <- worldsex$sum_suicides
z <- worldsex$sex
piepercent <- round(100*y/sum(y), 1)
pie(y, labels = piepercent, main = "the percentage of men and women suicide globally",col = rainbow(length(y)))
legend("topright", c("Male","Female"), cex = 0.8,
       fill = rainbow(length(y)))

########################################################################
sex_tibble <- data %>%
  select(year, sex, suicides_no, population) %>%
  group_by(year, sex) %>%
  summarise(sum_suicides = round((sum(suicides_no)/sum(population))*100000, 2))

# Pick color for gender.
sex_color <- c("#EE6AA7", "#87CEEB") # baby blue & pink

# Create line plot.
highchart() %>%
  hc_add_series(sex_tibble, hcaes(x = year, y = sum_suicides, group = sex), type = "line", color = sex_color) %>%
  hc_tooltip( borderWidth = 1.5, headerFormat = "", pointFormat =
               paste("Year: <b>{point.x}</b> <br>","Gender: <b>{point.sex}</b><br>", "Suicides: <b>{point.y}</b>")) %>%
  hc_title(text = "Worldwide suicides by Gender") %>%
  hc_subtitle(text = "1985-2016") %>%
  hc_xAxis(title = list(text = "Year")) %>%
  hc_yAxis(title = list(text = "Suicides per 100K people"),
           allowDecimals = FALSE,
           plotLines = list(list(
             color = "black", width = 1, dashStyle = "Dash",
             label = list( style = list(color = 'black', fontSize = 11))))) %>%
  hc_add_theme(custom_theme)

########################################################################
########################################################################
#####Q2
library(ggplot2)
ggplot(cleanedData,aes(x=suicides_no,y=year,group=sex))+geom_point(aes(color=sex))

library(plotly)

p <- ggplot(cleanedData, aes(suicides_no, year))
p <-  p + geom_point(aes(shape = factor(sex),colour = factor(sex)))

ggplotly(p)

########################################################################
########################################################################
#####Q3


library(ggplot2)
ggplot(cleanedData , aes(x= `suicides(/100k.pop)`, y= continent, colour = year )) +geom_point() +facet_wrap(~year ,nrow=10)


library(ggplot2)
ggplot(cleanedData , aes(x= `suicides(/100k.pop)`, y= continent, colour = age )) +geom_count() +facet_wrap(~age ,nrow=2)


library(ggplot2)
ggplot(cleanedData , aes(x= `suicides(/100k.pop)`, y= continent, colour = generation )) +geom_dotplot()+facet_wrap(~generation ,nrow=2)


library(ggplot2)
ggplot(cleanedData , aes(x= `suicides(/100k.pop)`, y= continent, colour = sex )) +geom_count()+facet_wrap(~sex ,nrow=1)

########################################################################
########################################################################
#####Q4

country_bar_sex <- cleanedData  %>%
  select(country, sex, suicides_no, population) %>%
  group_by(country, sex) %>%
  summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2))

country_tibble <- cleanedData %>%
  select(country, suicides_no, population) %>%
  group_by(country) %>%
  summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2))

# Create bar chart of suicide by sex.
highchart() %>%
  hc_add_series(country_bar_sex, hcaes(x = country, y = suicide_capita, group = sex), type = "bar", color = sex_color)  %>%
  hc_tooltip(borderWidth = 1.5, pointFormat = paste("Gender:
  <b>{point.sex} ({point.percentage:.1f}%)</b> <br> Suicides per 100K: <b>{point.y}</b>")) %>%
  hc_legend(enabled = TRUE, colorByPoint = TRUE) %>%
  hc_title(text = "Suicides by country and gender") %>%
  hc_subtitle(text = "1985-2016") %>%
  hc_xAxis(categories = country_tibble$country,
           labels = list(step = 1),
           min = 0, max = 25,
           scrollbar = list(enabled = TRUE)) %>%
  hc_yAxis(title = list(text = "Percentage of total suicides")) %>%
  hc_plotOptions(bar = list(stacking = "percent",
                            pointPadding = 0, groupPadding = 0, borderWidth = 0.4)) %>%
  hc_add_theme(custom_theme)


########################################################################
########################################################################
#####Q5


# Create a tibble for continent and sex.
continent_sex_tibble <- cleanedData %>%
  select(generation, sex, suicides_no, population) %>%
  group_by(generation, sex) %>%
  summarize(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2))

# Create histogram of suicides by continent.
highchart() %>%
  hc_add_series(continent_sex_tibble, hcaes(x = generation, y = suicide_capita, group = sex), type = "column")  %>%
  hc_colors(colors = sex_color) %>%
  hc_title(text = "Suicides by continent and <b>Gender</b>", style = (list(fontSize = '14px'))) %>%
  hc_subtitle(text = "1985-2016") %>%
  hc_tooltip(borderWidth = 1.5, pointFormat = paste("Gender: <b> {point.sex} </b> <br> Suicides: <b>{point.y}</b>")) %>%
  hc_xAxis(categories = c("GI Generation", "Silent", "Boomers", "Generation X", "Millennials", "Generation Z"),
           labels = list(style = list(fontSize = 8))) %>%
  hc_yAxis(labels = list(style = list(fontSize = 10)),
           title = list(text = "Suicides per 100K people",
                        style = list(fontSize = 10)),
           plotLines = list(
             list(color = "black", width = 1, dashStyle = "Dash",

                  label = list( style = list(color = "black", fontSize = 6))))) %>%
  hc_legend(verticalAlign = 'top', enabled = FALSE) %>%
  hc_add_theme(custom_theme)

########################################################################
########################################################################
#####Q6


static

ggplot(cleanedData,aes(x= population/1000, y=`suicides(/100k.pop)`/1000))+geom_smooth( formula = y ~ x,method="lm")


intarctive


library(plotly)
c <- ggplot(cleanedData, aes(x = population/1000, y = `suicides(/100k.pop)`/1000)) + geom_smooth(formula = y ~ x,method="lm")
plotly::ggplotly(c)

########################################################################
########################################################################
#####Q7

worldsu <- cleanedData %>%
  group_by(year) %>%
  summarise(mediansuicides = median(suicides_no)) %>%
  arrange(-mediansuicides)
worldsu


library(ggplot2)
ggplot(worldsu , aes(x= year, y= mediansuicides ,colour =  mediansuicides )) +geom_count(aes(size=mediansuicides))+
  geom_line(color="Black")+scale_color_gradient(low="blue", high="red")



a = ggplot(worldsu , aes(x= year, y= mediansuicides ,colour =  mediansuicides  )) +geom_count(aes(size=mediansuicides))+
  geom_line(color="Gray")+scale_color_gradient(low="blue", high="red")
ggplotly(a)


########################################################################
########################################################################
#####Q8

country_tibble <- cleanedData %>%
  select(country, suicides_no, population) %>%
  group_by(country) %>%
  summarize(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2))

# Create interactive world map.
highchart() %>%
  hc_add_series_map(worldgeojson, country_tibble, value = "suicide_capita", joinBy = c('name','country'))  %>%
  hc_colorAxis(stops = color_stops()) %>%
  hc_title(text = "Suicides by Country") %>%
  hc_subtitle(text = "1985-2016") %>%
  hc_tooltip(borderWidth = 1.5, headerFormat = "", valueSuffix = " suicides (per 100K people)") %>%
  hc_add_theme(custom_theme)


########################################################################
########################################################################
#####Q9

country_gdp <- cleanedData  %>%
  select(country,`gdp.percapita($)`,suicides_no) %>%
  group_by(country ) %>%
  summarise(gdp = median(suicides_no))
country_gdp


# Create bar chart of suicide by sex.
highchart() %>%
  hc_add_series(country_gdp, hcaes(x = country, y =gdp), type ="bar")  %>%
  hc_tooltip(borderWidth = 1.5, pointFormat = paste(" Suicides per 100K: <b>{point.y}</b>")) %>%
  hc_legend(enabled = TRUE, colorByPoint = TRUE) %>%
  hc_title(text = "Suicides by country and gdp") %>%
  hc_subtitle(text = "1985-2016") %>%
  hc_xAxis(categories = country_gdp$country,labels = list(step = 1),min = 0, max = 25,scrollbar = list(enabled = TRUE)) %>%
  hc_yAxis(title = list(text = "median")) %>%
  hc_plotOptions(bar = list( pointPadding = 0, groupPadding = 0, borderWidth = 0.4)) %>%
  hc_add_theme(custom_theme)



########################################################################
########################################################################

library(plotly)
library(gapminder)
df <- cleanedData
fig <- df %>%
  plot_ly(
    x = ~`gdp.percapita($)`,
    y = ~cleanedData$`suicides(/100k.pop)`,
    size = 1,
    color = ~continent,
    frame = ~year,
    text = ~country,
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers'
  )
fig <- fig %>%
  layout(
    xaxis = list(
      type = "log"
    )
  )
fig <- fig %>%

  animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")
fig <- fig %>%

  animation_slider(currentvalue = list(prefix = "YEAR ", font = list(color="blue")))
fig



########################################################################
########################################################################
c<- ggplot(cleanedData, aes(fill=age, y=`suicides(/100k.pop)`/1000, x=continent)) + geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() +
  xlab("")+ theme_economist() +
  scale_color_economist()

plotly::ggplotly(c)


########################################################################
########################################################################




library(plotly)

fig <- cleanedData %>%
  plot_ly(type = 'violin')
fig <- fig %>%
  add_trace(x = ~continent[cleanedData$sex == 'male'],
    y = ~`suicides(/100k.pop)`[cleanedData$sex == 'male'],
    legendgroup = 'male',scalegroup = 'male',name = 'male',
    side = 'positive',box = list(visible = T),
    meanline = list(visible = T),color = I(" cyan"))
fig <- fig %>%
  add_trace(x = ~continent[cleanedData$sex == 'female'],
    y = ~`suicides(/100k.pop)`[cleanedData$sex == 'female'],
    legendgroup = 'female',scalegroup = 'female',name = 'female',
    side = 'negative',box = list(visible = T),meanline = list(visible = T),
    color = I("pink"))

fig <- fig %>%
  layout( xaxis = list( title = ""  ),yaxis = list(title = "",zeroline = F),
    violingap = 0, violingroupgap = 0, violinmode = 'overlay')

fig


########################################################################
########################################################################

datos_gapminder_map <- cleanedData %>%
  mutate(Name = recode_factor(country,
                              `Congo, Dem. Rep.`= "Dem. Rep. Congo",
                              `Congo, Rep.`= "Congo",
                              `Cote d'Ivoire`= "Côte d'Ivoire",
                              `Central African Republic`= "Central African Rep.",
                              `Yemen, Rep.`= "Yemen",
                              `Korea, Rep.`= "Korea",
                              `Korea, Dem. Rep.`= "Dem. Rep. Korea",
                              `Czech Republic`= "Czech Rep.",
                              `Slovak Republic`= "Slovakia",
                              `Dominican Republic`= "Dominican Rep.",
                              `Equatorial Guinea`= "Eq. Guinea"))
datos_gapminder_map %>%
  group_by(year) %>%
  e_chart(Name, timeline = TRUE) %>%
  e_map(`suicides(/100k.pop)`) %>%
  e_visual_map(min= 0, max= 200,type = 'piecewise') %>%
  e_title("Suicide rate  by country and year", left = "center") %>%
  e_tooltip(trigger = "item",formatter = e_tooltip_choro_formatter())






























