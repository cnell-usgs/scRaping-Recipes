#### FLAVORS OF 2019 ----------------------------------------------------------------------------------------------------------------------

## digesting the best recipes of the year from bon appetit 

## objectives
# scrape monthly recipe lists from website
# parse recipe info into useable data

#### PRELIMINARIES ------------------------------------------------------------------------------------------------------------------------

library(rvest);library(RSelenium) # scraping

## the selectorgadget chrome pluginmakes this process easier
# i use it to get the css codes for html_nodes etc
# https://selectorgadget.com/

#### PULL RECIPE DATA ---------------------------------------------------------------------------------------------------------------------

### NYT recipes best of 2019 - do later
NYT<-'https://cooking.nytimes.com/68861692-nyt-cooking/18142387-the-50-recipes-our-most-devoted-readers-loved-this-year'

### BON APPETIT
# for each month there was a list of the most popular recipes
# weblinks followed at similar format with the month name changed

# list of months
mos<-c('january','february','march','april','may','june','july','august','october','november')

## recipe list from every month
mos.link<-'https://www.bonappetit.com/gallery/most-popular-recipes-'
mos.link


## read page
# a function to read in the
read_page<-function(month){
  page.mo<-read_html(paste0(mos.link, month, '-2019'))
  
  ba.data<-data.frame(recipe = page.mo%>%html_nodes('.gallery-slide-caption__hed')%>%html_text(),
                      link= page.mo%>%html_nodes('.gallery-slide-caption__cta')%>% html_attr("href"),
                      mo = paste0(month))
  return(ba.data)
}


read_page(month='january')

# read links from all monthly best of pages
month.recipes<-lapply(mos,read_page)%>%bind_rows
month.recipes

# search sublists for ingredients
get_ing<-function(link){
  linkl<-as.character(link)
  ba.ing<-data.frame(ingredient = read_html(as.character(link))%>%html_nodes('.ingredients__text')%>%html_text()) # ingrediensts
  rec<-ba.data%>%filter(link == linkl)
  #ba.notes<-data.frame(ingredients = read_html(linkl)%>%
  #  html_nodes('p')%>%html_text())%>% # cooking description
  #  mutate(recipe = paste(rec$recipe))
  #ba.new<-ba.notes[-grep('GET THE MAGAZINE', ba.notes):-length(ba.notes)]
  return(ba.ing)
}

recipes.2019<-lapply(month.recipes$link, get_ing)%>%setNames(month.recipes$recipe)
str(recipes.2019)

recipes.2019$`Pan-Roasted Steak with Crispy Broccoli`
rec<-recipes.2019%>%bind_rows(.id='recipe')%>%
  mutate(ingredient_clean = gsub(',', '', str_remove_all(ingredient, "\\s*\\([^\\)]+\\)")))%>%
  mutate(units=word(ingredient_clean, 1, 2), food_1 = word(ingredient_clean, 3,5))
str(rec)
unique(rec$ingredient) #932 vs 1207
unique(rec$ingredient_clean) #919
food.list<-unique(rec$food_1) #300
food.list

## clean up ingredients
## extract measurements

veg.page<-'https://en.wikipedia.org/wiki/List_of_vegetables'
veg.html<-read_html(veg.page)%>%html_nodes('a')%>%html_attr('title')
veg.list<-na.omit(veg.html[34:305])
veg.list<-veg.list[c(-grep('Edit section:', veg.list), -grep('Binomial nomenclature', veg.list), -grep('Enlarge', veg.list), -grep('Santa Barbara County', veg.list))]
veg.list<-gsub('\\(bean\\)', '', gsub('\\(plant\\)', '', gsub('\\(vegetable\\)', '', veg.list)))
veg.list

## list of leafy vegetables
leaf.page<-'https://en.wikipedia.org/wiki/List_of_leaf_vegetables'
leaf.table<-read_html(leaf.page)%>%html_table(fill=TRUE)
leaf.list<-leaf.table[2]

## world cheeses
cheez.page<-'https://en.wikipedia.org/wiki/List_of_cheeses'
cheez.table<-read_html(cheez.page)%>%html_table(fill=FALSE, header=TRUE, trim=TRUE)
tabs<-cheez.table[1:65]%>%bind_rows()[,1:4]
cheez.list<-tabs[,1:4]
str(cheez.list)

View(rec)
write.csv(rec, 'ingredients.csv', row.names=FALSE)

ing<-read_excel('/Users/collnell/Dropbox/rstats/ingredients.xlsx')%>%
  filter(type != 'basic', !is.na(type))%>%
  filter(type %in% c('spice','sauce','heat','sour'))
unique(ing$foody)#259
unique(ing$type)#22
str(ing)

## number of recipes & months for each ingredient
ing.rank<-ing%>%
  group_by(type,foody)%>%
  summarize(rec_n=length(unique(recipe)))
#View(ing.rank)
ing.cast<-ing%>%dcast(recipe~foody)%>%column_to_rownames('recipe')
plotweb(ing.cast)


library(igraph)
library(bipartite)

ing.graph<-graph_from_incidence_matrix(ing.cast)
plot(ing.graph)

##################################################################################
rec<-rec%>%mutate(units = word(ingredient, 1, 2))
rec
unique(rec$units) #237

# use this to build list of units?
## pull measurements from here
# make list of measurements that are likely incorrect, need further cleaning
units<-unique(rec$units)

# spoonacular api access
api.host<-'spoonacular-recipe-food-nutrition-v1.p.rapidapi.com'
api.key<-'a003db0960mshb5246039509f151p1cda6djsn55e828dbb29c'

api_attrs_degree<-stringr::str_c(api.host)

parse_ing<-'https://api.spoonacular.com/recipes/parseIngredients'

api_attrs_degree_GET<-httr::POST(url = parse_ing)
api_attrs_degree_GET
httr::http_status(api_attrs_degree_GET)




