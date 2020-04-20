library(tidyverse)

splitAt <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos)))

pokemonFormat <- function(x){
        x <- x[2:12,]
        x$value <- str_remove_all(string = x$value,"\t")
        x$value <- str_remove_all(string = x$value,"\\,")
        x %>% mutate(number = str_remove(string = x$value[1],pattern = "num: "),
                     name = str_remove(string = str_remove(string = x$value[2],pattern = "name: \\\""),pattern = "\\\""),
                     type = str_split(string = str_remove(string = str_remove(string = str_remove_all(string = str_remove(string = x$value[3],pattern = "types: "),pattern = "\""),"\\["),pattern = "\\]"),pattern = " ",n = Inf),
                     genderRatio = str_split(string = str_remove_all(str_remove(string = x$value[4],pattern = "genderRatio: "),"[{}MF:]"),pattern = "  "),
                     baseHP = str_remove(x$value[5],pattern = ))
}

setwd("C:/Users/tk59607/Desktop/pokemon-showdown-master/data")
pokedex <- enframe(readLines(con = "pokedex.js"))[5:16263,]
pokedex <- splitAt(x = pokedex$value,grep(pattern = "^\t}.$",x = pokedex$value,perl = T)+1)
pokedex <- lapply(pokedex, enframe)

