#' @export
counts_for <- function(data,var){
  result <- data %>%
    group_by_(var) %>%
    summarize(count=sum(weight),count1=sum(wt1),count2=sum(wt2),count3=sum(wt3),count4=sum(wt4),count5=sum(wt5),count6=sum(wt6),count7=sum(wt7),count8=sum(wt8))
  return(result)
}

#' @export
percent_for <- function(data,var){
  result <- data %>% mutate(p=count/sum(count),p1=count1/sum(count1),p2=count2/sum(count2),p3=count3/sum(count3),p4=count4/sum(count4),p5=count5/sum(count5),p6=count6/sum(count6),p7=count7/sum(count7),p8=count8/sum(count8)) %>% select(var,p,p1,p2,p3,p4,p5,p6,p7,p8)
  return(result)
}

#' @export
raw_percent_for <- function(data,var){
  if ("year" %in% names(data)) {
    return(
      rbind(
        data %>% filter(year==2006) %>% counts_for(var) %>% percent_for(var) %>% mutate(year=2006),
        data %>% filter(year==2011) %>% counts_for(var) %>% percent_for(var) %>% mutate(year=2011)
      )
    )
  } else {
    return(data %>% counts_for(var) %>% percent_for(var))
  }
}

#' @export
for_box_plot <- function(data,var) {
  return(data %>% gather_(key="run",value=var,c("p","p1","p2","p3","p4","p5","p6","p7","p8")))
}

#' @export
cases_summary_text <- function(dat){
  weighted_cases=dat %>% summarize(count=sum(weight),count1=sum(wt1),count2=sum(wt2),count3=sum(wt3),count4=sum(wt4),count5=sum(wt5),count6=sum(wt6),count7=sum(wt7),count8=sum(wt8))
  return(paste0("Total: average=",round(mean(as.numeric(weighted_cases))),", min=",round(min(as.numeric(weighted_cases)))," max=",round(max(as.numeric(weighted_cases)))," (",nrow(dat)," cases)"))
  return(text)
}

#' @export
pob_names <- list(
  "1"="Canada",
  "2"="United States",
  "3"="Central America",
  "4"="Jamaica",
  "5"="Other Caribbean and Bermuda",
  "6"="South America",
  "7"="United Kingdom",
  "8"="Germany",
  "9"="Other Northern and Western Europe",
  "10"="Poland",
  "11"="Other Eastern Europe",
  "12"="Italy",
  "13"="Portugal",
  "14"="Other Southern Europe",
  "15"="Eastern Africa",
  "16"="Northern Africa",
  "17"="Other Africa",
  "18"="West Central Asia and the Middle East",
  "19"="China",
  "20"="Hong Kong, Special Administrative Region",
  "21"="Other Eastern Asia",
  "22"="Philippines",
  "23"="Other Southeast Asia",
  "24"="India",
  "25"="Pakistan",
  "26"="Other Southern Asia",
  "27"="Oceania and others",
  "88"="Not available"
)

#' @export
lico_at_names <- list(
  "1"="Not in LICO-AT",
  "2"="LICO-AT",
  "8"="Unavalailable"
)

#' @export
immstat_names_2011 <- list(
  "1"="Non-immigrants",
  "2"="Immigrants",
  "3"="Non-permanent residents"
)
#' @export
immstat_names_2006 <- list(
  "1"="Non-permanent residents",
  "2"="Non-immigrants",
  "3"="Immigrants"
)

#' @export
tenur_names <- list(
  "1"="Owner Household",
  "2"="Renter or Band Household",
  "8"="Unavalailable",
  "9"="Unavalailable"
)

#' @export
agegrp_names <- list(
  "1"="0 to 4 years",
  "2"="5 to 6 years",
  "3"="7 to 9 years",
  "4"="10 to 11 years",
  "5"="12 to 14 years",
  "6"="15 to 17 years",
  "7"="18 to 19 years",
  "8"="20 to 24 years",
  "9"="25 to 29 years",
  "10"="30 to 34 years",
  "11"="35 to 39 years",
  "12"="40 to 44 years",
  "13"="45 to 49 years",
  "14"="50 to 54 years",
  "15"="55 to 59 years",
  "16"="60 to 64 years",
  "17"="65 to 69 years",
  "18"="70 to 74 years",
  "19"="75 to 79 years",
  "20"="80 to 84 years",
  "21"="85 years and over",
  "88"="Unavalailable"
)

#' @export
mob5_names <- list(
  "1"="Non-movers",
  "2"="Non-migrants",
  "3"="Different CSD, same census division",
  "4"="Different CD, same province",
  "5"="Interprovincial migrants",
  "6"="External migrants",
  "9"="Not Applicable"
)

#' @export
mode_names <- list(
  "1"="Bicycle",
  "2"="Car, truck, van as driver",
  "3"="Motorcycle",
  "4"="Other mode",
  "5"="Car, truck, van as passenger",
  "7"="Public transit",
  "8"="Walked",
  "9"="Not Applicable"
)

#' @export
cma_names <- list(
  "205"= "Halifax",
  "399"= "Moncton – Saint John",
  "421"= "Québec",
  "462"= "Montréal",
  "499"= "Sherbrooke – Trois-Rivières",
  "505"= "Ottawa – Gatineau",
  "532"= "Oshawa",
  "535"= "Toronto",
  "537"= "Hamilton",
  "539"= "St. Catharines – Niagara",
  "541"= "Kitchener – Cambridge – Waterloo",
  "555"= "London",
  "559"= "Windsor",
  "577"= "Brantford – Guelph – Barrie",
  "588"= "Kingston – Peterborough",
  "599"= "Greater Sudbury / Grand Sudbury – Thunder Bay",
  "602"= "Winnipeg",
  "799"= "Regina – Saskatoon",
  "825"= "Calgary",
  "835"= "Edmonton",
  "933"= "Vancouver",
  "935"= "Victoria",
  "988"= "Kelowna – Abbotsford",
  "999"= "Other census metropolitan areas, census agglomerations and other geographies"
)

#' @export
citizen_names <- list(
  "1" = "Canada, by birth",
  "2" = "Canada, by naturalization",
  "3" = "Not a Canadian citizen",
  "8" = "Not available"
)

#' @export
citoth_name = list(
  "1" = "United States",
  "2" = "Other Americas",
  "3" = "Europe",
  "4" = "Africa",
  "5" = "West Central Asia and the Middle East",
  "6" = "Eastern Asia",
  "7" = "Southeast Asia",
  "8" = "Southern Asia",
  "9" = "Oceania and other",
  "10" = "Two other countries",
  "88" =  "Not available",
  "99" = "No other country of citizenship"
)


#' @export
hhinc_name = list(
  "1" = "Under $2,000",
  "2" = "$2,000 to $4,999",
  "3" = "$5,000 to $6,999",
  "4"	= "$7,000 to $9,999",
  "5"	= "$10,000 to $11,999",
  "6" = "$12,000 to $14,999",
  "7" = "$15,000 to $16,999",
  "8" = "$17,000 to $19,999",
  "9"	= "$20,000 to $24,999",
  "10" = "$25,000 to $29,999",
  "11" = "$30,000 to $34,999",
  "12" = "$35,000 to $39,999",
  "13" = "$40,000 to $44,999",
  "14" = "$45,000 to $49,999",
  "15" = "$50,000 to $54,999",
  "16" = "$55,000 to $59,999",
  "17" = "$60,000 to $64,999",
  "18"="$65,000 to $69,999",
  "19"="$70,000 to $74,999",
  "20"	="$75,000 to $79,999",
  "21"	="$80,000 to $84,999",
  "22"="$85,000 to $89,999",
  "23"="$90,000 to $94,999",
  "24"="$95,000 to $99,999",
  "25"="$100,000 to $109,999",
  "26"="$110,000 to $119,999",
  "27"="$120,000 to $129,999",
  "28"	="$130,000 to $139,999",
  "29"="$140,000 to $149,999",
  "30"	="$150,000 to $174,999",
  "31"="$175,000 to $199,999",
  "32"	="$200,000 to $249,999",
  "33"="$250,000 and over",
  "88" = "Not available"
)


#' @export
data_for_year <- function(year, cma_code=NA){
  data <- read_csv(paste0(getOption("custom_data_path"),"pumf/CA_",year,"_individual.csv"))
  if (!is.na(cma_code)) {
    data <- data %>% filter(cma==cma_code)
  }
  return(data %>% mutate(year=year))
}



