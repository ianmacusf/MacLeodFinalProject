#' @title StatScrape
#'
#' @description Displays the top stat leaders for a specified year and stat. Default number of players displayed is set to 30, but can also be changed via user input.
#'
#' @param year Year to be drawn from. Can choose any year between 2009-2019 in 4-digit format
#' @param stat Stat to be displayed. Can choose from "G," "A," "Pts," "PIM," & "PlusMinus"
#' @param topNum Number of players displayed. Default set to 30, but can be changed.
#'
#' @return dataframe
#'
#' @examples
#' StatScrape(2011, "G")
#' StatScrape(2018, "Pts", 50)
#'
#' @export

StatScrape <- function(year, stat, topNum = 30){
  #Saves year as a numeric to be used for conditional operations later
  yearNUM <- year

  #Generates string to be used in URL call
  Y <- year + 1

  Y <- Y - 2000

  year <- as.character(year)
  Y <- as.character(Y)

  tag_year <- paste(year, Y, sep = "-")

  #Creates strings of URL to be stitched together around the variable 'tag_year'
  tag1 <- "https://www.quanthockey.com/scripts/AjaxPaginate.php?cat=Season&pos=Players&SS="
  tag2 <- "&af=0&nat="
  tag3 <- "&st=reg&sort=Age&so=DESC&page="
  tag4 <- "&league=NHL&lang=en&rnd=30317571&dt=2&sd=undefined&ed=undefined"
  tag_i <- 1

  #Sets boolean variable to be used later for error catching
  bool <- TRUE

  #Strings are stitched together to create URL, which is then read by 'read_html'and assigned to 'webpage'
  url <- paste(tag1, tag_year, tag2, tag_year, tag3, tag_i, tag4, sep = "")
  webpage <- read_html(url)

  #The website the data is taken from features a different format for years 2009-2015 than it does for 2016-2019
  #If the user input year is between 2009-2015, these html nodes are used to scrape data
  if(yearNUM >= 2009 && yearNUM <= 2015){
    #'html_nodes' searches 'webpage' for specified html node that contains the data, then scrapes text using 'html_text.' Repeats process for each of the other stats.
    players_html <- html_nodes(webpage, ".qh-nowrap")
    players_data <- html_text(players_html)

    goals_html <- html_nodes(webpage, "td:nth-child(7)")
    goals_data <- html_text(goals_html)

    assists_html <- html_nodes(webpage, "td:nth-child(8)")
    assists_data <- html_text(assists_html)

    points_html <- html_nodes(webpage, "td:nth-child(9)")
    points_data <- html_text(points_html)

    PIM_html <- html_nodes(webpage, "td:nth-child(10)")
    PIM_data <- html_text(PIM_html)

    PLMI_html <- html_nodes(webpage, "td:nth-child(11)")
    PLMI_data <- html_text(PLMI_html)

    #The website separates the data by page with 50 observations per page. The maximum amount of pages is 20
    #This 'for loop' loops through up to 20 pages to pull the rest of the data and append it to what was found on the previous pages
    for(i in 2:20){
      url <- paste(tag1, tag_year, tag2, tag_year, tag3, i, tag4, sep = "")
      webpage <- read_html(url)

      players_html <- html_nodes(webpage, ".qh-nowrap")
      players_data <- append(players_data, html_text(players_html))

      goals_html <- html_nodes(webpage, "td:nth-child(7)")
      goals_data <- append(goals_data, html_text(goals_html))

      assists_html <- html_nodes(webpage, "td:nth-child(8)")
      assists_data <- append(assists_data, html_text(assists_html))

      points_html <- html_nodes(webpage, "td:nth-child(9)")
      points_data <- append(points_data, html_text(points_html))

      PIM_html <- html_nodes(webpage, "td:nth-child(10)")
      PIM_data <- append(PIM_data, html_text(PIM_html))

      PLMI_html <- html_nodes(webpage, "td:nth-child(11)")
      PLMI_data <- append(PLMI_data, html_text(PLMI_html))
    }
    #If the user input year is between 2016-2019, these html nodes are used to scrape data
  } else if(yearNUM >= 2016 && yearNUM <= 2019){
    #Scraping process follows same basic procedure as found above
    players_html <- html_nodes(webpage, ".qh-nowrap")
    players_data <- html_text(players_html)

    goals_html <- html_nodes(webpage, "td:nth-child(8)")
    goals_data <- html_text(goals_html)

    assists_html <- html_nodes(webpage, "td:nth-child(9)")
    assists_data <- html_text(assists_html)

    points_html <- html_nodes(webpage, "td:nth-child(10)")
    points_data <- html_text(points_html)

    PIM_html <- html_nodes(webpage, "td:nth-child(11)")
    PIM_data <- html_text(PIM_html)

    PLMI_html <- html_nodes(webpage, "td:nth-child(12)")
    PLMI_data <- html_text(PLMI_html)

    for(i in 2:20){
      url <- paste(tag1, tag_year, tag2, tag_year, tag3, i, tag4, sep = "")
      webpage <- read_html(url)

      players_html <- html_nodes(webpage, ".qh-nowrap")
      players_data <- append(players_data, html_text(players_html))

      goals_html <- html_nodes(webpage, "td:nth-child(8)")
      goals_data <- append(goals_data, html_text(goals_html))

      assists_html <- html_nodes(webpage, "td:nth-child(9)")
      assists_data <- append(assists_data, html_text(assists_html))

      points_html <- html_nodes(webpage, "td:nth-child(10)")
      points_data <- append(points_data, html_text(points_html))

      PIM_html <- html_nodes(webpage, "td:nth-child(11)")
      PIM_data <- append(PIM_data, html_text(PIM_html))

      PLMI_html <- html_nodes(webpage, "td:nth-child(12)")
      PLMI_data <- append(PLMI_data, html_text(PLMI_html))
    }
    #If user input year is not a year between 2009-2019, returns error message with instructions for further use
  } else{
    return("Invalid input. Must choose a year between 2009-2019.")
    #Sets boolean to FALSE. This will prevent function from proceeding to dataframe output
    bool <- FALSE
  }

  #If user input stat is not one of the provided stats, returns error message with instructions for further use
  if(stat != "G" & stat != "A" & stat != "Pts" & stat != "PIM" & stat != "PlusMinus"){
    #Sets boolean to FALSE. This will prevent function from proceeding to dataframe output
    bool <- FALSE
    return("Invalid input. 'G,' 'A,' 'Pts,' 'PIM,' and 'PlusMinus' are the only stats currently available.")
  }

  #If all user inputs are valid, boolean will remain TRUE, allowing function to proceed with output
  if(bool){
    #Dataframe created with each of the stats. Player names set to characters, stats set to numeric
    df <- data.frame(Player = players_data, Goals = goals_data, Assists = assists_data, Points = points_data, PIM = PIM_data, PlusMinus = PLMI_data, stringsAsFactors = FALSE)
    df$Player <- as.character(df$Player)
    df$Goals <- as.numeric(df$Goals)
    df$Assists <- as.numeric(df$Assists)
    df$Points <- as.numeric(df$Points)
    df$PIM <- as.numeric(df$PIM)
    df$PlusMinus <- as.numeric(df$PlusMinus)

    #Switch statement to sort and display dataframe based off user input stat
    switch(stat,
           "G" ={
             df <- subset(df, select = c(Player, Goals))
             df <- df[order(df$Goals, decreasing = TRUE),]
             rownames(df) <- NULL
             df <- df[1:topNum,]
             View(df)
           },
           "A" ={
             df <- subset(df, select = c(Player, Assists))
             df <- df[order(df$Assists, decreasing = TRUE),]
             rownames(df) <- NULL
             df <- df[1:topNum,]
             View(df)
           },
           "Pts" ={
             df <- subset(df, select = c(Player, Points))
             df <- df[order(df$Points, decreasing = TRUE),]
             rownames(df) <- NULL
             df <- df[1:topNum,]
             View(df)
           },
           "PIM" ={
             df <- subset(df, select = c(Player, PIM))
             df <- df[order(df$PIM, decreasing = TRUE),]
             rownames(df) <- NULL
             df <- df[1:topNum,]
             View(df)
           },
           "PlusMinus" ={
             df <- subset(df, select = c(Player, PlusMinus))
             df <- df[order(df$PlusMinus, decreasing = TRUE),]
             rownames(df) <- NULL
             df <- df[1:topNum,]
             View(df)
           },
           return("ERROR"))

  }
}
