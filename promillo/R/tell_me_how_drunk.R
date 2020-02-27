library(checkmate)


#' Decide, whether hard drinks are present in a list of drinks
#'
#' @param drinks list of drinks
#'
#' @return boolean indicating whether hard drinks are present
hard_drinks <- function(drinks){
  drinks_unlisted <- unlist(drinks)
  "schnaps" %in% names(drinks_unlisted)
}

#' Check legal drinking age in Germany
#'
#' @param age numerical vector indicating the ages of the drinkers
#' @param drinks list of drinks
#'
#' @return no return, function issues a warning in case of illegal drinking
check_age <- function(age, drinks) {
  if (age < 16 | (16 <= age & age < 18 & hard_drinks(drinks))) warning("illegal")
}

#' Input sanitation for drinks
#'
#' @param drinks named integer vector containing drinks
#'
#' @return named integer vector of valid entries in drinks
check_drinks_input <- function(drinks) {
  valid_drinks = numeric()
  drinks_unlisted = unlist(drinks)
  known_drinks = c('hoibe', 'massn', 'wein', 'schnaps', 'pils')
  for (index in seq_along(drinks_unlisted)) {
    current_drink = names(drinks_unlisted)[index]
    if (is.null(current_drink)) {
      warning("drink without name!")
    } else {
      if (!(current_drink %in% known_drinks)) {
        warning(paste("unrecognized drink ", current_drink))
      } else {
        if (drinks_unlisted[current_drink] < 0) {
          warning(paste("strange number of drinks: ", drinks_unlisted[current_drink]), " ",
                  current_drink)
        } else {
          valid_drinks[current_drink] <- drinks_unlisted[[current_drink]]
        }
      }
    }
  }
  valid_drinks
}

#' Compute amount of alcohol contained in drinks
#'
#' @param drinks named integer vector containing drinks
#'
#' @return double denoting the amount of alcohol in grams contained in drinks
tell_me_alcohol_amount <- function(drinks) {
  amount <- 0
  possible_drinks = data.frame(c(500, 1000, 200, 40, 330),
                               c(0.06, 0.06, 0.11, 0.4, 0.05) ,
                               row.names = c('hoibe', 'massn', 'wein',
                                             'schnaps', 'pils'))
  colnames(possible_drinks) = c("amount", "alcohol_percentage")
  drinks_checked <- check_drinks_input(drinks)
  if (length(drinks_checked) == 0 | is.null(length(drinks_checked))) {
    return(amount)
  }
  for (index in 1:length(drinks_checked)) {
    current_drink = names(drinks_checked)[index]
      amount <- amount + as.numeric(drinks_checked[[index]]) * 0.8 *
        possible_drinks[current_drink, 'amount'] *
        possible_drinks[current_drink, 'alcohol_percentage']
  }
  amount
}

#' Compute amount of body water in a human
#'
#' @param age numeric type denoting the age
#' @param height numeric type denoting the height in cm
#' @param weight numeric type denoting the weight in kg
#' @param sex sex of person
#'
#' @return
tell_me_amount_body_water <- function(age, height, weight,
                                      sex = c("male", "female")){
  gender <- match.arg(tolower(sex), c("male", "female"))
  if (gender == "male") {
    return(2.447 - 0.09516 * age + 0.1074 * height + 0.3362 * weight)
  }
  if (gender == "female") {
    return(0.203 - 0.07 * age + 0.1069 * height + 0.2466 * weight)
  }
  stop("This program only supports two genders :(")
}

#' Computes distribution factor
#'
#' @param amount_body_water Amount of body water in kg
#' @param weight weight in kg
#'
#' @return distribution factor
tell_me_distribution_factor <- function(amount_body_water, weight){
  return(1.055 * amount_body_water / (0.8 * weight))
}

#' Computes blood alcohol level for a person
#'
#' @param alcohol_amount amount of alcohol consumed
#' @param weight weight in kg
#' @param distribution_factor distribution factor
#'
#' @return
tell_me_blood_alcohol_level <- function(alcohol_amount, weight,
                                        distribution_factor){
  return(alcohol_amount / (weight * distribution_factor))
}

#' Computes the current blood alcohol level based on drinking time
#'
#' @param blood_alcohol_level blood alcohol level at time of drinking
#' @param drinking_time POSIXct timestamps containing start and end of drinking
#'
#' @return blood alcohol level at the end of drinking
tell_me_current_blood_alcohol_level <- function(blood_alcohol_level,
                                                drinking_time){
  duration <- 0
  tryCatch(
    {
    start_time <- drinking_time[1]
    end_time <- drinking_time[length(drinking_time)]
    duration <- as.numeric(difftime(end_time, start_time, units = "hours"))
    }, error = function() {
      warning(paste("could not convert drinking time, is it properly formatted?",
      "Returning blood alcohol content at start of drinking"))
    }
  )
  if (duration > 1) {
    current_blood_alcohol_level <- blood_alcohol_level - (duration - 1) * 0.15
  } else current_blood_alcohol_level <- blood_alcohol_level
  return(max(0, current_blood_alcohol_level))
}

#' Computes blood alcohol level for a person
#'
#' @param age Age of person (numeric)
#' @param sex Sex of person ("male", "female")
#' @param height Height of person in cm
#' @param weight Weight of person in kg
#' @param drinking_time POSIXct timestamps containing start and end of drinking
#' @param drinks named integer vector containing drinks consumed
#'
#' @return blood alcohol level at the end of drinking
#' @export
tell_me_how_drunk <- function(age, sex = c("male", "female"), height, weight,
                              drinking_time, drinks) {
  check_age(age, drinks)
  alcohol_amount <- tell_me_alcohol_amount(drinks)
  amount_body_water <- tell_me_amount_body_water(age, height, weight, sex)
  distribution_factor <- tell_me_distribution_factor(amount_body_water, weight)
  blood_alcohol_level <- tell_me_blood_alcohol_level(alcohol_amount, weight, distribution_factor)
  current_alcohol_level <- tell_me_current_blood_alcohol_level(blood_alcohol_level, drinking_time)
  current_alcohol_level
}
