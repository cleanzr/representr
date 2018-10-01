#' 500 records suitable for record linkage with additional regression variables
#'
#' Simulated datasets containing the name, birthdate, and additional attributes of 500 records,
#' of which there are 350 unique individuals.
#'
#'
#' There is a known relationship between three of the
#' variables in the dataset, blood pressure (bp), income, and sex.
#' \deqn{bp = 160 + 10I(sex = "M") - income + 0.5 income*I(sex = "M") + \epsilon}
#' where \eqn{\epsilon ~ Normal(0, \sigma^2)} and \eqn{\sigma = 1, 2, 5}.
#'
#' The 150 duplicated records have randomly generated errors.
#'
#' @format
#'
#' \code{rl_reg1} and \code{rl_reg5} are data frames with 500 rows and 9 columns. Each row represents 1 records
#' with the following columns:
#' \describe{
#'     \item{fname}{First name}
#'     \item{lname}{Last name}
#'     \item{bm}{Birth month (numeric)}
#'     \item{bd}{Birth day}
#'     \item{by}{Birth year}
#'     \item{sex}{Sex ("M" or "F")}
#'     \item{education}{Education level ("Less than a high school diploma", ""High school graduates, no college",
#'     "Some college or associate degree", "Bachelor's degree only", or "Advanced degree")}
#'     \item{income}{Yearly income (in 1000s)}
#'     \item{bp}{Systolic blood pressure}
#' }
#'
#' \code{identity.rl_reg1} and \code{identity.rl_reg5} are integer vectors indicating the true
#' record ids of the two datasets. Two records represent the same individual if and only if their
#' corresponding identity values are equal.
#'
#' \code{linkage.rl} contains the result of running 100,000 iterations of a record linkage model using
#' the package \code{blink}.
#'
#'
#' @source Names and birthdates generated with the ANU Online Personal Data Generator and Corruptor (GeCO) version 0.1 \url{https://dmm.anu.edu.au/geco/}.
"rl_reg1"

#' @rdname rl_reg1
"rl_reg2"

#' @rdname rl_reg1
"rl_reg5"

#' @rdname rl_reg1
"identity.rl_reg1"

#' @rdname rl_reg1
"identity.rl_reg2"

#' @rdname rl_reg1
"identity.rl_reg5"

#' @rdname rl_reg1
"linkage.rl"
