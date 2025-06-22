# API

library(uuid)
library(stringr)
library(redux)
library(mirt)
library(mirtCAT)


# Redis as DB backend
reduxdb <- redux::hiredis()


#* RedCap asks for the registration id the first time it uses the PROMIS API, so this is just bogus
#* @post /2012-01/Registration/
function(res){
  res$body <- "RegistrationOID\n42\nToken\n42"
  return(res)
}



#* Returns a token to access the assesment requested
#* Reference: https://www.assessmentcenter.net/ac_api/2012-01/Docs/Assessment
#* @serializer unboxedJSON
#* @post /2012-01/Assessments/<OID>.json
function(OID, UID = "", Expiration = 1, res) {
  cat("POST - /2012-01/Assesments/\n")
  # Check if OID is in the assess lists
  asses_oid_redux <- paste0("asses-", OID)
  if (reduxdb$EXISTS(asses_oid_redux) == 1) {
    asses <- redux::bin_to_object(reduxdb$GET(asses_oid_redux))

    df <- asses$mcat_df
    mo <- asses$mo
    name <- asses$name

    # Create an individual mirtCAT environment for the participant
    mcat <- mirtCAT::mirtCAT(df, mo,
      design_elements = TRUE,
      criteria = "MI",
      start_item = "MI"
    )

    # Populate redis with the participant information
    partic <- list()
    partic$asses_oid <- asses_oid_redux
    partic$asses_name <- name
    partic$mcat <- mcat
    partic$last_question <- NULL
    partic$nitems <- 0
    partic$result <- list()

    partic_oid <- uuid::UUIDgenerate()
    partic_oid_redux <- paste0("partic-", partic_oid)
    reduxdb$SETEX(partic_oid_redux, 86400, redux::object_to_bin(partic))

    response <- list(
      OID = partic_oid,
      UID = UID,
      Expiration = strftime(Sys.time() + 86400, format = "%m/%d/%Y %I:%M %p")
    )
    return(response)

  } else {
    res$status <- 404
    res$body <- "Assessment not found"
    return(res)
  }
}



#* Returns the current question determined by the IRT algorithm
#* Reference: https://www.assessmentcenter.net/ac_api/2012-01/Docs/Participant
#* @serializer unboxedJSON
#* @post /2012-01/Participants/<OID>.json
function(OID, ItemResponseOID = NULL, Response = NULL, Persist = NULL) {
  cat("POST - /2012-01/Participants/\n")
  # Check if OID is in the participants lists
  partic_oid_redux <- paste0("partic-", OID)
  if (reduxdb$EXISTS(partic_oid_redux) == 1) {
    partic <- redux::bin_to_object(reduxdb$GET(partic_oid_redux))

    assess_oid_redux <- as.character(partic$asses_oid)
    asses <- redux::bin_to_object(reduxdb$GET(assess_oid_redux))

    mcat <- partic$mcat

    # If it is not the first question
    # Update the mirtCAT design with the answer to the previous question
    # Also keep the answer history so that the Results endpoint can return it
    if (!is.null(partic$last_question)) {

      # Change the answer to the nearest one that changes the Theta
      last_question <- as.integer(partic$last_question)
      answer <- as.integer(Response)
      n_answers <- length(asses$promis_questions[[last_question]][[1]]$Elements[[2]]$Map)
      if (answer == n_answers) {
        answer <- answer - 1
      }

      mcat <- mirtCAT::updateDesign(
        mcat,
        new_item = last_question,
        new_response = answer,
        updateTheta = TRUE
      )

      theta <- as.numeric(mirtCAT::extract.mirtCAT(mcat$person, "thetas"))
      theta_se <- as.numeric(mirtCAT::extract.mirtCAT(mcat$person, "thetas_SE"))

      partic$nitems <- partic$nitems + 1

      partic$result <- append(x = partic$result, after = 0,
        values = list(list(
          FormItemOID = asses$promis_questions[[partic$last_question]][[1]]$FormItemOID,
          ItemResponseOID = ItemResponseOID,
          ID = asses$promis_questions[[partic$last_question]][[1]]$ID,
          Position = as.character(partic$nitems),
          StdError = as.character(theta_se),
          Theta = as.character((theta - 50) / 10),
          Elements = asses$promis_questions[[partic$last_question]][[1]]$Elements
        ))
      )

    } else {
      theta <- 0
      theta_se <- 1
    }

    # Find the next item
    next_item <- mirtCAT::findNextItem(mcat, criteria = "MI")
    partic$last_question <- next_item
    partic$last_theta <- theta
    partic$mcat <- mcat
    reduxdb$SETEX(partic_oid_redux, 86400, redux::object_to_bin(partic))

    if (theta_se <= 0.3 || is.na(next_item)) {
      response <- list(
        DateFinished = strftime(Sys.time(), format = "%m/%d/%Y %I:%M %p"),
        Items = list()
      )
    } else {
      response <- list(
        DateFinished = "",
        Items = asses$promis_questions[[next_item]]
      )
    }

  } else {
    response <- list(Error = "No data found.")
  }
  return(response)
}

#* Returns the assessment score
#* Reference: https://www.assessmentcenter.net/ac_api/2012-01/Docs/Score
#* @serializer unboxedJSON
#* @post /2012-01/Results/<OID>.json
function(OID, ItemResponseOID = NULL, Response = NULL, Persist = NULL) {
  cat("POST - /2012-01/Results/\n")
  # Check if OID is in the participants lists
  partic_oid_redux <- paste0("partic-", OID)
  if (reduxdb$EXISTS(partic_oid_redux) == 1) {
    partic <- redux::bin_to_object(reduxdb$GET(partic_oid_redux))
    response <- list(
      UID = "",
      Name = partic$asses_name,
      Items = partic$result
    )
  } else {
    response <- list(Error = "No data found.")
  }
  return(response)
}



#* Log some information about the incoming request
#* @filter logger
function(req){
  cat(as.character(Sys.time()), "-",
    req$REQUEST_METHOD, req$PATH_INFO, "-", req$QUERY_STRING, "-",
    req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "-", req$postBody, "\n"
  )
  plumber::forward()
}
