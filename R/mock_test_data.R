test_data <- function() {

  ########################
  # Demographics dataset #
  ########################

  # Number of rows
  n <- 100

  # Create content
  studyid_dm <- rep("1234-5678", n)
  domain <- rep("DM", n)
  subjid <- as.character(1e3 + 1:n)

  random_site_country <- sample(x = 1:5, size = n, replace = TRUE)
  siteid <- as.character(floor(runif(5, min = 100, max = 1000)))[random_site_country]
  country <- c("BEL", "NLD", "USA", "USA", "JPN")[random_site_country]

  usubjid_dm <- paste0(studyid_dm, "-", "1", siteid, subjid)

  rficdtc <- as.Date("2020-01-01") + sample(x = 1:100, size = n, replace = TRUE)
  rfxstdtc <- rficdtc + sample(x = 1:100, size = n, replace = TRUE)
  rfxendtc <- rfxstdtc + sample(x = 300:400, size = n, replace = TRUE)
  rfstdtc <- rfxstdtc
  rfendtc <- rfxendtc

  age <- sample(x = 30:70, size = n, replace = TRUE)
  ageu <- rep("YEARS", n)
  sex <- sample(x = c("F", "M"), size = n, replace = TRUE)
  arm <- sample(x = c("Drug 1", "Drug 2", "Placebo"), size = n, replace = TRUE)
  race <- sample(x = c("WHITE", "BLACK OR AFRICAN AMERICAN", "AMERICAN INDIAN OR ALASKA NATIVE"), size = n, replace = TRUE) # nolint

  # Gather content in one data frame
  dm_test <- data.frame(
    STUDYID = studyid_dm,
    DOMAIN = domain,
    USUBJID = usubjid_dm,
    SUBJID = subjid,
    RFSTDTC = rfstdtc,
    RFENDTC = rfendtc,
    RFXSTDTC = rfxstdtc,
    RFXENDTC = rfxendtc,
    RFICDTC = rficdtc,
    SITEID = siteid,
    AGE = age,
    AGEU = ageu,
    SEX = sex,
    ARM = arm,
    ACTARM = arm,
    COUNTRY = country,
    RACE = race
  )

  # Set labels
  attributes(dm_test$STUDYID)$label <- "Study Identifier"
  attributes(dm_test$DOMAIN)$label <- "Domain Abbreviation"
  attributes(dm_test$USUBJID)$label <- "Unique Subject Identifier"
  attributes(dm_test$SUBJID)$label <- "Subject Identifier for the Study"
  attributes(dm_test$RFSTDTC)$label <- "Subject Reference Start Date/Time"
  attributes(dm_test$RFENDTC)$label <- "Subject Reference End Date/Time"
  attributes(dm_test$RFXSTDTC)$label <- "Date/Time of First Study Treatment"
  attributes(dm_test$RFXENDTC)$label <- "Date/Time of Last Study Treatment"
  attributes(dm_test$RFICDTC)$label <- "Date/Time of Informed Consent"
  attributes(dm_test$SITEID)$label <- "Study Site Identifier"
  attributes(dm_test$AGE)$label <- "Age"
  attributes(dm_test$AGEU)$label <- "Age Units"
  attributes(dm_test$SEX)$label <- "Sex"
  attributes(dm_test$ARM)$label <- "Description of Planned Arm"
  attributes(dm_test$ACTARM)$label <- "Actual Arm"
  attributes(dm_test$COUNTRY)$label <- "Country"
  attributes(dm_test$RACE)$label <- "Race"

  # Rewrite data frame as tibble
  dm_test <- tibble::as_tibble(dm_test)





  ##########################
  # Adverse Events dataset #
  ##########################

  # Create content based on DM dataset
  num_ae <- sample(x = 0:5, size = n, replace = TRUE)
  studyid <- rep(studyid_dm, num_ae)
  domain <- rep("AE", sum(num_ae))
  usubjid <- rep(usubjid_dm, num_ae)
  arm <- rep(arm, num_ae)
  aeseq <- 1:sum(num_ae)
  aeterm <- sample(x = c("HEADACHE", "BACK PAIN", "FATIGUE", "NAUSEA"), size = sum(num_ae), replace = TRUE)
  aesev <- sample(x = c("MILD", "MODERATE", "SEVERE"), size = sum(num_ae), replace = TRUE)
  aeser <- rep("N", sum(num_ae))
  aeser[aesev == "SEVERE"] <- "Y"
  aerel <- sample(x = c("UNLIKELY RELATED", "POSSIBLY RELATED", "RELATED"), size = sum(num_ae), replace = TRUE)
  aestdtc <- rep(rfxstdtc, num_ae) + sample(x = 0:200, size = sum(num_ae), replace = TRUE)
  aeendtc <- aestdtc + sample(x = 0:200, size = sum(num_ae), replace = TRUE)
  aestdy <- sample(-50:200, size = sum(num_ae), replace = TRUE)

  # Gather content in one data frame
  ae_test <- data.frame(
    STUDYID = studyid,
    DOMAIN = domain,
    USUBJID = usubjid,
    ARM = arm,
    AESEQ = aeseq,
    AETERM = aeterm,
    AESEV = aesev,
    AESER = aeser,
    AEREL = aerel,
    AESTDTC = aestdtc,
    AEENDTC = aeendtc,
    AESTDY = aestdy
  )

  # Set labels
  attributes(ae_test$STUDYID)$label <- "Study Identifier"
  attributes(ae_test$DOMAIN)$label <- "Domain Abbreviation"
  attributes(ae_test$USUBJID)$label <- "Unique Subject Identifier"
  attributes(ae_test$ARM)$label <- "Description of Planned Arm"
  attributes(ae_test$AESEQ)$label <- "Sequence Number"
  attributes(ae_test$AETERM)$label <- "Reported Term for the Adverse Event"
  attributes(ae_test$AESEV)$label <- "Severity/Intensity"
  attributes(ae_test$AESER)$label <- "Serious Event"
  attributes(ae_test$AEREL)$label <- "Causality"
  attributes(ae_test$AESTDTC)$label <- "Start Date/Time of Adverse Event"
  attributes(ae_test$AEENDTC)$label <- "End Date/Time of Adverse Event"
  attributes(ae_test$AESTDY)$label <- "Study day of start of Adverse Event"

  # Rewrite data frame as tibble
  ae_test <- tibble::as_tibble(ae_test)



  ##########################
  # Disposition Events dataset #
  ##########################
  # Create content based on DM dataset
  num_ds <- sample(x = 0:4, size = n, replace = TRUE)
  studyid <- rep(studyid_dm, num_ds)
  domain <- rep("DS", sum(num_ds))
  usubjid <- rep(usubjid_dm, num_ds)
  dsdecod <- sample(x = c("RANDOMIZED", "COMPLETED", "FINAL LAB VISIT", "ADVERSE EVENT", "FINAL RETRIEVAL VISIT",
                          "STUDY TERMINATED BY SPONSOR", "SCREEN FAILURE", "WITHDRAWAL BY SUBJECT", "DEATH"),
                    size = sum(num_ds), replace = TRUE)
  dsstdtc <- rep(rficdtc, num_ds) + sample(x = 0:300, size = sum(num_ds), replace = TRUE)
  dsstdy <- as.numeric(dsstdtc - rep(rficdtc, num_ds))

  # Gather content in one data frame
  ds_test <- data.frame(
    STUDYID = studyid,
    DOMAIN = domain,
    USUBJID = usubjid,
    DSDECOD = dsdecod,
    DSSTDTC = dsstdtc,
    DSSTDY = dsstdy
  )

  # Set labels
  attributes(ds_test$STUDYID)$label <- "Study Identifier"
  attributes(ds_test$DOMAIN)$label <- "Domain Abbreviation"
  attributes(ds_test$USUBJID)$label <- "Unique Subject Identifier"
  attributes(ds_test$DSDECOD)$label <- "Standardized Disposition Term"
  attributes(ds_test$DSSTDTC)$label <- "Start Date/Time of Disposition Event"
  attributes(ds_test$DSSTDY)$label <- "Study Day of Start of Disposition Event"

  # Rewrite data frame as a tibble
  ds_test <- tibble::as_tibble(ds_test)

  dm_test <- dm_test |> dplyr::mutate(ARM = as.factor(.data[["ARM"]]),
                                      SEX = as.factor(.data[["SEX"]]),
                                      ACTARM = as.factor(.data[["ACTARM"]]),
                                      SITEID = as.factor(.data[["SITEID"]]))
  ds_test <- ds_test |> dplyr::mutate(DSSTDTC = as.Date(.data[["DSSTDTC"]]),
                                      DSDECOD = as.factor(.data[["DSDECOD"]]))

  ae_test <- ae_test |> dplyr::mutate(AESTDTC = as.Date(.data[["AESTDTC"]]))


  ##################################
  # Prepared Adverse Event dataset #
  ##################################
  ae_prepared_test <- dv.reportrate:::prepare_ae_data(dataset = ae_test,
                                                      subjid_var = "USUBJID",
                                                      date_var = "AESTDTC",
                                                      day_var = "AESTDY")

  ######################################
  # Prepared Disposition Event dataset #
  ######################################
  ds_prepared_test <- dv.reportrate:::prepare_ds_data(dataset = ds_test,
                                                      subjid_var = "USUBJID",
                                                      event_var = "DSDECOD",
                                                      entry_terms =  c("RANDOMIZED"),
                                                      exit_terms =  c("COMPLETED", "WITHDRAWAL BY SUBJECT", "DEATH"),
                                                      date_var = "DSSTDTC",
                                                      day_var = "DSSTDY")

  list(dm = dm_test, ae = ae_test, ds = ds_test,
       ae_prepared = ae_prepared_test, ds_prepared = ds_prepared_test)

}
