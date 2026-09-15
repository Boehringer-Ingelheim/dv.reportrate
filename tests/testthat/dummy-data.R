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
dm_dummy <- data.frame(
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
attributes(dm_dummy$STUDYID)$label <- "Study Identifier"
attributes(dm_dummy$DOMAIN)$label <- "Domain Abbreviation"
attributes(dm_dummy$USUBJID)$label <- "Unique Subject Identifier"
attributes(dm_dummy$SUBJID)$label <- "Subject Identifier for the Study"
attributes(dm_dummy$RFSTDTC)$label <- "Subject Reference Start Date/Time"
attributes(dm_dummy$RFENDTC)$label <- "Subject Reference End Date/Time"
attributes(dm_dummy$RFXSTDTC)$label <- "Date/Time of First Study Treatment"
attributes(dm_dummy$RFXENDTC)$label <- "Date/Time of Last Study Treatment"
attributes(dm_dummy$RFICDTC)$label <- "Date/Time of Informed Consent"
attributes(dm_dummy$SITEID)$label <- "Study Site Identifier"
attributes(dm_dummy$AGE)$label <- "Age"
attributes(dm_dummy$AGEU)$label <- "Age Units"
attributes(dm_dummy$SEX)$label <- "Sex"
attributes(dm_dummy$ARM)$label <- "Description of Planned Arm"
attributes(dm_dummy$ACTARM)$label <- "Actual Arm"
attributes(dm_dummy$COUNTRY)$label <- "Country"
attributes(dm_dummy$RACE)$label <- "Race"

# Rewrite data frame as tibble
dm_dummy <- tibble::as_tibble(dm_dummy)





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
ae_dummy <- data.frame(
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
attributes(ae_dummy$STUDYID)$label <- "Study Identifier"
attributes(ae_dummy$DOMAIN)$label <- "Domain Abbreviation"
attributes(ae_dummy$USUBJID)$label <- "Unique Subject Identifier"
attributes(ae_dummy$ARM)$label <- "Description of Planned Arm"
attributes(ae_dummy$AESEQ)$label <- "Sequence Number"
attributes(ae_dummy$AETERM)$label <- "Reported Term for the Adverse Event"
attributes(ae_dummy$AESEV)$label <- "Severity/Intensity"
attributes(ae_dummy$AESER)$label <- "Serious Event"
attributes(ae_dummy$AEREL)$label <- "Causality"
attributes(ae_dummy$AESTDTC)$label <- "Start Date/Time of Adverse Event"
attributes(ae_dummy$AEENDTC)$label <- "End Date/Time of Adverse Event"
attributes(ae_dummy$AESTDY)$label <- "Study day of start of Adverse Event"

# Rewrite data frame as tibble
ae_dummy <- tibble::as_tibble(ae_dummy)



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
ds_dummy <- data.frame(
   STUDYID = studyid,
   DOMAIN = domain,
   USUBJID = usubjid,
   DSDECOD = dsdecod,
   DSSTDTC = dsstdtc,
   DSSTDY = dsstdy
)

# Set labels
attributes(ds_dummy$STUDYID)$label <- "Study Identifier"
attributes(ds_dummy$DOMAIN)$label <- "Domain Abbreviation"
attributes(ds_dummy$USUBJID)$label <- "Unique Subject Identifier"
attributes(ds_dummy$DSDECOD)$label <- "Standardized Disposition Term"
attributes(ds_dummy$DSSTDTC)$label <- "Start Date/Time of Disposition Event"
attributes(ds_dummy$DSSTDY)$label <- "Study Day of Start of Disposition Event"

# Rewrite data frame as a tibble
ds_dummy <- tibble::as_tibble(ds_dummy)

dm_dummy <- dm_dummy |> dplyr::mutate(ARM = as.factor(.data[["ARM"]]),
                                      SEX = as.factor(.data[["SEX"]]),
                                      ACTARM = as.factor(.data[["ACTARM"]]),
                                      SITEID = as.factor(.data[["SITEID"]]))
ds_dummy <- ds_dummy |> dplyr::mutate(DSSTDTC = as.Date(.data[["DSSTDTC"]]),
                                      DSDECOD = as.factor(.data[["DSDECOD"]]))

ae_dummy <- ae_dummy |> dplyr::mutate(AESTDTC = as.Date(.data[["AESTDTC"]]))


##################################
# Prepared Adverse Event dataset #
##################################
ae_prepared_dummy <- dv.reportrate:::prepare_ae_data(dataset = ae_dummy,
                                                     subjid_var = "USUBJID",
                                                     date_var = "AESTDTC",
                                                     day_var = "AESTDY")

######################################
# Prepared Disposition Event dataset #
######################################
ds_prepared_dummy <- dv.reportrate:::prepare_ds_data(dataset = ds_dummy,
                               subjid_var = "USUBJID",
                               event_var = "DSDECOD",
                               entry_terms =  c("RANDOMIZED"),
                               exit_terms =  c("COMPLETED", "WITHDRAWAL BY SUBJECT", "DEATH"),
                               date_var = "DSSTDTC",
                               day_var = "DSSTDY")
