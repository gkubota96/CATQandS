# Load the static data

library(uuid)
library(stringr)
library(redux)
library(DBI)
library(RMariaDB)
library(base64enc)
library(mirtCAT)


assess_oids <- list()

assess_oids[[uuid::UUIDgenerate()]] <- list(
  id = 42,
  name = "danielandrade_pain",
  title = "Pain CAT",
  projects = c(${PROJ_ID}),
  overwrite = TRUE
)

assess_oids[[uuid::UUIDgenerate()]] <- list(
  id = 42,
  name = "danielandrade_sleep",
  title = "Sleep CAT",
  projects = c(${PROJ_ID}),
  overwrite = TRUE
)

assess_oids[[uuid::UUIDgenerate()]] <- list(
  id = 42,
  name = "danielandrade_qol",
  title = "QoL CAT",
  projects = c(${PROJ_ID}),
  overwrite = TRUE
)

# Redis as DB backend
reduxdb <- redux::hiredis()

mysqldb <- DBI::dbConnect(RMariaDB::MariaDB(),
  dbname = "${DBNAME}",
  username = "${DBSER}",
  password = "${DBPASS}",
  host = "${DBHOST}",
  unix.socket = "/var/run/mysqld/mysqld.sock"
)

# Load the dictionary listed above
# For each create the mirt object
for (asses_oid in names(assess_oids)) {

  asses_id <- assess_oids[[asses_oid]]$id
  asses_name <- assess_oids[[asses_oid]]$name
  asses_title <- assess_oids[[asses_oid]]$title
  asses_projects <- assess_oids[[asses_oid]]$projects
  asses_overwrite <- assess_oids[[asses_oid]]$overwrite

  data <- read.csv(paste0("dictionaries/", asses_name, ".csv"), stringsAsFactors = FALSE)
  nfactors <- sum(stringr::str_detect(names(data), "d([0-9])"))

  asses <- list()
  asses$name <- asses_name

  # Create the mirt object
  vars <- c("a1", paste0("d", 1:nfactors))
  pars <- data[vars]
  itemtype <- as.character(data[1, "itemtype"])
  asses$mo <- mirtCAT::generate.mirt_object(pars, itemtype)

  # Create the mirtCAT dictionary
  nfactors <- nfactors + 1
  vars <- c("question_label", paste0("level_", 1:nfactors))
  mcat_df <- data[vars]
  colnames(mcat_df) <- c("Question", paste0("Option.", 1:nfactors))
  mcat_df$Type <- "radio"
  asses$mcat_df <- mcat_df

  # Create the PROMIS questions strutcture
  promis_questions <- vector(mode = "list", length = nrow(data))
  for (idx in 1:nrow(data)) {

    valid_levels <- sum(!is.na(data[idx, paste0("level_", 1:nfactors)]))

    map <- vector(mode = "list", length = valid_levels)
    for (level in 1:valid_levels) {
      FormItemOID <- uuid::UUIDgenerate()

      map[[level]] <- list(
        ElementOID = uuid::UUIDgenerate(),
        Description = paste0("  ", as.character(data[idx, paste0("level_", level)])),
        FormItemOID = FormItemOID,
        ItemResponseOID = uuid::UUIDgenerate(),
        Value = data[idx, paste0("value_level_", level)],
        Position = level
      )
    }

    # If the question has an image attached
    # we encode it in base64
    question_label <- ifelse(is.na(data[idx, "image"]),
      data[idx, "question_label"],
      paste0(
        data[idx, "question_label"],
        "<br><br><img src='data:image/jpeg;base64,",
        base64enc::base64encode(file(paste0("dictionaries/", data[idx, "image"]), "rb")),
        "' width='130%'/>"
      )
    )

    promis_questions[[idx]] <- list(list(
      FormItemOID = FormItemOID,
      ID = data[idx, "question"],
      Order = idx,
      Elements = list(
        list(
          ElementOID = uuid::UUIDgenerate(),
          Description = question_label,
          ElementOrder = 1
        ),
        list(
          ElementOID = uuid::UUIDgenerate(),
          Description = "Container",
          ElementOrder = 2,
          Map = map
        )
      )
    ))
  }
  asses$promis_questions <- promis_questions

  # Save the object in Redis
  # If the object already exists then overwrite it
  asses_oid_redux <- paste0("asses-", asses_oid)
  reduxdb$SET(asses_oid_redux, redux::object_to_bin(asses))


  # Now we have to change the RedCap database to reflect the new adaptative instrument
  # instead of the one added from the Shared Library
  # So we do this from the group up directly in the database
  # Let's iterate over the list of projects, this is nice if we have the same assessment in multiple projects
  for (project_id in asses_projects) {

    if (asses_overwrite) {
      query_str <- paste0("DELETE FROM redcap_library_map WHERE project_id = ", project_id, " AND form_name = '", asses_name, "'")
      query <- DBI::dbSendQuery(mysqldb, query_str)
      DBI::dbClearResult(query)

      query_str <- paste0("DELETE FROM redcap_surveys WHERE project_id = ", project_id, " AND form_name = '", asses_name, "'")
      query <- DBI::dbSendQuery(mysqldb, query_str)
      DBI::dbClearResult(query)

      query_str <- paste0("DELETE FROM redcap_metadata WHERE project_id = ", project_id, " AND form_name = '", asses_name, "'")
      query <- DBI::dbSendQuery(mysqldb, query_str)
      DBI::dbClearResult(query)
    }

    query_str <- paste0("SELECT MAX(field_order) AS field_max FROM redcap_metadata WHERE project_id = ", project_id)
    query <- DBI::dbSendQuery(mysqldb, query_str)
    max_order <- DBI::dbFetch(query)
    max_order <- ifelse(all(is.na(max_order)), 1, as.integer(max_order))
    DBI::dbClearResult(query)

    max_order <- max_order + 1

    query_str <- paste0("INSERT INTO `redcap_metadata` ",
      "(project_id, field_name, form_name, form_menu_description, field_order, ",
      "element_preceding_header, element_type, element_label, element_validation_type, ",
      "element_validation_checktype, branching_logic, field_req, edoc_display_img, ",
      "grid_rank, video_display_inline) ",
      "VALUES (",
      project_id, ",",
      "'", asses_name, "_tscore',",
      "'", asses_name, "',",
      "'", asses_name, "',",
      max_order, ",",
      "'FINAL RESULTS',",
      "'text',",
      "'Theta',",
      "'float',",
      "'soft_typed',",
      "'[", asses_name, "_tscore] <> \\\'\\\'',",
      "0,",
      "0,",
      "0,",
      "0)"
    )
    query <- DBI::dbSendQuery(mysqldb, query_str)
    DBI::dbClearResult(query)

    max_order <- max_order + 1

    query_str <- paste0("INSERT INTO `redcap_metadata` ",
      "(project_id, field_name, form_name, field_order, element_type, element_label, ",
      "element_validation_type, element_validation_checktype, branching_logic, ",
      "field_req, edoc_display_img, grid_rank, video_display_inline) ",
      "VALUES (",
      project_id, ",",
      "'", asses_name, "_std_error',",
      "'", asses_name, "',",
      max_order, ",",
      "'text',",
      "'Standard Error',",
      "'float',",
      "'soft_typed',",
      "'[", asses_name, "_std_error] <> \\\'\\\'',",
      "0,",
      "0,",
      "0,",
      "0)"
    )
    query <- DBI::dbSendQuery(mysqldb, query_str)
    DBI::dbClearResult(query)

    # For each question add it to redcap_metadata
    for (idx in 1:nrow(data)) {

      max_order <- max_order + 1

      valid_levels <- sum(!is.na(data[idx, paste0("level_", 1:nfactors)]))

      # If the question has an image attached
      # we encode it in base64
      question_label <- ifelse(is.na(data[idx, "image"]),
        data[idx, "question_label"],
        paste0(
          data[idx, "question_label"],
          "<br><br><img src='data:image/jpeg;base64,",
          base64enc::base64encode(file(paste0("dictionaries/", data[idx, "image"]), "rb")),
          "' width='130%'/>"
        )
      )

      query_str <- paste0("INSERT INTO `redcap_metadata` ",
        "(project_id, field_name, form_name, field_order, element_preceding_header, ",
        "element_type, element_label, element_enum, branching_logic, ",
        "field_req, edoc_display_img, grid_rank, video_display_inline) ",
        "VALUES (",
        project_id, ",",
        "'promis_", data[idx, "question"], "',",
        "'", asses_name, "',",
        max_order, ",",
        "'", data[idx, "question"], "',",
        "'radio',",
        "'", data[idx, "question_label"], "',",
        "'", paste0(paste0(1:valid_levels, ", ", data[idx, paste0("level_", 1:valid_levels)]), collapse = " \\\\n "), "',",
        "'[promis_", data[idx, "question"], "_qposition] <> \\\'\\\'',",
        "0,",
        "0,",
        "0,",
        "0)"
      )
      query <- DBI::dbSendQuery(mysqldb, query_str)
      DBI::dbClearResult(query)

      max_order <- max_order + 1

      query_str <- paste0("INSERT INTO `redcap_metadata` ",
        "(project_id, field_name, form_name, field_order, element_type, element_label, ",
        "element_validation_type, element_validation_checktype, branching_logic, ",
        "field_req, edoc_display_img, grid_rank, video_display_inline) ",
        "VALUES (",
        project_id, ",",
        "'promis_", data[idx, "question"], "_tscore',",
        "'", asses_name, "',",
        max_order, ",",
        "'text',",
        "'Theta (Cumulative)',",
        "'float',",
        "'soft_typed',",
        "'[promis_", data[idx, "question"], "_qposition] <> \\\'\\\'',",
        "0,",
        "0,",
        "0,",
        "0)"
      )
      query <- DBI::dbSendQuery(mysqldb, query_str)
      DBI::dbClearResult(query)

      max_order <- max_order + 1

      query_str <- paste0("INSERT INTO `redcap_metadata` ",
        "(project_id, field_name, form_name, field_order, element_type, element_label, ",
        "element_validation_type, element_validation_checktype, branching_logic, ",
        "field_req, edoc_display_img, grid_rank, video_display_inline) ",
        "VALUES (",
        project_id, ",",
        "'promis_", data[idx, "question"], "_stderror',",
        "'", asses_name, "',",
        max_order, ",",
        "'text',",
        "'Standard Error (Cumulative)',",
        "'float',",
        "'soft_typed',",
        "'[promis_", data[idx, "question"], "_qposition] <> \\\'\\\'',",
        "0,",
        "0,",
        "0,",
        "0)"
      )
      query <- DBI::dbSendQuery(mysqldb, query_str)
      DBI::dbClearResult(query)

      max_order <- max_order + 1

      query_str <- paste0("INSERT INTO `redcap_metadata` ",
        "(project_id, field_name, form_name, field_order, element_type, element_label, ",
        "element_validation_type, element_validation_checktype, branching_logic, ",
        "field_req, edoc_display_img, grid_rank, video_display_inline) ",
        "VALUES (",
        project_id, ",",
        "'promis_", data[idx, "question"], "_qposition',",
        "'", asses_name, "',",
        max_order, ",",
        "'text',",
        "'Question Position',",
        "'int',",
        "'soft_typed',",
        "'[promis_", data[idx, "question"], "_qposition] <> \\\'\\\'',",
        "0,",
        "0,",
        "0,",
        "0)"
      )
      query <- DBI::dbSendQuery(mysqldb, query_str)
      DBI::dbClearResult(query)

    }

    max_order <- max_order + 1

    query_str <- paste0("INSERT INTO `redcap_metadata` ",
      "(project_id, field_name, form_name, field_order, element_preceding_header, ",
      "element_type, element_label, element_enum, field_req) ",
      "VALUES (",
      project_id, ",",
      "'", asses_name, "_complete',",
      "'", asses_name, "',",
      max_order, ",",
      "'Form Status',",
      "'select',",
      "'Complete?',",
      "'0, Incomplete \\\\n 1, Unverified \\\\n 2, Complete',",
      "0)"
    )
    query <- DBI::dbSendQuery(mysqldb, query_str)
    DBI::dbClearResult(query)

    query_str <- paste0("INSERT INTO `redcap_library_map` ",
      "(project_id, form_name, type, library_id, promis_key, scoring_type, battery) ",
      "VALUES (",
      project_id, ",",
      "'", asses_name, "',",
      "1,",
      asses_id, ",",
      "'", asses_oid, "',",
      "'EACH_ITEM',",
      "0)"
    )
    query <- DBI::dbSendQuery(mysqldb, query_str)
    DBI::dbClearResult(query)

    query_str <- paste0("UPDATE redcap_projects SET surveys_enabled = 1 WHERE project_id = ", project_id)
    query <- DBI::dbSendQuery(mysqldb, query_str)
    DBI::dbClearResult(query)

    query_str <- paste0("INSERT INTO `redcap_surveys` ",
      "(project_id, form_name, title, instructions, acknowledgement, ",
      "question_auto_numbering, end_survey_redirect_next_survey)",
      "VALUES (",
      project_id, ",",
      "'", asses_name, "',",
      "'", asses_title, "',",
      "'<p><strong>Please complete the survey below.</strong></p>',",
      "'<p><strong>Thank you for taking the survey.</strong></p>',",
      "0,",
      "0)"
    )
    query <- DBI::dbSendQuery(mysqldb, query_str)
    DBI::dbClearResult(query)
  }
}



reduxdb$BGSAVE()

DBI::dbDisconnect(mysqldb)
