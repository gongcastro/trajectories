#' Get participant-level data
#'
#' @param bvq_data A named list resulting from calling [get_bvq()]
#' @param longitudinal Should longitudinal data be included? If `"all"` (default), all responses (including repeated measures) are included. If `"no"`, participants with more than one responses to the questionnaire (regardless of the version) are excluded. If `"first"`, only the first response of each participant is included. If `"last"`, only the last response of each participant is included. If `"only"`, only responses with repeated measures are included.
#' @param age Numeric vector of length two indicating the minimum and maximum age of participants that will be included in the resulting dataset.
#' @param lp Character vector indicating the language profile (LP) of the participants that will be included in the resulting dataset. In takes `"Monolingual"`, `"Bilingual"`, and/or `"Other"` as values.
#' @param other_threshold Numeric value between 0 and 1 indicating the minimum exposure to a language other than Catalan or Spanish that a participant need to be exposed to to be excluded.
get_participants <- function(bvq_data,
                             longitudinal = "all",
                             age = c(12, 32),
                             lp = c("Monolingual", "Bilingual"),
                             other_threshold = 0.1) {
    
    participants <- bvq_data$logs |>
        dplyr::filter(
            completed,
            # get only short versions of the questionnaire
            grepl("Lockdown|Short", version),
            # get only data from complete questionnaire responses
            # rlang::.env makes sure we use the objects provided in the arguments
            # of the function, and not variables in the piped data frame
            lp %in% .env$lp,
            between(age, .env$age[1], .env$age[2]),
            sum(doe_catalan + doe_spanish) > .env$other_threshold,
            # make sure that degrees of exposure are between 0 and 1
            between(doe_spanish, 0, 1),
            between(doe_catalan, 0, 1),
            between(doe_others, 0, 1)
        ) |> 
        mutate(time = as.integer(time)) |> 
        get_longitudinal(longitudinal = longitudinal) |> 
        select(id, time, time_stamp = date_finished, list = version,
               age, sex, lp, doe_catalan, doe_spanish, edu_parent) |> 
        arrange(id)
    
    # export data
    save_files(participants,
               formats = "csv",
               folder = "data")
    
    return(participants)
}
