get_ProlificID <- function(session_name, exp) {
  file_path_questionnaire <- paste0(session_name, glue("/{exp}_"), session_name, "_questionnaire.csv")
  file_path_prolific <- paste0(session_name, glue("/{exp}_"), session_name, "_prolific.csv")
  
  prolific <- read_csv(file_path_prolific) %>% 
    dplyr::select(`Submission id`, `Participant id`) %>% 
    rename(`Participant External Session ID`=`Submission id`, ProlificID=`Participant id`)
  
  questionnaire <- read_csv(file_path_questionnaire) %>% 
    dplyr::select(`Participant Private ID`, `Participant External Session ID`) %>%
    left_join(prolific, by="Participant External Session ID") %>% 
    distinct() %>%
    filter(!is.na(`Participant Private ID`)) %>%
    dplyr::select(-`Participant External Session ID`)
  
  return(questionnaire)
}

process_pre_test_df <- function(df) {
  processed_pre <- df %>% 
    dplyr::select(`Participant Private ID`, Display, Response, contains('Spreadsheet: pre'), `randomiser-qxk8`, `Response Type`) %>% 
    unite(trial, `Spreadsheet: pre 1`:`Spreadsheet: pre 3`, na.rm=TRUE) %>% 
    left_join(session1_ID, by="Participant Private ID") %>% 
    filter(str_detect(Display, "pre")) 
  return(processed_pre)
}

process_session2_df <- function(df, exposure) {
  processed_session2 <- df %>%
    dplyr::select(`Participant Private ID`, Display, Response, `randomiser-qxk8`, `Response Type`, 
                  contains(glue('Spreadsheet: {exposure}')), contains('Spreadsheet: post')) %>%
    unite('trial', contains('Spreadsheet'), na.rm = TRUE) %>%
    filter(str_detect(Display, glue("post|{exposure}"))) %>%
    filter(`Response Type` == 'tooEarly') %>%
    left_join(session2_ID, by = "Participant Private ID")
  
  return(processed_session2)
}

get_baseline_plots <- function(df, par, session) {
  
  along_VOT <- df %>% 
    group_by(VOT) %>% 
    summarise(mean_prob_p=mean(prob_p), s_error_mean=sd(prob_p)/sqrt(1), .groups='drop') %>% 
    filter(!is.na(VOT))
  
  decision_boundary_along_VOT <- along_VOT %>% 
    ggplot(aes(x=VOT, y=mean_prob_p, group=1)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin=mean_prob_p-s_error_mean, ymax=mean_prob_p+s_error_mean), width=.2,
                  position=position_dodge(.9)) +
    labs(x='VOT', y='Probability of voiceless response (%)') +
    scale_y_continuous(breaks=seq(0,100,by=20), limits=c(0,100)) +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(color = "grey", size = 0.25),
      axis.title.x = element_text(size=10),
      axis.title.y = element_text(size=4)
    )
  
  ggsave(glue("./{session}_figures/baseline_vot_{par}.png"), plot=decision_boundary_along_VOT, width=592, height=592, dpi=300, units = "px")
  
  along_f0 <- df %>% 
    group_by(f0) %>% 
    summarise(mean_prob_p=mean(prob_p), s_error_mean=sd(prob_p)/sqrt(1), .groups='drop') %>%
    filter(!is.na(f0))
  
  decision_boundary_along_f0 <- along_f0 %>% 
    ggplot(aes(x=f0, y=mean_prob_p, group=1)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin=mean_prob_p-s_error_mean, ymax=mean_prob_p+s_error_mean), width=.2,
                  position=position_dodge(.9)) +
    labs(x='f0', y='Probability of voiceless response (%)') +
    scale_y_continuous(breaks=seq(0,100,by=20), limits=c(0,100)) +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(color = "grey", size = 0.25),
      axis.title.x = element_text(size=10),
      axis.title.y = element_text(size=4)
    )
  ggsave(glue("./{session}_figures/baseline_f0_{par}.png"), plot=decision_boundary_along_f0, width=592, height=592, dpi=300, units = "px")
  
  df_heatmap <- df %>%
    group_by(VOT, f0) %>% 
    summarise(`Probability of voiceless response`=mean(prob_p), .groups='drop') %>% 
    filter(!is.na(VOT) & !is.na(f0))
  
  # Color Brewer palette
  heatmap <- ggplot(df_heatmap, aes(VOT, f0, fill= `Probability of voiceless response`)) + 
    geom_tile() +
    geom_text(aes(label=round(`Probability of voiceless response`, 2)), size=1) +
    scale_fill_gradient(low="#eff3ff", high="#2171b5") +
    theme_ipsum() +
    coord_fixed(ratio=1) +
    theme(
      legend.text = element_text(size=5),
      legend.title = element_text(size=5),
      legend.key.height = unit(0.4, "cm"),
      legend.key.width = unit(0.3, "cm"),
      axis.text.x = element_text(size=5),
      axis.text.y = element_text(size=5)
    )
  
  ggsave(glue("./{session}_figures/baseline_heatmap_{par}.png"), plot=heatmap, width=1200, height=800, dpi=300, units = "px")
}


traj_test <- function(df,block,par) {
  # get standard error of the mean for this figure too.
  df <- df %>% 
    mutate(Display_num = str_extract(Display, "\\d+")) %>% 
    mutate(Display = factor(Display_num, levels = as.character(1:30))) %>% 
    dplyr::select(-Display_num)
  
  p_traj <- ggplot(data=df, aes(x=Display, y=prob_p, group = trial, color = trial), alpha=0.5) +
    scale_color_manual(values = c("highf0"='blue', "lowf0"='orange')) +
    geom_point() +
    geom_line() +
    scale_y_continuous(breaks=seq(0,100,by=25)) +
    xlab("Iterations") +
    ylab("VAS response") + 
    theme(
      panel.background = element_rect(fill='white'),
      panel.grid.major = element_line(color='black', linewidth=0.3),
      legend.text = element_text(size=5),
      legend.title = element_text(size=5),
      legend.key.height = unit(0.4, "cm"),
      legend.key.width = unit(0.3, "cm"),
      axis.text.x = element_text(size=5),
      axis.text.y = element_text(size=5)
    )
  
  ggsave(glue("./{block}_figures/baseline_f0_{par}.png"), plot=p_traj, width=1500, height=592, dpi=300, units = "px")
  
  return(p_traj)
}

count_missed_trials <- function(df) {
    missing_count <- df %>% 
    filter(`Response Type` == 'tooEarly') %>% 
    filter(is.na(Response)) %>% 
    group_by(ProlificID) %>%
    summarize(count=n())
  
  return(missing_count)
} 

filter_needed_trials <- function(df) {
  filtered_results <- df %>% 
    filter(!is.na(Response) & !str_detect(Response, 'audio|Participant')) %>% 
    mutate(Response=as.numeric(Response)) %>% 
    rename(rand=`randomiser-qxk8`)
  
  return(filtered_results)
}



get_false_probe_trials <- function(df) {
  false_probe <- df %>% 
    filter(str_detect(trial, "slide")) %>% 
    mutate(should_be = ifelse(trial=='slide_left.mp3', 0, 100)) %>% 
    mutate(diff = abs(Response - should_be)) %>% 
    filter(diff!=0) 
  
  return(false_probe)
  
}

process_session_results <- function(df) {
  session_results <- df %>% 
    filter(!str_detect(trial, "slide")) %>% 
    mutate(prob_p=ifelse(rand=='beer_pier', Response, 100-Response)) %>% 
    mutate(stimuli=gsub(".mp3", "", trial)) %>% 
    separate(stimuli, into = c("PoA", "cue2", "f0", "cue1", "VOT"), sep = "_") %>% 
    dplyr::select(-c("PoA", "cue2", "cue1", "Response", "trial", "rand"))
  return(session_results)
}

format_pre_post_curve_fitter <- function(df) {
  formatted_df <- df %>% 
    dplyr::select(-Display) %>% 
    rename(subject=ProlificID, Response.100=prob_p) %>% 
    dplyr::select(subject, f0, VOT, Response.100) 
  
  return(formatted_df)
  }

format_exposure_trials <- function(df) {
  exposure_results <- df %>% 
    filter(!str_detect(trial, "slide")) %>% 
    mutate(prob_p=ifelse(rand=='beer_pier', Response, 100-Response)) %>% 
    filter(str_detect(trial, "low|high")) %>% 
    mutate(trial=gsub(".mp3", "", trial), trial=gsub("^_", "", trial))
  return(exposure_results)
}
  
 
get_var <- function(df) {
  var_df <- df %>% 
    rowwise() %>% 
    mutate(mse = (Response_100-pred) ^ 2) %>% 
    ungroup() %>% 
    group_by(ProlificID) %>% 
    summarise(sum_mse=sum(mse), .groups='drop') %>% 
    mutate(sum_mse=scale(sum_mse)) %>% 
    rename(subject=ProlificID) 
  
  return(var_df)
}
