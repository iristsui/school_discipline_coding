# PBIS Cleaning File - EBS-specific cleaning
# By Casey Tsui
# Format: R
# Last updated: 10/4/10


# Author Comment: This file is called from the main cleaning file when handling
# the EBS data.
  # Create the 4-category measures of EBS scores.


for (i in 1:nrow(b)) {
  b$schoolwide.disc.sys[i] <- sum(b$sasurvey_studentexpectationss[i] +
                                  b$sasurvey_behaviorstaughts[i] +
                                  b$sasurvey_behaviorsrewardeds[i] +
                                  b$sasurvey_problembehaviorsdefineds[i] +
                                  b$sasurvey_consequencesdefineds[i] +
                                  b$sasurvey_officeclassroomdistinctionss[i] +
                                  b$sasurvey_optionsexists[i] +
                                  b$sasurvey_emergencyproceduress[i] +
                                  b$sasurvey_behaviorsupportteams[i] +
                                  b$sasurvey_administratoronteams[i] +
                                  b$sasurvey_datasummarizeds[i] +
                                  b$sasurvey_patternsreporteds[i] +
                                  b$sasurvey_informingfamiliess[i] +
                                  b$sasurvey_boostertrainings[i] +
                                  b$sasurvey_supportteambudgets[i] +
                                  b$sasurvey_allstaffinvolveds[i] +
                                  b$sasurvey_trainingfromdistricts[i] +
                                  b$sasurvey_districtreports[i],
                                  na.rm=TRUE)

  b$nonclassroom.set[i] <- sum(b$sasurvey_ncrbehaviorss[i] +
                               b$sasurvey_ncrbehaviorstaughts[i] +
                               b$sasurvey_ncrsupervisions[i] +
                               b$sasurvey_ncrrewardss[i] +
                               b$sasurvey_ncrfeaturesmodifieds[i] +
                               b$sasurvey_ncrmovementschedulings[i] +
                               b$sasurvey_ncrstaffsupervisionskillss[i] +
                               b$sasurvey_ncrbehaviorevaluateds[i] +
                               b$sasurvey_ncrallstaffs[i],
                               na.rm=TRUE)

  b$classroom.set[i] <- sum(b$sasurvey_crbehaviorsdefineds[i] +
                            b$sasurvey_crproblembehaviorss[i] +
                            b$sasurvey_crbehaviorstaughts[i] +
                            b$sasurvey_crbehaviorsrewardeds[i] +
                            b$sasurvey_crconsequencess[i] +
                            b$sasurvey_crproceduress[i] +
                            b$sasurvey_croptionsexists[i] +
                            b$sasurvey_crmaterialsmatchabilitys[i] +
                            b$sasurvey_cracademicsuccesss[i] +
                            b$sasurvey_crteachersassisteds[i] +
                            b$sasurvey_crtransitionsefficients[i],
                            na.rm=TRUE)

  b$indiv.student.sys[i] <- sum(b$sasurvey_indregularassessmentss[i] +
                                b$sasurvey_indrequestassistanceprocesss[i] +
                                b$sasurvey_indbehaviorsupportteams[i] +
                                b$sasurvey_indbehavioralassessments[i] +
                                b$sasurvey_indlocalresourcess[i] +
                                b$sasurvey_indfamilymemberss[i] +
                                b$sasurvey_indfamilytrainings[i] +
                                b$sasurvey_indbehaviormonitoreds[i],
                                na.rm=TRUE)
}

# Validate additions by calculating the ranges of the aggregate variables
range(b$schoolwide.disc.sys, na.rm=TRUE)  # Should be 0-36
range(b$nonclassroom.set,    na.rm=TRUE)  # Should be 0-18
range(b$classroom.set,       na.rm=TRUE)  # Should be 0-22
range(b$indiv.student.sys,   na.rm=TRUE)  # Should be 0-16
