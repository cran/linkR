linkR_examples <- function(name='salmon'){

	# RETURN LIST
	rlist <- list()

	# GET EXTDATA DIRECTORY
	if("package:linkR" %in% search()){
		fdir <- paste0(path.package("linkR"), "/extdata/")
	}else{
		fdir <- '/Applications/XAMPP/xamppfiles/htdocs/data_analysis/r_package_development/linkR/inst/extdata/'
	}

	if(tolower(name) == 'salmon'){

		# GET LANDMARKS
		rlist$landmarks <- as.matrix(read.table(file=paste0(fdir, 'salmon_landmarks.txt'), row.names=1))

		# GET LANDMARK-LINK ASSOCIATIONS
		rlist$lm.assoc.ref <- as.matrix(read.table(file=paste0(fdir, 'salmon_link_associations.txt')))

		# SET PATH CONNECTIONS
		rlist$path.connect <- list(
			c("HM-NC_ant_L", "HM-NC_pos_L", "HY-HM_L", "LJ-QT_L", "HM-NC_ant_L"),
			c("HM-NC_ant_R", "HM-NC_pos_R", "HY-HM_R", "LJ-QT_R", "HM-NC_ant_R"),
			c("HY-HM_L", "HY_mve", "HY_mid_dor", "HY-HM_L"),
			c("HY-HM_R", "HY_mve", "HY_mid_dor", "HY-HM_R"),
			c("LJ_mant", paste0("lowerjaw_lm", 1:3, "_R"), "LJ-QT_R", paste0("lowerjaw_lm", 5:4, "_R"), "LJ_mpos"),
			c("LJ_mant", paste0("lowerjaw_lm", 1:3, "_L"), "LJ-QT_L", paste0("lowerjaw_lm", 5:4, "_L"), "LJ_mpos"),
			c("maxilla_midline_ant", "basicranium_ant", "basicranium_centrum"),
			c("HY_mve", "HY_mid_dor"),
			c("maxilla_midline_ant", "premaxilla_tooth5_L", "orbit_ant_L", paste0("orbit", 2:8, "_L")),
			c("maxilla_midline_ant", "premaxilla_tooth5_R", "orbit_ant_R", paste0("orbit", 2:8, "_R")),
			c("HM_proc_ant_L", "hyomand_process_L", "HM-NC_ant_L"),
			c("HM_proc_ant_R", "hyomand_process_R", "HM-NC_ant_R"),
			c("HY_mve", "LJ_mpos"),
			c("LJ_mant", "LJ_mpos"),
			c("PM_mant", "cranium_sagittal_ant", "cranium_sagittal_mid", "cranium_sagittal_curve_pos", "cranium_sagittal_pos")
		)
	}

	if(tolower(name) == 'owl'){

		# GET LANDMARKS
		rlist$landmarks <- as.matrix(read.table(file=paste0(fdir, 'owl_landmarks.txt'), row.names=1))

		# GET LANDMARK-LINK ASSOCIATIONS
		rlist$lm.assoc.ref <- as.matrix(read.table(file=paste0(fdir, 'owl_link_associations.txt')))

		rlist$path.connect <- list(
			c("QT_NC_MC_R", "QT_NC_LC_R", "QT-JU_R", "mand_condyle_quadrate_uni_R", "mand_condyle_quadrate_lat_R", "mand_condyle_quadrate_med_R", "QT-PT_R", "QT_NC_MC_R"),
			c("QT_NC_MC_L", "QT_NC_LC_L", "QT-JU_L", "mand_condyle_quadrate_uni_L", "mand_condyle_quadrate_lat_L", "mand_condyle_quadrate_med_L", "QT-PT_L", "QT_NC_MC_L"),
			c("QT_NC_MC_L", "orbital_proc_quadrate_sup_base_L", "orbital_proc_quadrate_distal_L", "orbital_proc_quadrate_inf_base_L", "QT-PT_L"),
			c("QT_NC_LC_L", "orbital_proc_quadrate_sup_base_L", "QT_NC_MC_L"),
			c("orbital_proc_quadrate_inf_base_L", "QT-JU_L"),
			c("QT_NC_MC_R", "orbital_proc_quadrate_sup_base_R", "orbital_proc_quadrate_distal_R", "orbital_proc_quadrate_inf_base_R", "QT-PT_R"),
			c("QT_NC_LC_R", "orbital_proc_quadrate_sup_base_R", "QT_NC_MC_R"),
			c("orbital_proc_quadrate_inf_base_R", "QT-JU_R", "mand_condyle_quadrate_uni_R", "mand_condyle_quadrate_lat_R", "mand_condyle_quadrate_med_R", "QT-PT_R"),
			c("orbital_proc_quadrate_inf_base_L", "QT-JU_L", "mand_condyle_quadrate_uni_L", "mand_condyle_quadrate_lat_L", "mand_condyle_quadrate_med_L", "QT-PT_L"),
			c("upper_bill_culmen", "CFH-NC_L", "JU-UB_L", "upper_bill_tomium_L"),
			c("upper_bill_culmen", "CFH-NC_R", "JU-UB_R", "upper_bill_tomium_R"),
			c("CFH-NC_R", "JU-UB_R"),
			c("CFH-NC_L", "JU-UB_L"),
			c("CFH-NC_R", "CFH-NC_L"),
			c("cranium_sagittal", "cranium_occipital"),
			c("QT-PT_R", "PT-PA_R", "UB-PA_R"),
			c("QT-PT_L", "PT-PA_L", "UB-PA_L"),
			c("QT-JU_R", "JU-UB_R"),
			c("QT-JU_L", "JU-UB_L")
		)
	}

	# MATCH LANDMARK AND LINK ASSOCIATIONS
	rlist$lm.assoc <- rep(NA, nrow(rlist$landmarks))
	for(i in 1:nrow(rlist$landmarks)){
		for(j in 1:nrow(rlist$lm.assoc.ref)){
			if(grepl(rlist$lm.assoc.ref[j, 1], rownames(rlist$landmarks)[i])){
				rlist$lm.assoc[i] <- rlist$lm.assoc.ref[j, 2]
				break
			}
		}
	}

	rlist
}
