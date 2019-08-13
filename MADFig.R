### check plot trajectories ###
MT.data.sur <- mt_subset(MT.data, cond.correct == "Surprise")

mt_plot_aggregate(MT.data.sur, use = "tn_trajectories", color = "cond.load")
