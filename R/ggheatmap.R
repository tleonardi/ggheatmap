require(dplyr)
require(tibble)
require(reshape2)
require(ggdendro)
require(RColorBrewer)

#orderCol=T
#orderRow=T
#dendroLineSize=0.5
#fontSize=20
#colorPalette="Spectral"
ggheatmap <- function(dataMatrix, orderCol=T, orderRow=T, dendroLineSize=0.5, fontSize=20, colorScheme="Spectral", scaleName="value", distMethod="euclidean", clustMethod="complete"){

	data_m <- rownames_to_column(dataMatrix) %>% melt()
	 
	if(orderRow){
		dd.row <- as.dendrogram(hclust(dist(dataMatrix, method=distMethod), method=clustMethod))
		row.ord <- order.dendrogram(dd.row)
		ordered_row_names <- row.names(dataMatrix[row.ord,])
		data_m$rowname <- factor(data_m$rowname, levels=ordered_row_names)
	}
	
	# Cluster columns
	if(orderCol){
		dd.col <- as.dendrogram(hclust(dist(t(dataMatrix), method=distMethod), method=clustMethod))
		col.ord <- order.dendrogram(dd.col)
		ordered_col_names <- colnames(dataMatrix[,col.ord])
		data_m$variable <- factor(data_m$variable, levels=ordered_col_names)
	}

	heat_plot <- ggplot2::ggplot(data_m, aes(x=variable, y=rowname, fill=value)) + 
			geom_tile() + 
			theme_minimal() + 
			theme(axis.line=element_line(size=0), text = element_text(size=fontSize)) + 
			scale_y_discrete(position = "right") + 
			xlab("") + 
			ylab("") +
			scale_fill_distiller(palette=colorPalette, name=scaleName)
	
	final_plot <- heat_plot
	# Cluster rows
	if(orderRow){
		dendro_data_row <- ggdendro::dendro_data(dd.row, type = "rectangle")
		dendro_row <- axis_canvas(heat_plot, axis = 'y', coord_flip = TRUE) + 
		  geom_segment(data = segment(dendro_data_row), aes(y = -y, x = x, xend = xend, yend = -yend), size=dendroLineSize) +
		  coord_flip() 
	  	final_plot <- insert_yaxis_grob(final_plot, dendro_row, grid::unit(.2, "null"), position = "left")
	}
	
	# Cluster columns
	if(orderCol){
		dendro_data_col <- ggdendro::dendro_data(dd.col, type = "rectangle")
		dendro_col <- axis_canvas(heat_plot, axis = 'x') + 
		  geom_segment(data=segment(dendro_data_col), aes(x = x, y = y, xend = xend, yend = yend), size=dendroLineSize)
	  	final_plot <- insert_xaxis_grob(final_plot, dendro_col, grid::unit(.2, "null"), position = "top")
	}
	
	ggdraw(final_plot)
	
}
