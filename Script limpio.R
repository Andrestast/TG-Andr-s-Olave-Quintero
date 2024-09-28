############################## LIBRERÍAS USADAS ################################----
# Listado de librerias requeridas por el script
lib_req<-c("readxl","psych","corrplot","FactoMineR","factoextra","ggrepel"
           ,"Factoshiny","plm","dplyr","kableExtra") 
easypackages::packages(lib_req)
################################### datos ######################################----
data <- read_excel("DATA COMPLETA.xlsx") #bd con información completa
niños <- read_excel("niños.xlsx") # bd con puntajes de ítems 7 pruebas
puntajes.niños <- read_excel("niños_puntajes.xlsx") # bd con punt. finales 
padres <- read_excel("padres.xlsx") # bd con puntajes de ítems ship
############################ análisis exploratorio #############################----
lapply(data,table)
################## correlaciones entre items de las pruebas ####################----
x11()
corrplot(cor(niños), method = "circle", tl.cex = 0.5) #cor. de Pearson
dev.off()
########################### transformación datos ###############################----
niños <- niños[,-1] # variables escolares-adolescentes sin indx
puntajes.niños <- puntajes.niños[,-1] # varia. puntajes finales pruebas sin indx
reverse_score <- function(x) { # función para revertir los valores 
  max_val <- max(x, na.rm = TRUE)
  max_val + 1 - x
}
niños_inv <- niños # variables escolares.adolescentes a trabajar
cinv <- c(31:40) # variables a revertir
niños_inv[cinv] <- lapply(niños[cinv],reverse_score) # aplicación reverse_score
puntajes.niños[,5] <- 1/puntajes.niños[,5] # invertir puntaje prueba asws
x11()
corrplot(cor(niños_inv), method = "circle", tl.cex = 0.5) #cor. de Pearson 
dev.off()
################### factibilidad de análisis multivariado ######################----
KMO(cor(niños_inv)) # kmo datos soportados por la teoría de los intervalos
####################### AFM datos escolares-adolescentes #######################----
group = c(6,8,8,8,10,8,8) # numero de items por tabla 
fma_niños <- MFA(niños_inv,group = group ,type = rep("s",length(group)) # AFM
                 ,name.group = c("Insomnio","Somnolencia","Alteración",
                                 "Deterioro","Sueño y vigilia","Ansiedad",
                                 "Depresión"))
MFAshiny(fma_niños) # resultados AFM
# % varianza
g1_var <- fviz_eig(fma_niños , addlabels = TRUE, ylim = c(0, 70))+
  labs(title = "", x = "Dimensión",y = "% de Varianza Explicado")
# tipología grupos
fviz_mfa_var(fma_niños, "group",repel = TRUE)+ ggtitle("")
# circulo de correlación
fviz_mfa_var(fma_niños, "quanti.var", palette = "jco",
             col.var.sup = "violet", repel = TRUE, labelsize = 2)+ ggtitle("")
# contribución variables dim 1 y dim 2
g1_contri1 <- fviz_contrib(fma_niños, choice = "quanti.var", axes = 1, top = 10, 
                           ggtheme = theme_minimal()) +
  labs(title = "",x = "Variables", y = "Contribución (%)")
g1_contri2 <- fviz_contrib(fma_niños, choice = "quanti.var", axes = 2, top = 10, 
                           ggtheme = theme_minimal()) +
  labs(title = "",x = "Variables", y = "Contribución (%)") 
# nube de individuos
fviz_mfa_ind(fma_niños, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = F,labelsize = 2, pointsize = 1) + ggtitle("")
# ejes parciales
fviz_mfa_axes(fma_niños,labelsize = 3,repel = TRUE)
################################ Tablas y ITSN1 ################################----
fma_niños$eig
fma_niños$group$coord[,c(1:3)]
data.frame(coord = fma_niños$quanti.var$coord[,c(1:2)], 
                          contrib = fma_niños$quanti.var$contrib[,c(1:2)])
eig.vectors <- fma_niños$global.pca$svd$V # vectores propios
pisi_eig <- fma_niños$separate.analyses$Insomnio$eig # valores propios 
ess_eig <- fma_niños$separate.analyses$Somnolencia$eig 
promis_sd_eig <- fma_niños$separate.analyses$Alteración$eig
promis_sri_eig <- fma_niños$separate.analyses$Deterioro$eig
asws_eig <- fma_niños$separate.analyses$`Sueño y vigilia`$eig
promis_anxiety_eig <- fma_niños$separate.analyses$Ansiedad$eig
promis_depression_eig <- fma_niños$separate.analyses$Depresión$eig
Mu <- diag(c(rep(1/pisi_eig,6),rep(1/ess_eig,8),rep(1/promis_sd_eig,8),
        rep(1/promis_sri_eig,8),rep(1/asws_eig,10),rep(1/promis_anxiety_eig,8),
        rep(1/promis_depression_eig,8)),nrow=56,ncol=56)%*%eig.vectors[,1]
Mu_ponderado <- Mu/sum(Mu) 
Comp_1 <- as.matrix(niños_inv)%*%Mu_ponderado # Expresión de ITSN1
escale_niños1 <- (Comp_1 - min(Comp_1))*100/(max(Comp_1)-min(Comp_1)) # escala 1
min_teor_niños1 <- sum(Mu_ponderado[1:6]*0,Mu_ponderado[7:14]*0,
                       Mu_ponderado[15:22]*1,Mu_ponderado[23:30]*1 #min teórico
                       ,Mu_ponderado[31:40]*1,Mu_ponderado[41:48]*1,
                       Mu_ponderado[49:56]*0)
max_teor_niños1 <- sum(Mu_ponderado[1:6]*5,Mu_ponderado[7:14]*3,
                       Mu_ponderado[15:22]*5,Mu_ponderado[23:30]*5 #max teórico
                       ,Mu_ponderado[31:40]*6,Mu_ponderado[41:48]*5,
                       Mu_ponderado[49:56]*4)
print(min(Comp_1)) # intervalo inferior aplicado
print(max(Comp_1)) # intervalo superior aplicado
print(min_teor_niños1) # intervalo inferior teórico
print(max_teor_niños1) # intervalo superior teórico
######### ACP primeros componentes pruebas datos escolares-adolescentes ########----
pca_pisi <- PCA(niños_inv[,1:6]) # PCA por tablas
pca_ess <- PCA(niños_inv[,7:14])
pca_promis_sd <- PCA(niños_inv[,15:22])
pca_promis_sri <- PCA(niños_inv[,23:30]) 
pca_asws <- PCA(niños_inv[,31:40])
pca_promis_anxiety <- PCA(niños_inv[,41:48])
pca_promis_depression <- PCA(niños_inv[,49:56])
v1_pisi <- pca_pisi$svd$V # 1er vect propio tabla Insomnio
Comp_1.t1 <- as.matrix(niños_inv[,1:6])%*%v1_pisi[,1]
min_pisi <- rep(0,6)%*%v1_pisi[,1] # min y max para sacar intervalo luego
max_pisi <- rep(3,6)%*%v1_pisi[,1]
v1_ess <- pca_ess$svd$V # 1er vect propio tabla Somnolencia
Comp_1.t2 <- as.matrix(niños_inv[,7:14])%*%v1_ess[,1] # 1er 
min_ess <- rep(0,8)%*%v1_ess[,1]
max_ess <- rep(3,8)%*%v1_ess[,1]
v1_promis_sd <- pca_promis_sd$svd$V # 1er vect propio tabla Alteración
Comp_1.t3 <- as.matrix(niños_inv[,15:22])%*%v1_promis_sd[,1]
min_promis_sd <- rep(1,8)%*%v1_ess[,1]
max_promis_sd <- rep(5,8)%*%v1_ess[,1]
v1_promis_sri <- pca_promis_sri$svd$V # 1er vect propio tabla Deterioro
Comp_1.t4 <- as.matrix(niños_inv[,23:30])%*%v1_promis_sri[,1]
min_promis_sri <- rep(1,8)%*%v1_promis_sri[,1]
max_promis_sri <- rep(5,8)%*%v1_promis_sri[,1]
v1_asws <- pca_asws$svd$V # 1er vect propio tabla Sueño y Vigilia
Comp_1.t5 <- as.matrix(niños_inv[,31:40])%*%v1_asws[,1]
min_asws <- rep(1,10)%*%v1_asws[,1]
max_asws <- rep(6,10)%*%v1_asws[,1]
v1_promis_anxiety <- pca_promis_anxiety$svd$V # 1er vect propio tabla ansiedad
Comp_1.t6 <- as.matrix(niños_inv[,41:48])%*%v1_promis_anxiety[,1]
min_anxiety <- rep(1,8)%*%v1_promis_anxiety[,1]
max_anxiety <- rep(5,8)%*%v1_promis_anxiety[,1]
v1_promis_depression <- pca_promis_depression$svd$V # 1er vect propio tabla depresión
Comp_1.t7 <- as.matrix(niños_inv[,49:56])%*%v1_promis_depression[,1]
min_depression <- rep(0,8)%*%v1_promis_depression[,1]
max_depression <- rep(4,8)%*%v1_promis_depression[,1]
mat.1vects <- data.frame(cbind(Comp_1.t1,Comp_1.t2,Comp_1.t3,Comp_1.t4,
                               Comp_1.t5,Comp_1.t6,Comp_1.t7)) # matriz 1 comps
colnames(mat.1vects) <- c("Insomnio","Somnolencia","Alteración",
                          "Deterioro","Sueño y vigilia","Ansiedad",
                          "Depresión")
acp_niños_tablas_1v <- PCA(mat.1vects) # PCA matriz de primeros componentes
# % varianza
g2_var <- fviz_eig(acp_niños_tablas_1v, addlabels = TRUE, ylim = c(0, 70))
+ labs(title = "", x = "Dimensión",y = "% de Varianza Explicado")
# circulo de correlación
fviz_pca_var(acp_niños_tablas_1v, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
# contribución variables dim 1 y dim 2
g2_contri1 <- fviz_contrib(acp_niños_tablas_1v, choice="var", axes = 1, 
                           top = 10, ggtheme = theme_minimal()) +
  labs(title = "",x = "Variables", y = "Contribución (%)")
g2_contri2 <- fviz_contrib(acp_niños_tablas_1v, choice="var", axes = 2, 
                           top = 10, ggtheme = theme_minimal()) +
  labs(title = "",x = "Variables", y = "Contribución (%)")
# nube de individuos
fviz_pca_ind(acp_niños_tablas_1v, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = F,
             labelsize = 2, pointsize = 1) + ggtitle("")
################################ Tablas y ITSN2 ################################----
acp_niños_tablas_1v$eig
data.frame(coord = acp_niños_tablas_1v$var$coord[,c(1:2)], 
                          contrib = acp_niños_tablas_1v$var$contrib[,c(1:2)])
eig.vectors.tablas <- acp_niños_tablas_1v$svd$V # 1er vect propio matriz 1 comps
ponderado_m2_1 <- eig.vectors.tablas[,1]/sum(eig.vectors.tablas[,1])
sum(ponderado_m2_1)
Comp_1.tablas <- as.matrix(mat.1vects)%*%ponderado_m2_1 # expresión de ITSN2
escale_niños2 <- (Comp_1.tablas - min(Comp_1.tablas))*100/(max(Comp_1.tablas)-min(Comp_1.tablas))
min_teor_niños2 <- sum(ponderado_m2_1[1]*min_pisi,ponderado_m2_1[2]*min_ess,
                       ponderado_m2_1[3]*min_promis_sd
                       ,ponderado_m2_1[4]*min_promis_sri,
                       ponderado_m2_1[5]*min_asws,ponderado_m2_1[6]*min_anxiety,
                       ponderado_m2_1[7]*min_depression)
max_teor_niños2 <- sum(ponderado_m2_1[1]*max_pisi,ponderado_m2_1[2]*max_ess,
                       ponderado_m2_1[3]*max_promis_sd
                       ,ponderado_m2_1[4]*max_promis_sri,
                       ponderado_m2_1[5]*max_asws,ponderado_m2_1[6]*max_anxiety,
                       ponderado_m2_1[7]*max_depression)
print(min(Comp_1.tablas)) # intervalo inferior aplicado
print(max(Comp_1.tablas)) # intervalo superior aplicado
print(min_teor_niños2) # intervalo inferior teórico
print(max_teor_niños2) # intervalo superior teórico
################# ACP puntajes finales escolares-adolescentes ##################----
pca_niños <- PCA(puntajes.niños)
# % varianza
g3_var <- fviz_eig(pca_niños, addlabels = TRUE, ylim = c(0, 70))+ 
  labs(title = "", x = "Dimensión",y = "% de Varianza Explicado")
# circulo de correlación
fviz_pca_var(pca_niños, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
# contribución variables dim 1 y dim 2
g3_contri1 <- fviz_contrib(pca_niños, choice="var", axes = 1, top = 10, 
                           ggtheme = theme_minimal()) +
  labs(title = "",x = "Variables", y = "Contribución (%)")
g3_contri2 <- fviz_contrib(pca_niños, choice="var", axes = 2, top = 10, 
                           ggtheme = theme_minimal()) +
  labs(title = "",x = "Variables", y = "Contribución (%)")
# nube de individuos
fviz_pca_ind(pca_niños, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = F,labelsize = 2, pointsize = 1) + ggtitle("")
################################ Tablas y ITSN3 ################################----
pca_niños$eig
data.frame(coord = pca_niños$var$coord[,c(1:2)], 
                          contrib = pca_niños$var$contrib[,c(1:2)])
eig.vectors.punt <- pca_niños$svd$V # vectores propios
ponderado_m3 <- eig.vectors.punt[,1]/sum(eig.vectors.punt[,1])
Comp_1.punt <- as.matrix(puntajes.niños)%*%ponderado_m3 # expresión de la medida sintética 3
escale_niños3 <- (Comp_1.punt - min(Comp_1.punt))*100/(max(Comp_1.punt)-min(Comp_1.punt)) # escala 3
min_teor_niños3 <- sum(ponderado_m3[1]*0 + ponderado_m3[2]*0 
                       + ponderado_m3[3]*36.6 + ponderado_m3[4]*37.4 
                       + ponderado_m3[5]*0.2 + ponderado_m3[6]*37.1 + 
                         ponderado_m3[7]*35)
max_teor_niños3 <- sum(ponderado_m3[1]*30 + ponderado_m3[2]*24 
                       + ponderado_m3[3]*82.7 + ponderado_m3[4]*84 
                       + ponderado_m3[5]*1 + ponderado_m3[6]*83.1 + 
                         ponderado_m3[7]*82)
min(Comp_1.punt) # intervalo inferior aplicado
max(Comp_1.punt) # intervalo superior aplicado
print(min_teor_niños3) # intervalo inferior teórico
print(max_teor_niños3) # intervalo superior teórico
######### Likert equidistante de los datos de escolares-adolescentes ###########----
frecuencias_respuestas <- apply(niños_inv, 2, table)
pesos <- lapply(frecuencias_respuestas, function(x) {
  as.numeric(x) / sum(x)
})
datos_con_pesos <- lapply(seq_along(niños_inv), function(i) {
  niños_inv[, i] * pesos[[i]]
})
datos_con_pesos <- do.call(cbind, datos_con_pesos)
colnames(datos_con_pesos) <- colnames(niños_inv)
###################### L.E. AFM datos escolares-adolescentes ###################----
group = c(6,8,8,8,10,8,8) # numero de items por tabla 
fma_niños_mc <- MFA(datos_con_pesos,group = group ,type = rep("s",length(group)) # AFM
                    ,name.group = c("Insomnio","Somnolencia","Alteración",
                                    "Deterioro","Sueño y vigilia","Ansiedad",
                                    "Depresión"))
MFAshiny(fma_niños_mc) # resultados AFM
# % varianza
g1_var_mc <- fviz_eig(fma_niños_mc , addlabels = TRUE, ylim = c(0, 70))+ 
  labs(title = "", x = "Dimensión",y = "% de Varianza Explicado")
# tipologia grupos
fviz_mfa_var(fma_niños_mc, "group",repel = TRUE)+ ggtitle("")
# circulo de correlación
fviz_mfa_var(fma_niños_mc, "quanti.var", palette = "jco",
             col.var.sup = "violet", repel = TRUE, labelsize = 2)+ ggtitle("")
# contribución variables dim 1 y dim 2
g1_contri1_mc <- fviz_contrib(fma_niños_mc, choice = "quanti.var", axes = 1, 
                              top = 10,ggtheme = theme_minimal()) +
  labs(title = "",x = "Variables", y = "Contribución (%)")
g1_contri2_mc <- fviz_contrib(fma_niños_mc, choice = "quanti.var", axes = 2, 
                              top = 10,  ggtheme = theme_minimal()) +
  labs(title = "",x = "Variables", y = "Contribución (%)")
# nube de individuos
fviz_mfa_ind(fma_niños_mc, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = F,labelsize = 2, pointsize = 1) + ggtitle("")
# ejes parciales
fviz_mfa_axes(fma_niños_mc,labelsize = 3,repel = TRUE) + ggtitle("")
################################ Tablas y ITSNE1 ###############################----
fma_niños_mc$eig
fma_niños_mc$group$coord[,c(1:3)]
data.frame(coord = fma_niños_mc$quanti.var$coord[,c(1:2)], 
                          contrib = fma_niños_mc$quanti.var$contrib[,c(1:2)])
eig.vectors_mc <- fma_niños_mc$global.pca$svd$V # vectores propios
pisi_eig_mc <- fma_niños_mc$separate.analyses$Insomnio$eig # valores propios 
ess_eig_mc <- fma_niños_mc$separate.analyses$Somnolencia$eig 
promis_sd_eig_mc <- fma_niños_mc$separate.analyses$Alteración$eig
promis_sri_eig_mc <- fma_niños_mc$separate.analyses$Deterioro$eig
asws_eig_mc <- fma_niños$separate.analyses_mc$`Sueño y vigilia`$eig
promis_anxiety_eig_mc <- fma_niños_mc$separate.analyses$Ansiedad$eig
promis_depression_eig_mc <- fma_niños_mc$separate.analyses$Depresión$eig
Mu_mc <- diag(c(rep(1/pisi_eig_mc,6),rep(1/ess_eig_mc,8),rep(1/promis_sd_eig_mc,8),
                rep(1/promis_sri_eig_mc,8),rep(1/asws_eig_mc,10),
                rep(1/promis_anxiety_eig_mc,8),rep(1/promis_depression_eig_mc,8))
              ,nrow=56,ncol=56)%*%eig.vectors_mc[,1]
Mu_mc_ponderado <- Mu_mc/sum(Mu_mc)
sum(Mu_mc_ponderado)
Comp_1_mc <- as.matrix(datos_con_pesos)%*%Mu_mc_ponderado # Expresión de ITSNE1
predict(fma_niños_mc, newdata=datos_con_pesos)$coord[,1:3] # verificación
escale_niños1_mc <- (Comp_1_mc - min(Comp_1_mc))*100/(max(Comp_1_mc)-min(Comp_1_mc)) # escala 1
print(min(Comp_1_mc)) # intervalo inferior 
print(max(Comp_1_mc)) # intervalo superior 
####### L.E. ACP primeros componentes pruebas datos escolares-adolescentes #####----
pca_pisi_mc <- PCA(datos_con_pesos[,1:6]) # PCA por tablas
pca_ess_mc <- PCA(datos_con_pesos[,7:14])
pca_promis_sd_mc <- PCA(datos_con_pesos[,15:22])
pca_promis_sri_mc <- PCA(datos_con_pesos[,23:30]) 
pca_asws_mc <- PCA(datos_con_pesos[,31:40])
pca_promis_anxiety_mc <- PCA(datos_con_pesos[,41:48])
pca_promis_depression_mc <- PCA(datos_con_pesos[,49:56])
v1_pisi_mc <- pca_pisi_mc$svd$V # 1er vect propio tabla Insomnio
Comp_1.t1_mc <- as.matrix(datos_con_pesos[,1:6])%*%v1_pisi_mc[,1]
v1_ess_mc <- pca_ess_mc$svd$V # 1er vect propio tabla Somnolencia
Comp_1.t2_mc <- as.matrix(datos_con_pesos[,7:14])%*%v1_ess_mc[,1] # 1er 
v1_promis_sd_mc <- pca_promis_sd_mc$svd$V # 1er vect propio tabla Alteración
Comp_1.t3_mc <- as.matrix(datos_con_pesos[,15:22])%*%v1_promis_sd_mc[,1]
v1_promis_sri_mc <- pca_promis_sri_mc$svd$V # 1er vect propio tabla Deterioro
Comp_1.t4_mc <- as.matrix(datos_con_pesos[,23:30])%*%v1_promis_sri_mc[,1]
v1_asws_mc <- pca_asws_mc$svd$V # 1er vect propio tabla Sueño y Vigilia
Comp_1.t5_mc <- as.matrix(datos_con_pesos[,31:40])%*%v1_asws_mc[,1]
v1_promis_anxiety_mc <- pca_promis_anxiety_mc$svd$V # 1er vect propio tabla ansiedad
Comp_1.t6_mc <- as.matrix(datos_con_pesos[,41:48])%*%v1_promis_anxiety_mc[,1]
v1_promis_depression_mc <- pca_promis_depression_mc$svd$V # 1er vect propio tabla depresión
Comp_1.t7_mc <- as.matrix(datos_con_pesos[,49:56])%*%v1_promis_depression_mc[,1]
mat.1vects_mc <- data.frame(cbind(Comp_1.t1_mc,Comp_1.t2_mc,Comp_1.t3_mc,
                                  Comp_1.t4_mc,Comp_1.t5_mc,Comp_1.t6_mc,
                                  Comp_1.t7_mc)) # matriz 1 comps
colnames(mat.1vects_mc) <- c("Insomnio","Somnolencia","Alteración",
                             "Deterioro","Sueño y vigilia","Ansiedad",
                             "Depresión")
acp_niños_tablas_1v_mc <- PCA(mat.1vects_mc) # PCA matriz de primeros componentes
# % varianza
g2_var_mc <- fviz_eig(acp_niños_tablas_1v_mc, addlabels = TRUE, ylim = c(0, 70))+
  labs(title = "", x = "Dimensión",y = "% de Varianza Explicado")
# circulo de correlación
fviz_pca_var(acp_niños_tablas_1v_mc, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
# contribución variables dim 1 y dim 2
g2_contri1_mc <- fviz_contrib(acp_niños_tablas_1v_mc, choice="var", axes = 1,
                              top = 10, ggtheme = theme_minimal()) +
  labs(title = "",x = "Variables", y = "Contribución (%)")
g2_contri2_mc <- fviz_contrib(acp_niños_tablas_1v_mc, choice="var", axes = 2,
                              top = 10, ggtheme = theme_minimal()) +
  labs(title = "",x = "Variables", y = "Contribución (%)")
# nube de individuos
fviz_pca_ind(acp_niños_tablas_1v_mc, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = F,
             labelsize = 2, pointsize = 1) + ggtitle("")
################################ Tablas y ITSNE2 ###############################----
acp_niños_tablas_1v_mc$eig
data.frame(coord = acp_niños_tablas_1v_mc$var$coord[,c(1:2)], 
                          contrib = acp_niños_tablas_1v_mc$var$contrib[,c(1:2)])
eig.vectors.tablas_mc <- acp_niños_tablas_1v_mc$svd$V # 1er vect propio matriz 1 comps
ponderado_m2_2 <- eig.vectors.tablas_mc[,1]/sum(eig.vectors.tablas_mc[,1])
sum(ponderado_m2_2)
Comp_1.tablas_mc <- as.matrix(mat.1vects_mc)%*%ponderado_m2_2 # expresión de ITSNE2
escale_niños2_mc <- (Comp_1.tablas_mc - min(Comp_1.tablas_mc))*100/(max(Comp_1.tablas_mc)-min(Comp_1.tablas_mc))
print(min(Comp_1.tablas_mc)) # intervalo inferior 
print(max(Comp_1.tablas_mc)) # intervalo superior 
############################# ACP datos padres ship ############################----
padres_ship <- padres[,-1]
padres_ship <- padres_ship[,c(17:31)]
pca_padres_ship <- PCA(padres_ship)
# % varianza
fviz_eig(pca_padres_ship, addlabels = TRUE, ylim = c(0, 30))+ 
  labs(title = "", x = "Dimensión",y = "% de Varianza Explicado")
# circulo de correlación
fviz_pca_var(pca_padres_ship, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
# contribución variables dim 1 y dim 2
fviz_contrib(pca_padres_ship, choice="var", axes = 1, top = 10, 
             ggtheme = theme_minimal()) +
  labs(title = "",x = "Variables", y = "Contribución (%)")
fviz_contrib(pca_padres_ship, choice="var", axes = 2, top = 10, 
             ggtheme = theme_minimal()) +
  labs(title = "",x = "Variables", y = "Contribución (%)")
# nube de individuos
fviz_pca_ind(pca_padres_ship, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = F,labelsize = 2, pointsize = 1) + ggtitle("")
################################ Tablas y ITSP ###############################----
pca_padres_ship$eig
data.frame(coord = pca_padres_ship$var$coord[,c(1:2)], 
                          contrib = pca_padres_ship$var$contrib[,c(1:2)])
eig.vectors.ship <- pca_padres_ship$svd$V # vectores propios
ponderado_padres <- eig.vectors.ship[,1]/sum(eig.vectors.ship[,1])
Comp_1.padres_ship <- as.matrix(padres_ship)%*%ponderado_padres # expresión de ITSP
escale_padres_ship <- (Comp_1.padres_ship - min(Comp_1.padres_ship))*100/(max(Comp_1.padres_ship)-min(Comp_1.padres_ship)) # escala 3
min_teor_padres_ship <- sum(ponderado_padres[1]*1 + ponderado_padres[2]*1 
                            + ponderado_padres[3]*1 + ponderado_padres[4]*1 + 
                              ponderado_padres[5]*1 + ponderado_padres[6]*1 + 
                              ponderado_padres[7]*1 + ponderado_padres[8]*1 
                            + ponderado_padres[9]*1 + ponderado_padres[10]*1 + 
                              ponderado_padres[11]*1 + ponderado_padres[12]*1 
                            + ponderado_padres[13]*1 +ponderado_padres[14]*1
                            + ponderado_padres[15]*1)
max_teor_padres_ship <- sum(ponderado_padres[1]*3 + ponderado_padres[2]*3
                            + ponderado_padres[3]*3 + ponderado_padres[4]*3 + 
                              ponderado_padres[5]*3 + ponderado_padres[6]*3 + 
                              ponderado_padres[7]*3 + ponderado_padres[8]*3
                            + ponderado_padres[9]*3 + ponderado_padres[10]*3 + 
                              ponderado_padres[11]*3 + ponderado_padres[12]*3 + 
                              ponderado_padres[13]*3 + ponderado_padres[14]*3
                            + ponderado_padres[15]*3)
min(Comp_1.padres_ship) # intervalo inferior aplicado
max(Comp_1.padres_ship) # intervalo superior aplicado
print(min_teor_padres_ship) # intervalo inferior teórico
print(max_teor_padres_ship) # intervalo superior teórico
################## Correlación entre medidas niños y padres ####################----
cor(escale_niños1,rowMeans(puntajes.niños), method = "spearman")
cor(escale_niños2,rowMeans(puntajes.niños), method = "spearman")
cor(escale_niños3,rowMeans(puntajes.niños), method = "spearman")
cor(escale_niños1_mc,rowMeans(puntajes.niños), method = "spearman")
cor(escale_niños2_mc,rowMeans(puntajes.niños), method = "spearman")
cor(escale_padres_ship,rowMeans(puntajes.niños), method = "spearman")
################### Descriptivas medidas sintéticas aplicadas ##################----
summary(escale_niños1)
summary(escale_niños2)
summary(escale_niños3)
summary(escale_niños1_mc)
summary(escale_niños2_mc)
par(mfcol = c(2, 3))
hist(escale_niños1, col = "#104E8B", main = "ITSN1", xlab ="",ylab = "Frecuencia")
hist(escale_niños1_mc, col = "#1E90FF", main = "ITSNE1", xlab ="",ylab = "Frecuencia")
hist(escale_niños2, col = "#1874CD", main = "ITSN2", xlab= "",ylab = "Frecuencia")
hist(escale_niños2_mc, col = "#00BFFF", main = "ITSNE2", xlab ="",ylab = "Frecuencia")
hist(escale_niños3, col = "#1C86EE", main = "ITSN3", xlab ="",ylab="Frecuencia")
############################ Alpha de cronbach #################################----
matriz_med1 <- as.matrix(niños_inv) * matrix(rep(Mu_ponderado, 428), nrow = 428, byrow = TRUE)
psych::alpha(matriz_med1)
matriz_med2 <- as.matrix(mat.1vects) * matrix(rep(ponderado_m2_1, 428), nrow = 428, byrow = TRUE)
psych::alpha(matriz_med2)
matriz_med3 <- as.matrix(puntajes.niños) * matrix(rep(ponderado_m3, 428), nrow = 428, byrow = TRUE)
psych::alpha(matriz_med3)
matriz_med1_mc <- as.matrix(datos_con_pesos) * matrix(rep(Mu_mc_ponderado, 428), nrow = 428, byrow = TRUE)
psych::alpha(matriz_med1_mc)
matriz_med2_mc <- as.matrix(mat.1vects_mc) * matrix(rep(ponderado_m2_2, 428), nrow = 428, byrow = TRUE)
psych::alpha(matriz_med2_mc)
############################# Bootstrap y PECM #################################----
calculo_PECM <- function(df, simetria = 0){
  n_tamaños <- c(30,50,100,500,1000,10000,30000)
  
  if (simetria == 1){
    estimador_n <- numeric(7)
    estimador_boots <- list()
    
    for (j in 1:7) {
      n <- n_tamaños[j]
      set.seed(123)
      estimador_m <- numeric(10000)
      for (i in 1:10000) {
        boots <- sample(df,n,replace = T)
        estimador_m[i] <- mean(boots) 
      }
      estimador_boots[[j]] <- estimador_m
      estimador_n[j] <- mean(estimador_boots[[j]])
    }} else {
      estimador_n <- numeric(7)
      estimador_boots <- list()
      
      for (j in 1:7) {
        n <- n_tamaños[j]
        set.seed(123)
        estimador_m <- numeric(10000)
        for (i in 1:10000) {
          boots <- sample(df,n,replace = T)
          estimador_m[i] <- median(boots)  
        }
        estimador_boots[[j]] <- estimador_m
        estimador_n[j] <- median(estimador_boots[[j]])
      }}
  PECM <- NULL; VAR <- NULL ; SESGO_2 <- NULL
  for (k in 1:7) {
    PECM[k] <- var(estimador_boots[[k]]) + (mean(estimador_boots[[k]])-mean(estimador_n))^2
    VAR[k] <- var(estimador_boots[[k]])
    SESGO_2[k] <- (mean(estimador_boots[[k]])-mean(estimador_n))^2
  }
  return(list(PECM = PECM, estimador_boots = estimador_boots, VAR = VAR, SESGO_2 = SESGO_2))
}
PECM_ITSN1 <- calculo_PECM(escale_niños1)
PECM_ITSN2 <- calculo_PECM(escale_niños2)
PECM_ITSN3 <- calculo_PECM(escale_niños3)
PECM_ITSNE1 <- calculo_PECM(escale_niños1_mc)
PECM_ITSNE2 <- calculo_PECM(escale_niños2_mc)
par(mfcol = c(2, 3))
hist(PECM_ITSN1$medias_boots[[7]], col = "#6B8E23", main = "ITSN1", 
     xlab ="", ylab = "Frecuencia")
abline(v = mean(PECM_ITSN1$medias_boots[[7]]), col = "red", lwd = 2)  # lwd ajusta el grosor de la línea
text(x = mean(PECM_ITSN1$medias_boots[[7]]), y = 1700, 
     labels = round(mean(PECM_ITSN1$medias_boots[[7]]), 2), pos = 2, col = "red")
hist(PECM_ITSNE1$medias_boots[[7]], col = "#9ACD32", main = "ITSNE1", 
     xlab ="", ylab = "Frecuencia")
abline(v = mean(PECM_ITSNE1$medias_boots[[7]]), col = "red", lwd = 2)  # lwd ajusta el grosor de la línea
text(x = mean(PECM_ITSNE1$medias_boots[[7]]), y = 1600, 
     labels = round(mean(PECM_ITSNE1$medias_boots[[7]]), 2), pos = 2, col = "red")
hist(PECM_ITSN2$medias_boots[[7]], col = "#B3EE3A", main = "ITSN2", 
     xlab= "", ylab = "Frecuencia")
abline(v = mean(PECM_ITSN2$medias_boots[[7]]), col = "red", lwd = 2)  # lwd ajusta el grosor de la línea
text(x = mean(PECM_ITSN2$medias_boots[[7]]), y = 1650, 
     labels = round(mean(PECM_ITSN2$medias_boots[[7]]), 2), pos = 2, col = "red")
hist(PECM_ITSNE2$medias_boots[[7]], col = "#7CCD7C", main = "ITSNE2", 
     xlab ="", ylab = "Frecuencia")
abline(v = mean(PECM_ITSNE2$medias_boots[[7]]), col = "red", lwd = 2)  # lwd ajusta el grosor de la línea
text(x = mean(PECM_ITSNE2$medias_boots[[7]]), y = 1650, 
     labels = round(mean(PECM_ITSNE2$medias_boots[[7]]), 2), pos = 2, col = "red")
hist(PECM_ITSN3$medias_boots[[7]], col = "palegreen1", main = "ITSN3", 
     xlab ="", ylab = "Frecuencia")
abline(v = mean(PECM_ITSN3$medias_boots[[7]]), col = "red", lwd = 2)  # lwd ajusta el grosor de la línea
text(x = mean(PECM_ITSN3$medias_boots[[7]]), y = 1650, 
     labels = round(mean(PECM_ITSN3$medias_boots[[7]]), 2), pos = 2, col = "red")
tabu1 <- data.frame(Tamaño = c(30, 50, 100, 500, 1000, 10000, 30000),
                    PECM = PECM_ITSN1$PECM,
                    VAR = PECM_ITSN1$VAR,
                    SESGO = PECM_ITSN1$SESGO)
tabu2 <- data.frame(Tamaño = c(30, 50, 100, 500, 1000, 10000, 30000),
                    PECM = PECM_ITSN2$PECM,
                    VAR = PECM_ITSN2$VAR,
                    SESGO = PECM_ITSN2$SESGO)
tabu3 <- data.frame(Tamaño = c(30, 50, 100, 500, 1000, 10000, 30000),
                    PECM = PECM_ITSN3$PECM,
                    VAR = PECM_ITSN3$VAR,
                    SESGO = PECM_ITSN3$SESGO)
tabu4 <- data.frame(Tamaño = c(30, 50, 100, 500, 1000, 10000, 30000),
                    PECM = PECM_ITSNE1$PECM,
                    VAR = PECM_ITSNE1$VAR,
                    SESGO = PECM_ITSNE1$SESGO)
tabu5 <- data.frame(Tamaño = c(30, 50, 100, 500, 1000, 10000, 30000),
                    PECM = PECM_ITSNE2$PECM,
                    VAR = PECM_ITSNE2$VAR,
                    SESGO = PECM_ITSNE2$SESGO)
TABULADOS <- rbind(tabu1,tabu2,tabu3,tabu4,tabu5)
############################# Clúster y pruebas nopa ###########################----
fviz_nbclust(escale_niños2_mc, kmeans, method = "wss") + # n clúster óptimo
  geom_vline(xintercept = 3, linetype = 2) +
  labs(title = "Número óptimo de clusters", x = "Número de clusters", y = "Suma de cuadrados total")
# Configuraciones
n_iterations <- 10000
n_clusters <- 3
sample_kmeans <- function(datos) {    # función bootstrap para min y max clúster
  samplek <- sample(datos, length(datos), replace = TRUE)
  cluster <- kmeans(samplek, centers = n_clusters, nstart = 10)
  cluster_means <- sapply(1:3, function(k) mean(samplek[cluster$cluster == k]))
  cluster_order <- order(cluster_means)
  c(c1_min = min(samplek[cluster$cluster == cluster_order[1]]),
    c1_max = max(samplek[cluster$cluster == cluster_order[1]]),
    c2_min = min(samplek[cluster$cluster == cluster_order[2]]),
    c2_max = max(samplek[cluster$cluster == cluster_order[2]]),
    c3_min = min(samplek[cluster$cluster == cluster_order[3]]),
    c3_max = max(samplek[cluster$cluster == cluster_order[3]]))
}
set.seed(123)
resultados <- replicate(n_iterations, sample_kmeans(escale_niños2_mc))
mean(resultados[1,])
mean(resultados[2,])
mean(resultados[3,])
mean(resultados[4,])
mean(resultados[5,])
mean(resultados[6,])
data$puntajes <- escale_niños2_mc # Agregar los puntajes de ITSNE2a los datos
# pruebas chi cuadrado
leves <- data[data$puntajes<=19.40,]
moderado <- data[data$puntajes>19.40 & data$puntajes <= 44.30,]
severo <- data[data$puntajes>44.30,]
data$categoria <- ifelse(data$puntajes <= 19.40, "leve",
                         ifelse(data$puntajes > 19.40 & data$puntajes <= 44.30, "moderado",
                                "severo"))
sexo_t <- table(data$categoria,data$data.sexo)   
prop.table(sexo_t)
chi_sexo <- chisq.test(sexo_t) # si hay relación
prop.table(imc_t_combined)
imc_t <- table(data$categoria,data$`data.Diagnóstico IMC`)
imc_t_combined <- imc_t[, colnames(imc_t) != "null"]
imc_t_combined <- cbind(
  "delgadez + normal" = imc_t_combined[, c("normal")],
  "obesidad + sobrepeso" = rowSums(imc_t_combined[, c("obesidad", "sobrepeso")])
)
chi_imc <- chisq.test(imc_t_combined)
prop.table(g_edad_t)
g_edad_t <- table(data$categoria,data$data.grupedad)
chi_g_edad <- chisq.test(g_edad_t)
prop.table(enf)
enf <- table(data$categoria,data$data.enfermedad)
enf <- enf[, colnames(enf) != "null"]
chisq.test(enf)
prop.table(enf_fliar)
enf_fliar <- table(data$categoria,data$data.enfermedad_fliar)
enf_fliar <- enf_fliar[, colnames(enf_fliar) != "null"]
chisq.test(enf_fliar)
prop.table(gastro)
gastro <- table(data$categoria,data$data.dgf2)
gastro <- gastro[,colnames(gastro) != "null"]
chisq.test(gastro)