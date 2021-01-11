library(MplusAutomation)

shinyServer(function(input, output) {

    get_param <- reactive({
        req(input$file1)
        inFile <- input$file1
        mod <- readModels(target = inFile$datapath, recursive = TRUE)
        get_params <- as.data.frame(mod$parameters)
        return(get_params)
        })

    get_var <- reactive({
        req(input$file1)
        inFile <- input$file1
        mod <- readModels(target = inFile$datapath, recursive = TRUE)
        get_variance<-as.data.frame(mod$sampstat$covariance)
        return(get_variance)
        })

    get_var_solution <- reactive({
        req(input$file1)
        inFile <- input$file1
        mod <- readModels(target = inFile$datapath, recursive = TRUE)

        get_params<-as.data.frame(mod$parameters$unstandardized)
        get_variance<-as.data.frame(mod$sampstat$covariance)


        temp <- get_params[grep("\\.ON",get_params$paramHeader),]
        psi_names <- unique(unlist(lapply(temp[,1], function(x) strsplit(x,"\\.ON")[[1]]))) #Names of endogenous variables
        phi_names <- unique(temp[,2])[which(!unique(temp[,2])%in%psi_names)] #Names of exogenous variables

        psi_mat <- matrix(0, length(psi_names), length(psi_names)) #construct empty psi matrix
        colnames(psi_mat) <- rownames(psi_mat) <- psi_names

        phi_mat <- matrix(0, length(phi_names), length(phi_names)) #construct empty phi matrix
        colnames(phi_mat) <- phi_names
        rownames(phi_mat) <- phi_names

        beta_mat <- matrix(0, length(psi_names), length(psi_names)) #construct empty beta matrix
        colnames(beta_mat) <- psi_names
        rownames(beta_mat) <- psi_names

        gamma_mat <- matrix(0, length(psi_names), length(phi_names)) #construct empty gamma matrix
        colnames(gamma_mat) <- phi_names
        rownames(gamma_mat) <- psi_names

        ### PSI
        tempwith <- get_params[grep("\\.WITH",get_params$paramHeader),] #single out WITH relationships
        poswith <- which(tempwith$param %in% psi_names) #identify which WITH relationships (rows) pertain to endogenous variables
        psi_nameswith <- unique(unlist(lapply(tempwith[poswith,1], function(x) strsplit(x,"\\.WITH")[[1]]))) #identify names of endogenous variables with relationships

        ### for off diagonals for psi (covariances)
        if(length(poswith) == 0) {poswith=0} else{poswith=poswith}
        for(i in poswith){
            rowj=which(rownames(psi_mat) %in% psi_nameswith[i])
            colk=which(colnames(psi_mat) %in% tempwith[i,2])
            psi_mat[rowj,colk]=tempwith$est[i]
            psi_mat[colk,rowj]=tempwith$est[i]
        }

        ### for diagonals for psi (variances)
        resvar<- get_params[get_params$paramHeader=="Residual.Variances",] #Single out residual variances
        pos_varpsi <- which(resvar$param %in% psi_names) #identify which residual variances pertain to endogenous variables
        resvar_psi<-resvar[pos_varpsi,] 

        for(i in 1:length(pos_varpsi)){
            diagi<- which(psi_names %in% resvar_psi$param[i])
            psi_mat[diagi,diagi]<- resvar_psi$est[i]
        }


        ### PHI
        tempwith <- get_params[grep("\\.WITH",get_params$paramHeader),] #Single out WITH relationships
        poswith <- which(tempwith$param %in% phi_names) #identify which WITH relationships pertain to exogenous variables
        phi_nameswith <- unique(unlist(lapply(tempwith[poswith,1], function(x) strsplit(x,"\\.WITH")[[1]]))) #Identify names of exogenous variables with WITH relationships

        ### for off diagonals of phi (covariances)
        if(length(poswith) == 0) {poswith=0} else{poswith=poswith}
        for(h in 1:poswith){
            for(i in 1:nrow(tempwith)){
                rowj=which(rownames(phi_mat) %in% phi_nameswith[h])
                colk=which(colnames(phi_mat) %in% tempwith[i,2])
                phi_mat[rowj,colk]=tempwith$est[i]
                phi_mat[colk,rowj]=tempwith$est[i]
            }
        }

        ### for diagonals for phi (latent)
        resvar<- get_params[get_params$paramHeader=="Variances",] #Single out reported variances
        pos_varphi <- which(resvar$param %in% phi_names) #determine which variances are exogenous
        resvar_phi<-resvar[pos_varphi,] 

        for(i in 1:length(pos_varphi)){
            diagi<- which(phi_names %in% resvar_phi$param[i])
            phi_mat[diagi,diagi]<- resvar_phi$est[i]
        }

        ### for diagonals for phi (observed)
        for(i in nrow(get_variance)){
            diagi <- which(phi_names %in% rownames(get_variance))
            phi_mat[diagi, diagi] <- get_variance[i,i]
        }



        ### BETA

        tempwith <- get_params[grep("\\.ON",get_params$paramHeader),] #Single out ON relationshisps
        poswith <- which(tempwith$param %in% psi_names) #Determine which ON relationships regard endogenous variables
        #beta_nameswith <- unique(unlist(lapply(tempwith[poswith,1], function(x) strsplit(x,"\\.ON")[[1]]))) #no longer needed
        lhs <- unlist(lapply(tempwith[,1], function(x) strsplit(x,"\\.ON")[[1]])) #Isolate left hand side (DV)

        ### for off diagonals for beta
        if(length(poswith) == 0) {poswith=0} else{poswith=poswith}
        for(h in poswith){
            rowj=which(rownames(beta_mat) %in% lhs[h])
            colk=which(colnames(beta_mat) %in% tempwith$param[h])
            beta_mat[rowj,colk] = tempwith$est[h]
        }


        ### GAMMA
        tempwith <- get_params[grep("\\.ON",get_params$paramHeader),] #Single out ON relationships
        lhs <- unlist(lapply(tempwith[,1], function(x) strsplit(x,"\\.ON")[[1]])) #isolate left hand side (DV)
        poswith <- which(tempwith$param %in% phi_names) #determine which ON relationships regard exogenous variables

        if(length(poswith) == 0) {poswith=0} else{poswith=poswith}
        for(h in poswith){
            rowj=which(rownames(gamma_mat) %in% lhs[h])
            colk=which(colnames(gamma_mat) %in% tempwith$param[h])
            gamma_mat[rowj,colk] = tempwith$est[h]
        }


        ### SOLUTION

        Imat <- diag(nrow(beta_mat))  # Create the identity matrix to match the dimensions of the Beta matrix you created.
        sol <- solve((Imat - beta_mat))%*%(gamma_mat%*%phi_mat%*%t(gamma_mat) + psi_mat)%*%t(solve((Imat - beta_mat))) # Calculate Frisby & Diemer 2020 solution.

        #print("Variances")
        diag(sol) # See variance calculations for all endogenous latent variables. 

        #print("Standard Deviations")
        sqrt(diag(sol)) # See standard deviation calculations for all endogenous latent variables.

        return(diag(sol))
    })


    get_sd_solution <- reactive({
        req(input$file1)
        inFile <- input$file1
        mod <- readModels(target = inFile$datapath, recursive = TRUE)

        get_params<-as.data.frame(mod$parameters$unstandardized)
        get_variance<-as.data.frame(mod$sampstat$covariance)


        temp <- get_params[grep("\\.ON",get_params$paramHeader),]
        psi_names <- unique(unlist(lapply(temp[,1], function(x) strsplit(x,"\\.ON")[[1]]))) #Names of endogenous variables
        phi_names <- unique(temp[,2])[which(!unique(temp[,2])%in%psi_names)] #Names of exogenous variables

        psi_mat <- matrix(0, length(psi_names), length(psi_names)) #construct empty psi matrix
        colnames(psi_mat) <- rownames(psi_mat) <- psi_names

        phi_mat <- matrix(0, length(phi_names), length(phi_names)) #construct empty phi matrix
        colnames(phi_mat) <- phi_names
        rownames(phi_mat) <- phi_names

        beta_mat <- matrix(0, length(psi_names), length(psi_names)) #construct empty beta matrix
        colnames(beta_mat) <- psi_names
        rownames(beta_mat) <- psi_names

        gamma_mat <- matrix(0, length(psi_names), length(phi_names)) #construct empty gamma matrix
        colnames(gamma_mat) <- phi_names
        rownames(gamma_mat) <- psi_names

        ### PSI
        tempwith <- get_params[grep("\\.WITH",get_params$paramHeader),] #single out WITH relationships
        poswith <- which(tempwith$param %in% psi_names) #identify which WITH relationships (rows) pertain to endogenous variables
        psi_nameswith <- unique(unlist(lapply(tempwith[poswith,1], function(x) strsplit(x,"\\.WITH")[[1]]))) #identify names of endogenous variables with relationships

        ### for off diagonals for psi (covariances)
        if(length(poswith) == 0) {poswith=0} else{poswith=poswith}
        for(i in poswith){
            rowj=which(rownames(psi_mat) %in% psi_nameswith[i])
            colk=which(colnames(psi_mat) %in% tempwith[i,2])
            psi_mat[rowj,colk]=tempwith$est[i]
            psi_mat[colk,rowj]=tempwith$est[i]
        }

        ### for diagonals for psi (variances)
        resvar<- get_params[get_params$paramHeader=="Residual.Variances",] #Single out residual variances
        pos_varpsi <- which(resvar$param %in% psi_names) #identify which residual variances pertain to endogenous variables
        resvar_psi<-resvar[pos_varpsi,] 

        for(i in 1:length(pos_varpsi)){
            diagi<- which(psi_names %in% resvar_psi$param[i])
            psi_mat[diagi,diagi]<- resvar_psi$est[i]
        }


        ### PHI
        tempwith <- get_params[grep("\\.WITH",get_params$paramHeader),] #Single out WITH relationships
        poswith <- which(tempwith$param %in% phi_names) #identify which WITH relationships pertain to exogenous variables
        phi_nameswith <- unique(unlist(lapply(tempwith[poswith,1], function(x) strsplit(x,"\\.WITH")[[1]]))) #Identify names of exogenous variables with WITH relationships

        ### for off diagonals of phi (covariances)
        if(length(poswith) == 0) {poswith=0} else{poswith=poswith}
        for(h in 1:poswith){
            for(i in 1:nrow(tempwith)){
                rowj=which(rownames(phi_mat) %in% phi_nameswith[h])
                colk=which(colnames(phi_mat) %in% tempwith[i,2])
                phi_mat[rowj,colk]=tempwith$est[i]
                phi_mat[colk,rowj]=tempwith$est[i]
            }
        }

        ### for diagonals for phi (latent)
        resvar<- get_params[get_params$paramHeader=="Variances",] #Single out reported variances
        pos_varphi <- which(resvar$param %in% phi_names) #determine which variances are exogenous
        resvar_phi<-resvar[pos_varphi,] 

        for(i in 1:length(pos_varphi)){
            diagi<- which(phi_names %in% resvar_phi$param[i])
            phi_mat[diagi,diagi]<- resvar_phi$est[i]
        }

        ### for diagonals for phi (observed)
        for(i in nrow(get_variance)){
            diagi <- which(phi_names %in% rownames(get_variance))
            phi_mat[diagi, diagi] <- get_variance[i,i]
        }



        ### BETA

        tempwith <- get_params[grep("\\.ON",get_params$paramHeader),] #Single out ON relationshisps
        poswith <- which(tempwith$param %in% psi_names) #Determine which ON relationships regard endogenous variables
        #beta_nameswith <- unique(unlist(lapply(tempwith[poswith,1], function(x) strsplit(x,"\\.ON")[[1]]))) #no longer needed
        lhs <- unlist(lapply(tempwith[,1], function(x) strsplit(x,"\\.ON")[[1]])) #Isolate left hand side (DV)

        ### for off diagonals for beta
        if(length(poswith) == 0) {poswith=0} else{poswith=poswith}
        for(h in poswith){
            rowj=which(rownames(beta_mat) %in% lhs[h])
            colk=which(colnames(beta_mat) %in% tempwith$param[h])
            beta_mat[rowj,colk] = tempwith$est[h]
        }


        ### GAMMA
        tempwith <- get_params[grep("\\.ON",get_params$paramHeader),] #Single out ON relationships
        lhs <- unlist(lapply(tempwith[,1], function(x) strsplit(x,"\\.ON")[[1]])) #isolate left hand side (DV)
        poswith <- which(tempwith$param %in% phi_names) #determine which ON relationships regard exogenous variables

        if(length(poswith) == 0) {poswith=0} else{poswith=poswith}
        for(h in poswith){
            rowj=which(rownames(gamma_mat) %in% lhs[h])
            colk=which(colnames(gamma_mat) %in% tempwith$param[h])
            gamma_mat[rowj,colk] = tempwith$est[h]
        }


        ### SOLUTION

        Imat <- diag(nrow(beta_mat))  # Create the identity matrix to match the dimensions of the Beta matrix you created.
        sol <- solve((Imat - beta_mat))%*%(gamma_mat%*%phi_mat%*%t(gamma_mat) + psi_mat)%*%t(solve((Imat - beta_mat))) # Calculate Frisby & Diemer 2020 solution.

        #print("Variances")
        diag(sol) # See variance calculations for all endogenous latent variables. 

        #print("Standard Deviations")
        sqrt(diag(sol)) # See standard deviation calculations for all endogenous latent variables.

        return(diag(sqrt(sol)))
    })

    output$endovar <- renderTable(get_var_solution(), colnames=FALSE, rownames=TRUE, digits = 4, 
    caption = "Your Endogenous LV Variances", caption.placement = getOption("xtable.caption.placement", "top"),
    caption.width = getOption("xtable.caption.width", "4000px"))

    output$endosd <- renderTable(get_sd_solution(), colnames=FALSE, rownames=TRUE, digits = 4, 
    caption = "Your Endogenous LV Standard Deviations", caption.placement = getOption("xtable.caption.placement", "top"),
    caption.width = getOption("xtable.caption.width", "4000px"))

  })
