#' @export
Oyster_Volumes <-
function(subincr_matrix, Z_mat, IncG, Xstep=0.1){
    IncGAnet<-data.frame(matrix(0,ncol=length(IncG[1,]),nrow=length(IncG[,1])))
    colnames(IncGAnet)<-colnames(IncG)
    rownames(IncGAnet)<-rownames(IncG)
    VolC<-vector()
    As2<-vector()
    Y1s2<-vector()
    Ytops2<-vector()
    Y0s2<-vector()
    rs2<-vector()
    Ytopbs2<-vector()
    Y0bs2<-vector()
    rbs2<-vector()
    for(t in 1:length(IncGAnet[1,])){
        cat(paste("Calculating volume for subincrement: ",t),"\r")
        As<-vector()
        Y1s<-vector()
        Ytops<-vector()
        Y0s<-vector()
        rs<-vector()
        Ytopbs<-vector()
        Y0bs<-vector()
        rbs<-vector()
        for (x in 1:(length(IncGAnet[,1]))){
            # Universal parameters Z and Y1
            # Of which Y1 is the height of the base oval that forms the bottom of the shell under the increment, and Z is the distance from the x-axis to the side of the ellips
            Z<-Z_mat[x,t]
            Y1<-subincr_matrix[t,6] + ((subincr_matrix[t,7]-subincr_matrix[t,6])/(subincr_matrix[t,3]-subincr_matrix[t,2])) * (as.numeric(rownames(IncGAnet)[x])-subincr_matrix[t,2])
            if(is.na(Y1)){Y1<-0}
            # Parameters increment

            # Of which Ytop is the maximum height of the cross section of the shell under the increment, which is found for the X value by linear interpolation in the XY trace of the shell increment
            # Ytop is the Y value calculated in the incremental growth matrix

            # Y0 is the Y value of the center of the virtual circle that connects the top of the shell with the two points at the edge of the ellipse (Z1 and Z)
            # Formula of circle x^2 + y^2 = r^2 with points [-Z,Y1], [0,Ytop], and [Z,Y1], and center of circle = [0, Y0] gives: 
            # (0-0)^2 + (Ytop-Y0)^2 = (-Z-0)^2 + (Y1 - Y0)^2 = (Z- 0)^2 + (Y1 - Y0)^2
            # Y0 = (Ytop^2 - Y1^2 - Z^2) / (2*(Ytop - Y1))
            # r is the radius of said circle
            # r = (x^2 - y^2)^0.5 in point [Z, Y1] and center [0, Y0] gives:
            # r = (Z^2 + (Y1 - Y0)^2)^0.5
            Ytop<-IncG[x,t]
            Y0<-(Ytop^2-Y1^2-Z^2)/(2*(Ytop-Y1))
            r<-(Z^2+(Y1-Y0)^2)^0.5
            # Parameters shell top (b)
            # Ytopb and rb are calculated in the same way as Ytop and r above
            Ytopb<-IncG[x,1]
            Y0b<-(Ytopb^2-Y1^2-Z^2)/(2*(Ytopb-Y1))
            rb<-(Z^2+(Y1-Y0b)^2)^0.5

            # Prevent negative areas if increment go above the shell top
            if(Ytopb<Ytop){Ytop<-Ytopb}

            # Calculate area under shell increment
            # The area of the circle segment that is under the growth increment is found by 
            # subtracting the area of the triangle formed by the center of the circle and points P2[-Z,Y1] and P3[Z,Y1] from that of the corresponding circle sector 
            # A_circle = A_sector - A_triangle
            #          = angle/(2*pi) * pi * r^2 - rsin(angle)*rcos(angle)
            #          = 0.5*angle*r^2 - 0.5*r^2*sin(angle)
            #          = 0.5*r^2*(angle - sin(angle))
            # In which angle is the angle between the lines between the two points P2[-Z,Y1] and P3[Z,Y1] and the center of the circle (Pc)
            # The total area under the shell increment is always equal to the area between the circle segment and the x-axis
            # Area between the shell and the x-axis is not taken into account as the difference between top and shell increment is taken later on

            if(is.na(r)|r==abs(Inf)){
                angle<-0
                r<-0
            }
            else angle<-asin(Z/r)*2
            # If Y1 is higher than the top of the shell, the circle flips and has to be subtracted from the rectangle between Y0 and the x-axis
            if(Y1>Ytop){
                A<-(2*Z*Y1)-0.5*r^2*(angle - sin(angle))
            }
            else{
                # If Y0 is in between Y1 and Ytop, then the area is calculated by the full circle formed by Y0 (center) and Ytop minus the circlesection under Y1
                if(Y0>Y1 & !is.na(Y1) & !is.na(Y0)){
                    A<-r^2*pi-(0.5*r^2*(angle - sin(angle)))+(2*Z*Y1)
                }
                else A<-0.5*r^2*(angle - sin(angle))+(2*Z*Y1)
            }

            # Calculate area under shell top in the same way as the area under the increment was calculated
            if(is.na(rb)|rb==abs(Inf)){
                angleb<-0
                rb<-0
            }
            else angleb<-asin(Z/rb)*2
            if(Y1>Ytopb){
                Ab<-(2*Z*Y1)-0.5*rb^2*(angleb - sin(angleb))
            }
            else{
                # If Y0b is in between Y1 and Ytopb, then the area is calculated by the full circle between Y0b (center) and Ytopb minus the circlesection under Y1
                if(Y0b>Y1 & !is.na(Y1) & !is.na(Y0b)){
                    Ab<-rb^2*pi-(0.5*rb^2*(angleb - sin(angleb)))+(2*Z*Y1)
                }
                else Ab<-0.5*rb^2*(angleb - sin(angleb))+(2*Z*Y1)
            }
            # Prevent cases where increment overlaps with top of shell
            if(Ytop==Ytopb){
                Ab<-A
            }

            # Prevent cases of negative area
            if(Ab<A){
                Ab<-A
            }

            # Calculate area relative to shell top by subtracting area under top of shell from that under the shell (sub)increment
            IncGAnet[x,t]<-Ab-A
            As<-append(As,Ab-A)
            Y1s<-append(Y1s,Y1)
            Ytops<-append(Ytops,Ytop)
            Y0s<-append(Y0s,Y0)
            rs<-append(rs,r)
            Ytopbs<-append(Ytopbs,Ytopb)
            Y0bs<-append(Y0bs,Y0b)
            rbs<-append(rbs,rb)
        }
        # Calculate total volume relative to zero line per time increment by adding up the areas of all perpendicular (YZ)cross sections under the same increment and multiplying by the increment width
        VolC<-append(VolC,sum(As)*Xstep)
        As2<-cbind(As2,As)
        Y1s2<-cbind(Y1s2,Y1s)
        Ytops2<-cbind(Ytops2,Ytops)
        Y0s2<-cbind(Y0s2,Y0s)
        rs2<-cbind(rs2,rs)
        Ytopbs2<-cbind(Ytopbs2,Ytopbs)
        Y0bs2<-cbind(Y0bs2,Y0bs)
        rbs2<-cbind(rbs2,rbs)
    }
    # Calculate volume gain per time increment by subtracting volume value for each increment by its predecessor.
    # WARNING: changes in the slope of Y1 will be amplified into large oscillations of VolI. It is advised to use a smoothing of VolI for further analysis
    VolI<-c(VolC,0)-c(0,VolC)
    VolI<-VolI[-length(VolI)]
    # Prevent negative volume increments
    VolI[VolI<0]<-0
    VolC<-cumsum(VolI)
    subincr_matrix<-cbind(subincr_matrix,VolI,VolC)
    dev.new(); plot(subincr_matrix[,c(1,17)], type = "l")
    diagL<-list(subincr_matrix,IncGAnet)
    return(diagL)
}