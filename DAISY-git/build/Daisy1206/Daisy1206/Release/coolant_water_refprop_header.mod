	  Š+     k820309    ?          14.0        ĒĻ*Z                                                                                                           
       E:\documents\doctors degree\software\tansistant\NT\DAISY_verson_20170328\DAISY-git\git\src\thermal\coolant_water_refprop_header.f90 COOLANT_WATER_REFPROP_HEADER          WATERPROPERTY_REFPROP                  @                              
                                                      
                        @                              
       COOLANTPROPERTY               @                               '@            #COOLANT_TYPE    #DENSITY    #ENTHALPY    #TEMPERATURE    #CAPACITY 	   #CONDUCTIVITY 
   #VISCOSITY    #NUSSELT_NUMBER    #SET    #GET_DENSITY    #GET_ENTHALPY    #GET_TEMPERATURE    #GET_CAPACITY    #GET_CONDUCTIVITY "   #GET_VISCOSITY &   #GET_NUSSELT *             $                                                       $                                        
             $                                        
             $                                        
             $                             	            
             $                             
     (      
             $                                  0      
             $                                  8      
   1     Ā    $                                    	     #SET_COOLANTPROPERTY    #     @     @                                	               #THIS    #TYPE    #OPTION          
                                    @       #COOLANTPROPERTY          
                                              
                                        1     Ā    $                                   
     #GET_DENSITY_BY_TEMPERATURE_COOLANT    %     @    @                                              
   #THIS    #T_IN          
                                    @       #COOLANTPROPERTY          
                                     
  1     Ā    $                                        #GET_ENTHALPY_BY_TEMPERATURE_COOLANT    %     @    @                                              
   #THIS    #T_IN          
                                    @       #COOLANTPROPERTY          
                                     
  1     Ā    $                                        #GET_TEMPERATURE_BY_ENTHALPY_COOLANT    %     @    @                                              
   #THIS    #H_IN          
                                    @       #COOLANTPROPERTY          
                                     
  1     Ā    $                                        #GET_CAPACITY_BY_TEMPERATURE_COOLANT    %     @    @                                              
   #THIS     #T_IN !         
                                     @       #COOLANTPROPERTY          
                                !     
  1     Ā    $                          "              #GET_CONDUCTIVITY_BY_TEMPERATURE_COOLANT #   %     @    @                          #                    
   #THIS $   #T_IN %         
                               $     @       #COOLANTPROPERTY          
                                %     
  1     Ā    $                          &              #GET_VISCOSITY_BY_TEMPERATURE_COOLANT '   %     @    @                          '                    
   #THIS (   #T_IN )         
                               (     @       #COOLANTPROPERTY          
                                )     
  1     Ā    $                          *              #GET_NUSSELT_NUMBER +   %     @    @                          +                    
   #THIS ,   #P2D -   #VELOCITY .   #DH /   #T_IN 0         
                               ,     @       #COOLANTPROPERTY          
                                -     
        
                                .     
        
                                /     
        
                                0     
            @  @                          1     '0            #CASENAME 2   #MAIN 3   #MEMORY 4   #TIMELIST 5   #DET 6   #REACTIVITY 7   #POINTKINETICS 8   #PT 9   #TH_WARNING :   #TH_RBFD ;   #TH_HOT <   #TH_AVERAGE =             $                              2                         $                              3                        $                              4                        $                              5                        $                              6                        $                              7                        $                              8                        $                              9                        $                              :         	                $                              ;     $   
                $                              <     (                   $                              =     ,                       @                         >     'p           #COOLANTPROPERTY ?   #POINT @   #INIT S   #SET `   #GET_DENSITY e   #GET_ENTHALPY i   #GET_TEMPERATURE m   #GET_CAPACITY q   #GET_CONDUCTIVITY u   #GET_VISCOSITY y   #GET_NUSSELT }             $                              ?     @          #COOLANTPROPERTY              D                              @        @      #STAT_PARAMETER_TP A             @  @                          A     '            #WMM B   #T C   #P D   #RHO E   #ETA F   #TCX G   #E H   #H I   #S J   #CV K   #CP L   #W M   #HJT N   #D O   #DL P   #DV Q   #Q R                                           B            
                                           C           
                                           D           
                                           E           
                                           F            
                                           G     (      
                                           H     0      
                                           I     8      
                                           J     @   	   
                                           K     H   
   
                                           L     P      
                                           M     X      
                                           N     `      
                                           O     h      
                                           P     p      
                                           Q     x      
                                           R                        D                              S     Ļ  Č      #INITIAL_PARAMETER_TP T             @  @              @           T     'Ļ           #PRESSURE U   #NC_MAX V   #NC W   #IERR X   #X Y   #X_LIQUID Z   #X_VAPOR [   #HRF \   #HFMIX ]   #HERR ^   #HFILES _                                          U            
                                           V                                                      W                                                      X                                                    Y                 
        &                                                             Z        8         
        &                                                             [        \         
        &                                                              \                                                     ]     ĸ      	                                          ^     ĸ     
   .                                     _          ĸ              &                           1     Ā    $                           `              #SET_WATERPROPERTY a   #     @     @                             a                    #THIS b   #TYPE c   #OPTION d         
D @                              b     p      #WATERPROPERTY_REFPROP >         
                                  c             
                                  d       1     Ā    $                         e              #GET_DENSITY_BY_TEMPERATURE f   %     @    @                           f                    
   #THIS g   #T_IN h         
D @                              g     p      #WATERPROPERTY_REFPROP >         
                                 h     
  1     Ā    $                          i              #GET_ENTHALPY_BY_TEMPERATURE j   %     @    @                            j                    
   #THIS k   #T_IN l         
D @                              k     p      #WATERPROPERTY_REFPROP >         
                                 l     
  1     Ā    $                          m              #GET_TEMPERATURE_BY_ENTHALPY n   %     @    @                            n                    
   #THIS o   #H_IN p         
D @                              o     p      #WATERPROPERTY_REFPROP >         
                                 p     
  1     Ā    $                         q              #GET_CAPACITY_BY_TEMPERATURE r   %     @    @                           r                    
   #THIS s   #T_IN t         
D @                              s     p      #WATERPROPERTY_REFPROP >         
                                 t     
  1     Ā    $                         u         	     #GET_CONDUCTIVITY_BY_TEMPERATURE v   %     @    @                           v                    
   #THIS w   #T_IN x         
D @                              w     p      #WATERPROPERTY_REFPROP >         
                                 x     
  1     Ā    $                         y         
     #GET_VISCOSITY_BY_TEMPERATURE z   %     @    @                           z                    
   #THIS {   #T_IN |         
D @                              {     p      #WATERPROPERTY_REFPROP >         
                                 |     
  1     Ā    $                          }              #GET_NUSSELT_NUMBER ~   %     @    @                            ~                    
   #THIS    #P2D    #VELOCITY    #DH    #T_IN          
D @                                   p      #WATERPROPERTY_REFPROP >         
                                      
        
                                      
        
                                      
        
  @                                   
  *          n                 S              Cifmodintr.lib                         Đ      fn#fn 2   I  "   b   uapp(COOLANT_WATER_REFPROP_HEADER    k  <   J  CONSTANTS     §  <   J  ISO_FORTRAN_ENV )   ã  L   J  ABSTRACT_PROPERTY_HEADER 9   /  R      COOLANTPROPERTY+ABSTRACT_PROPERTY_HEADER F     @   a   COOLANTPROPERTY%COOLANT_TYPE+ABSTRACT_PROPERTY_HEADER A   Á  @   a   COOLANTPROPERTY%DENSITY+ABSTRACT_PROPERTY_HEADER B     @   a   COOLANTPROPERTY%ENTHALPY+ABSTRACT_PROPERTY_HEADER E   A  @   a   COOLANTPROPERTY%TEMPERATURE+ABSTRACT_PROPERTY_HEADER B     @   a   COOLANTPROPERTY%CAPACITY+ABSTRACT_PROPERTY_HEADER F   Á  @   a   COOLANTPROPERTY%CONDUCTIVITY+ABSTRACT_PROPERTY_HEADER C     @   a   COOLANTPROPERTY%VISCOSITY+ABSTRACT_PROPERTY_HEADER H   A  @   a   COOLANTPROPERTY%NUSSELT_NUMBER+ABSTRACT_PROPERTY_HEADER =     Y   a   COOLANTPROPERTY%SET+ABSTRACT_PROPERTY_HEADER =   Ú  d      SET_COOLANTPROPERTY+ABSTRACT_PROPERTY_HEADER B   >  Q   a   SET_COOLANTPROPERTY%THIS+ABSTRACT_PROPERTY_HEADER B     8   a   SET_COOLANTPROPERTY%TYPE+ABSTRACT_PROPERTY_HEADER D   Į  8   a   SET_COOLANTPROPERTY%OPTION+ABSTRACT_PROPERTY_HEADER E   ĸ  h   a   COOLANTPROPERTY%GET_DENSITY+ABSTRACT_PROPERTY_HEADER L   g  \      GET_DENSITY_BY_TEMPERATURE_COOLANT+ABSTRACT_PROPERTY_HEADER Q   Ã  Q   a   GET_DENSITY_BY_TEMPERATURE_COOLANT%THIS+ABSTRACT_PROPERTY_HEADER Q     8   a   GET_DENSITY_BY_TEMPERATURE_COOLANT%T_IN+ABSTRACT_PROPERTY_HEADER F   L  i   a   COOLANTPROPERTY%GET_ENTHALPY+ABSTRACT_PROPERTY_HEADER M   ĩ  \      GET_ENTHALPY_BY_TEMPERATURE_COOLANT+ABSTRACT_PROPERTY_HEADER R   	  Q   a   GET_ENTHALPY_BY_TEMPERATURE_COOLANT%THIS+ABSTRACT_PROPERTY_HEADER R   b	  8   a   GET_ENTHALPY_BY_TEMPERATURE_COOLANT%T_IN+ABSTRACT_PROPERTY_HEADER I   	  i   a   COOLANTPROPERTY%GET_TEMPERATURE+ABSTRACT_PROPERTY_HEADER M   
  \      GET_TEMPERATURE_BY_ENTHALPY_COOLANT+ABSTRACT_PROPERTY_HEADER R   _
  Q   a   GET_TEMPERATURE_BY_ENTHALPY_COOLANT%THIS+ABSTRACT_PROPERTY_HEADER R   °
  8   a   GET_TEMPERATURE_BY_ENTHALPY_COOLANT%H_IN+ABSTRACT_PROPERTY_HEADER F   č
  i   a   COOLANTPROPERTY%GET_CAPACITY+ABSTRACT_PROPERTY_HEADER M   Q  \      GET_CAPACITY_BY_TEMPERATURE_COOLANT+ABSTRACT_PROPERTY_HEADER R   ­  Q   a   GET_CAPACITY_BY_TEMPERATURE_COOLANT%THIS+ABSTRACT_PROPERTY_HEADER R   þ  8   a   GET_CAPACITY_BY_TEMPERATURE_COOLANT%T_IN+ABSTRACT_PROPERTY_HEADER J   6  m   a   COOLANTPROPERTY%GET_CONDUCTIVITY+ABSTRACT_PROPERTY_HEADER Q   Ģ  \      GET_CONDUCTIVITY_BY_TEMPERATURE_COOLANT+ABSTRACT_PROPERTY_HEADER V   ĸ  Q   a   GET_CONDUCTIVITY_BY_TEMPERATURE_COOLANT%THIS+ABSTRACT_PROPERTY_HEADER V   P  8   a   GET_CONDUCTIVITY_BY_TEMPERATURE_COOLANT%T_IN+ABSTRACT_PROPERTY_HEADER G     j   a   COOLANTPROPERTY%GET_VISCOSITY+ABSTRACT_PROPERTY_HEADER N   ō  \      GET_VISCOSITY_BY_TEMPERATURE_COOLANT+ABSTRACT_PROPERTY_HEADER S   N  Q   a   GET_VISCOSITY_BY_TEMPERATURE_COOLANT%THIS+ABSTRACT_PROPERTY_HEADER S     8   a   GET_VISCOSITY_BY_TEMPERATURE_COOLANT%T_IN+ABSTRACT_PROPERTY_HEADER E   Ũ  X   a   COOLANTPROPERTY%GET_NUSSELT+ABSTRACT_PROPERTY_HEADER <   /  {      GET_NUSSELT_NUMBER+ABSTRACT_PROPERTY_HEADER A   Š  Q   a   GET_NUSSELT_NUMBER%THIS+ABSTRACT_PROPERTY_HEADER @   û  8   a   GET_NUSSELT_NUMBER%P2D+ABSTRACT_PROPERTY_HEADER E   3  8   a   GET_NUSSELT_NUMBER%VELOCITY+ABSTRACT_PROPERTY_HEADER ?   k  8   a   GET_NUSSELT_NUMBER%DH+ABSTRACT_PROPERTY_HEADER A   Ģ  8   a   GET_NUSSELT_NUMBER%T_IN+ABSTRACT_PROPERTY_HEADER "   Û  ã      FILE_TP+CONSTANTS +   ū  @   a   FILE_TP%CASENAME+CONSTANTS '   þ  @   a   FILE_TP%MAIN+CONSTANTS )   >  @   a   FILE_TP%MEMORY+CONSTANTS +   ~  @   a   FILE_TP%TIMELIST+CONSTANTS &   ū  @   a   FILE_TP%DET+CONSTANTS -   þ  @   a   FILE_TP%REACTIVITY+CONSTANTS 0   >  @   a   FILE_TP%POINTKINETICS+CONSTANTS %   ~  @   a   FILE_TP%PT+CONSTANTS -   ū  @   a   FILE_TP%TH_WARNING+CONSTANTS *   þ  @   a   FILE_TP%TH_RBFD+CONSTANTS )   >  @   a   FILE_TP%TH_HOT+CONSTANTS -   ~  @   a   FILE_TP%TH_AVERAGE+CONSTANTS &   ū  û       WATERPROPERTY_REFPROP 6   đ  U   a   WATERPROPERTY_REFPROP%COOLANTPROPERTY ,     W   !   WATERPROPERTY_REFPROP%POINT "   e  É       STAT_PARAMETER_TP &   .  @   a   STAT_PARAMETER_TP%WMM $   n  @   a   STAT_PARAMETER_TP%T $   Ū  @   a   STAT_PARAMETER_TP%P &   î  @   a   STAT_PARAMETER_TP%RHO &   .  @   a   STAT_PARAMETER_TP%ETA &   n  @   a   STAT_PARAMETER_TP%TCX $   Ū  @   a   STAT_PARAMETER_TP%E $   î  @   a   STAT_PARAMETER_TP%H $   .  @   a   STAT_PARAMETER_TP%S %   n  @   a   STAT_PARAMETER_TP%CV %   Ū  @   a   STAT_PARAMETER_TP%CP $   î  @   a   STAT_PARAMETER_TP%W &   .  @   a   STAT_PARAMETER_TP%HJT $   n  @   a   STAT_PARAMETER_TP%D %   Ū  @   a   STAT_PARAMETER_TP%DL %   î  @   a   STAT_PARAMETER_TP%DV $   .  @   a   STAT_PARAMETER_TP%Q +   n  Z   !   WATERPROPERTY_REFPROP%INIT %   Č  ž       INITIAL_PARAMETER_TP .     @   a   INITIAL_PARAMETER_TP%PRESSURE ,   Ä  @   a   INITIAL_PARAMETER_TP%NC_MAX (     @   a   INITIAL_PARAMETER_TP%NC *   D  @   a   INITIAL_PARAMETER_TP%IERR '     l   a   INITIAL_PARAMETER_TP%X .   ð  l   a   INITIAL_PARAMETER_TP%X_LIQUID -   \  l   a   INITIAL_PARAMETER_TP%X_VAPOR )   Č  @   a   INITIAL_PARAMETER_TP%HRF +     @   a   INITIAL_PARAMETER_TP%HFMIX *   H  @   a   INITIAL_PARAMETER_TP%HERR ,     p   a   INITIAL_PARAMETER_TP%HFILES *   ø  W   a   WATERPROPERTY_REFPROP%SET "   O   d      SET_WATERPROPERTY '   ģ   W   a   SET_WATERPROPERTY%THIS '   
!  8   a   SET_WATERPROPERTY%TYPE )   B!  8   a   SET_WATERPROPERTY%OPTION 2   z!  `   a   WATERPROPERTY_REFPROP%GET_DENSITY +   Ú!  \      GET_DENSITY_BY_TEMPERATURE 0   6"  W   a   GET_DENSITY_BY_TEMPERATURE%THIS 0   "  8   a   GET_DENSITY_BY_TEMPERATURE%T_IN 3   Å"  a   a   WATERPROPERTY_REFPROP%GET_ENTHALPY ,   &#  \      GET_ENTHALPY_BY_TEMPERATURE 1   #  W   a   GET_ENTHALPY_BY_TEMPERATURE%THIS 1   Ų#  8   a   GET_ENTHALPY_BY_TEMPERATURE%T_IN 6   $  a   a   WATERPROPERTY_REFPROP%GET_TEMPERATURE ,   r$  \      GET_TEMPERATURE_BY_ENTHALPY 1   Î$  W   a   GET_TEMPERATURE_BY_ENTHALPY%THIS 1   %%  8   a   GET_TEMPERATURE_BY_ENTHALPY%H_IN 3   ]%  a   a   WATERPROPERTY_REFPROP%GET_CAPACITY ,   ū%  \      GET_CAPACITY_BY_TEMPERATURE 1   &  W   a   GET_CAPACITY_BY_TEMPERATURE%THIS 1   q&  8   a   GET_CAPACITY_BY_TEMPERATURE%T_IN 7   Đ&  e   a   WATERPROPERTY_REFPROP%GET_CONDUCTIVITY 0   '  \      GET_CONDUCTIVITY_BY_TEMPERATURE 5   j'  W   a   GET_CONDUCTIVITY_BY_TEMPERATURE%THIS 5   Á'  8   a   GET_CONDUCTIVITY_BY_TEMPERATURE%T_IN 4   ų'  b   a   WATERPROPERTY_REFPROP%GET_VISCOSITY -   [(  \      GET_VISCOSITY_BY_TEMPERATURE 2   ·(  W   a   GET_VISCOSITY_BY_TEMPERATURE%THIS 2   )  8   a   GET_VISCOSITY_BY_TEMPERATURE%T_IN 2   F)  X   a   WATERPROPERTY_REFPROP%GET_NUSSELT #   )  {      GET_NUSSELT_NUMBER (   *  W   a   GET_NUSSELT_NUMBER%THIS '   p*  8   a   GET_NUSSELT_NUMBER%P2D ,   Ļ*  8   a   GET_NUSSELT_NUMBER%VELOCITY &   ā*  8   a   GET_NUSSELT_NUMBER%DH (   +  8   a   GET_NUSSELT_NUMBER%T_IN    P+  Z      MsObjComment 