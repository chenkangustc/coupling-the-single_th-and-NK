	  ,     k820309    ?          14.0        ĒĻ*Z                                                                                                           
       E:\documents\doctors degree\software\tansistant\NT\DAISY_verson_20170328\DAISY-git\git\src\thermal\abstract_property_header.f90 ABSTRACT_PROPERTY_HEADER          COOLANTPROPERTY CLADPROPERTY GAPPROPERTY FUELPROPERTY                  @                              
                                                      
                 @  @                               '0            #CASENAME    #MAIN    #MEMORY    #TIMELIST    #DET    #REACTIVITY 	   #POINTKINETICS 
   #PT    #TH_WARNING    #TH_RBFD    #TH_HOT    #TH_AVERAGE              $                                                       $                                                      $                                                      $                                                      $                                                      $                              	                        $                              
                        $                                                      $                                       	                $                                   $   
                $                                   (                   $                                   ,                       @                               '@            #COOLANT_TYPE    #DENSITY    #ENTHALPY    #TEMPERATURE    #CAPACITY    #CONDUCTIVITY    #VISCOSITY    #NUSSELT_NUMBER    #SET    #GET_DENSITY    #GET_ENTHALPY "   #GET_TEMPERATURE &   #GET_CAPACITY *   #GET_CONDUCTIVITY .   #GET_VISCOSITY 2   #GET_NUSSELT 6             $                                                       $                                        
             $                                        
             $                                        
             $                                         
             $                                  (      
             $                                  0      
             $                                  8      
   1     Ā    $                                    	     #SET_COOLANTPROPERTY    #     @     @                                 	               #THIS    #TYPE    #OPTION          
                                    @       #COOLANTPROPERTY          
                                              
                                        1     Ā    $                                   
     #GET_DENSITY_BY_TEMPERATURE_COOLANT    %     @    @                                               
   #THIS     #T_IN !         
                                     @       #COOLANTPROPERTY          
                                !     
  1     Ā    $                          "              #GET_ENTHALPY_BY_TEMPERATURE_COOLANT #   %     @    @                           #                    
   #THIS $   #T_IN %         
                               $     @       #COOLANTPROPERTY          
                                %     
  1     Ā    $                          &              #GET_TEMPERATURE_BY_ENTHALPY_COOLANT '   %     @    @                           '                    
   #THIS (   #H_IN )         
                               (     @       #COOLANTPROPERTY          
                                )     
  1     Ā    $                          *              #GET_CAPACITY_BY_TEMPERATURE_COOLANT +   %     @    @                           +                    
   #THIS ,   #T_IN -         
                               ,     @       #COOLANTPROPERTY          
                                -     
  1     Ā    $                          .              #GET_CONDUCTIVITY_BY_TEMPERATURE_COOLANT /   %     @    @                           /                    
   #THIS 0   #T_IN 1         
                               0     @       #COOLANTPROPERTY          
                                1     
  1     Ā    $                          2              #GET_VISCOSITY_BY_TEMPERATURE_COOLANT 3   %     @    @                           3                    
   #THIS 4   #T_IN 5         
                               4     @       #COOLANTPROPERTY          
                                5     
  1     Ā    $                          6              #GET_NUSSELT_NUMBER 7   %     @    @                           7                    
   #THIS 8   #P2D 9   #VELOCITY :   #DH ;   #T_IN <         
                               8     @       #COOLANTPROPERTY          
                                9     
        
                                :     
        
                                ;     
        
                                <     
                @                          =     '(      
      #CLAD_TYPE >   #DENSITY ?   #CAPACITY @   #CONDUCTIVITY A   #EXPANSION B   #SET C   #GET_DENSITY G   #GET_CAPACITY K   #GET_CONDUCTIVITY O   #GET_EXPANSION S             $                              >                         $                             ?           
             $                             @           
             $                             A           
             $                             B            
   1     Ā    $                           C              #SET_CLADPROPERTY D   #     @     @                            D     	               #THIS E   #TYPE F         
                               E     (       #CLADPROPERTY =         
                                 F       1     Ā    $                          G              #GET_DENSITY_BY_TEMPERATURE_CLAD H   %     @    @                           H                    
   #THIS I   #T_IN J         
                               I     (       #CLADPROPERTY =         
                                J     
  1     Ā    $                          K              #GET_CAPACITY_BY_TEMPERATURE_CLAD L   %     @    @                           L                    
   #THIS M   #T_IN N         
                               M     (       #CLADPROPERTY =         
                                N     
  1     Ā    $                          O         	     #GET_CONDUCTIVITY_BY_TEMPERATURE_CLAD P   %     @    @                           P                    
   #THIS Q   #T_IN R         
                               Q     (       #CLADPROPERTY =         
                                R     
  1     Ā    $                          S         
     #GET_EXPANSION_BY_TEMPERATURE_CLAD T   %     @    @                           T                    
   #THIS U   #T_IN V         
                               U     (       #CLADPROPERTY =         
                                V     
                @                          W     '            #GAS_TYPE X   #H_TRANSFER Y   #SET Z   #GET_TRANSFER `             $                              X                         $                             Y           
   1     Ā    $                           Z              #SET_GAPPROPERTY [   #     @     @                            [     	               #THIS \   #TYPE ]   #X_XE ^   #X_KR _         
                               \            #GAPPROPERTY W         
                                 ]             
                               ^     
        
                               _     
  1     Ā    $                          `              #GET_TRANSFER_BY_TEMPERATURE_GAP a   %     @    @                           a                    
   #THIS b   #T_IN c   #PELLET d   #GAP e   #IS_INNER f         
                               b            #GAPPROPERTY W         
                                c     
        
                                d     
        
                                e     
        
                                 f                     @                          g     '8            #T_MELTING h   #MOL_MASS i   #FUEL_TYPE j   #DENSITY k   #CAPACITY l   #CONDUCTIVITY m   #EXPANSION n   #SET o   #GET_DENSITY v   #GET_CAPACITY z   #GET_CONDUCTIVITY ~   #GET_EXPANSION              $                             h            
             $                             i           
             $                              j                        $                             k           
             $                             l            
             $                             m     (      
             $                             n     0      
   1     Ā    $                           o              #SET_FUELPROPERTY p   #     @     @                            p     	               #THIS q   #TYPE r   #WEIGHT_ZR s   #X_PU t   #X_O2M u         
                               q     8       #FUELPROPERTY g         
                                 r             
                               s     
        
                               t     
        
                               u     
  1     Ā    $                          v         	     #GET_DENSITY_BY_TEMPERATURE_FUEL w   %     @    @                           w                    
   #THIS x   #T_IN y         
                               x     8       #FUELPROPERTY g         
                                y     
  1     Ā    $                          z         
     #GET_CAPACITY_BY_TEMPERATURE_FUEL {   %     @    @                           {                    
   #THIS |   #T_IN }         
                               |     8       #FUELPROPERTY g         
                                }     
  1     Ā    $                          ~              #GET_CONDUCTIVTY_BY_TEMPERATURE_FUEL    %     @    @                                               
   #THIS    #T_IN          
                                    8       #FUELPROPERTY g         
                                     
  1     Ā    $                                        #GET_EXPANSION_BY_TEMPERATURE_FUEL    %     @    @                                               
   #THIS    #T_IN          
                                    8       #FUELPROPERTY g         
                                     
  *          n                 )              Cifmodintr.lib                         Ą      fn#fn .   A  B   b   uapp(ABSTRACT_PROPERTY_HEADER      <   J  CONSTANTS     ŋ  <   J  ISO_FORTRAN_ENV "   û  ã      FILE_TP+CONSTANTS +   Þ  @   a   FILE_TP%CASENAME+CONSTANTS '     @   a   FILE_TP%MAIN+CONSTANTS )   ^  @   a   FILE_TP%MEMORY+CONSTANTS +     @   a   FILE_TP%TIMELIST+CONSTANTS &   Þ  @   a   FILE_TP%DET+CONSTANTS -     @   a   FILE_TP%REACTIVITY+CONSTANTS 0   ^  @   a   FILE_TP%POINTKINETICS+CONSTANTS %     @   a   FILE_TP%PT+CONSTANTS -   Þ  @   a   FILE_TP%TH_WARNING+CONSTANTS *     @   a   FILE_TP%TH_RBFD+CONSTANTS )   ^  @   a   FILE_TP%TH_HOT+CONSTANTS -     @   a   FILE_TP%TH_AVERAGE+CONSTANTS     Þ  R      COOLANTPROPERTY -   0  @   a   COOLANTPROPERTY%COOLANT_TYPE (   p  @   a   COOLANTPROPERTY%DENSITY )   °  @   a   COOLANTPROPERTY%ENTHALPY ,   ð  @   a   COOLANTPROPERTY%TEMPERATURE )   0  @   a   COOLANTPROPERTY%CAPACITY -   p  @   a   COOLANTPROPERTY%CONDUCTIVITY *   °  @   a   COOLANTPROPERTY%VISCOSITY /   ð  @   a   COOLANTPROPERTY%NUSSELT_NUMBER $   0	  Y   a   COOLANTPROPERTY%SET $   	  d      SET_COOLANTPROPERTY )   í	  Q   a   SET_COOLANTPROPERTY%THIS )   >
  8   a   SET_COOLANTPROPERTY%TYPE +   v
  8   a   SET_COOLANTPROPERTY%OPTION ,   Ū
  h   a   COOLANTPROPERTY%GET_DENSITY 3     \      GET_DENSITY_BY_TEMPERATURE_COOLANT 8   r  Q   a   GET_DENSITY_BY_TEMPERATURE_COOLANT%THIS 8   Ã  8   a   GET_DENSITY_BY_TEMPERATURE_COOLANT%T_IN -   û  i   a   COOLANTPROPERTY%GET_ENTHALPY 4   d  \      GET_ENTHALPY_BY_TEMPERATURE_COOLANT 9   Ā  Q   a   GET_ENTHALPY_BY_TEMPERATURE_COOLANT%THIS 9     8   a   GET_ENTHALPY_BY_TEMPERATURE_COOLANT%T_IN 0   I  i   a   COOLANTPROPERTY%GET_TEMPERATURE 4   ē  \      GET_TEMPERATURE_BY_ENTHALPY_COOLANT 9     Q   a   GET_TEMPERATURE_BY_ENTHALPY_COOLANT%THIS 9   _  8   a   GET_TEMPERATURE_BY_ENTHALPY_COOLANT%H_IN -     i   a   COOLANTPROPERTY%GET_CAPACITY 4      \      GET_CAPACITY_BY_TEMPERATURE_COOLANT 9   \  Q   a   GET_CAPACITY_BY_TEMPERATURE_COOLANT%THIS 9   ­  8   a   GET_CAPACITY_BY_TEMPERATURE_COOLANT%T_IN 1   å  m   a   COOLANTPROPERTY%GET_CONDUCTIVITY 8   R  \      GET_CONDUCTIVITY_BY_TEMPERATURE_COOLANT =   Ū  Q   a   GET_CONDUCTIVITY_BY_TEMPERATURE_COOLANT%THIS =   ĸ  8   a   GET_CONDUCTIVITY_BY_TEMPERATURE_COOLANT%T_IN .   7  j   a   COOLANTPROPERTY%GET_VISCOSITY 5   Ą  \      GET_VISCOSITY_BY_TEMPERATURE_COOLANT :   ý  Q   a   GET_VISCOSITY_BY_TEMPERATURE_COOLANT%THIS :   N  8   a   GET_VISCOSITY_BY_TEMPERATURE_COOLANT%T_IN ,     X   a   COOLANTPROPERTY%GET_NUSSELT #   Þ  {      GET_NUSSELT_NUMBER (   Y  Q   a   GET_NUSSELT_NUMBER%THIS '   Š  8   a   GET_NUSSELT_NUMBER%P2D ,   â  8   a   GET_NUSSELT_NUMBER%VELOCITY &     8   a   GET_NUSSELT_NUMBER%DH (   R  8   a   GET_NUSSELT_NUMBER%T_IN      ä       CLADPROPERTY '   n  @   a   CLADPROPERTY%CLAD_TYPE %   Ū  @   a   CLADPROPERTY%DENSITY &   î  @   a   CLADPROPERTY%CAPACITY *   .  @   a   CLADPROPERTY%CONDUCTIVITY '   n  @   a   CLADPROPERTY%EXPANSION !   Ū  V   a   CLADPROPERTY%SET !     X      SET_CLADPROPERTY &   \  N   a   SET_CLADPROPERTY%THIS &   Š  8   a   SET_CLADPROPERTY%TYPE )   â  e   a   CLADPROPERTY%GET_DENSITY 0   G  \      GET_DENSITY_BY_TEMPERATURE_CLAD 5   Ģ  N   a   GET_DENSITY_BY_TEMPERATURE_CLAD%THIS 5   ņ  8   a   GET_DENSITY_BY_TEMPERATURE_CLAD%T_IN *   )  f   a   CLADPROPERTY%GET_CAPACITY 1     \      GET_CAPACITY_BY_TEMPERATURE_CLAD 6   ë  N   a   GET_CAPACITY_BY_TEMPERATURE_CLAD%THIS 6   9  8   a   GET_CAPACITY_BY_TEMPERATURE_CLAD%T_IN .   q  j   a   CLADPROPERTY%GET_CONDUCTIVITY 5   Û  \      GET_CONDUCTIVITY_BY_TEMPERATURE_CLAD :   7  N   a   GET_CONDUCTIVITY_BY_TEMPERATURE_CLAD%THIS :     8   a   GET_CONDUCTIVITY_BY_TEMPERATURE_CLAD%T_IN +   ―  g   a   CLADPROPERTY%GET_EXPANSION 2   $  \      GET_EXPANSION_BY_TEMPERATURE_CLAD 7     N   a   GET_EXPANSION_BY_TEMPERATURE_CLAD%THIS 7   Î  8   a   GET_EXPANSION_BY_TEMPERATURE_CLAD%T_IN      }       GAPPROPERTY %     @   a   GAPPROPERTY%GAS_TYPE '   Ã  @   a   GAPPROPERTY%H_TRANSFER       U   a   GAPPROPERTY%SET     X  l      SET_GAPPROPERTY %   Ä  M   a   SET_GAPPROPERTY%THIS %     8   a   SET_GAPPROPERTY%TYPE %   I  8   a   SET_GAPPROPERTY%X_XE %     8   a   SET_GAPPROPERTY%X_KR )   đ  e   a   GAPPROPERTY%GET_TRANSFER 0            GET_TRANSFER_BY_TEMPERATURE_GAP 5      M   a   GET_TRANSFER_BY_TEMPERATURE_GAP%THIS 5   ę   8   a   GET_TRANSFER_BY_TEMPERATURE_GAP%T_IN 7   "!  8   a   GET_TRANSFER_BY_TEMPERATURE_GAP%PELLET 4   Z!  8   a   GET_TRANSFER_BY_TEMPERATURE_GAP%GAP 9   !  8   a   GET_TRANSFER_BY_TEMPERATURE_GAP%IS_INNER    Ę!        FUELPROPERTY '   Ë"  @   a   FUELPROPERTY%T_MELTING &   #  @   a   FUELPROPERTY%MOL_MASS '   K#  @   a   FUELPROPERTY%FUEL_TYPE %   #  @   a   FUELPROPERTY%DENSITY &   Ë#  @   a   FUELPROPERTY%CAPACITY *   $  @   a   FUELPROPERTY%CONDUCTIVITY '   K$  @   a   FUELPROPERTY%EXPANSION !   $  V   a   FUELPROPERTY%SET !   á$  |      SET_FUELPROPERTY &   ]%  N   a   SET_FUELPROPERTY%THIS &   Ŧ%  8   a   SET_FUELPROPERTY%TYPE +   ã%  8   a   SET_FUELPROPERTY%WEIGHT_ZR &   &  8   a   SET_FUELPROPERTY%X_PU '   S&  8   a   SET_FUELPROPERTY%X_O2M )   &  e   a   FUELPROPERTY%GET_DENSITY 0   ð&  \      GET_DENSITY_BY_TEMPERATURE_FUEL 5   L'  N   a   GET_DENSITY_BY_TEMPERATURE_FUEL%THIS 5   '  8   a   GET_DENSITY_BY_TEMPERATURE_FUEL%T_IN *   Ō'  f   a   FUELPROPERTY%GET_CAPACITY 1   8(  \      GET_CAPACITY_BY_TEMPERATURE_FUEL 6   (  N   a   GET_CAPACITY_BY_TEMPERATURE_FUEL%THIS 6   â(  8   a   GET_CAPACITY_BY_TEMPERATURE_FUEL%T_IN .   )  i   a   FUELPROPERTY%GET_CONDUCTIVITY 4   )  \      GET_CONDUCTIVTY_BY_TEMPERATURE_FUEL 9   ß)  N   a   GET_CONDUCTIVTY_BY_TEMPERATURE_FUEL%THIS 9   -*  8   a   GET_CONDUCTIVTY_BY_TEMPERATURE_FUEL%T_IN +   e*  g   a   FUELPROPERTY%GET_EXPANSION 2   Ė*  \      GET_EXPANSION_BY_TEMPERATURE_FUEL 7   (+  N   a   GET_EXPANSION_BY_TEMPERATURE_FUEL%THIS 7   v+  8   a   GET_EXPANSION_BY_TEMPERATURE_FUEL%T_IN    Ū+  Z      MsObjComment 