	  $.     k820309    ?          14.0        ©Ø*Z                                                                                                           
       E:\documents\doctors degree\software\tansistant\NT\DAISY_verson_20170328\DAISY-git\git\src\thermal\coolant_water_parcs_header.f90 COOLANT_WATER_PARCS_HEADER          WATERPROPERTY_PARCS                  @                              
                                                      
                        @                              
       ERRORCOLLECTOR WARNINGCOLLECTOR                  @                              
       COOLANTPROPERTY               @                               'H           #INFO_LIST    #MESSAGE    #COUNTING    #CNT_LIMIT 	   #SET 
   #PRINT              $                                                      $                                                    $                                   @                                                         0         $                              	     D                                                        11     Ą    $                            
              #SET_ERRORCOLLECTOR    #     @     @                                               #SET_ERRORCOLLECTOR%TRIM    #SET_ERRORCOLLECTOR%ADJUSTL    #THIS    #INFO_LIST    #MESSAGE              @                                 TRIM           @                                 ADJUSTL       
                                     H      #ERRORCOLLECTOR          
                                             1       
                                             1 1     Ą    $                                          #PRINT_ERRORCOLLECTOR    #     @     @                                               #PRINT_ERRORCOLLECTOR%TRIM    #THIS    #UNIT_              @                                 TRIM       
                                      H     #ERRORCOLLECTOR          
                                                       @                               'H           #INFO_LIST    #MESSAGE    #COUNTING    #CNT_LIMIT    #SET    #PRINT "             $                                                      $                                                    $                                   @                                                         0         $                                   D                                         PĆ              500001     Ą    $                                          #SET_WARNINGCOLLECTOR    #     @     @                                               #SET_WARNINGCOLLECTOR%TRIM    #SET_WARNINGCOLLECTOR%ADJUSTL    #THIS    #INFO_LIST     #MESSAGE !             @                                 TRIM           @                                 ADJUSTL       
                                     H      #WARNINGCOLLECTOR          
                                              1       
                                 !            1 1     Ą    $                            "              #PRINT_WARNINGCOLLECTOR #   #     @     @                            #                   #PRINT_WARNINGCOLLECTOR%TRIM $   #THIS %   #UNIT_ &             @                            $     TRIM       
                                 %     H     #WARNINGCOLLECTOR          
                                  &                     @                          '     '@            #COOLANT_TYPE (   #DENSITY )   #ENTHALPY *   #TEMPERATURE +   #CAPACITY ,   #CONDUCTIVITY -   #VISCOSITY .   #NUSSELT_NUMBER /   #SET 0   #GET_DENSITY 5   #GET_ENTHALPY 9   #GET_TEMPERATURE =   #GET_CAPACITY A   #GET_CONDUCTIVITY E   #GET_VISCOSITY I   #GET_NUSSELT M             $                              (                         $                             )           
             $                             *           
             $                             +           
             $                             ,            
             $                             -     (      
             $                             .     0      
             $                             /     8      
   1     Ą    $                           0         	     #SET_COOLANTPROPERTY 1   #     @     @                           1     	               #THIS 2   #TYPE 3   #OPTION 4         
                               2     @       #COOLANTPROPERTY '         
                                 3             
                                 4       1     Ą    $                          5         
     #GET_DENSITY_BY_TEMPERATURE_COOLANT 6   %     @    @                          6                    
   #THIS 7   #T_IN 8         
                               7     @       #COOLANTPROPERTY '         
                                8     
  1     Ą    $                          9              #GET_ENTHALPY_BY_TEMPERATURE_COOLANT :   %     @    @                          :                    
   #THIS ;   #T_IN <         
                               ;     @       #COOLANTPROPERTY '         
                                <     
  1     Ą    $                          =              #GET_TEMPERATURE_BY_ENTHALPY_COOLANT >   %     @    @                          >                    
   #THIS ?   #H_IN @         
                               ?     @       #COOLANTPROPERTY '         
                                @     
  1     Ą    $                          A              #GET_CAPACITY_BY_TEMPERATURE_COOLANT B   %     @    @                          B                    
   #THIS C   #T_IN D         
                               C     @       #COOLANTPROPERTY '         
                                D     
  1     Ą    $                          E              #GET_CONDUCTIVITY_BY_TEMPERATURE_COOLANT F   %     @    @                          F                    
   #THIS G   #T_IN H         
                               G     @       #COOLANTPROPERTY '         
                                H     
  1     Ą    $                          I              #GET_VISCOSITY_BY_TEMPERATURE_COOLANT J   %     @    @                          J                    
   #THIS K   #T_IN L         
                               K     @       #COOLANTPROPERTY '         
                                L     
  1     Ą    $                          M              #GET_NUSSELT_NUMBER N   %     @    @                          N                    
   #THIS O   #P2D P   #VELOCITY Q   #DH R   #T_IN S         
                               O     @       #COOLANTPROPERTY '         
                                P     
        
                                Q     
        
                                R     
        
                                S     
            @  @                          T     '0            #CASENAME U   #MAIN V   #MEMORY W   #TIMELIST X   #DET Y   #REACTIVITY Z   #POINTKINETICS [   #PT \   #TH_WARNING ]   #TH_RBFD ^   #TH_HOT _   #TH_AVERAGE `             $                              U                         $                              V                        $                              W                        $                              X                        $                              Y                        $                              Z                        $                              [                        $                              \                        $                              ]         	                $                              ^     $   
                $                              _     (                   $                              `     ,                       @                          a     '@      	      #COOLANTPROPERTY b   #SET c   #GET_DENSITY h   #GET_ENTHALPY l   #GET_TEMPERATURE p   #GET_CAPACITY u   #GET_CONDUCTIVITY y   #GET_VISCOSITY }   #GET_NUSSELT              $                              b     @          #COOLANTPROPERTY '   1     Ą    $                           c              #SET_WATERPROPERTY d   #     @     @                             d                    #THIS e   #TYPE f   #OPTION g         
                                e     @       #WATERPROPERTY_PARCS a         
                                  f             
                                  g       1     Ą    $                         h              #GET_DENSITY_BY_TEMPERATURE i   %     @    @                           i                    
   #THIS j   #T_IN k         
D                                j     @       #WATERPROPERTY_PARCS a         
                                 k     
  1     Ą    $                         l              #GET_ENTHALPY_BY_TEMPERATURE m   %     @    @                           m                    
   #THIS n   #T_IN o         
D                                n     @       #WATERPROPERTY_PARCS a         
                                 o     
  1     Ą    $                          p              #GET_TEMPERATURE_BY_ENTHALPY q   %     @    @                            q                   
   #GET_TEMPERATURE_BY_ENTHALPY%ABS r   #THIS s   #H_IN t             @                            r     ABS       
D @                              s     @       #WATERPROPERTY_PARCS a         
                                 t     
  1     Ą    $                         u              #GET_CAPACITY_BY_TEMPERATURE v   %     @    @                           v                    
   #THIS w   #T_IN x         
D                                w     @       #WATERPROPERTY_PARCS a         
                                 x     
  1     Ą    $                         y              #GET_CONDUCTIVITY_BY_TEMPERATURE z   %     @    @                           z                    
   #THIS {   #T_IN |         
D                                {     @       #WATERPROPERTY_PARCS a         
                                 |     
  1     Ą    $                         }              #GET_VISCOSITY_BY_TEMPERATURE ~   %     @    @                           ~                    
   #THIS    #T_IN          
D                                     @       #WATERPROPERTY_PARCS a         
                                      
  1     Ą    $                                   	     #GET_NUSSELT_NUMBER    %     @    @                                                
   #THIS    #P2D    #VELOCITY    #DH    #T_IN          
D @                                   @       #WATERPROPERTY_PARCS a         
                                      
        
                                      
        
                                      
        
  @                                   
  *          n                 O              Cifmodintr.lib                         „      fn#fn 0   E      b   uapp(COOLANT_WATER_PARCS_HEADER    e  <   J  CONSTANTS     ”  <   J  ISO_FORTRAN_ENV !   Ż  \   J  EXCEPTION_HEADER )   9  L   J  ABSTRACT_PROPERTY_HEADER 0            ERRORCOLLECTOR+EXCEPTION_HEADER :     @   a   ERRORCOLLECTOR%INFO_LIST+EXCEPTION_HEADER 8   V  @   a   ERRORCOLLECTOR%MESSAGE+EXCEPTION_HEADER 9     }   a   ERRORCOLLECTOR%COUNTING+EXCEPTION_HEADER :     }   a   ERRORCOLLECTOR%CNT_LIMIT+EXCEPTION_HEADER 4     X   a   ERRORCOLLECTOR%SET+EXCEPTION_HEADER 4   č  §      SET_ERRORCOLLECTOR+EXCEPTION_HEADER >     9      SET_ERRORCOLLECTOR%TRIM+EXCEPTION_HEADER=TRIM D   Č  <      SET_ERRORCOLLECTOR%ADJUSTL+EXCEPTION_HEADER=ADJUSTL 9     P   a   SET_ERRORCOLLECTOR%THIS+EXCEPTION_HEADER >   T  @   a   SET_ERRORCOLLECTOR%INFO_LIST+EXCEPTION_HEADER <     @   a   SET_ERRORCOLLECTOR%MESSAGE+EXCEPTION_HEADER 6   Ō  Z   a   ERRORCOLLECTOR%PRINT+EXCEPTION_HEADER 6   .  x      PRINT_ERRORCOLLECTOR+EXCEPTION_HEADER @   ¦  9      PRINT_ERRORCOLLECTOR%TRIM+EXCEPTION_HEADER=TRIM ;   ß  P   a   PRINT_ERRORCOLLECTOR%THIS+EXCEPTION_HEADER <   /  8   a   PRINT_ERRORCOLLECTOR%UNIT_+EXCEPTION_HEADER 2   g         WARNINGCOLLECTOR+EXCEPTION_HEADER <   ų  @   a   WARNINGCOLLECTOR%INFO_LIST+EXCEPTION_HEADER :   8	  @   a   WARNINGCOLLECTOR%MESSAGE+EXCEPTION_HEADER ;   x	  }   a   WARNINGCOLLECTOR%COUNTING+EXCEPTION_HEADER <   õ	     a   WARNINGCOLLECTOR%CNT_LIMIT+EXCEPTION_HEADER 6   v
  Z   a   WARNINGCOLLECTOR%SET+EXCEPTION_HEADER 6   Š
  «      SET_WARNINGCOLLECTOR+EXCEPTION_HEADER @   {  9      SET_WARNINGCOLLECTOR%TRIM+EXCEPTION_HEADER=TRIM F   “  <      SET_WARNINGCOLLECTOR%ADJUSTL+EXCEPTION_HEADER=ADJUSTL ;   š  R   a   SET_WARNINGCOLLECTOR%THIS+EXCEPTION_HEADER @   B  @   a   SET_WARNINGCOLLECTOR%INFO_LIST+EXCEPTION_HEADER >     @   a   SET_WARNINGCOLLECTOR%MESSAGE+EXCEPTION_HEADER 8   Ā  \   a   WARNINGCOLLECTOR%PRINT+EXCEPTION_HEADER 8     z      PRINT_WARNINGCOLLECTOR+EXCEPTION_HEADER B     9      PRINT_WARNINGCOLLECTOR%TRIM+EXCEPTION_HEADER=TRIM =   Ń  R   a   PRINT_WARNINGCOLLECTOR%THIS+EXCEPTION_HEADER >   #  8   a   PRINT_WARNINGCOLLECTOR%UNIT_+EXCEPTION_HEADER 9   [  R      COOLANTPROPERTY+ABSTRACT_PROPERTY_HEADER F   ­  @   a   COOLANTPROPERTY%COOLANT_TYPE+ABSTRACT_PROPERTY_HEADER A   ķ  @   a   COOLANTPROPERTY%DENSITY+ABSTRACT_PROPERTY_HEADER B   -  @   a   COOLANTPROPERTY%ENTHALPY+ABSTRACT_PROPERTY_HEADER E   m  @   a   COOLANTPROPERTY%TEMPERATURE+ABSTRACT_PROPERTY_HEADER B   ­  @   a   COOLANTPROPERTY%CAPACITY+ABSTRACT_PROPERTY_HEADER F   ķ  @   a   COOLANTPROPERTY%CONDUCTIVITY+ABSTRACT_PROPERTY_HEADER C   -  @   a   COOLANTPROPERTY%VISCOSITY+ABSTRACT_PROPERTY_HEADER H   m  @   a   COOLANTPROPERTY%NUSSELT_NUMBER+ABSTRACT_PROPERTY_HEADER =   ­  Y   a   COOLANTPROPERTY%SET+ABSTRACT_PROPERTY_HEADER =     d      SET_COOLANTPROPERTY+ABSTRACT_PROPERTY_HEADER B   j  Q   a   SET_COOLANTPROPERTY%THIS+ABSTRACT_PROPERTY_HEADER B   »  8   a   SET_COOLANTPROPERTY%TYPE+ABSTRACT_PROPERTY_HEADER D   ó  8   a   SET_COOLANTPROPERTY%OPTION+ABSTRACT_PROPERTY_HEADER E   +  h   a   COOLANTPROPERTY%GET_DENSITY+ABSTRACT_PROPERTY_HEADER L     \      GET_DENSITY_BY_TEMPERATURE_COOLANT+ABSTRACT_PROPERTY_HEADER Q   ļ  Q   a   GET_DENSITY_BY_TEMPERATURE_COOLANT%THIS+ABSTRACT_PROPERTY_HEADER Q   @  8   a   GET_DENSITY_BY_TEMPERATURE_COOLANT%T_IN+ABSTRACT_PROPERTY_HEADER F   x  i   a   COOLANTPROPERTY%GET_ENTHALPY+ABSTRACT_PROPERTY_HEADER M   į  \      GET_ENTHALPY_BY_TEMPERATURE_COOLANT+ABSTRACT_PROPERTY_HEADER R   =  Q   a   GET_ENTHALPY_BY_TEMPERATURE_COOLANT%THIS+ABSTRACT_PROPERTY_HEADER R     8   a   GET_ENTHALPY_BY_TEMPERATURE_COOLANT%T_IN+ABSTRACT_PROPERTY_HEADER I   Ę  i   a   COOLANTPROPERTY%GET_TEMPERATURE+ABSTRACT_PROPERTY_HEADER M   /  \      GET_TEMPERATURE_BY_ENTHALPY_COOLANT+ABSTRACT_PROPERTY_HEADER R     Q   a   GET_TEMPERATURE_BY_ENTHALPY_COOLANT%THIS+ABSTRACT_PROPERTY_HEADER R   Ü  8   a   GET_TEMPERATURE_BY_ENTHALPY_COOLANT%H_IN+ABSTRACT_PROPERTY_HEADER F     i   a   COOLANTPROPERTY%GET_CAPACITY+ABSTRACT_PROPERTY_HEADER M   }  \      GET_CAPACITY_BY_TEMPERATURE_COOLANT+ABSTRACT_PROPERTY_HEADER R   Ł  Q   a   GET_CAPACITY_BY_TEMPERATURE_COOLANT%THIS+ABSTRACT_PROPERTY_HEADER R   *  8   a   GET_CAPACITY_BY_TEMPERATURE_COOLANT%T_IN+ABSTRACT_PROPERTY_HEADER J   b  m   a   COOLANTPROPERTY%GET_CONDUCTIVITY+ABSTRACT_PROPERTY_HEADER Q   Ļ  \      GET_CONDUCTIVITY_BY_TEMPERATURE_COOLANT+ABSTRACT_PROPERTY_HEADER V   +  Q   a   GET_CONDUCTIVITY_BY_TEMPERATURE_COOLANT%THIS+ABSTRACT_PROPERTY_HEADER V   |  8   a   GET_CONDUCTIVITY_BY_TEMPERATURE_COOLANT%T_IN+ABSTRACT_PROPERTY_HEADER G   “  j   a   COOLANTPROPERTY%GET_VISCOSITY+ABSTRACT_PROPERTY_HEADER N     \      GET_VISCOSITY_BY_TEMPERATURE_COOLANT+ABSTRACT_PROPERTY_HEADER S   z  Q   a   GET_VISCOSITY_BY_TEMPERATURE_COOLANT%THIS+ABSTRACT_PROPERTY_HEADER S   Ė  8   a   GET_VISCOSITY_BY_TEMPERATURE_COOLANT%T_IN+ABSTRACT_PROPERTY_HEADER E     X   a   COOLANTPROPERTY%GET_NUSSELT+ABSTRACT_PROPERTY_HEADER <   [  {      GET_NUSSELT_NUMBER+ABSTRACT_PROPERTY_HEADER A   Ö  Q   a   GET_NUSSELT_NUMBER%THIS+ABSTRACT_PROPERTY_HEADER @   '  8   a   GET_NUSSELT_NUMBER%P2D+ABSTRACT_PROPERTY_HEADER E   _  8   a   GET_NUSSELT_NUMBER%VELOCITY+ABSTRACT_PROPERTY_HEADER ?     8   a   GET_NUSSELT_NUMBER%DH+ABSTRACT_PROPERTY_HEADER A   Ļ  8   a   GET_NUSSELT_NUMBER%T_IN+ABSTRACT_PROPERTY_HEADER "     ć      FILE_TP+CONSTANTS +   ź  @   a   FILE_TP%CASENAME+CONSTANTS '   *  @   a   FILE_TP%MAIN+CONSTANTS )   j  @   a   FILE_TP%MEMORY+CONSTANTS +   Ŗ  @   a   FILE_TP%TIMELIST+CONSTANTS &   ź  @   a   FILE_TP%DET+CONSTANTS -   *  @   a   FILE_TP%REACTIVITY+CONSTANTS 0   j  @   a   FILE_TP%POINTKINETICS+CONSTANTS %   Ŗ  @   a   FILE_TP%PT+CONSTANTS -   ź  @   a   FILE_TP%TH_WARNING+CONSTANTS *   *   @   a   FILE_TP%TH_RBFD+CONSTANTS )   j   @   a   FILE_TP%TH_HOT+CONSTANTS -   Ŗ   @   a   FILE_TP%TH_AVERAGE+CONSTANTS $   ź   ę       WATERPROPERTY_PARCS 4   Š!  U   a   WATERPROPERTY_PARCS%COOLANTPROPERTY (   %"  W   a   WATERPROPERTY_PARCS%SET "   |"  d      SET_WATERPROPERTY '   ą"  U   a   SET_WATERPROPERTY%THIS '   5#  8   a   SET_WATERPROPERTY%TYPE )   m#  8   a   SET_WATERPROPERTY%OPTION 0   „#  `   a   WATERPROPERTY_PARCS%GET_DENSITY +   $  \      GET_DENSITY_BY_TEMPERATURE 0   a$  U   a   GET_DENSITY_BY_TEMPERATURE%THIS 0   ¶$  8   a   GET_DENSITY_BY_TEMPERATURE%T_IN 1   ī$  a   a   WATERPROPERTY_PARCS%GET_ENTHALPY ,   O%  \      GET_ENTHALPY_BY_TEMPERATURE 1   «%  U   a   GET_ENTHALPY_BY_TEMPERATURE%THIS 1    &  8   a   GET_ENTHALPY_BY_TEMPERATURE%T_IN 4   8&  a   a   WATERPROPERTY_PARCS%GET_TEMPERATURE ,   &        GET_TEMPERATURE_BY_ENTHALPY 0   '  8      GET_TEMPERATURE_BY_ENTHALPY%ABS 1   R'  U   a   GET_TEMPERATURE_BY_ENTHALPY%THIS 1   §'  8   a   GET_TEMPERATURE_BY_ENTHALPY%H_IN 1   ß'  a   a   WATERPROPERTY_PARCS%GET_CAPACITY ,   @(  \      GET_CAPACITY_BY_TEMPERATURE 1   (  U   a   GET_CAPACITY_BY_TEMPERATURE%THIS 1   ń(  8   a   GET_CAPACITY_BY_TEMPERATURE%T_IN 5   ))  e   a   WATERPROPERTY_PARCS%GET_CONDUCTIVITY 0   )  \      GET_CONDUCTIVITY_BY_TEMPERATURE 5   ź)  U   a   GET_CONDUCTIVITY_BY_TEMPERATURE%THIS 5   ?*  8   a   GET_CONDUCTIVITY_BY_TEMPERATURE%T_IN 2   w*  b   a   WATERPROPERTY_PARCS%GET_VISCOSITY -   Ł*  \      GET_VISCOSITY_BY_TEMPERATURE 2   5+  U   a   GET_VISCOSITY_BY_TEMPERATURE%THIS 2   +  8   a   GET_VISCOSITY_BY_TEMPERATURE%T_IN 0   Ā+  X   a   WATERPROPERTY_PARCS%GET_NUSSELT #   ,  {      GET_NUSSELT_NUMBER (   ,  U   a   GET_NUSSELT_NUMBER%THIS '   ź,  8   a   GET_NUSSELT_NUMBER%P2D ,   "-  8   a   GET_NUSSELT_NUMBER%VELOCITY &   Z-  8   a   GET_NUSSELT_NUMBER%DH (   -  8   a   GET_NUSSELT_NUMBER%T_IN    Ź-  Z      MsObjComment 