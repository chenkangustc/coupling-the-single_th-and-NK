	  “/     k820309    ?          14.0        (Z                                                                                                           
       E:\documents\doctors degree\software\tansistant\NT\DAISY_verson_20170328\DAISY-git\git\src\thermal\coolant_HLM_header.f90 COOLANT_HLM_HEADER          COOLANTPROPERTY_HLM                  @                              
                                                      
                        @                              
       ERRORCOLLECTOR WARNINGCOLLECTOR                  @                              
       COOLANTPROPERTY               @                               'H           #INFO_LIST    #MESSAGE    #COUNTING    #CNT_LIMIT 	   #SET 
   #PRINT              $                                                      $                                                    $                                   @                                                         0         $                              	     D                                                        11     Ą    $                           
              #SET_ERRORCOLLECTOR    #     @     @                                              #SET_ERRORCOLLECTOR%TRIM    #SET_ERRORCOLLECTOR%ADJUSTL    #THIS    #INFO_LIST    #MESSAGE              @                                 TRIM           @                                 ADJUSTL       
                                     H      #ERRORCOLLECTOR          
                                             1       
                                             1 1     Ą    $                                         #PRINT_ERRORCOLLECTOR    #     @     @                                              #PRINT_ERRORCOLLECTOR%TRIM    #THIS    #UNIT_              @                                 TRIM       
                                      H     #ERRORCOLLECTOR          
                                                       @                               'H           #INFO_LIST    #MESSAGE    #COUNTING    #CNT_LIMIT    #SET    #PRINT "             $                                                      $                                                    $                                   @                                                         0         $                                   D                                         PĆ              500001     Ą    $                                          #SET_WARNINGCOLLECTOR    #     @     @                                               #SET_WARNINGCOLLECTOR%TRIM    #SET_WARNINGCOLLECTOR%ADJUSTL    #THIS    #INFO_LIST     #MESSAGE !             @                                 TRIM           @                                 ADJUSTL       
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
                $                              _     (                   $                              `     ,                       @                          a     'X            #COOLANTPROPERTY b   #T_MELTING c   #T_BOILING d   #MOL_MASS e   #SET f   #GET_DENSITY k   #GET_ENTHALPY o   #GET_TEMPERATURE s   #GET_CAPACITY x   #GET_CONDUCTIVITY |   #GET_VISCOSITY    #GET_NUSSELT              $                              b     @          #COOLANTPROPERTY '             D                             c     @      
             D                             d     H      
             D                             e     P      
   1     Ą    $                           f              #SET_COOLANTPROPERTY g   #     @     @                            g                    #THIS h   #TYPE i   #OPTION j         
D                                h     X       #COOLANTPROPERTY_HLM a         
                                  i             
                                  j       1     Ą    $                         k              #GET_DENSITY_BY_TEMPERATURE l   %     @    @                           l                    
   #THIS m   #T_IN n         
D                                m     X       #COOLANTPROPERTY_HLM a         
                                 n     
  1     Ą    $                         o              #GET_ENTHALPY_BY_TEMPERATURE p   %     @    @                           p                    
   #THIS q   #T_IN r         
D                                q     X       #COOLANTPROPERTY_HLM a         
                                 r     
  1     Ą    $                          s              #GET_TEMPERATURE_BY_ENTHALPY t   %     @    @                            t                   
   #GET_TEMPERATURE_BY_ENTHALPY%ABS u   #THIS v   #H_IN w             @                            u     ABS       
D @                              v     X       #COOLANTPROPERTY_HLM a         
                                 w     
  1     Ą    $                         x         	     #GET_CAPACITY_BY_TEMPERATURE y   %     @    @                           y                    
   #THIS z   #T_IN {         
D                                z     X       #COOLANTPROPERTY_HLM a         
                                 {     
  1     Ą    $                         |         
     #GET_CONDUCTIVITY_BY_TEMPERATURE }   %     @    @                           }                    
   #THIS ~   #T_IN          
D                                ~     X       #COOLANTPROPERTY_HLM a         
                                      
  1     Ą    $                                       #GET_VISCOSITY_BY_TEMPERATURE    %     @    @                                              
   #GET_VISCOSITY_BY_TEMPERATURE%EXP    #THIS    #T_IN              @                                 EXP       
D                                     X       #COOLANTPROPERTY_HLM a         
                                      
  1     Ą    $                                        #GET_NUSSELT_NUMBER    %     @    @                                               
   #GET_NUSSELT_NUMBER%EXP    #THIS    #P2D    #VELOCITY    #DH    #T_IN              @                                 EXP       
D @                                   X       #COOLANTPROPERTY_HLM a         
                                      
        
                                      
        
                                      
        
  @                                   
  *          n                               Cifmodintr.lib                               fn#fn (   5      b   uapp(COOLANT_HLM_HEADER    U  <   J  CONSTANTS       <   J  ISO_FORTRAN_ENV !   Ķ  \   J  EXCEPTION_HEADER )   )  L   J  ABSTRACT_PROPERTY_HEADER 0   u         ERRORCOLLECTOR+EXCEPTION_HEADER :     @   a   ERRORCOLLECTOR%INFO_LIST+EXCEPTION_HEADER 8   F  @   a   ERRORCOLLECTOR%MESSAGE+EXCEPTION_HEADER 9     }   a   ERRORCOLLECTOR%COUNTING+EXCEPTION_HEADER :     }   a   ERRORCOLLECTOR%CNT_LIMIT+EXCEPTION_HEADER 4     X   a   ERRORCOLLECTOR%SET+EXCEPTION_HEADER 4   Ų  §      SET_ERRORCOLLECTOR+EXCEPTION_HEADER >     9      SET_ERRORCOLLECTOR%TRIM+EXCEPTION_HEADER=TRIM D   ø  <      SET_ERRORCOLLECTOR%ADJUSTL+EXCEPTION_HEADER=ADJUSTL 9   ō  P   a   SET_ERRORCOLLECTOR%THIS+EXCEPTION_HEADER >   D  @   a   SET_ERRORCOLLECTOR%INFO_LIST+EXCEPTION_HEADER <     @   a   SET_ERRORCOLLECTOR%MESSAGE+EXCEPTION_HEADER 6   Ä  Z   a   ERRORCOLLECTOR%PRINT+EXCEPTION_HEADER 6     x      PRINT_ERRORCOLLECTOR+EXCEPTION_HEADER @     9      PRINT_ERRORCOLLECTOR%TRIM+EXCEPTION_HEADER=TRIM ;   Ļ  P   a   PRINT_ERRORCOLLECTOR%THIS+EXCEPTION_HEADER <     8   a   PRINT_ERRORCOLLECTOR%UNIT_+EXCEPTION_HEADER 2   W         WARNINGCOLLECTOR+EXCEPTION_HEADER <   č  @   a   WARNINGCOLLECTOR%INFO_LIST+EXCEPTION_HEADER :   (	  @   a   WARNINGCOLLECTOR%MESSAGE+EXCEPTION_HEADER ;   h	  }   a   WARNINGCOLLECTOR%COUNTING+EXCEPTION_HEADER <   å	     a   WARNINGCOLLECTOR%CNT_LIMIT+EXCEPTION_HEADER 6   f
  Z   a   WARNINGCOLLECTOR%SET+EXCEPTION_HEADER 6   Ą
  «      SET_WARNINGCOLLECTOR+EXCEPTION_HEADER @   k  9      SET_WARNINGCOLLECTOR%TRIM+EXCEPTION_HEADER=TRIM F   ¤  <      SET_WARNINGCOLLECTOR%ADJUSTL+EXCEPTION_HEADER=ADJUSTL ;   ą  R   a   SET_WARNINGCOLLECTOR%THIS+EXCEPTION_HEADER @   2  @   a   SET_WARNINGCOLLECTOR%INFO_LIST+EXCEPTION_HEADER >   r  @   a   SET_WARNINGCOLLECTOR%MESSAGE+EXCEPTION_HEADER 8   ²  \   a   WARNINGCOLLECTOR%PRINT+EXCEPTION_HEADER 8     z      PRINT_WARNINGCOLLECTOR+EXCEPTION_HEADER B     9      PRINT_WARNINGCOLLECTOR%TRIM+EXCEPTION_HEADER=TRIM =   Į  R   a   PRINT_WARNINGCOLLECTOR%THIS+EXCEPTION_HEADER >     8   a   PRINT_WARNINGCOLLECTOR%UNIT_+EXCEPTION_HEADER 9   K  R      COOLANTPROPERTY+ABSTRACT_PROPERTY_HEADER F     @   a   COOLANTPROPERTY%COOLANT_TYPE+ABSTRACT_PROPERTY_HEADER A   Ż  @   a   COOLANTPROPERTY%DENSITY+ABSTRACT_PROPERTY_HEADER B     @   a   COOLANTPROPERTY%ENTHALPY+ABSTRACT_PROPERTY_HEADER E   ]  @   a   COOLANTPROPERTY%TEMPERATURE+ABSTRACT_PROPERTY_HEADER B     @   a   COOLANTPROPERTY%CAPACITY+ABSTRACT_PROPERTY_HEADER F   Ż  @   a   COOLANTPROPERTY%CONDUCTIVITY+ABSTRACT_PROPERTY_HEADER C     @   a   COOLANTPROPERTY%VISCOSITY+ABSTRACT_PROPERTY_HEADER H   ]  @   a   COOLANTPROPERTY%NUSSELT_NUMBER+ABSTRACT_PROPERTY_HEADER =     Y   a   COOLANTPROPERTY%SET+ABSTRACT_PROPERTY_HEADER =   ö  d      SET_COOLANTPROPERTY+ABSTRACT_PROPERTY_HEADER B   Z  Q   a   SET_COOLANTPROPERTY%THIS+ABSTRACT_PROPERTY_HEADER B   «  8   a   SET_COOLANTPROPERTY%TYPE+ABSTRACT_PROPERTY_HEADER D   ć  8   a   SET_COOLANTPROPERTY%OPTION+ABSTRACT_PROPERTY_HEADER E     h   a   COOLANTPROPERTY%GET_DENSITY+ABSTRACT_PROPERTY_HEADER L     \      GET_DENSITY_BY_TEMPERATURE_COOLANT+ABSTRACT_PROPERTY_HEADER Q   ß  Q   a   GET_DENSITY_BY_TEMPERATURE_COOLANT%THIS+ABSTRACT_PROPERTY_HEADER Q   0  8   a   GET_DENSITY_BY_TEMPERATURE_COOLANT%T_IN+ABSTRACT_PROPERTY_HEADER F   h  i   a   COOLANTPROPERTY%GET_ENTHALPY+ABSTRACT_PROPERTY_HEADER M   Ń  \      GET_ENTHALPY_BY_TEMPERATURE_COOLANT+ABSTRACT_PROPERTY_HEADER R   -  Q   a   GET_ENTHALPY_BY_TEMPERATURE_COOLANT%THIS+ABSTRACT_PROPERTY_HEADER R   ~  8   a   GET_ENTHALPY_BY_TEMPERATURE_COOLANT%T_IN+ABSTRACT_PROPERTY_HEADER I   ¶  i   a   COOLANTPROPERTY%GET_TEMPERATURE+ABSTRACT_PROPERTY_HEADER M     \      GET_TEMPERATURE_BY_ENTHALPY_COOLANT+ABSTRACT_PROPERTY_HEADER R   {  Q   a   GET_TEMPERATURE_BY_ENTHALPY_COOLANT%THIS+ABSTRACT_PROPERTY_HEADER R   Ģ  8   a   GET_TEMPERATURE_BY_ENTHALPY_COOLANT%H_IN+ABSTRACT_PROPERTY_HEADER F     i   a   COOLANTPROPERTY%GET_CAPACITY+ABSTRACT_PROPERTY_HEADER M   m  \      GET_CAPACITY_BY_TEMPERATURE_COOLANT+ABSTRACT_PROPERTY_HEADER R   É  Q   a   GET_CAPACITY_BY_TEMPERATURE_COOLANT%THIS+ABSTRACT_PROPERTY_HEADER R     8   a   GET_CAPACITY_BY_TEMPERATURE_COOLANT%T_IN+ABSTRACT_PROPERTY_HEADER J   R  m   a   COOLANTPROPERTY%GET_CONDUCTIVITY+ABSTRACT_PROPERTY_HEADER Q   æ  \      GET_CONDUCTIVITY_BY_TEMPERATURE_COOLANT+ABSTRACT_PROPERTY_HEADER V     Q   a   GET_CONDUCTIVITY_BY_TEMPERATURE_COOLANT%THIS+ABSTRACT_PROPERTY_HEADER V   l  8   a   GET_CONDUCTIVITY_BY_TEMPERATURE_COOLANT%T_IN+ABSTRACT_PROPERTY_HEADER G   ¤  j   a   COOLANTPROPERTY%GET_VISCOSITY+ABSTRACT_PROPERTY_HEADER N     \      GET_VISCOSITY_BY_TEMPERATURE_COOLANT+ABSTRACT_PROPERTY_HEADER S   j  Q   a   GET_VISCOSITY_BY_TEMPERATURE_COOLANT%THIS+ABSTRACT_PROPERTY_HEADER S   »  8   a   GET_VISCOSITY_BY_TEMPERATURE_COOLANT%T_IN+ABSTRACT_PROPERTY_HEADER E   ó  X   a   COOLANTPROPERTY%GET_NUSSELT+ABSTRACT_PROPERTY_HEADER <   K  {      GET_NUSSELT_NUMBER+ABSTRACT_PROPERTY_HEADER A   Ę  Q   a   GET_NUSSELT_NUMBER%THIS+ABSTRACT_PROPERTY_HEADER @     8   a   GET_NUSSELT_NUMBER%P2D+ABSTRACT_PROPERTY_HEADER E   O  8   a   GET_NUSSELT_NUMBER%VELOCITY+ABSTRACT_PROPERTY_HEADER ?     8   a   GET_NUSSELT_NUMBER%DH+ABSTRACT_PROPERTY_HEADER A   æ  8   a   GET_NUSSELT_NUMBER%T_IN+ABSTRACT_PROPERTY_HEADER "   ÷  ć      FILE_TP+CONSTANTS +   Ś  @   a   FILE_TP%CASENAME+CONSTANTS '     @   a   FILE_TP%MAIN+CONSTANTS )   Z  @   a   FILE_TP%MEMORY+CONSTANTS +     @   a   FILE_TP%TIMELIST+CONSTANTS &   Ś  @   a   FILE_TP%DET+CONSTANTS -     @   a   FILE_TP%REACTIVITY+CONSTANTS 0   Z  @   a   FILE_TP%POINTKINETICS+CONSTANTS %     @   a   FILE_TP%PT+CONSTANTS -   Ś  @   a   FILE_TP%TH_WARNING+CONSTANTS *      @   a   FILE_TP%TH_RBFD+CONSTANTS )   Z   @   a   FILE_TP%TH_HOT+CONSTANTS -      @   a   FILE_TP%TH_AVERAGE+CONSTANTS $   Ś         COOLANTPROPERTY_HLM 4   ģ!  U   a   COOLANTPROPERTY_HLM%COOLANTPROPERTY .   A"  @   !   COOLANTPROPERTY_HLM%T_MELTING .   "  @   !   COOLANTPROPERTY_HLM%T_BOILING -   Į"  @   !   COOLANTPROPERTY_HLM%MOL_MASS (   #  Y   a   COOLANTPROPERTY_HLM%SET $   Z#  d      SET_COOLANTPROPERTY )   ¾#  U   a   SET_COOLANTPROPERTY%THIS )   $  8   a   SET_COOLANTPROPERTY%TYPE +   K$  8   a   SET_COOLANTPROPERTY%OPTION 0   $  `   a   COOLANTPROPERTY_HLM%GET_DENSITY +   ć$  \      GET_DENSITY_BY_TEMPERATURE 0   ?%  U   a   GET_DENSITY_BY_TEMPERATURE%THIS 0   %  8   a   GET_DENSITY_BY_TEMPERATURE%T_IN 1   Ģ%  a   a   COOLANTPROPERTY_HLM%GET_ENTHALPY ,   -&  \      GET_ENTHALPY_BY_TEMPERATURE 1   &  U   a   GET_ENTHALPY_BY_TEMPERATURE%THIS 1   Ž&  8   a   GET_ENTHALPY_BY_TEMPERATURE%T_IN 4   '  a   a   COOLANTPROPERTY_HLM%GET_TEMPERATURE ,   w'        GET_TEMPERATURE_BY_ENTHALPY 0   ų'  8      GET_TEMPERATURE_BY_ENTHALPY%ABS 1   0(  U   a   GET_TEMPERATURE_BY_ENTHALPY%THIS 1   (  8   a   GET_TEMPERATURE_BY_ENTHALPY%H_IN 1   ½(  a   a   COOLANTPROPERTY_HLM%GET_CAPACITY ,   )  \      GET_CAPACITY_BY_TEMPERATURE 1   z)  U   a   GET_CAPACITY_BY_TEMPERATURE%THIS 1   Ļ)  8   a   GET_CAPACITY_BY_TEMPERATURE%T_IN 5   *  e   a   COOLANTPROPERTY_HLM%GET_CONDUCTIVITY 0   l*  \      GET_CONDUCTIVITY_BY_TEMPERATURE 5   Č*  U   a   GET_CONDUCTIVITY_BY_TEMPERATURE%THIS 5   +  8   a   GET_CONDUCTIVITY_BY_TEMPERATURE%T_IN 2   U+  b   a   COOLANTPROPERTY_HLM%GET_VISCOSITY -   ·+        GET_VISCOSITY_BY_TEMPERATURE 1   9,  8      GET_VISCOSITY_BY_TEMPERATURE%EXP 2   q,  U   a   GET_VISCOSITY_BY_TEMPERATURE%THIS 2   Ę,  8   a   GET_VISCOSITY_BY_TEMPERATURE%T_IN 0   ž,  X   a   COOLANTPROPERTY_HLM%GET_NUSSELT #   V-        GET_NUSSELT_NUMBER '   ķ-  8      GET_NUSSELT_NUMBER%EXP (   %.  U   a   GET_NUSSELT_NUMBER%THIS '   z.  8   a   GET_NUSSELT_NUMBER%P2D ,   ².  8   a   GET_NUSSELT_NUMBER%VELOCITY &   ź.  8   a   GET_NUSSELT_NUMBER%DH (   "/  8   a   GET_NUSSELT_NUMBER%T_IN    Z/  Z      MsObjComment 