	  P'  v   k820309    ?          14.0        ©Ø*Z                                                                                                           
       E:\documents\doctors degree\software\tansistant\NT\DAISY_verson_20170328\DAISY-git\git\src\thermal\fuel_caramic_header.f90 FUEL_CARAMIC_HEADER          FUELPROPERTY_CERAMIC                  @                              
                                                      
                        @                              
       ERRORCOLLECTOR WARNINGCOLLECTOR                  @                              
       FUELPROPERTY               @                               'H           #INFO_LIST    #MESSAGE    #COUNTING    #CNT_LIMIT 	   #SET 
   #PRINT              $                                                      $                                                    $                                   @                                                         0         $                              	     D                                                        11     Ą    $                           
              #SET_ERRORCOLLECTOR    #     @     @                                              #SET_ERRORCOLLECTOR%TRIM    #SET_ERRORCOLLECTOR%ADJUSTL    #THIS    #INFO_LIST    #MESSAGE              @                                 TRIM           @                                 ADJUSTL       
                                     H      #ERRORCOLLECTOR          
                                             1       
                                             1 1     Ą    $                                         #PRINT_ERRORCOLLECTOR    #     @     @                                              #PRINT_ERRORCOLLECTOR%TRIM    #THIS    #UNIT_              @                                 TRIM       
                                      H     #ERRORCOLLECTOR          
                                                       @                               'H           #INFO_LIST    #MESSAGE    #COUNTING    #CNT_LIMIT    #SET    #PRINT "             $                                                      $                                                    $                                   @                                                         0         $                                   D                                         PĆ              500001     Ą    $                                          #SET_WARNINGCOLLECTOR    #     @     @                                               #SET_WARNINGCOLLECTOR%TRIM    #SET_WARNINGCOLLECTOR%ADJUSTL    #THIS    #INFO_LIST     #MESSAGE !             @                                 TRIM           @                                 ADJUSTL       
                                     H      #WARNINGCOLLECTOR          
                                              1       
                                 !            1 1     Ą    $                            "              #PRINT_WARNINGCOLLECTOR #   #     @     @                            #                   #PRINT_WARNINGCOLLECTOR%TRIM $   #THIS %   #UNIT_ &             @                            $     TRIM       
                                 %     H     #WARNINGCOLLECTOR          
                                  &                     @                          '     '8            #T_MELTING (   #MOL_MASS )   #FUEL_TYPE *   #DENSITY +   #CAPACITY ,   #CONDUCTIVITY -   #EXPANSION .   #SET /   #GET_DENSITY 6   #GET_CAPACITY :   #GET_CONDUCTIVITY >   #GET_EXPANSION B             $                             (            
             $                             )           
             $                              *                        $                             +           
             $                             ,            
             $                             -     (      
             $                             .     0      
   1     Ą    $                           /              #SET_FUELPROPERTY 0   #     @     @                           0     	               #THIS 1   #TYPE 2   #WEIGHT_ZR 3   #X_PU 4   #X_O2M 5         
                               1     8       #FUELPROPERTY '         
                                 2             
                               3     
        
                               4     
        
                               5     
  1     Ą    $                          6         	     #GET_DENSITY_BY_TEMPERATURE_FUEL 7   %     @    @                          7                    
   #THIS 8   #T_IN 9         
                               8     8       #FUELPROPERTY '         
                                9     
  1     Ą    $                          :         
     #GET_CAPACITY_BY_TEMPERATURE_FUEL ;   %     @    @                          ;                    
   #THIS <   #T_IN =         
                               <     8       #FUELPROPERTY '         
                                =     
  1     Ą    $                          >              #GET_CONDUCTIVTY_BY_TEMPERATURE_FUEL ?   %     @    @                          ?                    
   #THIS @   #T_IN A         
                               @     8       #FUELPROPERTY '         
                                A     
  1     Ą    $                          B              #GET_EXPANSION_BY_TEMPERATURE_FUEL C   %     @    @                          C                    
   #THIS D   #T_IN E         
                               D     8       #FUELPROPERTY '         
                                E     
            @  @                          F     '0            #CASENAME G   #MAIN H   #MEMORY I   #TIMELIST J   #DET K   #REACTIVITY L   #POINTKINETICS M   #PT N   #TH_WARNING O   #TH_RBFD P   #TH_HOT Q   #TH_AVERAGE R             $                              G                         $                              H                        $                              I                        $                              J                        $                              K                        $                              L                        $                              M                        $                              N                        $                              O         	                $                              P     $   
                $                              Q     (                   $                              R     ,                       @                          S     'P      	      #FUELPROPERTY T   #TD U   #X_PU V   #X_O2M W   #SET X   #GET_DENSITY `   #GET_CAPACITY e   #GET_CONDUCTIVITY j   #GET_EXPANSION p             $                              T     8          #FUELPROPERTY '             D                             U     8      
             D                             V     @      
             D                             W     H      
   1     Ą    $                           X              #SET_FUELPROPERTY Y   #     @     @                            Y                   #SET_FUELPROPERTY%PRESENT Z   #THIS [   #TYPE \   #WEIGHT_ZR ]   #X_PU ^   #X_O2M _             @                            Z     PRESENT       
D                                [     P       #FUELPROPERTY_CERAMIC S         
                                  \             
                                ]     
        
 @                              ^     
        
 @                              _     
  1     Ą    $                         `              #GET_DENSITY_BY_TEMPERATURE a   %     @    @                           a                   
   #GET_DENSITY_BY_TEMPERATURE%EXP b   #THIS c   #T_IN d             @                            b     EXP       
D                                c     P       #FUELPROPERTY_CERAMIC S         
                                 d     
  1     Ą    $                          e              #GET_CAPACITY_BY_TEMPERATURE f   %     @    @                            f                   
   #GET_CAPACITY_BY_TEMPERATURE%EXP g   #THIS h   #T_IN i             @                            g     EXP       
D @                              h     P       #FUELPROPERTY_CERAMIC S         
                                 i     
  1     Ą    $                          j              #GET_CONDUCTIVTY_BY_TEMPERATURE k   %     @    @                            k                   
   #GET_CONDUCTIVTY_BY_TEMPERATURE%ABS l   #GET_CONDUCTIVTY_BY_TEMPERATURE%EXP m   #THIS n   #T_IN o             @                            l     ABS           @                            m     EXP       
D                                n     P       #FUELPROPERTY_CERAMIC S         
                                 o     
  1     Ą    $                          p         	     #GET_EXPANSION_BY_TEMPERATURE q   %     @    @                            q                    
   #THIS r   #T_IN s         
D                                r     P       #FUELPROPERTY_CERAMIC S         
                                 s     
  *          n                 ×              Cifmodintr.lib                               fn#fn )   7  !   b   uapp(FUEL_CARAMIC_HEADER    X  <   J  CONSTANTS       <   J  ISO_FORTRAN_ENV !   Š  \   J  EXCEPTION_HEADER )   ,  I   J  ABSTRACT_PROPERTY_HEADER 0   u         ERRORCOLLECTOR+EXCEPTION_HEADER :     @   a   ERRORCOLLECTOR%INFO_LIST+EXCEPTION_HEADER 8   F  @   a   ERRORCOLLECTOR%MESSAGE+EXCEPTION_HEADER 9     }   a   ERRORCOLLECTOR%COUNTING+EXCEPTION_HEADER :     }   a   ERRORCOLLECTOR%CNT_LIMIT+EXCEPTION_HEADER 4     X   a   ERRORCOLLECTOR%SET+EXCEPTION_HEADER 4   Ų  §      SET_ERRORCOLLECTOR+EXCEPTION_HEADER >     9      SET_ERRORCOLLECTOR%TRIM+EXCEPTION_HEADER=TRIM D   ø  <      SET_ERRORCOLLECTOR%ADJUSTL+EXCEPTION_HEADER=ADJUSTL 9   ō  P   a   SET_ERRORCOLLECTOR%THIS+EXCEPTION_HEADER >   D  @   a   SET_ERRORCOLLECTOR%INFO_LIST+EXCEPTION_HEADER <     @   a   SET_ERRORCOLLECTOR%MESSAGE+EXCEPTION_HEADER 6   Ä  Z   a   ERRORCOLLECTOR%PRINT+EXCEPTION_HEADER 6     x      PRINT_ERRORCOLLECTOR+EXCEPTION_HEADER @     9      PRINT_ERRORCOLLECTOR%TRIM+EXCEPTION_HEADER=TRIM ;   Ļ  P   a   PRINT_ERRORCOLLECTOR%THIS+EXCEPTION_HEADER <     8   a   PRINT_ERRORCOLLECTOR%UNIT_+EXCEPTION_HEADER 2   W         WARNINGCOLLECTOR+EXCEPTION_HEADER <   č  @   a   WARNINGCOLLECTOR%INFO_LIST+EXCEPTION_HEADER :   (	  @   a   WARNINGCOLLECTOR%MESSAGE+EXCEPTION_HEADER ;   h	  }   a   WARNINGCOLLECTOR%COUNTING+EXCEPTION_HEADER <   å	     a   WARNINGCOLLECTOR%CNT_LIMIT+EXCEPTION_HEADER 6   f
  Z   a   WARNINGCOLLECTOR%SET+EXCEPTION_HEADER 6   Ą
  «      SET_WARNINGCOLLECTOR+EXCEPTION_HEADER @   k  9      SET_WARNINGCOLLECTOR%TRIM+EXCEPTION_HEADER=TRIM F   ¤  <      SET_WARNINGCOLLECTOR%ADJUSTL+EXCEPTION_HEADER=ADJUSTL ;   ą  R   a   SET_WARNINGCOLLECTOR%THIS+EXCEPTION_HEADER @   2  @   a   SET_WARNINGCOLLECTOR%INFO_LIST+EXCEPTION_HEADER >   r  @   a   SET_WARNINGCOLLECTOR%MESSAGE+EXCEPTION_HEADER 8   ²  \   a   WARNINGCOLLECTOR%PRINT+EXCEPTION_HEADER 8     z      PRINT_WARNINGCOLLECTOR+EXCEPTION_HEADER B     9      PRINT_WARNINGCOLLECTOR%TRIM+EXCEPTION_HEADER=TRIM =   Į  R   a   PRINT_WARNINGCOLLECTOR%THIS+EXCEPTION_HEADER >     8   a   PRINT_WARNINGCOLLECTOR%UNIT_+EXCEPTION_HEADER 6   K        FUELPROPERTY+ABSTRACT_PROPERTY_HEADER @   L  @   a   FUELPROPERTY%T_MELTING+ABSTRACT_PROPERTY_HEADER ?     @   a   FUELPROPERTY%MOL_MASS+ABSTRACT_PROPERTY_HEADER @   Ģ  @   a   FUELPROPERTY%FUEL_TYPE+ABSTRACT_PROPERTY_HEADER >     @   a   FUELPROPERTY%DENSITY+ABSTRACT_PROPERTY_HEADER ?   L  @   a   FUELPROPERTY%CAPACITY+ABSTRACT_PROPERTY_HEADER C     @   a   FUELPROPERTY%CONDUCTIVITY+ABSTRACT_PROPERTY_HEADER @   Ģ  @   a   FUELPROPERTY%EXPANSION+ABSTRACT_PROPERTY_HEADER :     V   a   FUELPROPERTY%SET+ABSTRACT_PROPERTY_HEADER :   b  |      SET_FUELPROPERTY+ABSTRACT_PROPERTY_HEADER ?   Ž  N   a   SET_FUELPROPERTY%THIS+ABSTRACT_PROPERTY_HEADER ?   ,  8   a   SET_FUELPROPERTY%TYPE+ABSTRACT_PROPERTY_HEADER D   d  8   a   SET_FUELPROPERTY%WEIGHT_ZR+ABSTRACT_PROPERTY_HEADER ?     8   a   SET_FUELPROPERTY%X_PU+ABSTRACT_PROPERTY_HEADER @   Ō  8   a   SET_FUELPROPERTY%X_O2M+ABSTRACT_PROPERTY_HEADER B     e   a   FUELPROPERTY%GET_DENSITY+ABSTRACT_PROPERTY_HEADER I   q  \      GET_DENSITY_BY_TEMPERATURE_FUEL+ABSTRACT_PROPERTY_HEADER N   Ķ  N   a   GET_DENSITY_BY_TEMPERATURE_FUEL%THIS+ABSTRACT_PROPERTY_HEADER N     8   a   GET_DENSITY_BY_TEMPERATURE_FUEL%T_IN+ABSTRACT_PROPERTY_HEADER C   S  f   a   FUELPROPERTY%GET_CAPACITY+ABSTRACT_PROPERTY_HEADER J   ¹  \      GET_CAPACITY_BY_TEMPERATURE_FUEL+ABSTRACT_PROPERTY_HEADER O     N   a   GET_CAPACITY_BY_TEMPERATURE_FUEL%THIS+ABSTRACT_PROPERTY_HEADER O   c  8   a   GET_CAPACITY_BY_TEMPERATURE_FUEL%T_IN+ABSTRACT_PROPERTY_HEADER G     i   a   FUELPROPERTY%GET_CONDUCTIVITY+ABSTRACT_PROPERTY_HEADER M     \      GET_CONDUCTIVTY_BY_TEMPERATURE_FUEL+ABSTRACT_PROPERTY_HEADER R   `  N   a   GET_CONDUCTIVTY_BY_TEMPERATURE_FUEL%THIS+ABSTRACT_PROPERTY_HEADER R   ®  8   a   GET_CONDUCTIVTY_BY_TEMPERATURE_FUEL%T_IN+ABSTRACT_PROPERTY_HEADER D   ę  g   a   FUELPROPERTY%GET_EXPANSION+ABSTRACT_PROPERTY_HEADER K   M  \      GET_EXPANSION_BY_TEMPERATURE_FUEL+ABSTRACT_PROPERTY_HEADER P   ©  N   a   GET_EXPANSION_BY_TEMPERATURE_FUEL%THIS+ABSTRACT_PROPERTY_HEADER P   ÷  8   a   GET_EXPANSION_BY_TEMPERATURE_FUEL%T_IN+ABSTRACT_PROPERTY_HEADER "   /  ć      FILE_TP+CONSTANTS +     @   a   FILE_TP%CASENAME+CONSTANTS '   R  @   a   FILE_TP%MAIN+CONSTANTS )     @   a   FILE_TP%MEMORY+CONSTANTS +   Ņ  @   a   FILE_TP%TIMELIST+CONSTANTS &     @   a   FILE_TP%DET+CONSTANTS -   R  @   a   FILE_TP%REACTIVITY+CONSTANTS 0     @   a   FILE_TP%POINTKINETICS+CONSTANTS %   Ņ  @   a   FILE_TP%PT+CONSTANTS -     @   a   FILE_TP%TH_WARNING+CONSTANTS *   R  @   a   FILE_TP%TH_RBFD+CONSTANTS )     @   a   FILE_TP%TH_HOT+CONSTANTS -   Ņ  @   a   FILE_TP%TH_AVERAGE+CONSTANTS %     Č       FUELPROPERTY_CERAMIC 2   Ś  R   a   FUELPROPERTY_CERAMIC%FUELPROPERTY (   ,  @   !   FUELPROPERTY_CERAMIC%TD *   l  @   !   FUELPROPERTY_CERAMIC%X_PU +   ¬  @   !   FUELPROPERTY_CERAMIC%X_O2M )   ģ  V   a   FUELPROPERTY_CERAMIC%SET !   B        SET_FUELPROPERTY )   Ü  <      SET_FUELPROPERTY%PRESENT &     V   a   SET_FUELPROPERTY%THIS &   n  8   a   SET_FUELPROPERTY%TYPE +   ¦  8   a   SET_FUELPROPERTY%WEIGHT_ZR &   Ž  8   a   SET_FUELPROPERTY%X_PU '      8   a   SET_FUELPROPERTY%X_O2M 1   N   `   a   FUELPROPERTY_CERAMIC%GET_DENSITY +   ®         GET_DENSITY_BY_TEMPERATURE /   .!  8      GET_DENSITY_BY_TEMPERATURE%EXP 0   f!  V   a   GET_DENSITY_BY_TEMPERATURE%THIS 0   ¼!  8   a   GET_DENSITY_BY_TEMPERATURE%T_IN 2   ō!  a   a   FUELPROPERTY_CERAMIC%GET_CAPACITY ,   U"        GET_CAPACITY_BY_TEMPERATURE 0   Ö"  8      GET_CAPACITY_BY_TEMPERATURE%EXP 1   #  V   a   GET_CAPACITY_BY_TEMPERATURE%THIS 1   d#  8   a   GET_CAPACITY_BY_TEMPERATURE%T_IN 6   #  d   a   FUELPROPERTY_CERAMIC%GET_CONDUCTIVITY /    $  ¬      GET_CONDUCTIVTY_BY_TEMPERATURE 3   ¬$  8      GET_CONDUCTIVTY_BY_TEMPERATURE%ABS 3   ä$  8      GET_CONDUCTIVTY_BY_TEMPERATURE%EXP 4   %  V   a   GET_CONDUCTIVTY_BY_TEMPERATURE%THIS 4   r%  8   a   GET_CONDUCTIVTY_BY_TEMPERATURE%T_IN 3   Ŗ%  b   a   FUELPROPERTY_CERAMIC%GET_EXPANSION -   &  \      GET_EXPANSION_BY_TEMPERATURE 2   h&  V   a   GET_EXPANSION_BY_TEMPERATURE%THIS 2   ¾&  8   a   GET_EXPANSION_BY_TEMPERATURE%T_IN    ö&  Z      MsObjComment 