	  4&  s   k820309    ?          14.0        (Z                                                                                                           
       E:\documents\doctors degree\software\tansistant\NT\DAISY_verson_20170328\DAISY-git\git\src\thermal\fuel_metallic_header.f90 FUEL_METALLIC_HEADER          FUELPROPERTY_METALLIC                  @                              
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
                $                              Q     (                   $                              R     ,                       @                          S     'X      
      #FUELPROPERTY T   #WEIGHT_ZR U   #WEIGHT_PU V   #X_ZR W   #X_PU X   #SET Y   #GET_DENSITY a   #GET_CAPACITY e   #GET_CONDUCTIVITY i   #GET_EXPANSION m             $                              T     8          #FUELPROPERTY '             D                             U     8      
             D                             V     @      
             D                             W     H      
             D                             X     P      
   1     Ą    $                           Y              #SET_FUELPROPERTY Z   #     @     @                            Z                   #SET_FUELPROPERTY%PRESENT [   #THIS \   #TYPE ]   #WEIGHT_ZR ^   #X_PU _   #X_O2M `             @                            [     PRESENT       
D                                \     X       #FUELPROPERTY_METALLIC S         
                                  ]             
 @                              ^     
        
                                _     
        
                                `     
  1     Ą    $                          a              #GET_DENSITY_BY_TEMPERATURE b   %     @    @                            b                    
   #THIS c   #T_IN d         
D                                c     X       #FUELPROPERTY_METALLIC S         
                                 d     
  1     Ą    $                          e              #GET_CAPACITY_BY_TEMPERATURE f   %     @    @                            f                    
   #THIS g   #T_IN h         
D @                              g     X       #FUELPROPERTY_METALLIC S         
                                 h     
  1     Ą    $                          i         	     #GET_CONDUCTIVTY_BY_TEMPERATURE j   %     @    @                            j                    
   #THIS k   #T_IN l         
D @                              k     X       #FUELPROPERTY_METALLIC S         
                                 l     
  1     Ą    $                          m         
     #GET_EXPANSION_BY_TEMPERATURE n   %     @    @                            n                    
   #THIS o   #T_IN p         
D                                o     X       #FUELPROPERTY_METALLIC S         
                                 p     
  *          n                 ž              Cifmodintr.lib                               fn#fn *   9  "   b   uapp(FUEL_METALLIC_HEADER    [  <   J  CONSTANTS       <   J  ISO_FORTRAN_ENV !   Ó  \   J  EXCEPTION_HEADER )   /  I   J  ABSTRACT_PROPERTY_HEADER 0   x         ERRORCOLLECTOR+EXCEPTION_HEADER :   	  @   a   ERRORCOLLECTOR%INFO_LIST+EXCEPTION_HEADER 8   I  @   a   ERRORCOLLECTOR%MESSAGE+EXCEPTION_HEADER 9     }   a   ERRORCOLLECTOR%COUNTING+EXCEPTION_HEADER :     }   a   ERRORCOLLECTOR%CNT_LIMIT+EXCEPTION_HEADER 4     X   a   ERRORCOLLECTOR%SET+EXCEPTION_HEADER 4   Ū  §      SET_ERRORCOLLECTOR+EXCEPTION_HEADER >     9      SET_ERRORCOLLECTOR%TRIM+EXCEPTION_HEADER=TRIM D   »  <      SET_ERRORCOLLECTOR%ADJUSTL+EXCEPTION_HEADER=ADJUSTL 9   ÷  P   a   SET_ERRORCOLLECTOR%THIS+EXCEPTION_HEADER >   G  @   a   SET_ERRORCOLLECTOR%INFO_LIST+EXCEPTION_HEADER <     @   a   SET_ERRORCOLLECTOR%MESSAGE+EXCEPTION_HEADER 6   Ē  Z   a   ERRORCOLLECTOR%PRINT+EXCEPTION_HEADER 6   !  x      PRINT_ERRORCOLLECTOR+EXCEPTION_HEADER @     9      PRINT_ERRORCOLLECTOR%TRIM+EXCEPTION_HEADER=TRIM ;   Ņ  P   a   PRINT_ERRORCOLLECTOR%THIS+EXCEPTION_HEADER <   "  8   a   PRINT_ERRORCOLLECTOR%UNIT_+EXCEPTION_HEADER 2   Z         WARNINGCOLLECTOR+EXCEPTION_HEADER <   ė  @   a   WARNINGCOLLECTOR%INFO_LIST+EXCEPTION_HEADER :   +	  @   a   WARNINGCOLLECTOR%MESSAGE+EXCEPTION_HEADER ;   k	  }   a   WARNINGCOLLECTOR%COUNTING+EXCEPTION_HEADER <   č	     a   WARNINGCOLLECTOR%CNT_LIMIT+EXCEPTION_HEADER 6   i
  Z   a   WARNINGCOLLECTOR%SET+EXCEPTION_HEADER 6   Ć
  «      SET_WARNINGCOLLECTOR+EXCEPTION_HEADER @   n  9      SET_WARNINGCOLLECTOR%TRIM+EXCEPTION_HEADER=TRIM F   §  <      SET_WARNINGCOLLECTOR%ADJUSTL+EXCEPTION_HEADER=ADJUSTL ;   ć  R   a   SET_WARNINGCOLLECTOR%THIS+EXCEPTION_HEADER @   5  @   a   SET_WARNINGCOLLECTOR%INFO_LIST+EXCEPTION_HEADER >   u  @   a   SET_WARNINGCOLLECTOR%MESSAGE+EXCEPTION_HEADER 8   µ  \   a   WARNINGCOLLECTOR%PRINT+EXCEPTION_HEADER 8     z      PRINT_WARNINGCOLLECTOR+EXCEPTION_HEADER B     9      PRINT_WARNINGCOLLECTOR%TRIM+EXCEPTION_HEADER=TRIM =   Ä  R   a   PRINT_WARNINGCOLLECTOR%THIS+EXCEPTION_HEADER >     8   a   PRINT_WARNINGCOLLECTOR%UNIT_+EXCEPTION_HEADER 6   N        FUELPROPERTY+ABSTRACT_PROPERTY_HEADER @   O  @   a   FUELPROPERTY%T_MELTING+ABSTRACT_PROPERTY_HEADER ?     @   a   FUELPROPERTY%MOL_MASS+ABSTRACT_PROPERTY_HEADER @   Ļ  @   a   FUELPROPERTY%FUEL_TYPE+ABSTRACT_PROPERTY_HEADER >     @   a   FUELPROPERTY%DENSITY+ABSTRACT_PROPERTY_HEADER ?   O  @   a   FUELPROPERTY%CAPACITY+ABSTRACT_PROPERTY_HEADER C     @   a   FUELPROPERTY%CONDUCTIVITY+ABSTRACT_PROPERTY_HEADER @   Ļ  @   a   FUELPROPERTY%EXPANSION+ABSTRACT_PROPERTY_HEADER :     V   a   FUELPROPERTY%SET+ABSTRACT_PROPERTY_HEADER :   e  |      SET_FUELPROPERTY+ABSTRACT_PROPERTY_HEADER ?   į  N   a   SET_FUELPROPERTY%THIS+ABSTRACT_PROPERTY_HEADER ?   /  8   a   SET_FUELPROPERTY%TYPE+ABSTRACT_PROPERTY_HEADER D   g  8   a   SET_FUELPROPERTY%WEIGHT_ZR+ABSTRACT_PROPERTY_HEADER ?     8   a   SET_FUELPROPERTY%X_PU+ABSTRACT_PROPERTY_HEADER @   ×  8   a   SET_FUELPROPERTY%X_O2M+ABSTRACT_PROPERTY_HEADER B     e   a   FUELPROPERTY%GET_DENSITY+ABSTRACT_PROPERTY_HEADER I   t  \      GET_DENSITY_BY_TEMPERATURE_FUEL+ABSTRACT_PROPERTY_HEADER N   Š  N   a   GET_DENSITY_BY_TEMPERATURE_FUEL%THIS+ABSTRACT_PROPERTY_HEADER N     8   a   GET_DENSITY_BY_TEMPERATURE_FUEL%T_IN+ABSTRACT_PROPERTY_HEADER C   V  f   a   FUELPROPERTY%GET_CAPACITY+ABSTRACT_PROPERTY_HEADER J   ¼  \      GET_CAPACITY_BY_TEMPERATURE_FUEL+ABSTRACT_PROPERTY_HEADER O     N   a   GET_CAPACITY_BY_TEMPERATURE_FUEL%THIS+ABSTRACT_PROPERTY_HEADER O   f  8   a   GET_CAPACITY_BY_TEMPERATURE_FUEL%T_IN+ABSTRACT_PROPERTY_HEADER G     i   a   FUELPROPERTY%GET_CONDUCTIVITY+ABSTRACT_PROPERTY_HEADER M     \      GET_CONDUCTIVTY_BY_TEMPERATURE_FUEL+ABSTRACT_PROPERTY_HEADER R   c  N   a   GET_CONDUCTIVTY_BY_TEMPERATURE_FUEL%THIS+ABSTRACT_PROPERTY_HEADER R   ±  8   a   GET_CONDUCTIVTY_BY_TEMPERATURE_FUEL%T_IN+ABSTRACT_PROPERTY_HEADER D   é  g   a   FUELPROPERTY%GET_EXPANSION+ABSTRACT_PROPERTY_HEADER K   P  \      GET_EXPANSION_BY_TEMPERATURE_FUEL+ABSTRACT_PROPERTY_HEADER P   ¬  N   a   GET_EXPANSION_BY_TEMPERATURE_FUEL%THIS+ABSTRACT_PROPERTY_HEADER P   ś  8   a   GET_EXPANSION_BY_TEMPERATURE_FUEL%T_IN+ABSTRACT_PROPERTY_HEADER "   2  ć      FILE_TP+CONSTANTS +     @   a   FILE_TP%CASENAME+CONSTANTS '   U  @   a   FILE_TP%MAIN+CONSTANTS )     @   a   FILE_TP%MEMORY+CONSTANTS +   Õ  @   a   FILE_TP%TIMELIST+CONSTANTS &     @   a   FILE_TP%DET+CONSTANTS -   U  @   a   FILE_TP%REACTIVITY+CONSTANTS 0     @   a   FILE_TP%POINTKINETICS+CONSTANTS %   Õ  @   a   FILE_TP%PT+CONSTANTS -     @   a   FILE_TP%TH_WARNING+CONSTANTS *   U  @   a   FILE_TP%TH_RBFD+CONSTANTS )     @   a   FILE_TP%TH_HOT+CONSTANTS -   Õ  @   a   FILE_TP%TH_AVERAGE+CONSTANTS &     Ż       FUELPROPERTY_METALLIC 3   ņ  R   a   FUELPROPERTY_METALLIC%FUELPROPERTY 0   D  @   !   FUELPROPERTY_METALLIC%WEIGHT_ZR 0     @   !   FUELPROPERTY_METALLIC%WEIGHT_PU +   Ä  @   !   FUELPROPERTY_METALLIC%X_ZR +     @   !   FUELPROPERTY_METALLIC%X_PU *   D  V   a   FUELPROPERTY_METALLIC%SET !           SET_FUELPROPERTY )   4  <      SET_FUELPROPERTY%PRESENT &   p  W   a   SET_FUELPROPERTY%THIS &   Ē  8   a   SET_FUELPROPERTY%TYPE +   ’  8   a   SET_FUELPROPERTY%WEIGHT_ZR &   7   8   a   SET_FUELPROPERTY%X_PU '   o   8   a   SET_FUELPROPERTY%X_O2M 2   §   `   a   FUELPROPERTY_METALLIC%GET_DENSITY +   !  \      GET_DENSITY_BY_TEMPERATURE 0   c!  W   a   GET_DENSITY_BY_TEMPERATURE%THIS 0   ŗ!  8   a   GET_DENSITY_BY_TEMPERATURE%T_IN 3   ņ!  a   a   FUELPROPERTY_METALLIC%GET_CAPACITY ,   S"  \      GET_CAPACITY_BY_TEMPERATURE 1   Æ"  W   a   GET_CAPACITY_BY_TEMPERATURE%THIS 1   #  8   a   GET_CAPACITY_BY_TEMPERATURE%T_IN 7   >#  d   a   FUELPROPERTY_METALLIC%GET_CONDUCTIVITY /   ¢#  \      GET_CONDUCTIVTY_BY_TEMPERATURE 4   ž#  W   a   GET_CONDUCTIVTY_BY_TEMPERATURE%THIS 4   U$  8   a   GET_CONDUCTIVTY_BY_TEMPERATURE%T_IN 4   $  b   a   FUELPROPERTY_METALLIC%GET_EXPANSION -   ļ$  \      GET_EXPANSION_BY_TEMPERATURE 2   K%  W   a   GET_EXPANSION_BY_TEMPERATURE%THIS 2   ¢%  8   a   GET_EXPANSION_BY_TEMPERATURE%T_IN    Ś%  Z      MsObjComment 