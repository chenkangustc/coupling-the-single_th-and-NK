	  æO  Ō   k820309    ?          14.0        5-Z                                                                                                           
       E:\documents\doctors degree\software\tansistant\NT\DAISY_verson_20170328\DAISY-git\git\src\IMPC2\IMP_assm_global.f90 IMP_ASSM_GLOBAL                                                 
                        @                              
                     @                               '0     	      #HYDRAU    #GEOM    #MESH /   #PROPERTY G   #TH_BOUNDARY U   #INITDATA g   #CONFACTOR_ {   #POW    #THERMAL                                                            #HYDRAULIC                  @                               '             #FRIC    #AFLOW    #WET    #DE 	   #SET 
   #PRINT    #CAL                                                       
                                                     
                                                     
                                          	           
   1     Ā    $                            
              #SET_HYDRAULIC    #     @     @                                                #THIS    #FRIC          
                                             #HYDRAULIC          
                                      
  1     Ā    $                                          #PRINT_HYDRAULIC    #     @     @                                                #THIS          
                                             #HYDRAULIC    1     Ā    $                                          #CAL_HYDRAULIC    #     @     @                                                #THIS    #RC    #PD          
                                             #HYDRAULIC          
                                      
        
                                      
                                               h          #ASSMGEOM                  @              @                'h            #PELLET    #BOND    #CLADTH    #PITCH    #PD    #ROD    #AREA    #HEIGHT    #N_FUELPIN     #N_PIN !   #SET "   #PRINT ,                                                      
                                                     
                                                     
                                                     
                                                      
                                               (      
                                               0      
                                                8         
        &                                                                     \   	                                              !     `   
      1     Ā    $                            "              #SET_ASSMGEOM #   #     @     @                            #                    #THIS $   #PELLET %   #BOND &   #CLADTH '   #PITCH (   #HEIGHT )   #PD *   #N_FUELPIN +         
                                $     h       #ASSMGEOM          
                                 %     
        
                                 &     
        
                                 '     
        
                                 (     
        
                                 )           
          &                             
                                 *     
        
                                  +       1     Ā    $                            ,              #PRINT_ASSMGEOM -   #     @     @                            -                    #THIS .         
                                .     h       #ASSMGEOM                                            /     |         #ASSMMESH 0                 @              @           0     '|            #NF 1   #NG 2   #NS 3   #N_ZONE 4   #NY 5   #LAYER_BOTTOM 6   #LAYER_TOP 7   #R 8   #Z 9   #SET :   #PRINT D                                           1                                                       2                                                      3                                                      4                                                      5                                                      6                                                      7                                                   8                 
        &           &                                                            9        L      	   
        &           &                       1     Ā    $                            :         
     #SET_ASSMMESH ;   #     @     @                            ;                    #THIS <   #NF =   #NG >   #NS ?   #N_ZONE @   #NY A   #LAYER_BOTTOM B   #LAYER_TOP C         
                                <     |       #ASSMMESH 0         
                                  =             
                                  >             
                                  ?             
                                  @             
                                  A             
                                  B             
                                  C       1     Ā    $                            D              #PRINT_ASSMMESH E   #     @     @                            E                    #THIS F         
                                F     |       #ASSMMESH 0                                           G     ä        #MATERIAL H                 @              @           H     'ä            #RHO I   #SHC J   #CTC K   #DVS L   #HTC M   #INIT N                                        I                  
        &           &                                                            J        0         
        &           &                                                            K        `         
        &           &                                                            L                 
        &           &                                                            M        Ā         
        &                       1     Ā    $                            N              #INIT_MATERIAL O   #     @     @                            O                    #THIS P   #NF Q   #NG R   #NS S   #NY T         
                                P     ä       #MATERIAL H         
                                  Q             
                                  R             
                                  S             
                                  T                                               U     0   č     #TH_BOUNDARY V                 @                          V     '0            #P W   #U [   #T \   #INIT ]   #UPDATE c                                           W               #BOUNDARY X                 @                          X     '            #INLET Y   #OUTLET Z                                          Y            
                                          Z           
                                           [              #BOUNDARY X                                           \               #BOUNDARY X   1     Ā    $                            ]              #INIT_TH_BOUNDARY ^   #     @     @                            ^                    #THIS _   #TIN `   #UIN a   #POUT b         
                                _     0       #TH_BOUNDARY V         
                                 `     
        
                                 a     
        
                                 b     
  1     Ā    $                            c              #UPDATE_TH_BOUNDARY d   #     @     @                            d                    #THIS e   #CTIME f         
                                e     0       #TH_BOUNDARY V         
                                 f     
                                          g     0        #ASSMINIT h                 @                          h     '0            #TI i   #PI j   #UI k   #TIN l   #POUT m   #UIN n   #SET o   #PRINT x                                          i            
                                          j           
                                          k           
                                          l           
                                          m            
                                          n     (      
   1     Ā    $                            o              #SET_ASSMINIT p   #     @     @                            p                    #THIS q   #TI r   #PI s   #UI t   #TIN u   #POUT v   #UIN w         
                                q     0       #ASSMINIT h         
                                 r     
        
                                 s     
        
                                 t     
        
                                 u     
        
                                 v     
        
                                 w     
  1     Ā    $                            x              #PRINT_ASSMINIT y   #     @     @                            y                    #THIS z         
                                z     0       #ASSMINIT h                                           {        H     #CONFACTOR |                 @                          |     '            #ALPHA }   #SIGMA ~   #SET    #PRINT                                           }            
                                          ~           
   1     Ā    $                                          #SET_CONFACTOR    #     @     @                                                #THIS    #ALPHA    #SIGMA          
                                            #CONFACTOR |         
                                      
        
                                      
  1     Ā    $                                          #PRINT_CONFACTOR    #     @     @                                                #THIS          
                                            #CONFACTOR |                                                `   X     #ASSMPOW                  @              @                '`            #POWER    #FQ_CORE    #SET    #PRINT                                                           
        &           &                                                                    0         
        &           &                       1     Ā    $                                          #SET_POWER    #     @     @                                                #THIS    #POWER    #FQ_CORE          
                                     `       #ASSMPOW          
                                            
          &           &                             
                                            
          &           &                       1     Ā    $                                          #PRINT_POWER    #     @     @                                                #THIS          
                                     `       #ASSMPOW                                                 x   ļ  	   #THERMAL                  @              @                'x            #TEMPERATURE    #PRESSURE    #VELOCITY    #INIT                                                           
        &           &                                                                   0         
        &                                                                    T         
        &                       1     Ā    $                                          #INIT_THERMAL    #     @     @                                               #INIT_THERMAL%SIZE    #THIS    #TEMPERATURE    #PRESSURE    #VELOCITY                                                SIZE       
                                     x       #THERMAL          
                                      
        
                                      
        
                                      
                @               @                '            #LTIME     #CTIME Ą   #NT Ē   #DT Ģ   #TTOTAL Ī   #TOUT Ĩ   #POW Ķ   #UIN §   #SET Ļ   #INIT ­   #RECORD ē   #ALLOC ļ   #CLC ŧ             $                                          
             $                             Ą           
             $                              Ē                        $                             Ģ           
             $                             Ī            
           $                             Ĩ        (         
        &                               $                             Ķ        L         
        &                               $                             §        p         
        &                       1     Ā    $                            Ļ         	     #SET_TIMER Đ   #     @     @                            Đ                    #THIS Š   #TTOTAL Ŧ   #NT Ž         
                                Š            #SYS_TIMER          
                                 Ŧ     
        
                                  Ž       1     Ā    $                            ­         
     #INIT_TIMER Ū   #     @     @                            Ū                    #THIS Ŋ   #CTIME °   #LTIME ą         
                                Ŋ            #SYS_TIMER          
                                 °     
        
                                 ą     
  1     Ā    $                            ē              #RECORD_TIMER ģ   #     @     @                            ģ                    #THIS ī   #TOUT ĩ   #UIN ķ   #POW ·         
                                ī            #SYS_TIMER          
                                 ĩ     
        
                                 ķ     
        
                                 ·     
  1     Ā    $                            ļ              #ALLOC_TIMER đ   #     @     @                            đ                    #THIS š         
                                š            #SYS_TIMER    1     Ā    $                            ŧ              #CLC_TIMER ž   #     @     @                            ž                   #CLC_TIMER%ALLOCATED ―   #THIS ū             @                            ―     ALLOCATED       
                                ū            #SYS_TIMER               @  @                          ŋ     '0            #CASENAME Ā   #MAIN Á   #MEMORY Â   #TIMELIST Ã   #DET Ä   #REACTIVITY Å   #POINTKINETICS Æ   #PT Į   #TH_WARNING Č   #TH_RBFD É   #TH_HOT Ę   #TH_AVERAGE Ë             $                              Ā                         $                              Á                        $                              Â                        $                              Ã                        $                              Ä                        $                              Å                        $                              Æ                        $                              Į                        $                              Č         	                $                              É     $   
                $                              Ę     (                   $                              Ë     ,                                                 Ė                     !  ę              C.thwarning                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Í                     !  ë              C.RBFD.bin                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               Î                     !  ė              C.thhotchannel                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           Ï                     !  í              C.thavgchannel                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            Ð     0  #SYS_ASSEMBLY                                             Ņ        #SYS_TIMER                 fn#fn $   -  <   J   IMP_ASSEMBLY_HEADER !   i  <   J   IMP_TIMER_HEADER 1   Ĩ  ·       SYS_ASSEMBLY+IMP_ASSEMBLY_HEADER 8   \  O   a   SYS_ASSEMBLY%HYDRAU+IMP_ASSEMBLY_HEADER *   Ŧ         HYDRAULIC+IMP_ASSM_HEADER /   2  @   a   HYDRAULIC%FRIC+IMP_ASSM_HEADER 0   r  @   a   HYDRAULIC%AFLOW+IMP_ASSM_HEADER .   ē  @   a   HYDRAULIC%WET+IMP_ASSM_HEADER -   ō  @   a   HYDRAULIC%DE+IMP_ASSM_HEADER .   2  S   a   HYDRAULIC%SET+IMP_ASSM_HEADER .     X      SET_HYDRAULIC+IMP_ASSM_HEADER 3   Ý  K   a   SET_HYDRAULIC%THIS+IMP_ASSM_HEADER 3   (  8   a   SET_HYDRAULIC%FRIC+IMP_ASSM_HEADER 0   `  U   a   HYDRAULIC%PRINT+IMP_ASSM_HEADER 0   ĩ  N      PRINT_HYDRAULIC+IMP_ASSM_HEADER 5     K   a   PRINT_HYDRAULIC%THIS+IMP_ASSM_HEADER .   N  S   a   HYDRAULIC%CAL+IMP_ASSM_HEADER .   Ą  ^      CAL_HYDRAULIC+IMP_ASSM_HEADER 3   ĸ  K   a   CAL_HYDRAULIC%THIS+IMP_ASSM_HEADER 1   J  8   a   CAL_HYDRAULIC%RC+IMP_ASSM_HEADER 1     8   a   CAL_HYDRAULIC%PD+IMP_ASSM_HEADER 6   š  N   a   SYS_ASSEMBLY%GEOM+IMP_ASSEMBLY_HEADER )     Æ       ASSMGEOM+IMP_ASSM_HEADER 0   Î  @   a   ASSMGEOM%PELLET+IMP_ASSM_HEADER .   	  @   a   ASSMGEOM%BOND+IMP_ASSM_HEADER 0   N	  @   a   ASSMGEOM%CLADTH+IMP_ASSM_HEADER /   	  @   a   ASSMGEOM%PITCH+IMP_ASSM_HEADER ,   Î	  @   a   ASSMGEOM%PD+IMP_ASSM_HEADER -   
  @   a   ASSMGEOM%ROD+IMP_ASSM_HEADER .   N
  @   a   ASSMGEOM%AREA+IMP_ASSM_HEADER 0   
  l   a   ASSMGEOM%HEIGHT+IMP_ASSM_HEADER 3   ú
  @   a   ASSMGEOM%N_FUELPIN+IMP_ASSM_HEADER /   :  @   a   ASSMGEOM%N_PIN+IMP_ASSM_HEADER -   z  R   a   ASSMGEOM%SET+IMP_ASSM_HEADER -   Ė        SET_ASSMGEOM+IMP_ASSM_HEADER 2   j  J   a   SET_ASSMGEOM%THIS+IMP_ASSM_HEADER 4   ī  8   a   SET_ASSMGEOM%PELLET+IMP_ASSM_HEADER 2   ė  8   a   SET_ASSMGEOM%BOND+IMP_ASSM_HEADER 4   $  8   a   SET_ASSMGEOM%CLADTH+IMP_ASSM_HEADER 3   \  8   a   SET_ASSMGEOM%PITCH+IMP_ASSM_HEADER 4     h   a   SET_ASSMGEOM%HEIGHT+IMP_ASSM_HEADER 0   ü  8   a   SET_ASSMGEOM%PD+IMP_ASSM_HEADER 7   4  8   a   SET_ASSMGEOM%N_FUELPIN+IMP_ASSM_HEADER /   l  T   a   ASSMGEOM%PRINT+IMP_ASSM_HEADER /   Ā  N      PRINT_ASSMGEOM+IMP_ASSM_HEADER 4     J   a   PRINT_ASSMGEOM%THIS+IMP_ASSM_HEADER 6   X  N   a   SYS_ASSEMBLY%MESH+IMP_ASSEMBLY_HEADER )   Ķ  ģ       ASSMMESH+IMP_ASSM_HEADER ,   Y  @   a   ASSMMESH%NF+IMP_ASSM_HEADER ,     @   a   ASSMMESH%NG+IMP_ASSM_HEADER ,   Ų  @   a   ASSMMESH%NS+IMP_ASSM_HEADER 0     @   a   ASSMMESH%N_ZONE+IMP_ASSM_HEADER ,   Y  @   a   ASSMMESH%NY+IMP_ASSM_HEADER 6     @   a   ASSMMESH%LAYER_BOTTOM+IMP_ASSM_HEADER 3   Ų  @   a   ASSMMESH%LAYER_TOP+IMP_ASSM_HEADER +     |   a   ASSMMESH%R+IMP_ASSM_HEADER +     |   a   ASSMMESH%Z+IMP_ASSM_HEADER -     R   a   ASSMMESH%SET+IMP_ASSM_HEADER -   c        SET_ASSMMESH+IMP_ASSM_HEADER 2   þ  J   a   SET_ASSMMESH%THIS+IMP_ASSM_HEADER 0   H  8   a   SET_ASSMMESH%NF+IMP_ASSM_HEADER 0     8   a   SET_ASSMMESH%NG+IMP_ASSM_HEADER 0   ļ  8   a   SET_ASSMMESH%NS+IMP_ASSM_HEADER 4   ð  8   a   SET_ASSMMESH%N_ZONE+IMP_ASSM_HEADER 0   (  8   a   SET_ASSMMESH%NY+IMP_ASSM_HEADER :   `  8   a   SET_ASSMMESH%LAYER_BOTTOM+IMP_ASSM_HEADER 7     8   a   SET_ASSMMESH%LAYER_TOP+IMP_ASSM_HEADER /   Ð  T   a   ASSMMESH%PRINT+IMP_ASSM_HEADER /   $  N      PRINT_ASSMMESH+IMP_ASSM_HEADER 4   r  J   a   PRINT_ASSMMESH%THIS+IMP_ASSM_HEADER :   ž  N   a   SYS_ASSEMBLY%PROPERTY+IMP_ASSEMBLY_HEADER )   
  {       MATERIAL+IMP_ASSM_HEADER -     |   a   MATERIAL%RHO+IMP_ASSM_HEADER -     |   a   MATERIAL%SHC+IMP_ASSM_HEADER -   }  |   a   MATERIAL%CTC+IMP_ASSM_HEADER -   ų  |   a   MATERIAL%DVS+IMP_ASSM_HEADER -   u  l   a   MATERIAL%HTC+IMP_ASSM_HEADER .   á  S   a   MATERIAL%INIT+IMP_ASSM_HEADER .   4  n      INIT_MATERIAL+IMP_ASSM_HEADER 3   Ē  J   a   INIT_MATERIAL%THIS+IMP_ASSM_HEADER 1   ė  8   a   INIT_MATERIAL%NF+IMP_ASSM_HEADER 1   $  8   a   INIT_MATERIAL%NG+IMP_ASSM_HEADER 1   \  8   a   INIT_MATERIAL%NS+IMP_ASSM_HEADER 1     8   a   INIT_MATERIAL%NY+IMP_ASSM_HEADER =   Ė  Q   a   SYS_ASSEMBLY%TH_BOUNDARY+IMP_ASSEMBLY_HEADER ,     o       TH_BOUNDARY+IMP_ASSM_HEADER .     N   a   TH_BOUNDARY%P+IMP_ASSM_HEADER )   Ú  [       BOUNDARY+IMP_ASSM_HEADER /   5  @   a   BOUNDARY%INLET+IMP_ASSM_HEADER 0   u  @   a   BOUNDARY%OUTLET+IMP_ASSM_HEADER .   ĩ  N   a   TH_BOUNDARY%U+IMP_ASSM_HEADER .     N   a   TH_BOUNDARY%T+IMP_ASSM_HEADER 1   Q  V   a   TH_BOUNDARY%INIT+IMP_ASSM_HEADER 1   §  j      INIT_TH_BOUNDARY+IMP_ASSM_HEADER 6     M   a   INIT_TH_BOUNDARY%THIS+IMP_ASSM_HEADER 5   ^  8   a   INIT_TH_BOUNDARY%TIN+IMP_ASSM_HEADER 5     8   a   INIT_TH_BOUNDARY%UIN+IMP_ASSM_HEADER 6   Î  8   a   INIT_TH_BOUNDARY%POUT+IMP_ASSM_HEADER 3      X   a   TH_BOUNDARY%UPDATE+IMP_ASSM_HEADER 3   ^   Y      UPDATE_TH_BOUNDARY+IMP_ASSM_HEADER 8   ·   M   a   UPDATE_TH_BOUNDARY%THIS+IMP_ASSM_HEADER 9   !  8   a   UPDATE_TH_BOUNDARY%CTIME+IMP_ASSM_HEADER :   <!  N   a   SYS_ASSEMBLY%INITDATA+IMP_ASSEMBLY_HEADER )   !         ASSMINIT+IMP_ASSM_HEADER ,   "  @   a   ASSMINIT%TI+IMP_ASSM_HEADER ,   V"  @   a   ASSMINIT%PI+IMP_ASSM_HEADER ,   "  @   a   ASSMINIT%UI+IMP_ASSM_HEADER -   Ö"  @   a   ASSMINIT%TIN+IMP_ASSM_HEADER .   #  @   a   ASSMINIT%POUT+IMP_ASSM_HEADER -   V#  @   a   ASSMINIT%UIN+IMP_ASSM_HEADER -   #  R   a   ASSMINIT%SET+IMP_ASSM_HEADER -   č#        SET_ASSMINIT+IMP_ASSM_HEADER 2   j$  J   a   SET_ASSMINIT%THIS+IMP_ASSM_HEADER 0   ī$  8   a   SET_ASSMINIT%TI+IMP_ASSM_HEADER 0   ė$  8   a   SET_ASSMINIT%PI+IMP_ASSM_HEADER 0   $%  8   a   SET_ASSMINIT%UI+IMP_ASSM_HEADER 1   \%  8   a   SET_ASSMINIT%TIN+IMP_ASSM_HEADER 2   %  8   a   SET_ASSMINIT%POUT+IMP_ASSM_HEADER 1   Ė%  8   a   SET_ASSMINIT%UIN+IMP_ASSM_HEADER /   &  T   a   ASSMINIT%PRINT+IMP_ASSM_HEADER /   X&  N      PRINT_ASSMINIT+IMP_ASSM_HEADER 4   Ķ&  J   a   PRINT_ASSMINIT%THIS+IMP_ASSM_HEADER <   ð&  O   a   SYS_ASSEMBLY%CONFACTOR_+IMP_ASSEMBLY_HEADER *   ?'  n       CONFACTOR+IMP_ASSM_HEADER 0   ­'  @   a   CONFACTOR%ALPHA+IMP_ASSM_HEADER 0   í'  @   a   CONFACTOR%SIGMA+IMP_ASSM_HEADER .   -(  S   a   CONFACTOR%SET+IMP_ASSM_HEADER .   (  d      SET_CONFACTOR+IMP_ASSM_HEADER 3   ä(  K   a   SET_CONFACTOR%THIS+IMP_ASSM_HEADER 4   /)  8   a   SET_CONFACTOR%ALPHA+IMP_ASSM_HEADER 4   g)  8   a   SET_CONFACTOR%SIGMA+IMP_ASSM_HEADER 0   )  U   a   CONFACTOR%PRINT+IMP_ASSM_HEADER 0   ô)  N      PRINT_CONFACTOR+IMP_ASSM_HEADER 5   B*  K   a   PRINT_CONFACTOR%THIS+IMP_ASSM_HEADER 5   *  M   a   SYS_ASSEMBLY%POW+IMP_ASSEMBLY_HEADER (   Ú*  p       ASSMPOW+IMP_ASSM_HEADER .   J+  |   a   ASSMPOW%POWER+IMP_ASSM_HEADER 0   Æ+  |   a   ASSMPOW%FQ_CORE+IMP_ASSM_HEADER ,   B,  O   a   ASSMPOW%SET+IMP_ASSM_HEADER *   ,  f      SET_POWER+IMP_ASSM_HEADER /   ũ,  I   a   SET_POWER%THIS+IMP_ASSM_HEADER 0   @-  x   a   SET_POWER%POWER+IMP_ASSM_HEADER 2   ļ-  x   a   SET_POWER%FQ_CORE+IMP_ASSM_HEADER .   0.  Q   a   ASSMPOW%PRINT+IMP_ASSM_HEADER ,   .  N      PRINT_POWER+IMP_ASSM_HEADER 1   Ï.  I   a   PRINT_POWER%THIS+IMP_ASSM_HEADER 9   /  M   a   SYS_ASSEMBLY%THERMAL+IMP_ASSEMBLY_HEADER (   e/  {       THERMAL+IMP_ASSM_HEADER 4   ā/  |   a   THERMAL%TEMPERATURE+IMP_ASSM_HEADER 1   \0  l   a   THERMAL%PRESSURE+IMP_ASSM_HEADER 1   Č0  l   a   THERMAL%VELOCITY+IMP_ASSM_HEADER -   41  R   a   THERMAL%INIT+IMP_ASSM_HEADER -   1        INIT_THERMAL+IMP_ASSM_HEADER 7   2  9      INIT_THERMAL%SIZE+IMP_ASSM_HEADER=SIZE 2   Q2  I   a   INIT_THERMAL%THIS+IMP_ASSM_HEADER 9   2  8   a   INIT_THERMAL%TEMPERATURE+IMP_ASSM_HEADER 6   Ō2  8   a   INIT_THERMAL%PRESSURE+IMP_ASSM_HEADER 6   
3  8   a   INIT_THERMAL%VELOCITY+IMP_ASSM_HEADER +   B3  Å       SYS_TIMER+IMP_TIMER_HEADER 1   4  @   a   SYS_TIMER%LTIME+IMP_TIMER_HEADER 1   G4  @   a   SYS_TIMER%CTIME+IMP_TIMER_HEADER .   4  @   a   SYS_TIMER%NT+IMP_TIMER_HEADER .   Į4  @   a   SYS_TIMER%DT+IMP_TIMER_HEADER 2   5  @   a   SYS_TIMER%TTOTAL+IMP_TIMER_HEADER 0   G5  l   a   SYS_TIMER%TOUT+IMP_TIMER_HEADER /   ģ5  l   a   SYS_TIMER%POW+IMP_TIMER_HEADER /   6  l   a   SYS_TIMER%UIN+IMP_TIMER_HEADER /   6  O   a   SYS_TIMER%SET+IMP_TIMER_HEADER +   Ú6  b      SET_TIMER+IMP_TIMER_HEADER 0   <7  K   a   SET_TIMER%THIS+IMP_TIMER_HEADER 2   7  8   a   SET_TIMER%TTOTAL+IMP_TIMER_HEADER .   ŋ7  8   a   SET_TIMER%NT+IMP_TIMER_HEADER 0   ũ7  P   a   SYS_TIMER%INIT+IMP_TIMER_HEADER ,   G8  d      INIT_TIMER+IMP_TIMER_HEADER 1   Ŧ8  K   a   INIT_TIMER%THIS+IMP_TIMER_HEADER 2   ö8  8   a   INIT_TIMER%CTIME+IMP_TIMER_HEADER 2   .9  8   a   INIT_TIMER%LTIME+IMP_TIMER_HEADER 2   f9  R   a   SYS_TIMER%RECORD+IMP_TIMER_HEADER .   ļ9  j      RECORD_TIMER+IMP_TIMER_HEADER 3   ":  K   a   RECORD_TIMER%THIS+IMP_TIMER_HEADER 3   m:  8   a   RECORD_TIMER%TOUT+IMP_TIMER_HEADER 2   Ĩ:  8   a   RECORD_TIMER%UIN+IMP_TIMER_HEADER 2   Ý:  8   a   RECORD_TIMER%POW+IMP_TIMER_HEADER 1   ;  Q   a   SYS_TIMER%ALLOC+IMP_TIMER_HEADER -   f;  N      ALLOC_TIMER+IMP_TIMER_HEADER 2   ī;  K   a   ALLOC_TIMER%THIS+IMP_TIMER_HEADER /   ĸ;  O   a   SYS_TIMER%CLC+IMP_TIMER_HEADER +   N<  g      CLC_TIMER+IMP_TIMER_HEADER ?   ĩ<  >      CLC_TIMER%ALLOCATED+IMP_TIMER_HEADER=ALLOCATED 0   ó<  K   a   CLC_TIMER%THIS+IMP_TIMER_HEADER "   >=  ã      FILE_TP+CONSTANTS +   !>  @   a   FILE_TP%CASENAME+CONSTANTS '   a>  @   a   FILE_TP%MAIN+CONSTANTS )   Ą>  @   a   FILE_TP%MEMORY+CONSTANTS +   á>  @   a   FILE_TP%TIMELIST+CONSTANTS &   !?  @   a   FILE_TP%DET+CONSTANTS -   a?  @   a   FILE_TP%REACTIVITY+CONSTANTS 0   Ą?  @   a   FILE_TP%POINTKINETICS+CONSTANTS %   á?  @   a   FILE_TP%PT+CONSTANTS -   !@  @   a   FILE_TP%TH_WARNING+CONSTANTS *   a@  @   a   FILE_TP%TH_RBFD+CONSTANTS )   Ą@  @   a   FILE_TP%TH_HOT+CONSTANTS -   á@  @   a   FILE_TP%TH_AVERAGE+CONSTANTS %   !A        TH_WARNING+CONSTANTS "   ŪD        TH_RBFD+CONSTANTS !   ;H        TH_HOT+CONSTANTS %   ČK        TH_AVERAGE+CONSTANTS    UO  J       ASSM1    O  G       TIMER1 