	  æH  ¹   k820309    ?          14.0        ³¢.Z                                                                                                           
       E:\documents\doctors degree\software\tansistant\NT\DAISY_verson_20170328\DAISY-git\git\src\IMPC2\IMP_assm_header.f90 IMP_ASSM_HEADER          SET_ASSMGEOM PRINT_ASSMGEOM SET_ASSMMESH PRINT_ASSMMESH SET_ASSMINIT PRINT_ASSMINIT SET_CONFACTOR PRINT_CONFACTOR SET_HYDRAULIC PRINT_HYDRAULIC SET_POWER PRINT_POWER INIT_TH_BOUNDARY UPDATE_TH_BOUNDARY INIT_MATERIAL INIT_THERMAL CAL_HYDRAULIC                  @                             
                                                       
                       @                              
                 @  @                               '0            #CASENAME    #MAIN    #MEMORY    #TIMELIST    #DET 	   #REACTIVITY 
   #POINTKINETICS    #PT    #TH_WARNING    #TH_RBFD    #TH_HOT    #TH_AVERAGE              $                                                       $                                                      $                                                      $                                                      $                              	                        $                              
                        $                                                      $                                                      $                                       	                $                                   $   
                $                                   (                   $                                   ,                                                                                                                                        
         
         (-DTû!	@        3.1415926535898D0#     @                                                      #RC    #PD    #AFLOW    #WET    #DE                                               
                                              
                                              
                                              
                                              
                                                                !                C.thwarning                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   !                C.RBFD.bin                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    !                C.thhotchannel                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                !                C.thavgchannel                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 @               @                'h            #PELLET    #BOND    #CLADTH     #PITCH !   #PD "   #ROD #   #AREA $   #HEIGHT %   #N_FUELPIN &   #N_PIN '   #SET (   #PRINT 2                                                      
                                                     
                                                      
                                          !           
                                          "            
                                          #     (      
                                          $     0      
                                        %        8         
        &                                                               &     \   	                                              '     `   
      1     À    $                            (              #SET_ASSMGEOM )   #     @     @                             )                    #THIS *   #PELLET +   #BOND ,   #CLADTH -   #PITCH .   #HEIGHT /   #PD 0   #N_FUELPIN 1         
D @                              *     h       #ASSMGEOM          
                                 +     
        
                                 ,     
        
                                 -     
        
                                 .     
        
                                 /           
          &                             
                                 0     
        
                                  1       1     À    $                           2              #PRINT_ASSMGEOM 3   #     @     @                            3                    #THIS 4         
                                4     h       #ASSMGEOM                  @               @           5     '|            #NF 6   #NG 7   #NS 8   #N_ZONE 9   #NY :   #LAYER_BOTTOM ;   #LAYER_TOP <   #R =   #Z >   #SET ?   #PRINT I                                           6                                                       7                                                      8                                                      9                                                      :                                                      ;                                                      <                                                   =                 
        &           &                                                            >        L      	   
        &           &                       1     À    $                            ?         
     #SET_ASSMMESH @   #     @     @                             @                    #THIS A   #NF B   #NG C   #NS D   #N_ZONE E   #NY F   #LAYER_BOTTOM G   #LAYER_TOP H         
D @                              A     |       #ASSMMESH 5         
                                  B             
                                  C             
                                  D             
                                  E             
                                  F             
                                  G             
                                  H       1     À    $                           I              #PRINT_ASSMMESH J   #     @     @                            J                    #THIS K         
                                K     |       #ASSMMESH 5                 @                          L     '            #INLET M   #OUTLET N                                          M            
                                          N           
                 @               @           O     'x            #TEMPERATURE P   #PRESSURE Q   #VELOCITY R                                        P                  
        &           &                                                            Q        0         
        &                                                            R        T         
        &                                     @                          S     '0            #P T   #U U   #T V   #INIT W   #UPDATE ]                                           T               #BOUNDARY L                                           U              #BOUNDARY L                                           V               #BOUNDARY L   1     À    $                            W              #INIT_TH_BOUNDARY X   #     @     @                             X                    #THIS Y   #TIN Z   #UIN [   #POUT \         
D                                Y     0       #TH_BOUNDARY S         
                                 Z     
        
                                 [     
        
                                 \     
  1     À    $                            ]              #UPDATE_TH_BOUNDARY ^   #     @     @                             ^                    #THIS _   #CTIME `         
D                                _     0       #TH_BOUNDARY S         
                                 `     
                @                           a     '             #FRIC b   #AFLOW c   #WET d   #DE e   #SET f   #PRINT j   #CAL m                                          b            
                                          c           
                                          d           
                                          e           
   1     À    $                            f              #SET_HYDRAULIC g   #     @     @                             g                    #THIS h   #FRIC i         
D @                              h             #HYDRAULIC a         
                                 i     
  1     À    $                           j              #PRINT_HYDRAULIC k   #     @     @                            k                    #THIS l         
                                l             #HYDRAULIC a   1     À    $                            m              #CAL_HYDRAULIC n   #     @     @                             n                    #THIS o   #RC p   #PD q         
D @                              o             #HYDRAULIC a         
@ @                              p     
        
@ @                              q     
                @              @           r     'ä            #RHO s   #SHC t   #CTC u   #DVS v   #HTC w   #INIT x                                        s                  
        &           &                                                            t        0         
        &           &                                                            u        `         
        &           &                                                            v                 
        &           &                                                            w        À         
        &                       1     À    $                            x              #INIT_MATERIAL y   #     @     @                             y                    #THIS z   #NF {   #NG |   #NS }   #NY ~         
D                                z     ä       #MATERIAL r         
                                  {             
                                  |             
                                  }             
                                  ~                     @              @                'x            #TEMPERATURE    #PRESSURE    #VELOCITY    #INIT                                                           
        &           &                                                                   0         
        &                                                                    T         
        &                       1     À    $                                          #INIT_THERMAL    #     @     @                                                #INIT_THERMAL%SIZE    #THIS    #TEMPERATURE    #PRESSURE    #VELOCITY                                                SIZE       
D                                     x       #THERMAL          
                                      
        
                                      
        
                                      
                @                                '0            #TI    #PI    #UI    #TIN    #POUT    #UIN    #SET    #PRINT                                                       
                                                     
                                                     
                                                     
                                                      
                                               (      
   1     À    $                                          #SET_ASSMINIT    #     @     @                                                 #THIS    #TI    #PI    #UI    #TIN    #POUT    #UIN          
D @                                   0       #ASSMINIT          
                                      
        
                                      
        
                                      
        
                                      
        
                                      
        
                                      
  1     À    $                                         #PRINT_ASSMINIT    #     @     @                                                #THIS          
                                     0       #ASSMINIT                  @                                '            #ALPHA    #SIGMA    #SET     #PRINT ¥                                                      
                                                     
   1     À    $                                           #SET_CONFACTOR ¡   #     @     @                             ¡                    #THIS ¢   #ALPHA £   #SIGMA ¤         
D @                              ¢            #CONFACTOR          
                                 £     
        
                                 ¤     
  1     À    $                           ¥              #PRINT_CONFACTOR ¦   #     @     @                            ¦                    #THIS §         
                                §            #CONFACTOR                  @              @           ¨     '`            #POWER ©   #FQ_CORE ª   #SET «   #PRINT °                                        ©                  
        &           &                                                            ª        0         
        &           &                       1     À    $                            «              #SET_POWER ¬   #     @     @                             ¬                    #THIS ­   #POWER ®   #FQ_CORE ¯         
D                                ­     `       #ASSMPOW ¨         
                                 ®           
          &           &                             
                                 ¯           
          &           &                       1     À    $                            °              #PRINT_POWER ±   #     @     @                             ±                    #THIS ²         
                                ²     `       #ASSMPOW ¨                 @                          ³     'ð           #TH_BOUNDARY ´   #THERMAL µ   #MATERIAL ¶   #POWER ·                                           ´     0          #TH_BOUNDARY S                                           µ     x   0      #THERMAL                                            ¶     ä   ¨      #MATERIAL r                                           ·     `        #ASSMPOW ¨                fn#fn %   -  ÿ   b   uapp(IMP_ASSM_HEADER    ,  <   J   IMP_MATHKEREL    h  <   J   IMP_PROPERTY    ¤  <   J   CONSTANTS "   à  ã      FILE_TP+CONSTANTS +   Ã  @   a   FILE_TP%CASENAME+CONSTANTS '     @   a   FILE_TP%MAIN+CONSTANTS )   C  @   a   FILE_TP%MEMORY+CONSTANTS +     @   a   FILE_TP%TIMELIST+CONSTANTS &   Ã  @   a   FILE_TP%DET+CONSTANTS -     @   a   FILE_TP%REACTIVITY+CONSTANTS 0   C  @   a   FILE_TP%POINTKINETICS+CONSTANTS %     @   a   FILE_TP%PT+CONSTANTS -   Ã  @   a   FILE_TP%TH_WARNING+CONSTANTS *     @   a   FILE_TP%TH_RBFD+CONSTANTS )   C  @   a   FILE_TP%TH_HOT+CONSTANTS -     @   a   FILE_TP%TH_AVERAGE+CONSTANTS     Ã  \       KREAL+CONSTANTS      m       PI+CONSTANTS -     p       GET_HYCONSTANT+IMP_MATHKEREL 0   ü  8   a   GET_HYCONSTANT%RC+IMP_MATHKEREL 0   4  8   a   GET_HYCONSTANT%PD+IMP_MATHKEREL 3   l  8   a   GET_HYCONSTANT%AFLOW+IMP_MATHKEREL 1   ¤  8   a   GET_HYCONSTANT%WET+IMP_MATHKEREL 0   Ü  8   a   GET_HYCONSTANT%DE+IMP_MATHKEREL %   	        TH_WARNING+CONSTANTS "   ¡        TH_RBFD+CONSTANTS !   .        TH_HOT+CONSTANTS %   »        TH_AVERAGE+CONSTANTS    H  Æ       ASSMGEOM       @   a   ASSMGEOM%PELLET    N  @   a   ASSMGEOM%BOND       @   a   ASSMGEOM%CLADTH    Î  @   a   ASSMGEOM%PITCH      @   a   ASSMGEOM%PD    N  @   a   ASSMGEOM%ROD      @   a   ASSMGEOM%AREA     Î  l   a   ASSMGEOM%HEIGHT #   :  @   a   ASSMGEOM%N_FUELPIN    z  @   a   ASSMGEOM%N_PIN    º  R   a   ASSMGEOM%SET            SET_ASSMGEOM "   ª  J   a   SET_ASSMGEOM%THIS $   ô  8   a   SET_ASSMGEOM%PELLET "   ,  8   a   SET_ASSMGEOM%BOND $   d  8   a   SET_ASSMGEOM%CLADTH #     8   a   SET_ASSMGEOM%PITCH $   Ô  h   a   SET_ASSMGEOM%HEIGHT     <  8   a   SET_ASSMGEOM%PD '   t  8   a   SET_ASSMGEOM%N_FUELPIN    ¬  T   a   ASSMGEOM%PRINT       N      PRINT_ASSMGEOM $   N  J   a   PRINT_ASSMGEOM%THIS      ³       ASSMMESH    K  @   a   ASSMMESH%NF      @   a   ASSMMESH%NG    Ë  @   a   ASSMMESH%NS        @   a   ASSMMESH%N_ZONE    K   @   a   ASSMMESH%NY &      @   a   ASSMMESH%LAYER_BOTTOM #   Ë   @   a   ASSMMESH%LAYER_TOP    !  |   a   ASSMMESH%R    !  |   a   ASSMMESH%Z    "  R   a   ASSMMESH%SET    U"        SET_ASSMMESH "   ð"  J   a   SET_ASSMMESH%THIS     :#  8   a   SET_ASSMMESH%NF     r#  8   a   SET_ASSMMESH%NG     ª#  8   a   SET_ASSMMESH%NS $   â#  8   a   SET_ASSMMESH%N_ZONE     $  8   a   SET_ASSMMESH%NY *   R$  8   a   SET_ASSMMESH%LAYER_BOTTOM '   $  8   a   SET_ASSMMESH%LAYER_TOP    Â$  T   a   ASSMMESH%PRINT    %  N      PRINT_ASSMMESH $   d%  J   a   PRINT_ASSMMESH%THIS    ®%  [       BOUNDARY    	&  @   a   BOUNDARY%INLET     I&  @   a   BOUNDARY%OUTLET    &  q       ITERATION &   ú&  |   a   ITERATION%TEMPERATURE #   v'  l   a   ITERATION%PRESSURE #   â'  l   a   ITERATION%VELOCITY    N(  o       TH_BOUNDARY    ½(  N   a   TH_BOUNDARY%P    )  N   a   TH_BOUNDARY%U    Y)  N   a   TH_BOUNDARY%T !   §)  V   a   TH_BOUNDARY%INIT !   ý)  j      INIT_TH_BOUNDARY &   g*  M   a   INIT_TH_BOUNDARY%THIS %   ´*  8   a   INIT_TH_BOUNDARY%TIN %   ì*  8   a   INIT_TH_BOUNDARY%UIN &   $+  8   a   INIT_TH_BOUNDARY%POUT #   \+  X   a   TH_BOUNDARY%UPDATE #   ´+  Y      UPDATE_TH_BOUNDARY (   ,  M   a   UPDATE_TH_BOUNDARY%THIS )   Z,  8   a   UPDATE_TH_BOUNDARY%CTIME    ,         HYDRAULIC    -  @   a   HYDRAULIC%FRIC     Y-  @   a   HYDRAULIC%AFLOW    -  @   a   HYDRAULIC%WET    Ù-  @   a   HYDRAULIC%DE    .  S   a   HYDRAULIC%SET    l.  X      SET_HYDRAULIC #   Ä.  K   a   SET_HYDRAULIC%THIS #   /  8   a   SET_HYDRAULIC%FRIC     G/  U   a   HYDRAULIC%PRINT     /  N      PRINT_HYDRAULIC %   ê/  K   a   PRINT_HYDRAULIC%THIS    50  S   a   HYDRAULIC%CAL    0  ^      CAL_HYDRAULIC #   æ0  K   a   CAL_HYDRAULIC%THIS !   11  8   a   CAL_HYDRAULIC%RC !   i1  8   a   CAL_HYDRAULIC%PD    ¡1  {       MATERIAL    2  |   a   MATERIAL%RHO    2  |   a   MATERIAL%SHC    3  |   a   MATERIAL%CTC    3  |   a   MATERIAL%DVS    4  l   a   MATERIAL%HTC    x4  S   a   MATERIAL%INIT    Ë4  n      INIT_MATERIAL #   95  J   a   INIT_MATERIAL%THIS !   5  8   a   INIT_MATERIAL%NF !   »5  8   a   INIT_MATERIAL%NG !   ó5  8   a   INIT_MATERIAL%NS !   +6  8   a   INIT_MATERIAL%NY    c6  {       THERMAL $   Þ6  |   a   THERMAL%TEMPERATURE !   Z7  l   a   THERMAL%PRESSURE !   Æ7  l   a   THERMAL%VELOCITY    28  R   a   THERMAL%INIT    8        INIT_THERMAL "   9  9      INIT_THERMAL%SIZE "   O9  I   a   INIT_THERMAL%THIS )   9  8   a   INIT_THERMAL%TEMPERATURE &   Ð9  8   a   INIT_THERMAL%PRESSURE &   :  8   a   INIT_THERMAL%VELOCITY    @:         ASSMINIT    Ì:  @   a   ASSMINIT%TI    ;  @   a   ASSMINIT%PI    L;  @   a   ASSMINIT%UI    ;  @   a   ASSMINIT%TIN    Ì;  @   a   ASSMINIT%POUT    <  @   a   ASSMINIT%UIN    L<  R   a   ASSMINIT%SET    <        SET_ASSMINIT "    =  J   a   SET_ASSMINIT%THIS     j=  8   a   SET_ASSMINIT%TI     ¢=  8   a   SET_ASSMINIT%PI     Ú=  8   a   SET_ASSMINIT%UI !   >  8   a   SET_ASSMINIT%TIN "   J>  8   a   SET_ASSMINIT%POUT !   >  8   a   SET_ASSMINIT%UIN    º>  T   a   ASSMINIT%PRINT    ?  N      PRINT_ASSMINIT $   \?  J   a   PRINT_ASSMINIT%THIS    ¦?  n       CONFACTOR     @  @   a   CONFACTOR%ALPHA     T@  @   a   CONFACTOR%SIGMA    @  S   a   CONFACTOR%SET    ç@  d      SET_CONFACTOR #   KA  K   a   SET_CONFACTOR%THIS $   A  8   a   SET_CONFACTOR%ALPHA $   ÎA  8   a   SET_CONFACTOR%SIGMA     B  U   a   CONFACTOR%PRINT     [B  N      PRINT_CONFACTOR %   ©B  K   a   PRINT_CONFACTOR%THIS    ôB  p       ASSMPOW    dC  |   a   ASSMPOW%POWER     àC  |   a   ASSMPOW%FQ_CORE    \D  O   a   ASSMPOW%SET    «D  f      SET_POWER    E  I   a   SET_POWER%THIS     ZE  x   a   SET_POWER%POWER "   ÒE  x   a   SET_POWER%FQ_CORE    JF  Q   a   ASSMPOW%PRINT    F  N      PRINT_POWER !   éF  I   a   PRINT_POWER%THIS    2G  {       SYS_TIME %   ­G  Q   a   SYS_TIME%TH_BOUNDARY !   þG  M   a   SYS_TIME%THERMAL "   KH  N   a   SYS_TIME%MATERIAL    H  M   a   SYS_TIME%POWER 