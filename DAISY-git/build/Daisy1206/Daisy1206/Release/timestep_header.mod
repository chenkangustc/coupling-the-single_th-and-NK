	  *  w   k820309    ?          14.0        ŖØ*Z                                                                                                           
       E:\documents\doctors degree\software\tansistant\NT\DAISY_verson_20170328\DAISY-git\git\src\header\timestep_header.f90 TIMESTEP_HEADER          TIMESTEPER EQUALSTEPER TIMESTEPINFO                  @                              
                                                      
                        @                              
       WARNINGCOLLECTOR               @                               'H           #INFO_LIST    #MESSAGE    #COUNTING    #CNT_LIMIT    #SET 	   #PRINT              $                                                      $                                                    $                                   @                                                         0         $                                   D                                         PĆ              500001     Ą    $                           	              #SET_WARNINGCOLLECTOR 
   #     @     @                           
                   #SET_WARNINGCOLLECTOR%TRIM    #SET_WARNINGCOLLECTOR%ADJUSTL    #THIS    #INFO_LIST    #MESSAGE              @                                 TRIM           @                                 ADJUSTL       
                                     H      #WARNINGCOLLECTOR          
                                             1       
                                             1 1     Ą    $                                         #PRINT_WARNINGCOLLECTOR    #     @     @                                              #PRINT_WARNINGCOLLECTOR%TRIM    #THIS    #UNIT_              @                                 TRIM       
                                      H     #WARNINGCOLLECTOR          
                                                   @  @                               '0            #CASENAME    #MAIN    #MEMORY    #TIMELIST    #DET    #REACTIVITY    #POINTKINETICS    #PT    #TH_WARNING    #TH_RBFD    #TH_HOT     #TH_AVERAGE !             $                                                       $                                                      $                                                      $                                                      $                                                      $                                                      $                                                      $                                                      $                                       	                $                                   $   
                $                                    (                   $                              !     ,                       @               A           "     '            #START_TIME #   #END_TIME $   #STEP_PER_SECTION %   #N_SECTION &   #N_INTERVAL '   #N_STEP (   #SECTIONS )   #I_SECTION -   #I_STEP .   #INTERVALS /   #ALLOC 4   #CLEAN 7   #SET_SECTION ;   #SET_INTERVAL ?   #GET_START D   #GET_END G   #GET_SECTION_COUNT J   #GET_STEP_COUNT N   #INFO Q   #CHECK X            $                             #           
                              
                                   $                             $          
                              
                                  $                              %                         &                                $                              &     4                                                           0         $                              '     8                                                           0         $                              (     <                                                           0       $                              )        @            #SECTION_TP *         &                                  @  @                          *     '            #POINT +   #LENGTH ,             $                             +            
             $                             ,           
            D                              -     d                                                           0         D                              .     h   	                                                        0        $                              /        l      
      #INTERVAL_TP 0         &                                  @  @                          0     '            #TEND 1   #DMACRO 2   #DMIDDLE 3             $                             1            
             $                             2           
             $                             3           
   1     Ą    $                            4              #ALLOCATE_TIMESTEPER 5   #     @     @                             5                    #THIS 6         
D @                              6            #TIMESTEPER "   1     Ą    $                           7              #FREE_TIMESTEPER 8   #     @     @                            8                   #FREE_TIMESTEPER%ALLOCATED 9   #THIS :             @                            9     ALLOCATED       
D                                :            #TIMESTEPER "   1     Ą    $                           ;              #SET_TIMESTEPER_BY_SECTION <   #     @     @                            <                   #SET_TIMESTEPER_BY_SECTION%FLOOR =   #THIS >             @                            =     FLOOR       
D                                >            #TIMESTEPER "   1     Ą    $                            ?              #SET_TIMESTEPER_BY_INTERVAL @   #     @     @                             @                   #SET_TIMESTEPER_BY_INTERVAL%NINT A   #SET_TIMESTEPER_BY_INTERVAL%ALLOCATED B   #THIS C             @                            A     NINT           @                            B     ALLOCATED       
D @                              C            #TIMESTEPER "   1     Ą    $                           D              #GET_START_TIME E   %     @    @                            E                    
   #THIS F         
                                 F           #TIMESTEPER "   1     Ą    $                           G              #GET_END_TIME H   %     @    @                            H                    
   #THIS I         
                                 I           #TIMESTEPER "   1     Ą    $                           J              #GET_SECTION_NUMBER K   %     @    @                            K                      #GET_SECTION_NUMBER%SIZE L   #THIS M             @                            L     SIZE       
                                 M           #TIMESTEPER "   1     Ą    $                           N              #GET_STEP_NUMBER O   %     @    @                            O                       #THIS P         
                                 P           #TIMESTEPER "   1     Ą    $                           Q          	    #GET_TIMESTEPER_INFO R   &     @    @                            R                       #GET_TIMESTEPER_INFO%PRESENT S   #THIS T   #TID U   #IS_STEP V   #TIMESTEPINFO W             @                            S     PRESENT       
D                                T            #TIMESTEPER "         
  @                               U             
 @                               V       1     Ą    $                           X          
    #CHECK_STEP_INDEX Y   #     @     @                            Y                   #CHECK_STEP_INDEX%PRESENT Z   #THIS [   #TID \   #IS_CURRENT ]   #IS_STEP ^             @                            Z     PRESENT       
                                 [           #TIMESTEPER "         
                                  \             
                                  ]             
 @                               ^                     @                          _     '0            #NSTEP `   #STEPSIZE a   #TOTAL b   #LEFT c   #RIGHT d   #IS_INIT e   #ADD f            $                              `                                                               1         $                             a          
                              
                                   $                             b          
                              
                                   $                             c          
                              
                                   $                             d           
                              
                                   $                              e     (                                                         ų’’’1     Ą    $                            f              #ADD_EQUALSTEPER_STEP g   #     @     @                             g                   #ADD_EQUALSTEPER_STEP%ABS h   #THIS i   #IN_STEP j   #OUT_STEP k   #IS_ADVANCE l             @                            h     ABS       
D                                i     0       #EQUALSTEPER _         
                                  j            #TIMESTEPINFO W         D                                 k             #TIMESTEPINFO W         D                                 l                      @                           W     '             #INDEX m   #LEFT n   #RIGHT o   #PACE p   #PRINT q             $                              m                         $                             n           
             $                             o           
             $                             p           
   1     Ą    $                            q              #PRINT_TIMESTEPINFO r   #     @     @                             r                    #THIS s   #UNIT_ t         
                                 s            #TIMESTEPINFO W         
                                  t       *          n                 J              Cifmodintr.lib                               fn#fn %   .  0   b   uapp(TIMESTEP_HEADER    ^  <   J  CONSTANTS       <   J  ISO_FORTRAN_ENV !   Ö  M   J  EXCEPTION_HEADER 2   #         WARNINGCOLLECTOR+EXCEPTION_HEADER <   “  @   a   WARNINGCOLLECTOR%INFO_LIST+EXCEPTION_HEADER :   ō  @   a   WARNINGCOLLECTOR%MESSAGE+EXCEPTION_HEADER ;   4  }   a   WARNINGCOLLECTOR%COUNTING+EXCEPTION_HEADER <   ±     a   WARNINGCOLLECTOR%CNT_LIMIT+EXCEPTION_HEADER 6   2  Z   a   WARNINGCOLLECTOR%SET+EXCEPTION_HEADER 6     «      SET_WARNINGCOLLECTOR+EXCEPTION_HEADER @   7  9      SET_WARNINGCOLLECTOR%TRIM+EXCEPTION_HEADER=TRIM F   p  <      SET_WARNINGCOLLECTOR%ADJUSTL+EXCEPTION_HEADER=ADJUSTL ;   ¬  R   a   SET_WARNINGCOLLECTOR%THIS+EXCEPTION_HEADER @   ž  @   a   SET_WARNINGCOLLECTOR%INFO_LIST+EXCEPTION_HEADER >   >  @   a   SET_WARNINGCOLLECTOR%MESSAGE+EXCEPTION_HEADER 8   ~  \   a   WARNINGCOLLECTOR%PRINT+EXCEPTION_HEADER 8   Ś  z      PRINT_WARNINGCOLLECTOR+EXCEPTION_HEADER B   T  9      PRINT_WARNINGCOLLECTOR%TRIM+EXCEPTION_HEADER=TRIM =     R   a   PRINT_WARNINGCOLLECTOR%THIS+EXCEPTION_HEADER >   ß  8   a   PRINT_WARNINGCOLLECTOR%UNIT_+EXCEPTION_HEADER "     ć      FILE_TP+CONSTANTS +   ś  @   a   FILE_TP%CASENAME+CONSTANTS '   :	  @   a   FILE_TP%MAIN+CONSTANTS )   z	  @   a   FILE_TP%MEMORY+CONSTANTS +   ŗ	  @   a   FILE_TP%TIMELIST+CONSTANTS &   ś	  @   a   FILE_TP%DET+CONSTANTS -   :
  @   a   FILE_TP%REACTIVITY+CONSTANTS 0   z
  @   a   FILE_TP%POINTKINETICS+CONSTANTS %   ŗ
  @   a   FILE_TP%PT+CONSTANTS -   ś
  @   a   FILE_TP%TH_WARNING+CONSTANTS *   :  @   a   FILE_TP%TH_RBFD+CONSTANTS )   z  @   a   FILE_TP%TH_HOT+CONSTANTS -   ŗ  @   a   FILE_TP%TH_AVERAGE+CONSTANTS    ś  p      TIMESTEPER &   j  |   a   TIMESTEPER%START_TIME $   ę  |   a   TIMESTEPER%END_TIME ,   b  l   a   TIMESTEPER%STEP_PER_SECTION %   Ī  }   a   TIMESTEPER%N_SECTION &   K  }   a   TIMESTEPER%N_INTERVAL "   Č  }   a   TIMESTEPER%N_STEP $   E  |   a   TIMESTEPER%SECTIONS    Į  [      SECTION_TP !     @   a   SECTION_TP%POINT "   \  @   a   SECTION_TP%LENGTH %     }   !   TIMESTEPER%I_SECTION "     }   !   TIMESTEPER%I_STEP %     }   a   TIMESTEPER%INTERVALS      g      INTERVAL_TP !   z  @   a   INTERVAL_TP%TEND #   ŗ  @   a   INTERVAL_TP%DMACRO $   ś  @   a   INTERVAL_TP%DMIDDLE !   :  Y   a   TIMESTEPER%ALLOC $     N      ALLOCATE_TIMESTEPER )   į  L   a   ALLOCATE_TIMESTEPER%THIS !   -  U   a   TIMESTEPER%CLEAN       m      FREE_TIMESTEPER *   ļ  >      FREE_TIMESTEPER%ALLOCATED %   -  L   a   FREE_TIMESTEPER%THIS '   y  _   a   TIMESTEPER%SET_SECTION *   Ų  s      SET_TIMESTEPER_BY_SECTION 0   K  :      SET_TIMESTEPER_BY_SECTION%FLOOR /     L   a   SET_TIMESTEPER_BY_SECTION%THIS (   Ń  `   a   TIMESTEPER%SET_INTERVAL +   1        SET_TIMESTEPER_BY_INTERVAL 0   Ī  9      SET_TIMESTEPER_BY_INTERVAL%NINT 5     >      SET_TIMESTEPER_BY_INTERVAL%ALLOCATED 0   E  L   a   SET_TIMESTEPER_BY_INTERVAL%THIS %     T   a   TIMESTEPER%GET_START    å  R      GET_START_TIME $   7  L   a   GET_START_TIME%THIS #     R   a   TIMESTEPER%GET_END    Õ  R      GET_END_TIME "   '  L   a   GET_END_TIME%THIS -   s  X   a   TIMESTEPER%GET_SECTION_COUNT #   Ė  o      GET_SECTION_NUMBER (   :  9      GET_SECTION_NUMBER%SIZE (   s  L   a   GET_SECTION_NUMBER%THIS *   æ  U   a   TIMESTEPER%GET_STEP_COUNT       R      GET_STEP_NUMBER %   f  L   a   GET_STEP_NUMBER%THIS     ²  Y   a   TIMESTEPER%INFO $           GET_TIMESTEPER_INFO ,   ¦  <      GET_TIMESTEPER_INFO%PRESENT )   ā  L   a   GET_TIMESTEPER_INFO%THIS (   .  8   a   GET_TIMESTEPER_INFO%TID ,   f  8   a   GET_TIMESTEPER_INFO%IS_STEP !     V   a   TIMESTEPER%CHECK !   ō        CHECK_STEP_INDEX )      <      CHECK_STEP_INDEX%PRESENT &   Ā   L   a   CHECK_STEP_INDEX%THIS %   !  8   a   CHECK_STEP_INDEX%TID ,   F!  8   a   CHECK_STEP_INDEX%IS_CURRENT )   ~!  8   a   CHECK_STEP_INDEX%IS_STEP    ¶!         EQUALSTEPER "   I"  }   a   EQUALSTEPER%NSTEP %   Ę"  |   a   EQUALSTEPER%STEPSIZE "   B#  |   a   EQUALSTEPER%TOTAL !   ¾#  |   a   EQUALSTEPER%LEFT "   :$  |   a   EQUALSTEPER%RIGHT $   ¶$  |   a   EQUALSTEPER%IS_INIT     2%  Z   a   EQUALSTEPER%ADD %   %        ADD_EQUALSTEPER_STEP )   #&  8      ADD_EQUALSTEPER_STEP%ABS *   [&  M   a   ADD_EQUALSTEPER_STEP%THIS -   Ø&  N   a   ADD_EQUALSTEPER_STEP%IN_STEP .   ö&  N   a   ADD_EQUALSTEPER_STEP%OUT_STEP 0   D'  8   a   ADD_EQUALSTEPER_STEP%IS_ADVANCE    |'  y       TIMESTEPINFO #   õ'  @   a   TIMESTEPINFO%INDEX "   5(  @   a   TIMESTEPINFO%LEFT #   u(  @   a   TIMESTEPINFO%RIGHT "   µ(  @   a   TIMESTEPINFO%PACE #   õ(  X   a   TIMESTEPINFO%PRINT #   M)  Y      PRINT_TIMESTEPINFO (   ¦)  N   a   PRINT_TIMESTEPINFO%THIS )   ō)  8   a   PRINT_TIMESTEPINFO%UNIT_    ,*  Z      MsObjComment 