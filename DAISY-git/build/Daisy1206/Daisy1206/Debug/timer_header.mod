	  )  b   k820309    ?          14.0        ��'Z                                                                                                           
       E:\documents\doctors degree\software\tansistant\NT\DAISY_verson_20170328\DAISY-git\git\src\frame\timer_header.f90 TIMER_HEADER          EVENTTIMECOUNTER TOTALTIMECOUNTER CPUTIMECOUNTER DATEHOLDER                  @                              
                                                      
                 @  @                               '0            #CASENAME    #MAIN    #MEMORY    #TIMELIST    #DET    #REACTIVITY 	   #POINTKINETICS 
   #PT    #TH_WARNING    #TH_RBFD    #TH_HOT    #TH_AVERAGE             � $                                                      � $                                                     � $                                                     � $                                                     � $                                                     � $                              	                       � $                              
                       � $                                                     � $                                       	               � $                                   $   
               � $                                   (                  � $                                   ,                       @                               '�F          #IDX    #NAME    #NCOUNT    #IBEG    #IEND    #TIME    #IS_RUNNING    #SET    #BEGIN    #END "   #PRINT &          � $                                   d              p      p d       p d                    d     d                         ��������        .        � $                                  d   �         p      p d       p d                        d     d                    !                  C                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        � $                                   d   :        p      p d       p d                    d     d                                      0        � $                                  d   �;        p      p d       p d                    d     d                                     1        � $                                  d   �>        p      p d       p d                    d     d                                     1        � $                                  d   �A      
  p      p d       p d                    d     d             
                         0.0D0        � $                                   d    E        p      p d       p d                    d     d                                   ��	1     �   � $                      �                    #SET_EVENTTIMECOUNTER    #     @     @                                                #SET_EVENTTIMECOUNTER%SIZE    #THIS    #IDX    #NAME              @                                 SIZE       
D                                     �F     #EVENTTIMECOUNTER          
                                               
                                             1 1     �   � $                      �               	     #BEGIN_EVENTTIMECOUNTER    #     @     @                                                 #THIS     #IDX !                                                  
D                                      �F     #EVENTTIMECOUNTER          
                                  !       1     �   � $                      �      "         
     #END_EVENTTIMECOUNTER #   #     @     @                             #                    #THIS $   #IDX %                                                
D                                $     �F     #EVENTTIMECOUNTER          
                                  %       1     �   � $                      �      &              #PRINT_EVENTTIMECOUNTER '   #     @     @                             '                   #PRINT_EVENTTIMECOUNTER%TRIM (   #PRINT_EVENTTIMECOUNTER%SIZE )   #THIS *   #UNIT_ +             @                            (     TRIM           @                            )     SIZE       
                                 *     �F    #EVENTTIMECOUNTER          
                                  +                     @                          ,     '(      	      #IS_RUNNING -   #TOTAL .   #STEP /   #START_COUNT 0   #END_COUNT 1   #START 2   #UPDATE 5   #STOP ;   #RESET ?           � $                              -                                                               ����        � $                             .          
                              
                                  � $                             /          
                              
                                  � $                             0                                                                0        � $                             1                                                                 01     �   � $                      �      2              #START_TIMECOUNTER 3   #     @     @                             3                    #THIS 4                                             
D                                4     (       #TOTALTIMECOUNTER ,   1     �   � $                     �      5              #UPDATE_TIMECOUNTER 6   #     @     @                            6                   #UPDATE_TIMECOUNTER%PRESENT 7   #THIS 8   #UNIT_ 9   #SILENT :                                                  @                            7     PRESENT       
D                                8     (       #TOTALTIMECOUNTER ,         
 @                               9             
 @                               :       1     �   � $                      �      ;              #STOP_TIMECOUNTER <   #     @     @                             <                    #THIS =   #UNIT_ >         
D @                              =     (       #TOTALTIMECOUNTER ,         
 @                               >       1     �   � $                      �      ?         	     #RESET_TIMECOUNTER @   #     @     @                             @                    #THIS A         
D                                A     (       #TOTALTIMECOUNTER ,                 @                          B     '(      	      #IS_RUNNING C   #TOTAL D   #STEP E   #START_POINT F   #END_POINT G   #START H   #UPDATE K   #STOP Q   #RESET U           � $                              C                                                               ����        � $                             D          
                              
                                  � $                             E          
                              
                                  � $                             F          
                              
                                  � $                             G           
                              
                          1     �   � $                      �      H              #START_CPUTIMECOUNTER I   #     @     @                             I                    #THIS J                                            
D                                J     (       #CPUTIMECOUNTER B   1     �   � $                     �      K              #UPDATE_CPUTIMECOUNTER L   #     @     @                            L                   #UPDATE_CPUTIMECOUNTER%PRESENT M   #THIS N   #UNIT_ O   #SILENT P                                                 @                            M     PRESENT       
D                                N     (       #CPUTIMECOUNTER B         
 @                               O             
 @                               P       1     �   � $                      �      Q              #STOP_CPUTIMECOUNTER R   #     @     @                             R                    #THIS S   #UNIT_ T         
D @                              S     (       #CPUTIMECOUNTER B         
 @                               T       1     �   � $                      �      U         	     #RESET_CPUTIMECOUNTER V   #     @     @                             V                    #THIS W         
D                                W     (       #CPUTIMECOUNTER B                 @                           X     '@           #DATE Y   #TIME Z   #PRINT [            � $                             Y                        � $                             Z              1     �   � $                      �      [              #PRINT_DATEHOLDER \   #     @     @                             \                   #PRINT_DATEHOLDER%PRESENT ]   #THIS ^   #UNIT_ _                                                 @                            ]     PRESENT       
D                                ^     @      #DATEHOLDER X         
 @                               _       *         � n                 <              Cifmodintr.lib                     �   �      fn#fn "   '  H   b   uapp(TIMER_HEADER    o  <   J  CONSTANTS     �  <   J  ISO_FORTRAN_ENV "   �  �      FILE_TP+CONSTANTS +   �  @   a   FILE_TP%CASENAME+CONSTANTS '   
  @   a   FILE_TP%MAIN+CONSTANTS )   J  @   a   FILE_TP%MEMORY+CONSTANTS +   �  @   a   FILE_TP%TIMELIST+CONSTANTS &   �  @   a   FILE_TP%DET+CONSTANTS -   
  @   a   FILE_TP%REACTIVITY+CONSTANTS 0   J  @   a   FILE_TP%POINTKINETICS+CONSTANTS %   �  @   a   FILE_TP%PT+CONSTANTS -   �  @   a   FILE_TP%TH_WARNING+CONSTANTS *   
  @   a   FILE_TP%TH_RBFD+CONSTANTS )   J  @   a   FILE_TP%TH_HOT+CONSTANTS -   �  @   a   FILE_TP%TH_AVERAGE+CONSTANTS !   �  �       EVENTTIMECOUNTER %   �  �   a   EVENTTIMECOUNTER%IDX &   7  �  a   EVENTTIMECOUNTER%NAME (      �   a   EVENTTIMECOUNTER%NCOUNT &   �  �   a   EVENTTIMECOUNTER%IBEG &   �  �   a   EVENTTIMECOUNTER%IEND &   ?  �   a   EVENTTIMECOUNTER%TIME ,   �  �   a   EVENTTIMECOUNTER%IS_RUNNING %   �  Z   a   EVENTTIMECOUNTER%SET %     �      SET_EVENTTIMECOUNTER *   �  9      SET_EVENTTIMECOUNTER%SIZE *   �  R   a   SET_EVENTTIMECOUNTER%THIS )     8   a   SET_EVENTTIMECOUNTER%IDX *   I  @   a   SET_EVENTTIMECOUNTER%NAME '   �  \   a   EVENTTIMECOUNTER%BEGIN '   �  �      BEGIN_EVENTTIMECOUNTER ,   e  R   a   BEGIN_EVENTTIMECOUNTER%THIS +   �  8   a   BEGIN_EVENTTIMECOUNTER%IDX %   �  Z   a   EVENTTIMECOUNTER%END %   I  ~      END_EVENTTIMECOUNTER *   �  R   a   END_EVENTTIMECOUNTER%THIS )     8   a   END_EVENTTIMECOUNTER%IDX '   Q  \   a   EVENTTIMECOUNTER%PRINT '   �  �      PRINT_EVENTTIMECOUNTER ,   H  9      PRINT_EVENTTIMECOUNTER%TRIM ,   �  9      PRINT_EVENTTIMECOUNTER%SIZE ,   �  R   a   PRINT_EVENTTIMECOUNTER%THIS -     8   a   PRINT_EVENTTIMECOUNTER%UNIT_ !   D  �       TOTALTIMECOUNTER ,   �  |   a   TOTALTIMECOUNTER%IS_RUNNING '   u  |   a   TOTALTIMECOUNTER%TOTAL &   �  |   a   TOTALTIMECOUNTER%STEP -   m  }   a   TOTALTIMECOUNTER%START_COUNT +   �  }   a   TOTALTIMECOUNTER%END_COUNT '   g  W   a   TOTALTIMECOUNTER%START "   �  r      START_TIMECOUNTER '   0  R   a   START_TIMECOUNTER%THIS (   �  X   a   TOTALTIMECOUNTER%UPDATE #   �  �      UPDATE_TIMECOUNTER +   �  <      UPDATE_TIMECOUNTER%PRESENT (   �  R   a   UPDATE_TIMECOUNTER%THIS )     8   a   UPDATE_TIMECOUNTER%UNIT_ *   J  8   a   UPDATE_TIMECOUNTER%SILENT &   �  V   a   TOTALTIMECOUNTER%STOP !   �  Y      STOP_TIMECOUNTER &   1  R   a   STOP_TIMECOUNTER%THIS '   �  8   a   STOP_TIMECOUNTER%UNIT_ '   �  W   a   TOTALTIMECOUNTER%RESET "     N      RESET_TIMECOUNTER '   `  R   a   RESET_TIMECOUNTER%THIS    �  �       CPUTIMECOUNTER *   g  |   a   CPUTIMECOUNTER%IS_RUNNING %   �  |   a   CPUTIMECOUNTER%TOTAL $   _  |   a   CPUTIMECOUNTER%STEP +   �  |   a   CPUTIMECOUNTER%START_POINT )   W   |   a   CPUTIMECOUNTER%END_POINT %   �   Z   a   CPUTIMECOUNTER%START %   -!  q      START_CPUTIMECOUNTER *   �!  P   a   START_CPUTIMECOUNTER%THIS &   �!  [   a   CPUTIMECOUNTER%UPDATE &   I"  �      UPDATE_CPUTIMECOUNTER .   �"  <      UPDATE_CPUTIMECOUNTER%PRESENT +   1#  P   a   UPDATE_CPUTIMECOUNTER%THIS ,   �#  8   a   UPDATE_CPUTIMECOUNTER%UNIT_ -   �#  8   a   UPDATE_CPUTIMECOUNTER%SILENT $   �#  Y   a   CPUTIMECOUNTER%STOP $   J$  Y      STOP_CPUTIMECOUNTER )   �$  P   a   STOP_CPUTIMECOUNTER%THIS *   �$  8   a   STOP_CPUTIMECOUNTER%UNIT_ %   +%  Z   a   CPUTIMECOUNTER%RESET %   �%  N      RESET_CPUTIMECOUNTER *   �%  P   a   RESET_CPUTIMECOUNTER%THIS    #&  c       DATEHOLDER     �&  @   a   DATEHOLDER%DATE     �&  @   a   DATEHOLDER%TIME !   '  V   a   DATEHOLDER%PRINT !   \'  �      PRINT_DATEHOLDER )   �'  <      PRINT_DATEHOLDER%PRESENT &   3(  L   a   PRINT_DATEHOLDER%THIS '   (  8   a   PRINT_DATEHOLDER%UNIT_    �(  Z      MsObjComment 