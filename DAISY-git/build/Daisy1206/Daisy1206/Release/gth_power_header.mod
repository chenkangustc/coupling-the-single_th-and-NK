	  �5  �   k820309    ?          14.0        (Z                                                                                                           
       E:\documents\doctors degree\software\tansistant\NT\DAISY_verson_20170328\DAISY-git\git\src\thermal\gth_power_header.f90 GTH_POWER_HEADER          LINEARPOWER                  @                              
                                                      
                        @                              
       THERMALSCALE THERMALGEOMETRY THERMALASSEMBLYGEOMETRY               @                                '       
      #NF    #NC    #N_MESH    #NR    #NA 	   #NA_START 
   #NA_END    #N_ASSM_GEOM    #MESH    #SET             � $                                                      � $                                                     � $                                                     � $                                                     � $                              	                       � $                              
                       � $                                                     � $                                            1     �   � $                      �               	     #SET_CONDUCTION_MESH    #     @     @                                                #THIS    #NF    #NC          
                                             #THERMALSCALE          
                                               
                                         1     �   � $                      �               
     #SET_THERMALSCALE    #     @     @                                                #THIS    #ZONE    #LAYER    #TOP_LAYER    #BOTTOM_LAYER          
                                             #THERMALSCALE          
                                               
                                               
                                               
                                                       @               A                '�      
      #HEIGHT    #GEOM_TYPE    #COOLANT_TYPE    #CLADDING_TYPE    #GAP_TYPE    #FUEL_TYPE    #FUEL_TRU     #ALLOC !   #CLEAN %   #SET_HEIGHT )          � $                                               
        &                              � $                                      $                 &                               � $                                   H                                                         1       � $                                      L                 &                              � $                                      p                 &                              � $                                      �                 &                              � $                                      �         
        &                       1     �   � $                      �      !              #ALLOC_THERMALGEOMETRY "   #     @     @                            "                    #THIS #   #NTH $         
                                #     �       #THERMALGEOMETRY          
                                  $            #THERMALSCALE    1     �   � $                      �      %         	     #FREE_THERMALGEOMETRY &   #     @     @                            &                   #FREE_THERMALGEOMETRY%ALLOCATED '   #THIS (             @                            '     ALLOCATED       
                                (     �       #THERMALGEOMETRY    1     �   � $                      �      )         
     #SET_THERMALGEOMETRY_HEIGHT *   #     @     @                            *                    #THIS +   #HEIGHT ,         
                                +     �       #THERMALGEOMETRY          
                                 ,           
 	         &                                     @               A           -     '�            #N_PIN .   #N_FUELPIN /   #PITCH 0   #ROD 1   #CLADTH 2   #BOND 3   #PELLET 4   #HOLE 5   #AREA 6   #FLOWAREA 7   #IS_HEXAGONAL 8   #P2D 9   #DH :   #X_POINT ;   #X_SURFACE <   #DF =   #DG >   #DC ?   #ALLOC @   #CLEAN D   #SET H            � $                              .                        � $                              /                       � $                             0           
            � $                             1           
            � $                             2           
            � $                             3            
            � $                             4     (      
            � $                             5     0      
            � $                             6     8   	   
            � $                             7     @   
   
           � $                              8     H                                             ��������    ����         � $                             9     P      
            � $                             :     X      
          � $                             ;        `         
        &                              � $                             <        �         
        &                                � $                             =     �      
            � $                             >     �      
            � $                             ?     �      
   1     �   � $                      �      @              #ALLOC_TYPEGEOMETRYASSEMBLY A   #     @     @                            A                    #THIS B   #NTH C         
                                B     �       #THERMALASSEMBLYGEOMETRY -         
                                  C            #THERMALSCALE    1     �   � $                      �      D              #FREE_TYPEGEOMETRYASSEMBLY E   #     @     @                            E                   #FREE_TYPEGEOMETRYASSEMBLY%ALLOCATED F   #THIS G             @                            F     ALLOCATED       
                                G     �       #THERMALASSEMBLYGEOMETRY -   1     �   � $                      �      H              #SET_TYPEGEOMETRYASSEMBLY I   #     @     @                            I                    #THIS J   #NTH K         
                                J     �       #THERMALASSEMBLYGEOMETRY -         
                                  K            #THERMALSCALE              @  @                          L     '0            #CASENAME M   #MAIN N   #MEMORY O   #TIMELIST P   #DET Q   #REACTIVITY R   #POINTKINETICS S   #PT T   #TH_WARNING U   #TH_RBFD V   #TH_HOT W   #TH_AVERAGE X            � $                              M                        � $                              N                       � $                              O                       � $                              P                       � $                              Q                       � $                              R                       � $                              S                       � $                              T                       � $                              U         	               � $                              V     $   
               � $                              W     (                  � $                              X     ,                       @               @           Y     '            #POWER Z   #FQ_LATTICE [   #FQ_CORE \   #NORMAL_OLD ]   #NORMAL_CURRENT ^   #AVG_LINEAR _   #MAX_LINEAR `   #GAMMA a   #ALLOC b   #CLEAN f   #PRINT j   #SET_FQ_LATTICE q   #FQ_FA u   #FQ_PIN |   #FQ_POINT �   #SET_POWER �   #SET_ASSEMBLY_POWER_3D �   #SET_ASSEMBLY_POWER_0D �         � $                             Z                  
        &           &                              � $                             [        0         
        &                              � $                             \        T         
        &           &                                � $                             ]     �      
            � $                             ^     �      
         � $                             _        �         
        &           &                              � $                             `        �         
        &           &                                � $                             a     �      
   1     �   � $                      �      b         	     #ALLOC_LINEARPOWER c   #     @     @                             c                    #THIS d   #NTH e         
D @                              d            #LINEARPOWER Y         
                                  e            #THERMALSCALE    1     �   � $                     �      f         
     #FREE_LINEARPOWER g   #     @     @                            g                   #FREE_LINEARPOWER%ALLOCATED h   #THIS i             @                            h     ALLOCATED       
D                                i            #LINEARPOWER Y   1     �   � $                      �      j              #PRINT_LINEARPOWER k   #     @     @                             k                   #PRINT_LINEARPOWER%PRESENT l   #PRINT_LINEARPOWER%SIZE m   #THIS n   #TO_AVG o   #TO_MAX p             @                            l     PRESENT           @                            m     SIZE       
                                n            #LINEARPOWER Y         
                                  o             
 @                               p       1     �   � $                      �      q              #SET_FQ_LATTICE_INFO r   #     @     @                             r                    #THIS s   #FQ t         
D                                s            #LINEARPOWER Y         
                                 t           
          &                       1     �   � $                      �      u              #GET_ASSEMBLY_FQ v   #     @     @                             v                   #GET_ASSEMBLY_FQ%SUM w   #GET_ASSEMBLY_FQ%SIZE x   #THIS y   #FQ z   #CHANNEL {             @                            w     SUM           @                            x     SIZE       
                                 y           #LINEARPOWER Y         
D                                z     
         
D                                 {        1     �   � $                      �      |              #GET_FUELPIN_FQ }   #     @     @                             }                   #GET_FUELPIN_FQ%SUM ~   #GET_FUELPIN_FQ%SIZE    #THIS �   #FQ �   #CHANNEL �             @                            ~     SUM           @                                 SIZE       
                                 �           #LINEARPOWER Y         
D                                �     
         
D                                 �        1     �   � $                      �      �              #GET_POINT_FQ �   #     @     @                             �                   #GET_POINT_FQ%SIZE �   #THIS �   #FQ �   #CHANNEL �   #AXIAL �             @                            �     SIZE       
                                 �           #LINEARPOWER Y         
D                                �     
         
D                                 �              
D                                 �        4     �   � $                         @    �            3     �   � $                         @             u #LINEARPOWER Y   #SET_ASSEMBLY_POWER_3D �   #SET_ASSEMBLY_POWER_0D �   1     �   � $                      �      �              #SET_ASSEMBLY_POWER_3D �   #     @     @                             �                   #SET_ASSEMBLY_POWER_3D%SIZE �   #THIS �   #NTH �   #GEOM_TH �   #GEOM_ASSM �   #POWER �   #FQ_CORE �             @                            �     SIZE       
D                                �            #LINEARPOWER Y         
                                  �            #THERMALSCALE          
                                  �     �      #THERMALGEOMETRY          
                                  �        �              &                       #THERMALASSEMBLYGEOMETRY -         
                                 �           
          &           &                             
                                 �           
 	         &           &                       1     �   � $                      �      �          	    #SET_ASSEMBLY_POWER_0D �   #     @     @                             �                    #THIS �   #POWER �   #IS_INITIAL �         
D                                �            #LINEARPOWER Y         
                                 �     
        
                                  �       *         � n                 1              Cifmodintr.lib                     �   �      fn#fn &   1     b   uapp(GTH_POWER_HEADER    I  <   J  CONSTANTS     �  <   J  ISO_FORTRAN_ENV $   �  q   J  GTH_GEOMETRY_HEADER 1   2  �       THERMALSCALE+GTH_GEOMETRY_HEADER 4   �  @   a   THERMALSCALE%NF+GTH_GEOMETRY_HEADER 4      @   a   THERMALSCALE%NC+GTH_GEOMETRY_HEADER 8   `  @   a   THERMALSCALE%N_MESH+GTH_GEOMETRY_HEADER 4   �  @   a   THERMALSCALE%NR+GTH_GEOMETRY_HEADER 4   �  @   a   THERMALSCALE%NA+GTH_GEOMETRY_HEADER :      @   a   THERMALSCALE%NA_START+GTH_GEOMETRY_HEADER 8   `  @   a   THERMALSCALE%NA_END+GTH_GEOMETRY_HEADER =   �  @   a   THERMALSCALE%N_ASSM_GEOM+GTH_GEOMETRY_HEADER 6   �  Y   a   THERMALSCALE%MESH+GTH_GEOMETRY_HEADER 8   9  ^      SET_CONDUCTION_MESH+GTH_GEOMETRY_HEADER =   �  N   a   SET_CONDUCTION_MESH%THIS+GTH_GEOMETRY_HEADER ;   �  8   a   SET_CONDUCTION_MESH%NF+GTH_GEOMETRY_HEADER ;     8   a   SET_CONDUCTION_MESH%NC+GTH_GEOMETRY_HEADER 5   U  V   a   THERMALSCALE%SET+GTH_GEOMETRY_HEADER 5   �  �      SET_THERMALSCALE+GTH_GEOMETRY_HEADER :   /  N   a   SET_THERMALSCALE%THIS+GTH_GEOMETRY_HEADER :   }  8   a   SET_THERMALSCALE%ZONE+GTH_GEOMETRY_HEADER ;   �  8   a   SET_THERMALSCALE%LAYER+GTH_GEOMETRY_HEADER ?   �  8   a   SET_THERMALSCALE%TOP_LAYER+GTH_GEOMETRY_HEADER B   %  8   a   SET_THERMALSCALE%BOTTOM_LAYER+GTH_GEOMETRY_HEADER 4   ]  �       THERMALGEOMETRY+GTH_GEOMETRY_HEADER ;   2	  l   a   THERMALGEOMETRY%HEIGHT+GTH_GEOMETRY_HEADER >   �	  l   a   THERMALGEOMETRY%GEOM_TYPE+GTH_GEOMETRY_HEADER A   

  }   a   THERMALGEOMETRY%COOLANT_TYPE+GTH_GEOMETRY_HEADER B   �
  l   a   THERMALGEOMETRY%CLADDING_TYPE+GTH_GEOMETRY_HEADER =   �
  l   a   THERMALGEOMETRY%GAP_TYPE+GTH_GEOMETRY_HEADER >   _  l   a   THERMALGEOMETRY%FUEL_TYPE+GTH_GEOMETRY_HEADER =   �  l   a   THERMALGEOMETRY%FUEL_TRU+GTH_GEOMETRY_HEADER :   7  [   a   THERMALGEOMETRY%ALLOC+GTH_GEOMETRY_HEADER :   �  W      ALLOC_THERMALGEOMETRY+GTH_GEOMETRY_HEADER ?   �  Q   a   ALLOC_THERMALGEOMETRY%THIS+GTH_GEOMETRY_HEADER >   :  N   a   ALLOC_THERMALGEOMETRY%NTH+GTH_GEOMETRY_HEADER :   �  Z   a   THERMALGEOMETRY%CLEAN+GTH_GEOMETRY_HEADER 9   �  r      FREE_THERMALGEOMETRY+GTH_GEOMETRY_HEADER M   T  >      FREE_THERMALGEOMETRY%ALLOCATED+GTH_GEOMETRY_HEADER=ALLOCATED >   �  Q   a   FREE_THERMALGEOMETRY%THIS+GTH_GEOMETRY_HEADER ?   �  `   a   THERMALGEOMETRY%SET_HEIGHT+GTH_GEOMETRY_HEADER ?   C  Z      SET_THERMALGEOMETRY_HEIGHT+GTH_GEOMETRY_HEADER D   �  Q   a   SET_THERMALGEOMETRY_HEIGHT%THIS+GTH_GEOMETRY_HEADER F   �  h   a   SET_THERMALGEOMETRY_HEIGHT%HEIGHT+GTH_GEOMETRY_HEADER <   V  ,      THERMALASSEMBLYGEOMETRY+GTH_GEOMETRY_HEADER B   �  @   a   THERMALASSEMBLYGEOMETRY%N_PIN+GTH_GEOMETRY_HEADER F   �  @   a   THERMALASSEMBLYGEOMETRY%N_FUELPIN+GTH_GEOMETRY_HEADER B     @   a   THERMALASSEMBLYGEOMETRY%PITCH+GTH_GEOMETRY_HEADER @   B  @   a   THERMALASSEMBLYGEOMETRY%ROD+GTH_GEOMETRY_HEADER C   �  @   a   THERMALASSEMBLYGEOMETRY%CLADTH+GTH_GEOMETRY_HEADER A   �  @   a   THERMALASSEMBLYGEOMETRY%BOND+GTH_GEOMETRY_HEADER C     @   a   THERMALASSEMBLYGEOMETRY%PELLET+GTH_GEOMETRY_HEADER A   B  @   a   THERMALASSEMBLYGEOMETRY%HOLE+GTH_GEOMETRY_HEADER A   �  @   a   THERMALASSEMBLYGEOMETRY%AREA+GTH_GEOMETRY_HEADER E   �  @   a   THERMALASSEMBLYGEOMETRY%FLOWAREA+GTH_GEOMETRY_HEADER I     |   a   THERMALASSEMBLYGEOMETRY%IS_HEXAGONAL+GTH_GEOMETRY_HEADER @   ~  @   a   THERMALASSEMBLYGEOMETRY%P2D+GTH_GEOMETRY_HEADER ?   �  @   a   THERMALASSEMBLYGEOMETRY%DH+GTH_GEOMETRY_HEADER D   �  l   a   THERMALASSEMBLYGEOMETRY%X_POINT+GTH_GEOMETRY_HEADER F   j  l   a   THERMALASSEMBLYGEOMETRY%X_SURFACE+GTH_GEOMETRY_HEADER ?   �  @   a   THERMALASSEMBLYGEOMETRY%DF+GTH_GEOMETRY_HEADER ?     @   a   THERMALASSEMBLYGEOMETRY%DG+GTH_GEOMETRY_HEADER ?   V  @   a   THERMALASSEMBLYGEOMETRY%DC+GTH_GEOMETRY_HEADER B   �  `   a   THERMALASSEMBLYGEOMETRY%ALLOC+GTH_GEOMETRY_HEADER ?   �  W      ALLOC_TYPEGEOMETRYASSEMBLY+GTH_GEOMETRY_HEADER D   M  Y   a   ALLOC_TYPEGEOMETRYASSEMBLY%THIS+GTH_GEOMETRY_HEADER C   �  N   a   ALLOC_TYPEGEOMETRYASSEMBLY%NTH+GTH_GEOMETRY_HEADER B   �  _   a   THERMALASSEMBLYGEOMETRY%CLEAN+GTH_GEOMETRY_HEADER >   S  w      FREE_TYPEGEOMETRYASSEMBLY+GTH_GEOMETRY_HEADER R   �  >      FREE_TYPEGEOMETRYASSEMBLY%ALLOCATED+GTH_GEOMETRY_HEADER=ALLOCATED C     Y   a   FREE_TYPEGEOMETRYASSEMBLY%THIS+GTH_GEOMETRY_HEADER @   a  ^   a   THERMALASSEMBLYGEOMETRY%SET+GTH_GEOMETRY_HEADER =   �  W      SET_TYPEGEOMETRYASSEMBLY+GTH_GEOMETRY_HEADER B     Y   a   SET_TYPEGEOMETRYASSEMBLY%THIS+GTH_GEOMETRY_HEADER A   o  N   a   SET_TYPEGEOMETRYASSEMBLY%NTH+GTH_GEOMETRY_HEADER "   �  �      FILE_TP+CONSTANTS +   �  @   a   FILE_TP%CASENAME+CONSTANTS '   �  @   a   FILE_TP%MAIN+CONSTANTS )      @   a   FILE_TP%MEMORY+CONSTANTS +   `  @   a   FILE_TP%TIMELIST+CONSTANTS &   �  @   a   FILE_TP%DET+CONSTANTS -   �  @   a   FILE_TP%REACTIVITY+CONSTANTS 0      @   a   FILE_TP%POINTKINETICS+CONSTANTS %   `  @   a   FILE_TP%PT+CONSTANTS -   �  @   a   FILE_TP%TH_WARNING+CONSTANTS *   �  @   a   FILE_TP%TH_RBFD+CONSTANTS )      @   a   FILE_TP%TH_HOT+CONSTANTS -   `  @   a   FILE_TP%TH_AVERAGE+CONSTANTS    �  Z      LINEARPOWER "   �  |   a   LINEARPOWER%POWER '   v   l   a   LINEARPOWER%FQ_LATTICE $   �   |   a   LINEARPOWER%FQ_CORE '   ^!  @   a   LINEARPOWER%NORMAL_OLD +   �!  @   a   LINEARPOWER%NORMAL_CURRENT '   �!  |   a   LINEARPOWER%AVG_LINEAR '   Z"  |   a   LINEARPOWER%MAX_LINEAR "   �"  @   a   LINEARPOWER%GAMMA "   #  W   a   LINEARPOWER%ALLOC "   m#  W      ALLOC_LINEARPOWER '   �#  M   a   ALLOC_LINEARPOWER%THIS &   $  N   a   ALLOC_LINEARPOWER%NTH "   _$  V   a   LINEARPOWER%CLEAN !   �$  n      FREE_LINEARPOWER +   #%  >      FREE_LINEARPOWER%ALLOCATED &   a%  M   a   FREE_LINEARPOWER%THIS "   �%  W   a   LINEARPOWER%PRINT "   &  �      PRINT_LINEARPOWER *   �&  <      PRINT_LINEARPOWER%PRESENT '   �&  9      PRINT_LINEARPOWER%SIZE '   '  M   a   PRINT_LINEARPOWER%THIS )   h'  8   a   PRINT_LINEARPOWER%TO_AVG )   �'  8   a   PRINT_LINEARPOWER%TO_MAX +   �'  Y   a   LINEARPOWER%SET_FQ_LATTICE $   1(  V      SET_FQ_LATTICE_INFO )   �(  M   a   SET_FQ_LATTICE_INFO%THIS '   �(  h   a   SET_FQ_LATTICE_INFO%FQ "   <)  U   a   LINEARPOWER%FQ_FA     �)  �      GET_ASSEMBLY_FQ $   '*  8      GET_ASSEMBLY_FQ%SUM %   _*  9      GET_ASSEMBLY_FQ%SIZE %   �*  M   a   GET_ASSEMBLY_FQ%THIS #   �*  8   a   GET_ASSEMBLY_FQ%FQ (   +  8   a   GET_ASSEMBLY_FQ%CHANNEL #   U+  T   a   LINEARPOWER%FQ_PIN    �+  �      GET_FUELPIN_FQ #   =,  8      GET_FUELPIN_FQ%SUM $   u,  9      GET_FUELPIN_FQ%SIZE $   �,  M   a   GET_FUELPIN_FQ%THIS "   �,  8   a   GET_FUELPIN_FQ%FQ '   3-  8   a   GET_FUELPIN_FQ%CHANNEL %   k-  R   a   LINEARPOWER%FQ_POINT    �-  �      GET_POINT_FQ "   B.  9      GET_POINT_FQ%SIZE "   {.  M   a   GET_POINT_FQ%THIS     �.  8   a   GET_POINT_FQ%FQ %    /  8   a   GET_POINT_FQ%CHANNEL #   8/  8   a   GET_POINT_FQ%AXIAL &   p/  <   a   LINEARPOWER%SET_POWER    �/  �   `   gen@SET_POWER 2   /0  [   a   LINEARPOWER%SET_ASSEMBLY_POWER_3D &   �0  �      SET_ASSEMBLY_POWER_3D +   51  9      SET_ASSEMBLY_POWER_3D%SIZE +   n1  M   a   SET_ASSEMBLY_POWER_3D%THIS *   �1  N   a   SET_ASSEMBLY_POWER_3D%NTH .   	2  Q   a   SET_ASSEMBLY_POWER_3D%GEOM_TH 0   Z2  �   a   SET_ASSEMBLY_POWER_3D%GEOM_ASSM ,   �2  x   a   SET_ASSEMBLY_POWER_3D%POWER .   W3  x   a   SET_ASSEMBLY_POWER_3D%FQ_CORE 2   �3  [   a   LINEARPOWER%SET_ASSEMBLY_POWER_0D &   *4  i      SET_ASSEMBLY_POWER_0D +   �4  M   a   SET_ASSEMBLY_POWER_0D%THIS ,   �4  8   a   SET_ASSEMBLY_POWER_0D%POWER 1   5  8   a   SET_ASSEMBLY_POWER_0D%IS_INITIAL    P5  Z      MsObjComment 