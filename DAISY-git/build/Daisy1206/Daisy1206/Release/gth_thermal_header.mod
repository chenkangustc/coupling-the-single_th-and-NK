	  ^g  "  k820309    ?          14.0        ��.Z                                                                                                           
       E:\documents\doctors degree\software\tansistant\NT\DAISY_verson_20170328\DAISY-git\git\src\thermal\gth_thermal_header.f90 GTH_THERMAL_HEADER          THERMALDESIGN THERMALCHANNEL THERMALHOTPOINT                  @                              
                                                      
                        @                              
                        @                              
       COOLANTPROPERTY                  @                              
       THERMALSCALE THERMALGEOMETRY THERMALASSEMBLYGEOMETRY               @                               '@            #COOLANT_TYPE    #DENSITY    #ENTHALPY 	   #TEMPERATURE 
   #CAPACITY    #CONDUCTIVITY    #VISCOSITY    #NUSSELT_NUMBER    #SET    #GET_DENSITY    #GET_ENTHALPY    #GET_TEMPERATURE    #GET_CAPACITY     #GET_CONDUCTIVITY $   #GET_VISCOSITY (   #GET_NUSSELT ,            � $                                                      � $                                        
            � $                             	           
            � $                             
           
            � $                                         
            � $                                  (      
            � $                                  0      
            � $                                  8      
   1     �   � $                      �              	     #SET_COOLANTPROPERTY    #     @     @                                	               #THIS    #TYPE    #OPTION          
                                    @       #COOLANTPROPERTY          
                                              
                                        1     �   � $                    �              
     #GET_DENSITY_BY_TEMPERATURE_COOLANT    %     @    @                                             
   #THIS    #T_IN          
                                    @       #COOLANTPROPERTY          
                                     
  1     �   � $                    �                   #GET_ENTHALPY_BY_TEMPERATURE_COOLANT    %     @    @                                             
   #THIS    #T_IN          
                                    @       #COOLANTPROPERTY          
                                     
  1     �   � $                     �                   #GET_TEMPERATURE_BY_ENTHALPY_COOLANT    %     @    @                                              
   #THIS    #H_IN          
                                    @       #COOLANTPROPERTY          
                                     
  1     �   � $                     �                    #GET_CAPACITY_BY_TEMPERATURE_COOLANT !   %     @    @                          !                    
   #THIS "   #T_IN #         
                               "     @       #COOLANTPROPERTY          
                                #     
  1     �   � $                     �     $              #GET_CONDUCTIVITY_BY_TEMPERATURE_COOLANT %   %     @    @                          %                    
   #THIS &   #T_IN '         
                               &     @       #COOLANTPROPERTY          
                                '     
  1     �   � $                     �     (              #GET_VISCOSITY_BY_TEMPERATURE_COOLANT )   %     @    @                          )                    
   #THIS *   #T_IN +         
                               *     @       #COOLANTPROPERTY          
                                +     
  1     �   � $                     �     ,              #GET_NUSSELT_NUMBER -   %     @    @                          -                    
   #THIS .   #P2D /   #VELOCITY 0   #DH 1   #T_IN 2         
                               .     @       #COOLANTPROPERTY          
                                /     
        
                                0     
        
                                1     
        
                                2     
                @                           3     '       
      #NF 4   #NC 5   #N_MESH 6   #NR 7   #NA 8   #NA_START 9   #NA_END :   #N_ASSM_GEOM ;   #MESH <   #SET A            � $                              4                        � $                              5                       � $                              6                       � $                              7                       � $                              8                       � $                              9                       � $                              :                       � $                              ;              1     �   � $                      �      <         	     #SET_CONDUCTION_MESH =   #     @     @                            =                    #THIS >   #NF ?   #NC @         
                                >             #THERMALSCALE 3         
                                  ?             
                                  @       1     �   � $                      �      A         
     #SET_THERMALSCALE B   #     @     @                            B                    #THIS C   #ZONE D   #LAYER E   #TOP_LAYER F   #BOTTOM_LAYER G         
                                C             #THERMALSCALE 3         
                                  D             
                                  E             
                                  F             
                                  G                     @               A           H     '�      
      #HEIGHT I   #GEOM_TYPE J   #COOLANT_TYPE K   #CLADDING_TYPE L   #GAP_TYPE M   #FUEL_TYPE N   #FUEL_TRU O   #ALLOC P   #CLEAN T   #SET_HEIGHT X          � $                             I                  
        &                              � $                              J        $                 &                               � $                              K     H                                                         1       � $                              L        L                 &                              � $                              M        p                 &                              � $                              N        �                 &                              � $                             O        �         
        &                       1     �   � $                      �      P              #ALLOC_THERMALGEOMETRY Q   #     @     @                            Q                    #THIS R   #NTH S         
                                R     �       #THERMALGEOMETRY H         
                                  S            #THERMALSCALE 3   1     �   � $                      �      T         	     #FREE_THERMALGEOMETRY U   #     @     @                            U                   #FREE_THERMALGEOMETRY%ALLOCATED V   #THIS W             @                            V     ALLOCATED       
                                W     �       #THERMALGEOMETRY H   1     �   � $                      �      X         
     #SET_THERMALGEOMETRY_HEIGHT Y   #     @     @                            Y                    #THIS Z   #HEIGHT [         
                                Z     �       #THERMALGEOMETRY H         
                                 [           
 	         &                                     @               A           \     '�            #N_PIN ]   #N_FUELPIN ^   #PITCH _   #ROD `   #CLADTH a   #BOND b   #PELLET c   #HOLE d   #AREA e   #FLOWAREA f   #IS_HEXAGONAL g   #P2D h   #DH i   #X_POINT j   #X_SURFACE k   #DF l   #DG m   #DC n   #ALLOC o   #CLEAN s   #SET w            � $                              ]                        � $                              ^                       � $                             _           
            � $                             `           
            � $                             a           
            � $                             b            
            � $                             c     (      
            � $                             d     0      
            � $                             e     8   	   
            � $                             f     @   
   
           � $                              g     H                                             ��������    ����         � $                             h     P      
            � $                             i     X      
          � $                             j        `         
        &                              � $                             k        �         
        &                                � $                             l     �      
            � $                             m     �      
            � $                             n     �      
   1     �   � $                      �      o              #ALLOC_TYPEGEOMETRYASSEMBLY p   #     @     @                            p                    #THIS q   #NTH r         
                                q     �       #THERMALASSEMBLYGEOMETRY \         
                                  r            #THERMALSCALE 3   1     �   � $                      �      s              #FREE_TYPEGEOMETRYASSEMBLY t   #     @     @                            t                   #FREE_TYPEGEOMETRYASSEMBLY%ALLOCATED u   #THIS v             @                            u     ALLOCATED       
                                v     �       #THERMALASSEMBLYGEOMETRY \   1     �   � $                      �      w              #SET_TYPEGEOMETRYASSEMBLY x   #     @     @                            x                    #THIS y   #NTH z         
                                y     �       #THERMALASSEMBLYGEOMETRY \         
                                  z            #THERMALSCALE 3             @  @                          {     '0            #CASENAME |   #MAIN }   #MEMORY ~   #TIMELIST    #DET �   #REACTIVITY �   #POINTKINETICS �   #PT �   #TH_WARNING �   #TH_RBFD �   #TH_HOT �   #TH_AVERAGE �            � $                              |                        � $                              }                       � $                              ~                       � $                                                     � $                              �                       � $                              �                       � $                              �                       � $                              �                       � $                              �         	               � $                              �     $   
               � $                              �     (                  � $                              �     ,                       @               A           �     '�           #TFTYPE �   #TFWEIGHT �   #IS_SEARCH �   #TCOOLIN �   #INIT_TCOOLIN �   #TCOOLOUT �   #FLOWRATE �   #TMINCOOLOUT �   #TMAXCOOLOUT �   #TMAXCLADSURF �   #MAX_VELOCITY �   #TRU_WEIGHT �   #CHANNEL_FLOWRATE �   #ASSEMBLY_FLOW �   #IS_ACTIVE_CHANNEL �   #IS_ACTIVE_NODAL �   #INIT_FLOW �   #INIT_FLOWRATE �   #IS_BLOCKAGE �   #BLOCK_MASK �   #BLOCK_TIME �   #PERCENTAGE �   #ALLOC �   #CLEAN �   #SET �   #PRINT �           � $                              �                                                               1        � $                             �          
                            
         ffffff�?        0.7D0        � $                              �                                                              ����         � $                             �           
            � $                             �            
            � $                             �     (      
            � $                             �     0      
            � $                             �     8      
            � $                             �     @   	   
            � $                             �     H   
   
            � $                             �     P      
          � $                             �        X         
        &                             � $                             �        |         
        &                              � $                             �        �         
        &                              � $                              �        �                 &                             � $                              �        �                 &           &                              � $                             �                
        &                              � $                             �        <        
        &                               � $                              �     `                                                        ����       � $                              �        d                &                               � $                             �     �    
                            
                         0.0D0        � $                             �     �    
                            
                         0.0D01     �   � $                      �      �              #ALLOC_THERMALDESIGN �   #     @     @                             �                    #THIS �   #NTH �         
D @                              �     �      #THERMALDESIGN �         
                                  �            #THERMALSCALE 3   1     �   � $                     �      �              #FREE_THERMALDESIGN �   #     @     @                            �                   #FREE_THERMALDESIGN%ALLOCATED �   #THIS �             @                            �     ALLOCATED       
D                                �     �      #THERMALDESIGN �   1     �   � $                      �      �              #SET_THERMALDESIGN_FLOW �   #     @     @                             �                    #THIS �   #A_COOLANT �   #NTH �   #GEOM_TH �   #GEOM_ASSM �   #LNPOWER �         
D                                �     �      #THERMALDESIGN �         
                                �     @      #COOLANTPROPERTY          
                                  �            #THERMALSCALE 3         
                                  �     �      #THERMALGEOMETRY H         
                                  �        �              &                       #THERMALASSEMBLYGEOMETRY \         
                                 �           
          &           &                       1     �   � $                      �      �              #PRINT_THERMALDESIGN �   #     @     @                             �                   #PRINT_THERMALDESIGN%SIZE �   #THIS �   #UNIT_ �             @                            �     SIZE       
                                 �     �     #THERMALDESIGN �         
                                  �                     @               @           �     '0           #TCOOLANT_OUT �   #RHOCOOLANT_OUT �   #FLOW_VELOCITY �   #CONVECTION �   #RHOCOOLANT �   #TCOOLANT �   #HJUNCTION �   #TCLAD_SURF �   #TCLAD_INNER �   #TFUEL_SURF �   #TFUEL_CENTER �   #TFUEL_AVG �   #TROD �   #ALLOC �   #CLEAN �   #UPDATE �   #FIX_BOTTOM �   #FIX_TOP �   #HOMO �   #PRINT �   #PRINT_AVG �   #PRINT_MAX �   #PRINT_ROD            � $                             �            
            � $                             �           
          � $                             �                 
        &           &                              � $                             �        @         
        &           &                              � $                             �        p         
        &           &                             � $                             �        �         
        &           &                              � $                             �        �         
        &           &                              � $                             �                 
        &           &                              � $                             �        0     	   
        &           &                              � $                             �        `     
   
        &           &                              � $                             �        �        
        &           &                              � $                             �        �        
        &           &                              � $                             �        �        
        &           &           &                       1     �   � $                      �      �              #ALLOC_CHANNELTHERMAL �   #     @     @                             �                    #THIS �   #NTH �         
D @                              �     0      #THERMALCHANNEL �         
                                  �            #THERMALSCALE 3   1     �   � $                     �      �              #FREE_CHANNELTHERMAL �   #     @     @                            �                   #FREE_CHANNELTHERMAL%ALLOCATED �   #THIS �             @                            �     ALLOCATED       
D                                �     0      #THERMALCHANNEL �   1     �   � $                      �      �              #UPDATE_CHANNELTHERMAL �   #     @     @                             �                    #THIS �   #A_ASSEMBLY �   #NTH �   #DESIGN �   #SOLVE �   #IA �   #IR �         
D                                �     0      #THERMALCHANNEL �         
                                  �     �      #THERMALASSEMBLYGEOMETRY \         
                                  �            #THERMALSCALE 3         
                                  �     �     #THERMALDESIGN �         
                                 �           
          &                             
                                  �             
                                  �       1     �   � $                      �      �              #SET_THERMALCHANNEL_BOTTOM �   #     @     @                             �                    #THIS �   #A_COOLANT �   #NTH �   #DESIGN �         
D                                �     0      #THERMALCHANNEL �         
  P                              �     @      #COOLANTPROPERTY          
                                  �            #THERMALSCALE 3         
                                  �     �     #THERMALDESIGN �   1     �   � $                      �      �              #SET_THERMALCHANNEL_TOP �   #     @     @                             �                    #THIS �   #NTH �   #DESIGN �         
D                                �     0      #THERMALCHANNEL �         
                                  �            #THERMALSCALE 3         
                                  �     �     #THERMALDESIGN �   1     �   � $                      �      �              #GET_HOMOGENEOUS_FEEDBACK �   #     @     @                             �                   #GET_HOMOGENEOUS_FEEDBACK%SUM �   #THIS �   #A_COOLANT �   #NTH �   #DESIGN �   #GEOM_TH �   #T_FUEL �   #T_COOLANT �   #RHO_COOLANT �             @                            �     SUM       
                                 �     0     #THERMALCHANNEL �         
  P                              �     @      #COOLANTPROPERTY          
                                  �            #THERMALSCALE 3         
                                  �     �     #THERMALDESIGN �         
                                  �     �      #THERMALGEOMETRY H         
D                                �     
         
D                                �     
         
D                                �     
   1     �   � $                      �      �              #PRINT_CHANNELTHERMAL �   #     @     @                             �                   #PRINT_CHANNELTHERMAL%TRIM �   #THIS �   #NTH �   #GEOM_TH �   #UNIT_ �             @                            �     TRIM       
                                �     0      #THERMALCHANNEL �         
                                  �            #THERMALSCALE 3         
                                  �     �      #THERMALGEOMETRY H         
                                  �       1     �   � $                      �      �              #PRINT_THERMALCHANNEL_AVERAGE �   #     @     @                             �                   #PRINT_THERMALCHANNEL_AVERAGE%SIZE �   #THIS �   #UNIT_ �   #DESIGN �   #NTH �   #TIDX �   #CTIME �             @                            �     SIZE       
 @                              �     0      #THERMALCHANNEL �         
                                  �             
  @                               �     �     #THERMALDESIGN �         
                                  �            #THERMALSCALE 3         
                                  �             
                                 �     
  1     �   � $                      �      �          	    #PRINT_THERMALCHENNEL_MAX �   #     @     @                             �                    #THIS �   #UNIT_ �   #DESIGN �   #NTH �   #TIDX �   #CTIME          
 @                              �     0      #THERMALCHANNEL �         
                                  �             
  @                               �     �     #THERMALDESIGN �         
                                  �            #THERMALSCALE 3         
                                  �             
                                      
  1     �   � $                      �               
    #PRINT_CHANNELTHERMAL_ROD   #     @     @                                               #PRINT_CHANNELTHERMAL_ROD%UBOUND   #THIS   #UNIT_   #NTH   #IR             @                                UBOUND       
                                    0      #THERMALCHANNEL �         
                                              
                                             #THERMALSCALE 3         
                                                      @                              '�            #COOLANT 	  #CLAD_SURF   #CLAD_INNER   #FUEL_CENTER   #TCOOLANT   #TCLAD_SURF   #TCLAD_INNER   #TFUEL_CENTER   #BY_FUEL_CENTER   #BY_CLAD_INNER   #RESULT   #HOT_FA   #SET            � $                              	              #HOTPOINT_RECORD_TP 
            @  @                         
    '            #AXIAL   #RADIAL   #VALUE           �                                                                                             1        �                                                                                            1        �                                       
                            
                         0         � $                                           #HOTPOINT_RECORD_TP 
           � $                                            #HOTPOINT_RECORD_TP 
           � $                                     0      #HOTPOINT_RECORD_TP 
           � $                                 @      
            � $                                 H      
            � $                                 P      
            � $                                 X      
           � $                                  `   	                                          ��������    ����        � $                                  d   
                                                      ����         � $                                     h      #HOTPOINT_RECORD_TP 
           � $                                  x         1     �   � $                      �                   #SET_HOTVALUE   #     @     @                                               #SET_HOTVALUE%PRESENT   #THIS   #HOT_CHANNEL   #NTH   #IS_INITIAL             @                                PRESENT       
D                                    �       #THERMALHOTPOINT         
                                      0     #THERMALCHANNEL �         
                                             #THERMALSCALE 3         
 @                                     *         � n                 �              Cifmodintr.lib                     �   �      fn#fn (   5  9   b   uapp(GTH_THERMAL_HEADER    n  <   J  CONSTANTS     �  <   J  ISO_FORTRAN_ENV    �  <   J  STASTICS )   "  L   J  ABSTRACT_PROPERTY_HEADER $   n  q   J  GTH_GEOMETRY_HEADER 9   �  R      COOLANTPROPERTY+ABSTRACT_PROPERTY_HEADER F   1  @   a   COOLANTPROPERTY%COOLANT_TYPE+ABSTRACT_PROPERTY_HEADER A   q  @   a   COOLANTPROPERTY%DENSITY+ABSTRACT_PROPERTY_HEADER B   �  @   a   COOLANTPROPERTY%ENTHALPY+ABSTRACT_PROPERTY_HEADER E   �  @   a   COOLANTPROPERTY%TEMPERATURE+ABSTRACT_PROPERTY_HEADER B   1  @   a   COOLANTPROPERTY%CAPACITY+ABSTRACT_PROPERTY_HEADER F   q  @   a   COOLANTPROPERTY%CONDUCTIVITY+ABSTRACT_PROPERTY_HEADER C   �  @   a   COOLANTPROPERTY%VISCOSITY+ABSTRACT_PROPERTY_HEADER H   �  @   a   COOLANTPROPERTY%NUSSELT_NUMBER+ABSTRACT_PROPERTY_HEADER =   1  Y   a   COOLANTPROPERTY%SET+ABSTRACT_PROPERTY_HEADER =   �  d      SET_COOLANTPROPERTY+ABSTRACT_PROPERTY_HEADER B   �  Q   a   SET_COOLANTPROPERTY%THIS+ABSTRACT_PROPERTY_HEADER B   ?  8   a   SET_COOLANTPROPERTY%TYPE+ABSTRACT_PROPERTY_HEADER D   w  8   a   SET_COOLANTPROPERTY%OPTION+ABSTRACT_PROPERTY_HEADER E   �  h   a   COOLANTPROPERTY%GET_DENSITY+ABSTRACT_PROPERTY_HEADER L     \      GET_DENSITY_BY_TEMPERATURE_COOLANT+ABSTRACT_PROPERTY_HEADER Q   s  Q   a   GET_DENSITY_BY_TEMPERATURE_COOLANT%THIS+ABSTRACT_PROPERTY_HEADER Q   �  8   a   GET_DENSITY_BY_TEMPERATURE_COOLANT%T_IN+ABSTRACT_PROPERTY_HEADER F   �  i   a   COOLANTPROPERTY%GET_ENTHALPY+ABSTRACT_PROPERTY_HEADER M   e	  \      GET_ENTHALPY_BY_TEMPERATURE_COOLANT+ABSTRACT_PROPERTY_HEADER R   �	  Q   a   GET_ENTHALPY_BY_TEMPERATURE_COOLANT%THIS+ABSTRACT_PROPERTY_HEADER R   
  8   a   GET_ENTHALPY_BY_TEMPERATURE_COOLANT%T_IN+ABSTRACT_PROPERTY_HEADER I   J
  i   a   COOLANTPROPERTY%GET_TEMPERATURE+ABSTRACT_PROPERTY_HEADER M   �
  \      GET_TEMPERATURE_BY_ENTHALPY_COOLANT+ABSTRACT_PROPERTY_HEADER R     Q   a   GET_TEMPERATURE_BY_ENTHALPY_COOLANT%THIS+ABSTRACT_PROPERTY_HEADER R   `  8   a   GET_TEMPERATURE_BY_ENTHALPY_COOLANT%H_IN+ABSTRACT_PROPERTY_HEADER F   �  i   a   COOLANTPROPERTY%GET_CAPACITY+ABSTRACT_PROPERTY_HEADER M     \      GET_CAPACITY_BY_TEMPERATURE_COOLANT+ABSTRACT_PROPERTY_HEADER R   ]  Q   a   GET_CAPACITY_BY_TEMPERATURE_COOLANT%THIS+ABSTRACT_PROPERTY_HEADER R   �  8   a   GET_CAPACITY_BY_TEMPERATURE_COOLANT%T_IN+ABSTRACT_PROPERTY_HEADER J   �  m   a   COOLANTPROPERTY%GET_CONDUCTIVITY+ABSTRACT_PROPERTY_HEADER Q   S  \      GET_CONDUCTIVITY_BY_TEMPERATURE_COOLANT+ABSTRACT_PROPERTY_HEADER V   �  Q   a   GET_CONDUCTIVITY_BY_TEMPERATURE_COOLANT%THIS+ABSTRACT_PROPERTY_HEADER V      8   a   GET_CONDUCTIVITY_BY_TEMPERATURE_COOLANT%T_IN+ABSTRACT_PROPERTY_HEADER G   8  j   a   COOLANTPROPERTY%GET_VISCOSITY+ABSTRACT_PROPERTY_HEADER N   �  \      GET_VISCOSITY_BY_TEMPERATURE_COOLANT+ABSTRACT_PROPERTY_HEADER S   �  Q   a   GET_VISCOSITY_BY_TEMPERATURE_COOLANT%THIS+ABSTRACT_PROPERTY_HEADER S   O  8   a   GET_VISCOSITY_BY_TEMPERATURE_COOLANT%T_IN+ABSTRACT_PROPERTY_HEADER E   �  X   a   COOLANTPROPERTY%GET_NUSSELT+ABSTRACT_PROPERTY_HEADER <   �  {      GET_NUSSELT_NUMBER+ABSTRACT_PROPERTY_HEADER A   Z  Q   a   GET_NUSSELT_NUMBER%THIS+ABSTRACT_PROPERTY_HEADER @   �  8   a   GET_NUSSELT_NUMBER%P2D+ABSTRACT_PROPERTY_HEADER E   �  8   a   GET_NUSSELT_NUMBER%VELOCITY+ABSTRACT_PROPERTY_HEADER ?     8   a   GET_NUSSELT_NUMBER%DH+ABSTRACT_PROPERTY_HEADER A   S  8   a   GET_NUSSELT_NUMBER%T_IN+ABSTRACT_PROPERTY_HEADER 1   �  �       THERMALSCALE+GTH_GEOMETRY_HEADER 4   9  @   a   THERMALSCALE%NF+GTH_GEOMETRY_HEADER 4   y  @   a   THERMALSCALE%NC+GTH_GEOMETRY_HEADER 8   �  @   a   THERMALSCALE%N_MESH+GTH_GEOMETRY_HEADER 4   �  @   a   THERMALSCALE%NR+GTH_GEOMETRY_HEADER 4   9  @   a   THERMALSCALE%NA+GTH_GEOMETRY_HEADER :   y  @   a   THERMALSCALE%NA_START+GTH_GEOMETRY_HEADER 8   �  @   a   THERMALSCALE%NA_END+GTH_GEOMETRY_HEADER =   �  @   a   THERMALSCALE%N_ASSM_GEOM+GTH_GEOMETRY_HEADER 6   9  Y   a   THERMALSCALE%MESH+GTH_GEOMETRY_HEADER 8   �  ^      SET_CONDUCTION_MESH+GTH_GEOMETRY_HEADER =   �  N   a   SET_CONDUCTION_MESH%THIS+GTH_GEOMETRY_HEADER ;   >  8   a   SET_CONDUCTION_MESH%NF+GTH_GEOMETRY_HEADER ;   v  8   a   SET_CONDUCTION_MESH%NC+GTH_GEOMETRY_HEADER 5   �  V   a   THERMALSCALE%SET+GTH_GEOMETRY_HEADER 5     �      SET_THERMALSCALE+GTH_GEOMETRY_HEADER :   �  N   a   SET_THERMALSCALE%THIS+GTH_GEOMETRY_HEADER :   �  8   a   SET_THERMALSCALE%ZONE+GTH_GEOMETRY_HEADER ;     8   a   SET_THERMALSCALE%LAYER+GTH_GEOMETRY_HEADER ?   F  8   a   SET_THERMALSCALE%TOP_LAYER+GTH_GEOMETRY_HEADER B   ~  8   a   SET_THERMALSCALE%BOTTOM_LAYER+GTH_GEOMETRY_HEADER 4   �  �       THERMALGEOMETRY+GTH_GEOMETRY_HEADER ;   �  l   a   THERMALGEOMETRY%HEIGHT+GTH_GEOMETRY_HEADER >   �  l   a   THERMALGEOMETRY%GEOM_TYPE+GTH_GEOMETRY_HEADER A   c  }   a   THERMALGEOMETRY%COOLANT_TYPE+GTH_GEOMETRY_HEADER B   �  l   a   THERMALGEOMETRY%CLADDING_TYPE+GTH_GEOMETRY_HEADER =   L  l   a   THERMALGEOMETRY%GAP_TYPE+GTH_GEOMETRY_HEADER >   �  l   a   THERMALGEOMETRY%FUEL_TYPE+GTH_GEOMETRY_HEADER =   $  l   a   THERMALGEOMETRY%FUEL_TRU+GTH_GEOMETRY_HEADER :   �  [   a   THERMALGEOMETRY%ALLOC+GTH_GEOMETRY_HEADER :   �  W      ALLOC_THERMALGEOMETRY+GTH_GEOMETRY_HEADER ?   B  Q   a   ALLOC_THERMALGEOMETRY%THIS+GTH_GEOMETRY_HEADER >   �  N   a   ALLOC_THERMALGEOMETRY%NTH+GTH_GEOMETRY_HEADER :   �  Z   a   THERMALGEOMETRY%CLEAN+GTH_GEOMETRY_HEADER 9   ;  r      FREE_THERMALGEOMETRY+GTH_GEOMETRY_HEADER M   �  >      FREE_THERMALGEOMETRY%ALLOCATED+GTH_GEOMETRY_HEADER=ALLOCATED >   �  Q   a   FREE_THERMALGEOMETRY%THIS+GTH_GEOMETRY_HEADER ?   <  `   a   THERMALGEOMETRY%SET_HEIGHT+GTH_GEOMETRY_HEADER ?   �  Z      SET_THERMALGEOMETRY_HEIGHT+GTH_GEOMETRY_HEADER D   �  Q   a   SET_THERMALGEOMETRY_HEIGHT%THIS+GTH_GEOMETRY_HEADER F   G  h   a   SET_THERMALGEOMETRY_HEIGHT%HEIGHT+GTH_GEOMETRY_HEADER <   �  ,      THERMALASSEMBLYGEOMETRY+GTH_GEOMETRY_HEADER B   �   @   a   THERMALASSEMBLYGEOMETRY%N_PIN+GTH_GEOMETRY_HEADER F   !  @   a   THERMALASSEMBLYGEOMETRY%N_FUELPIN+GTH_GEOMETRY_HEADER B   [!  @   a   THERMALASSEMBLYGEOMETRY%PITCH+GTH_GEOMETRY_HEADER @   �!  @   a   THERMALASSEMBLYGEOMETRY%ROD+GTH_GEOMETRY_HEADER C   �!  @   a   THERMALASSEMBLYGEOMETRY%CLADTH+GTH_GEOMETRY_HEADER A   "  @   a   THERMALASSEMBLYGEOMETRY%BOND+GTH_GEOMETRY_HEADER C   ["  @   a   THERMALASSEMBLYGEOMETRY%PELLET+GTH_GEOMETRY_HEADER A   �"  @   a   THERMALASSEMBLYGEOMETRY%HOLE+GTH_GEOMETRY_HEADER A   �"  @   a   THERMALASSEMBLYGEOMETRY%AREA+GTH_GEOMETRY_HEADER E   #  @   a   THERMALASSEMBLYGEOMETRY%FLOWAREA+GTH_GEOMETRY_HEADER I   [#  |   a   THERMALASSEMBLYGEOMETRY%IS_HEXAGONAL+GTH_GEOMETRY_HEADER @   �#  @   a   THERMALASSEMBLYGEOMETRY%P2D+GTH_GEOMETRY_HEADER ?   $  @   a   THERMALASSEMBLYGEOMETRY%DH+GTH_GEOMETRY_HEADER D   W$  l   a   THERMALASSEMBLYGEOMETRY%X_POINT+GTH_GEOMETRY_HEADER F   �$  l   a   THERMALASSEMBLYGEOMETRY%X_SURFACE+GTH_GEOMETRY_HEADER ?   /%  @   a   THERMALASSEMBLYGEOMETRY%DF+GTH_GEOMETRY_HEADER ?   o%  @   a   THERMALASSEMBLYGEOMETRY%DG+GTH_GEOMETRY_HEADER ?   �%  @   a   THERMALASSEMBLYGEOMETRY%DC+GTH_GEOMETRY_HEADER B   �%  `   a   THERMALASSEMBLYGEOMETRY%ALLOC+GTH_GEOMETRY_HEADER ?   O&  W      ALLOC_TYPEGEOMETRYASSEMBLY+GTH_GEOMETRY_HEADER D   �&  Y   a   ALLOC_TYPEGEOMETRYASSEMBLY%THIS+GTH_GEOMETRY_HEADER C   �&  N   a   ALLOC_TYPEGEOMETRYASSEMBLY%NTH+GTH_GEOMETRY_HEADER B   M'  _   a   THERMALASSEMBLYGEOMETRY%CLEAN+GTH_GEOMETRY_HEADER >   �'  w      FREE_TYPEGEOMETRYASSEMBLY+GTH_GEOMETRY_HEADER R   #(  >      FREE_TYPEGEOMETRYASSEMBLY%ALLOCATED+GTH_GEOMETRY_HEADER=ALLOCATED C   a(  Y   a   FREE_TYPEGEOMETRYASSEMBLY%THIS+GTH_GEOMETRY_HEADER @   �(  ^   a   THERMALASSEMBLYGEOMETRY%SET+GTH_GEOMETRY_HEADER =   )  W      SET_TYPEGEOMETRYASSEMBLY+GTH_GEOMETRY_HEADER B   o)  Y   a   SET_TYPEGEOMETRYASSEMBLY%THIS+GTH_GEOMETRY_HEADER A   �)  N   a   SET_TYPEGEOMETRYASSEMBLY%NTH+GTH_GEOMETRY_HEADER "   *  �      FILE_TP+CONSTANTS +   �*  @   a   FILE_TP%CASENAME+CONSTANTS '   9+  @   a   FILE_TP%MAIN+CONSTANTS )   y+  @   a   FILE_TP%MEMORY+CONSTANTS +   �+  @   a   FILE_TP%TIMELIST+CONSTANTS &   �+  @   a   FILE_TP%DET+CONSTANTS -   9,  @   a   FILE_TP%REACTIVITY+CONSTANTS 0   y,  @   a   FILE_TP%POINTKINETICS+CONSTANTS %   �,  @   a   FILE_TP%PT+CONSTANTS -   �,  @   a   FILE_TP%TH_WARNING+CONSTANTS *   9-  @   a   FILE_TP%TH_RBFD+CONSTANTS )   y-  @   a   FILE_TP%TH_HOT+CONSTANTS -   �-  @   a   FILE_TP%TH_AVERAGE+CONSTANTS    �-  �      THERMALDESIGN %   �/  }   a   THERMALDESIGN%TFTYPE '   V0  �   a   THERMALDESIGN%TFWEIGHT (   �0  |   a   THERMALDESIGN%IS_SEARCH &   S1  @   a   THERMALDESIGN%TCOOLIN +   �1  @   a   THERMALDESIGN%INIT_TCOOLIN '   �1  @   a   THERMALDESIGN%TCOOLOUT '   2  @   a   THERMALDESIGN%FLOWRATE *   S2  @   a   THERMALDESIGN%TMINCOOLOUT *   �2  @   a   THERMALDESIGN%TMAXCOOLOUT +   �2  @   a   THERMALDESIGN%TMAXCLADSURF +   3  @   a   THERMALDESIGN%MAX_VELOCITY )   S3  l   a   THERMALDESIGN%TRU_WEIGHT /   �3  l   a   THERMALDESIGN%CHANNEL_FLOWRATE ,   +4  l   a   THERMALDESIGN%ASSEMBLY_FLOW 0   �4  l   a   THERMALDESIGN%IS_ACTIVE_CHANNEL .   5  |   a   THERMALDESIGN%IS_ACTIVE_NODAL (   5  l   a   THERMALDESIGN%INIT_FLOW ,   �5  l   a   THERMALDESIGN%INIT_FLOWRATE *   W6  |   a   THERMALDESIGN%IS_BLOCKAGE )   �6  l   a   THERMALDESIGN%BLOCK_MASK )   ?7  �   a   THERMALDESIGN%BLOCK_TIME )   �7  �   a   THERMALDESIGN%PERCENTAGE $   A8  Y   a   THERMALDESIGN%ALLOC $   �8  W      ALLOC_THERMALDESIGN )   �8  O   a   ALLOC_THERMALDESIGN%THIS (   @9  N   a   ALLOC_THERMALDESIGN%NTH $   �9  X   a   THERMALDESIGN%CLEAN #   �9  p      FREE_THERMALDESIGN -   V:  >      FREE_THERMALDESIGN%ALLOCATED (   �:  O   a   FREE_THERMALDESIGN%THIS "   �:  \   a   THERMALDESIGN%SET '   ?;  �      SET_THERMALDESIGN_FLOW ,   �;  O   a   SET_THERMALDESIGN_FLOW%THIS 1   <  Q   a   SET_THERMALDESIGN_FLOW%A_COOLANT +   n<  N   a   SET_THERMALDESIGN_FLOW%NTH /   �<  Q   a   SET_THERMALDESIGN_FLOW%GEOM_TH 1   =  �   a   SET_THERMALDESIGN_FLOW%GEOM_ASSM /   �=  x   a   SET_THERMALDESIGN_FLOW%LNPOWER $   
>  Y   a   THERMALDESIGN%PRINT $   c>  w      PRINT_THERMALDESIGN )   �>  9      PRINT_THERMALDESIGN%SIZE )   ?  O   a   PRINT_THERMALDESIGN%THIS *   b?  8   a   PRINT_THERMALDESIGN%UNIT_    �?  �      THERMALCHANNEL ,   1A  @   a   THERMALCHANNEL%TCOOLANT_OUT .   qA  @   a   THERMALCHANNEL%RHOCOOLANT_OUT -   �A  |   a   THERMALCHANNEL%FLOW_VELOCITY *   -B  |   a   THERMALCHANNEL%CONVECTION *   �B  |   a   THERMALCHANNEL%RHOCOOLANT (   %C  |   a   THERMALCHANNEL%TCOOLANT )   �C  |   a   THERMALCHANNEL%HJUNCTION *   D  |   a   THERMALCHANNEL%TCLAD_SURF +   �D  |   a   THERMALCHANNEL%TCLAD_INNER *   E  |   a   THERMALCHANNEL%TFUEL_SURF ,   �E  |   a   THERMALCHANNEL%TFUEL_CENTER )   F  |   a   THERMALCHANNEL%TFUEL_AVG $   �F  �   a   THERMALCHANNEL%TROD %   G  Z   a   THERMALCHANNEL%ALLOC %   oG  W      ALLOC_CHANNELTHERMAL *   �G  P   a   ALLOC_CHANNELTHERMAL%THIS )   H  N   a   ALLOC_CHANNELTHERMAL%NTH %   dH  Y   a   THERMALCHANNEL%CLEAN $   �H  q      FREE_CHANNELTHERMAL .   .I  >      FREE_CHANNELTHERMAL%ALLOCATED )   lI  P   a   FREE_CHANNELTHERMAL%THIS &   �I  [   a   THERMALCHANNEL%UPDATE &   J  �      UPDATE_CHANNELTHERMAL +   �J  P   a   UPDATE_CHANNELTHERMAL%THIS 1   �J  Y   a   UPDATE_CHANNELTHERMAL%A_ASSEMBLY *   NK  N   a   UPDATE_CHANNELTHERMAL%NTH -   �K  O   a   UPDATE_CHANNELTHERMAL%DESIGN ,   �K  h   a   UPDATE_CHANNELTHERMAL%SOLVE )   SL  8   a   UPDATE_CHANNELTHERMAL%IA )   �L  8   a   UPDATE_CHANNELTHERMAL%IR *   �L  _   a   THERMALCHANNEL%FIX_BOTTOM *   "M  r      SET_THERMALCHANNEL_BOTTOM /   �M  P   a   SET_THERMALCHANNEL_BOTTOM%THIS 4   �M  Q   a   SET_THERMALCHANNEL_BOTTOM%A_COOLANT .   5N  N   a   SET_THERMALCHANNEL_BOTTOM%NTH 1   �N  O   a   SET_THERMALCHANNEL_BOTTOM%DESIGN '   �N  \   a   THERMALCHANNEL%FIX_TOP '   .O  c      SET_THERMALCHANNEL_TOP ,   �O  P   a   SET_THERMALCHANNEL_TOP%THIS +   �O  N   a   SET_THERMALCHANNEL_TOP%NTH .   /P  O   a   SET_THERMALCHANNEL_TOP%DESIGN $   ~P  ^   a   THERMALCHANNEL%HOMO )   �P  �      GET_HOMOGENEOUS_FEEDBACK -   �Q  8      GET_HOMOGENEOUS_FEEDBACK%SUM .   �Q  P   a   GET_HOMOGENEOUS_FEEDBACK%THIS 3   1R  Q   a   GET_HOMOGENEOUS_FEEDBACK%A_COOLANT -   �R  N   a   GET_HOMOGENEOUS_FEEDBACK%NTH 0   �R  O   a   GET_HOMOGENEOUS_FEEDBACK%DESIGN 1   S  Q   a   GET_HOMOGENEOUS_FEEDBACK%GEOM_TH 0   pS  8   a   GET_HOMOGENEOUS_FEEDBACK%T_FUEL 3   �S  8   a   GET_HOMOGENEOUS_FEEDBACK%T_COOLANT 5   �S  8   a   GET_HOMOGENEOUS_FEEDBACK%RHO_COOLANT %   T  Z   a   THERMALCHANNEL%PRINT %   rT  �      PRINT_CHANNELTHERMAL *    U  9      PRINT_CHANNELTHERMAL%TRIM *   9U  P   a   PRINT_CHANNELTHERMAL%THIS )   �U  N   a   PRINT_CHANNELTHERMAL%NTH -   �U  Q   a   PRINT_CHANNELTHERMAL%GEOM_TH +   (V  8   a   PRINT_CHANNELTHERMAL%UNIT_ )   `V  b   a   THERMALCHANNEL%PRINT_AVG -   �V  �      PRINT_THERMALCHANNEL_AVERAGE 2   lW  9      PRINT_THERMALCHANNEL_AVERAGE%SIZE 2   �W  P   a   PRINT_THERMALCHANNEL_AVERAGE%THIS 3   �W  8   a   PRINT_THERMALCHANNEL_AVERAGE%UNIT_ 4   -X  O   a   PRINT_THERMALCHANNEL_AVERAGE%DESIGN 1   |X  N   a   PRINT_THERMALCHANNEL_AVERAGE%NTH 2   �X  8   a   PRINT_THERMALCHANNEL_AVERAGE%TIDX 3   Y  8   a   PRINT_THERMALCHANNEL_AVERAGE%CTIME )   :Y  ^   a   THERMALCHANNEL%PRINT_MAX )   �Y  �      PRINT_THERMALCHENNEL_MAX .   Z  P   a   PRINT_THERMALCHENNEL_MAX%THIS /   kZ  8   a   PRINT_THERMALCHENNEL_MAX%UNIT_ 0   �Z  O   a   PRINT_THERMALCHENNEL_MAX%DESIGN -   �Z  N   a   PRINT_THERMALCHENNEL_MAX%NTH .   @[  8   a   PRINT_THERMALCHENNEL_MAX%TIDX /   x[  8   a   PRINT_THERMALCHENNEL_MAX%CTIME )   �[  ^   a   THERMALCHANNEL%PRINT_ROD )   \  �      PRINT_CHANNELTHERMAL_ROD 0   �\  ;      PRINT_CHANNELTHERMAL_ROD%UBOUND .   �\  P   a   PRINT_CHANNELTHERMAL_ROD%THIS /   (]  8   a   PRINT_CHANNELTHERMAL_ROD%UNIT_ -   `]  N   a   PRINT_CHANNELTHERMAL_ROD%NTH ,   �]  8   a   PRINT_CHANNELTHERMAL_ROD%IR     �]  
      THERMALHOTPOINT (   �^  X   a   THERMALHOTPOINT%COOLANT #   H_  f       HOTPOINT_RECORD_TP )   �_  }   a   HOTPOINT_RECORD_TP%AXIAL *   +`  }   a   HOTPOINT_RECORD_TP%RADIAL )   �`  }   a   HOTPOINT_RECORD_TP%VALUE *   %a  X   a   THERMALHOTPOINT%CLAD_SURF +   }a  X   a   THERMALHOTPOINT%CLAD_INNER ,   �a  X   a   THERMALHOTPOINT%FUEL_CENTER )   -b  @   a   THERMALHOTPOINT%TCOOLANT +   mb  @   a   THERMALHOTPOINT%TCLAD_SURF ,   �b  @   a   THERMALHOTPOINT%TCLAD_INNER -   �b  @   a   THERMALHOTPOINT%TFUEL_CENTER /   -c  |   a   THERMALHOTPOINT%BY_FUEL_CENTER .   �c  |   a   THERMALHOTPOINT%BY_CLAD_INNER '   %d  X   a   THERMALHOTPOINT%RESULT '   }d  @   a   THERMALHOTPOINT%HOT_FA $   �d  R   a   THERMALHOTPOINT%SET    e  �      SET_HOTVALUE %   �e  <      SET_HOTVALUE%PRESENT "   �e  Q   a   SET_HOTVALUE%THIS )   .f  P   a   SET_HOTVALUE%HOT_CHANNEL !   ~f  N   a   SET_HOTVALUE%NTH (   �f  8   a   SET_HOTVALUE%IS_INITIAL    g  Z      MsObjComment 