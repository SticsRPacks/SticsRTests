F: plant name and group                                                                             
         NO:                                                         codeplante           bet
         O: monocot or dicot                               2                              
                   M1: monocotyledon                       0                              
                   M2: dicotyledon                         0                              
                                                                     codemonocot          2
F: effect of atmospheric CO2 concentration
         NO:                                                         alphaco2              mai
F: phasic development                                                                    
         NO:                                                        tdmin                 2.00000
         NO:                                                        tdmax                 25.00000
         O: driving temperature                           2                              
                   M1: air temperature                    4                              
                   M2: temperature within canopy          7                              
                                                                    codetemp              1
                   O: time.scale                          2
                   M11: daily.scale                       0 
                   M12: hourly.scale                      0 
                                                                    codegdh               1
                   M2:                                              coeflevamf            1
                   M2:                                              coefamflax            1
                   M2:                                              coeflaxsen            1
                   M2:                                              coefsenlan            1
                   M2:                                              coeflevdrp            1
                   M2:                                              coefdrpmat            1
                   M2:                                              coefflodrp            1
         O: photoperiodic plant                           2                              
                   M1: yes                                2                              
                   M2: no                                 0                              
                                                                    codephot              2
                   M1:                                              phobase               999.9000
                   M1:                                              phosat                999.9000
         O: delay effect of stress                        2                              
                   M1: yes                                1                              
                   M2: no                                 0                               
                                                                    coderetflo            2
                   M1:                                              stressdev             0.2
         O: cold requirements                             3
                   M1: no                                 0 
                   M2: vernalisation (herbaceous)         4 
                   M3: dormancy (woody)                   15
                                                                     codebfroid           1
                   M2:                                               jvcmini              7.0
                   M2:                                               julvernal            274
                   M2:                                               tfroid               6.5
                   M2:                                               ampfroid             10.0
                   M3:                                               stdordebour          100
                   M3:                                               tdmindeb             5.0
                   M3:                                               tdmaxdeb             25.0
                   O: dormancy calculation                 3
                             M31: forcing                  1
                             M32: Richardson               0
                             M33: Bidabe                   2 
                                                                     codedormance         3
                             M31:                                    ifindorm             70
                             M33:                                    q10                  3.0
                             M33:                                    idebdorm             300
                   O: post dormancy calculation            2
                             M31: daily temperatures       0
                             M32: hourly temperatures      0
                                                                     codegdhdeb           2
F: emergence and starting                                                                                 
         O: annual or perennial                            2
                   M1: annual                             15 
                   M2: perennial                           0 
                                                                     codeperenne          1
                   O: germination or latency               2                              
                            M11: yes                       5                              
                            M12: no                        0                              
                                                                     codegermin           1
                            M11:                                     tgmin                3.00000
                            M11:                                     stpltger             45.00000
                            M11:                                     potgermi             -1.6
                            M11:                                     nbjgerlim            50.0
                            M11:                                     propjgermin          1.0
                   O: plantlet growth                      2                              
                            M21: hypocotyle growth         6                              
                            M22: planting                  4                              
                                                                     codehypo             1
                            M21:                                     belong               0.0115
                            M21:                                     celong               4.5701
                            M21:                                     elmax                7.21
                            M21:                                     nlevlim1             10
                            M21:                                     nlevlim2             50
                            M21:                                     vigueurbat           1.0
                            M22:                                     laiplantule          2.00000
                            M22:                                     nbfeuilplant         3
                            M22:                                     masecplantule        0.5
                            M22:                                     zracplantule         2.0
F: leaves                                                                                 
         NO:                                                         phyllotherme         120
         NO:                                                         bdens                7.000
         NO:                                                         laicomp              0.750
         NO:                                                         hautbase             0.0
         NO:                                                         hautmax              0.60000
         NO:                                                         tcxstop              100.0
         O: leaf dynamics                                  2
                   M1: LAI                                 27 
                   M2: ground cover                        4
                                                                     codelaitr            1
                   M1:                                               vlaimax              2.2
                   M1:                                               pentlaimax           5.5
                   M1:                                               udlaimax             3.0
                   M1:                                               ratiodurvieI         1.0
                   M1:                                               tcmin                2.00000
                   M1:                                               tcmax                30.00
                   M1:                                               ratiosen             0.87
                   M1:                                               abscission           0
                   M1:                                               parazofmorte         13.0
                   M1:                                               innturgmin           0.3
                   M1:                                               dlaimin              0.0
                   O: LAI calculation option              2
                             M11: direct LAInet           2 
                             M12: LAInet=LAIbrut-senes    4
                                                                     codlainet            2
                             M11:                                    dlaimax              4.2e-3
                             M11:                                    tustressmin          0.75
                             M12:                                    dlaimaxbrut          4.0e-3
                             M12:                                    durviesupmax         0.0
                             M12:                                    innsen               0.96
                             M12:                                    rapsenturg           0.51
                   O: effect of photoperiod on senescence  2
                             M11: oui                      4
                             M12: non                      0
                                                                     codestrphot          2
                             M11:                                    phobasesen           12.0
                             M11:                                    dltamsmaxsen         0.001
                             M11:                                    dltamsminsen         0.15
                             M11:                                    alphaphot            200.00						 
                   M2:                                               tauxrecouvmax        1.0
                   M2:                                               tauxrecouvkmax       1.0
                   M2:                                               pentrecouv           4.5
                   M2:                                               infrecouv            0.85
F: radiation interception                                                           
         O: radiation interception                         2                              
                   M1: Beer's law                          1                              
                   M2: radiation transfers                 6                              
                                                                     codetransrad         1
                   M1:                                               extin                0.58
                   M2:                                               ktrou                1.0000
                   M2:                                               forme                1
                   M2:                                               rapforme             2.000
                   M2:                                               adfol                1.0
                   M2:                                               dfolbas              5.0
                   M2:                                               dfolhaut             5.0
F: shoot biomass growth                                                                 
         NO:                                                         temin                3.000
         NO:                                                         temax                33.000
         NO:                                                         teopt                15.000
         NO:                                                         teoptbis             26.000
         NO:                                                         efcroijuv            1.24
         NO:                                                         efcroiveg            3.0
         NO:                                                         efcroirepro          4.83
         NO:                                                         remobres             1.0
         NO:                                                         coefmshaut            .000
F: partitioning of biomass in organs                                                                  
         NO:                                                         slamax               200
         NO:                                                         slamin               30.0
         NO:                                                         tigefeuil            0.0
         NO:                                                         envfruit             0.0
         NO:                                                         sea                  0.0
F: yield formation                                                                     
         O: growing dynamics                               2                              
                   M1: determinate growing plant           11                             
                   M2: indeterminate growing plant         16                              
                                                                     codeindetermin       2
                   M1:                                               nbjgrain             1000
                   M1:                                               cgrain               2000
                   M1:                                               cgrainv0             100
                   M1:                                               nbgrmin               .000
                   O: unit Harvest Index                   2                              
                             M11: days                     2                              
                             M12: degree days              1                              
                                                                     codeir               1
                             M11:                                    vitircarb            0.0000
                             M11:                                    irmax                .50000
                             M12:                                    vitircarbT           .00000
                   M2:                                               nboite               50
                   M2:                                               allocfrmax           0.9
                   M2:                                               afpf                 0.5
                   M2:                                               bfpf                 1.0
                   M2:                                               cfpf                 0.0
                   M2:                                               dfpf                 0.2
                   M2:                                               stdrpnou              250
                   M2:                                               spfrmin              0.00001
                   M2:                                               spfrmax              0.00002
                   M2:                                               splaimin             0.0001
                   M2:                                               splaimax             1.0
                   O: number of inflorescences            2
                             M21: prescribed              1 
                             M22: trophic status function 2
                                                                     codcalinflo          1
                             M21:                                    nbinflo              1.0
                             M22:                                    inflomax             5.0
                             M22:                                    pentinflores         0.8
         O: thermal stress on filling                      2                              
                   M1: yes                                 2                              
                   M2: no                                  0                              
                                                                     codetremp            2
                   M1:                                               tminremp             -5
                   M1:                                               tmaxremp             40
         NO:                                                         vitpropsucre         0.0008
         NO:                                                         vitprophuile         0.00
         NO:                                                         vitirazo             0.00
F: roots                                                                                
         NO:                                                         sensanox             0.0
         NO:                                                         stoprac              sen
         NO:                                                         sensrsec             0.0
         NO:                                                         contrdamax           0.3
         O: driving temperature                            2                              
                   M1: temperature within the canopy       0                              
                   M2: soil (threshold TGMIN)              0                              
                                                                     codetemprac          2
         O: root density                                   2                              
                   M1: standard profile                    3                              
                   M2: true density                        22                             
                                                                     coderacine           1
                   M1:                                               zlabour              102.0000
                   M1:                                               zpente               119.00000
                   M1:                                               zprlim               150.00000
                   M2:                                               draclong             200.00
                   M2:                                               debsenrac            1600.00
                   M2:                                               lvfront              0.25
                   M2:                                               longsperac           18182.0
                   O: N effect on root distribution        2
                             M21: yes                      0
                             M22: no                       3
                                                                     codazorac            2
                             M22:                                    minefnra             0.0
                             M22:                                    minazorac            0.1
                             M22:                                    maxazorac            1.0
                   O: trophic-linked production            3
                             M21: continuous link          3
                             M22: threshold                3
                             M23: no                       0
                                                                     codtrophrac          3
                             M21:                                    repracpermax         0.8
                             M21:                                    repracpermin         0.0
                             M21:                                    krepracperm          1.27
                             M22:                                    repracseumax         0.8
                             M22:                                    repracseumin         0.0
                             M22:                                    krepracseu           1.27
F: frost                                      
         NO:                                                        tletale               -25.0
         NO:                                                        tdebgel               -4.0
         O: plantlet or emergence frost                   2                                            
                   M1: no                                 0 
                   M2: yes                                3
                                                                     codgellev            1  
                   M2:                                               nbfgellev            2 
                   M2:                                               tgellev10            -4.0
                   M2:                                               tgellev90            -20.0
         O: leaf frost at juvenile phase (till AMF)       2                                            
                   M1: no                                 0 
                   M2: yes                                2
                                                                     codgeljuv            1
                   M2:                                               tgeljuv10            -10.0
                   M2:                                               tgeljuv90            -20.0
         O: leaf frost at adult phase                     2                                            
                   M1: no                                 0 
                   M2: yes                                2
                                                                     codgelveg            1
                   M2:                                               tgelveg10            -4.5
                   M2:                                               tgelveg90            -10.0
         O: flower/fruit frost (from FLO)                 2                                            
                   M1: no                                 0 
                   M2: yes                                2
                                                                     codgelflo            1
                   M2:                                               tgelflo10            -4.5
                   M2:                                               tgelflo90            -6.5
F: water                                                                                    
         NO:                                                         psisto               5.0
         NO:                                                         psiturg              2.0
         NO:                                                         h2ofeuilverte        0.90
         NO:                                                         h2ofeuiljaune        0.15
         NO:                                                         h2otigestruc         0.60
         NO:                                                         h2oreserve           0.80
         NO:                                                         h2ofrvert            0.80
         NO:                                                         deshydbase           0.000
         NO:                                                         tempdeshyd           0.000
         O: water requirements                             2                              
                   M1: crop coefficient                    1                              
                   M2: resistance approach                 1                              
                                                                     codebeso             1
                   M1:                                               kmax                 1.40000
                   M2:                                               rsmin                100.000
         O: interception of water by foliage               2                              
                   M1: yes                                 3                              
                   M2: no                                  0                              
                                                                     codeintercept        2
                   M1:                                               mouillabil              .55000
                   M1:                                               stemflowmax             .00000
                   M1:                                               kstemflow               .00000
F: nitrogen                                                                                  
         NO:                                                         Vmax1                   .00180
         NO:                                                         Kmabs1                50.00
         NO:                                                         Vmax2                   .05000
         NO:                                                         Kmabs2                25000.00
         NO:                                                         adil                   5.21000
         NO:                                                         bdil                    .56000
         NO:                                                         masecNmax              1.00000
         NO:                                                         INNmin                 0.3
         NO:                                                         INNimin                -0.5
         NO:                                                         inngrain1              2.0
         NO:                                                         inngrain2              2.0
         O: calculation nitrogen requirements              2                              
                   M1: dense canopies (initial)            2                              
                   M2: isolated plants (new calculation)   3                              
                                                                     codeplisoleN         1
                   M1:                                               adilmax              7.21000
                   M1:                                               bdilmax              0.56000
                   M2:                                               Nmeta                6.47
                   M2:                                               masecmeta            0.04
                   M2:                                               Nreserve             1.5
         O: calculation Nitrogen stress index (INN)        2
                   M1: INN cumulated                       0
                   M2: INN instantaneous                   0
                                                                     codeINN              1
         O: leguminous                                     2                              
                   M1: no                                  0                              
                   M2: yes                                 13                              
                                                                     codelegume           1
                   M2:                                               stlevdno             150.0
                   M2:                                               stdnofno             500.0
                   M2:                                               stfnofvino           300.0
                   M2:                                               vitno                0.003
                   M2:                                               profnod              40.0
                   M2:                                               concNnodseuil        1.3
                   M2:                                               concNrac0            1.2
                   M2:                                               concNrac100          0.4
                   M2:                                               tempnod1             0.0
                   M2:                                               tempnod2             30.0
                   M2:                                               tempnod3             36.0
                   M2:                                               tempnod4             50.0
                   O: maximal fixation capacity            2
                             M11: constant                 1 
                             M12: growth fonction          2
                                                                     codefixpot           2
                             M11:                                    fixmax               6.0
                             M12:                                    fixmaxveg            2.0
                             M12:                                    fixmaxgr             0.0
         O: nitrogen effect on fruit/grain number          2                              
                   M1: no                                  0                              
                   M2: yes (inns)                          0                              
                                                                     codazofruit          1
F: correspondance code BBCH                                                                                  
         NO:                                                         stadebbchplt         '-99'
         NO:                                                         stadebbchger         '-99'      
         NO:                                                         stadebbchlev         '-99'       
         NO:                                                         stadebbchamf         '-99'      
         NO:                                                         stadebbchlax         '-99'        
         NO:                                                         stadebbchsen         '-99'        
         NO:                                                         stadebbchflo         '-99'   
         NO:                                                         stadebbchdrp         '-99'      
         NO:                                                         stadebbchnou         '-99'     
         NO:                                                         stadebbchdebdes      '-99'      
         NO:                                                         stadebbchmat         '-99'   
         NO:                                                         stadebbchrec         '-99'   
         NO:                                                         stadebbchfindorm     '-99'       
F: cultivar parameters                                                                               
         TV: different cultivars                           1
                   M01: betterave                          22
                                                                     codevar              betterave
                   M01:                                              stlevamf             500
                   M01:                                              stamflax             5000
                   M01:                                              stlevdrp             450
                   M01:                                              stflodrp             0
                   M01:                                              stdrpdes             700.0
                   M01:                                              pgrainmaxi           2910
                   M01:                                              adens                -0.47
                   M01:                                              croirac              0.14
                   M01:                                              durvieF              68
                   O:  codebfroid                          1
                             M012:                                   jvc                  0
                   O:  codephot                            1
                             M011:                                   sensiphot            0
                   O:  codlainet                           2
                             M011:                                   stlaxsen             5000
                             M011:                                   stsenlan             5000.
                   O:  codeindetermin                      4
                             M011:                                   nbgrmax              19000
                             M011:                                   stdrpmat             5000
                             M012:                                   afruitpot            0.004
                             M012:                                   dureefruit           2000
