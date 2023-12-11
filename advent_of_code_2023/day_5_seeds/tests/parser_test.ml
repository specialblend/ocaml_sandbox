open Fun
open Seeds

let seeds_sample_text =
  {|
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
|}

let seeds_text =
  {|seeds: 41218238 421491713 1255413673 350530906 944138913 251104806 481818804 233571979 2906248740 266447632 3454130719 50644329 1920342932 127779721 2109326496 538709762 3579244700 267233350 4173137165 60179884

seed-to-soil map:
1389477588 1222450723 86190269
2369327568 3429737174 127508203
88123474 1366319913 182655004
1475667857 405321476 41320497
1258939826 536917987 41172751
1924266396 3404859218 24877956
1762699703 957158780 33280161
3452528837 3222194776 182664442
2196573512 1924266396 172754056
433176947 990438941 6166389
4047092335 2681059373 30705388
439343336 1626695089 181842577
1949144352 2940939059 125726128
979719551 446641973 90276014
2184073848 2711764761 12499664
4077797723 2605613670 27940277
2074870480 4240432416 54534880
3048538268 3066665187 104068222
621185913 1124514126 97936597
1157547656 773812762 84277017
1300112577 20266514 6655368
1151949413 1808537666 5598243
3435484067 2724264425 17044770
380487497 1308640992 52689450
1644897150 26921882 117802553
1516988354 996605330 127908796
1241824673 858089779 17115153
3932455534 2269053207 114636801
3319415958 3854507632 116068109
2129405360 4237225295 3207121
2729160001 2097020452 172032755
3635193279 3557245377 297262255
1795979864 578090738 107598550
2901192756 3970575741 147345512
4105738000 2416384374 189229296
0 685689288 88123474
1903578414 0 20266514
719122510 144724435 260597041
2132612481 3170733409 51461367
2496835771 2741309195 199629864
1306767945 1548974917 77720172
3200111916 4117921253 119304042
1069995565 875204932 81953848
3152606490 2633553947 47505426
270778478 1814135909 109709019
1384488117 1361330442 4989471
2696465635 2383690008 32694366

soil-to-fertilizer map:
1796371314 958475699 90518367
4004397333 4049196179 245771117
2175877891 3813840430 96544159
1966430612 3997904997 51291182
3155151482 799623922 79310846
4250168450 2358444962 15280909
4265449359 3910384589 29517937
3087542169 2534702057 67609313
1202725381 3631683738 113825873
852357580 2833874802 40691288
1452732352 2128818900 25726830
291197164 3745509611 68330819
1316551254 2602311370 60535393
2017721794 2764291908 69582894
498502503 445768845 353855077
3367678481 1860885729 203469524
3845535174 1124639771 94398512
1041749195 2373725871 160976186
2330424521 2874566090 757117648
1478459182 127856713 317912132
3234462328 1680414394 31771008
359527983 1219038283 138974520
893048868 1712185402 148700327
3571148005 1406027225 274387169
3939933686 2064355253 64463647
3266233336 2662846763 101445145
1886889681 878934768 79540931
87297932 2154545730 203899232
39283510 1358012803 48014422
2272422050 3939902526 58002471
1377086647 1048994066 75645705
2087304688 39283510 88573203

fertilizer-to-water map:
3988818582 3038666130 306148714
2927763871 3008779749 29886381
124309691 99049201 282856506
99049201 381905707 25260490
407166197 2131018623 602068357
3442767659 4213146266 81821030
2957650252 3344814844 485117407
3907802704 2927763871 81015878
1009234554 407166197 1723852426
3524588689 3829932251 383214015

water-to-light map:
1071892650 2651787028 57679970
1129572620 3396952543 81593150
1240611714 2163493623 488293405
0 2068015044 95478579
1211165770 3074252590 29445944
2592854025 0 138938366
2523843782 1948369545 69010243
924090948 883610805 76353493
2022159128 174281796 501684654
1000444441 2923208140 71448209
95478579 959964298 268093632
684655532 1228057930 239435416
3410916028 2709466998 213741142
363572211 1627286224 321083321
3324937342 2017379788 50635256
3183141431 3068711832 5540758
2939436746 3478545693 243704685
3624657170 2994656349 74055483
1728905119 3103698534 293254009
3930463154 3978112708 316854588
2731792391 675966450 207644355
3375572598 138938366 35343430
4247317742 3967989739 10122969
4257440711 3930463154 37526585
3188682189 3722250378 136255153
3698712653 1467493346 159792878

light-to-temperature map:
4148509456 1952010509 126270832
856886372 936932802 97162803
829640090 282271594 27246282
244444108 1274282332 107584318
1528329058 3192525971 211478915
2566760651 2178128911 792500107
478140779 1162859130 51849897
4274780288 1528329058 20187008
352028426 265852816 16418778
1739807973 2970629018 221896953
188336830 840381853 56107278
529990676 0 265852816
2424714410 1911677980 40332529
795843492 896489131 33796598
969062248 324530949 412804402
954049175 309517876 15013073
3359260758 3868594872 426372424
2465046939 3404004886 101713712
375094277 737335351 103046502
0 1214709027 59573305
2324866840 2078281341 99847570
1961704926 1548516066 247670884
2209375810 1796186950 115491030
59573305 1034095605 128763525
3785633182 3505718598 362876274
368447204 930285729 6647073

temperature-to-humidity map:
645925588 927807414 87140162
0 398577479 157531253
1936153073 3766846194 135269565
3964800672 3492411188 1957783
3660032389 3460150664 32260524
1374126579 1182630672 364804866
2334938774 2586583717 132274954
3729993364 4148156458 139151684
2071422638 2398735028 187848689
461859499 894601505 33205909
1128085880 3902115759 246040699
3966758455 1609937892 328208841
733065750 1045187965 45230417
3692292913 2718858671 37700451
1738931445 2854070578 51145436
3869145048 2758414954 95655624
446537472 670580619 15322027
3468753739 2905216014 191278650
1790076881 1547435538 62502354
3103241907 3096494664 363656000
778296167 1014947576 30240389
2467213728 3653280748 113565446
2739690951 1959797890 363550956
430471457 878535490 16066015
934173831 836762826 41772664
272165118 90734051 80200203
2259271327 1128614382 54016290
1852579235 2323348846 75386182
808536556 272940204 125637275
1927965417 4287308142 7659154
1935624571 1128085880 528502
352365321 194834068 72750816
495065408 685902646 150860180
248265304 170934254 23899814
975946495 556108732 114471887
157531253 0 90734051
3466897907 2756559122 1855832
2580779174 3494368971 158911777
425116137 267584884 5355320
2313287617 1938146733 21651157

humidity-to-location map:
2297594568 1304834363 199636291
964984478 962777545 102011627
3376226732 2612009119 78542873
3210191679 3257561655 73324720
960734175 2732971245 4250303
3552752951 3643184542 128526794
1654967093 1268999863 35834500
2805486965 2087320949 359714826
72263011 1608745500 171195806
4225512580 3861994731 69454716
1240952852 431398165 68767410
3695056291 298067962 76655045
1309720262 500165575 32124036
2768212426 260793423 37274539
258896561 532289611 263781213
1967976997 1084282606 71977571
2255175315 2690551992 42419253
1514000396 0 28227011
0 2539746108 72263011
3283516399 2447035775 92710333
1233825691 3330886375 7127161
614836670 2866104927 345897505
1341844298 88637325 172156098
2039954568 1779941306 215220747
3861994731 3931449447 363517849
1950982711 414403879 16994286
3454769605 3219988623 37573032
1690801593 3433704416 164490238
1855291831 3338013536 95690880
1195879484 1080226916 4055690
522677774 1995162053 92158896
2601505705 796070824 166706721
2497230859 1504470654 104274846
1199935174 380513362 33890517
3492342637 28227011 60410314
1066996105 2737221548 128883379
243458817 1064789172 15437744
3165201791 3598194654 44989888
3689265936 374723007 5790355
3681279745 3212002432 7986191
1542227407 1156260177 112739686
  |}

let%expect_test "parse_all sample" =
  seeds_sample_text
  |>| Parser.parse_almanac
  |>| Contract.show_almanac
  |>| print_endline;
  [%expect
    {|
    ([(Range (55, 68)); (Range (79, 93))],
     [[((Range (98, 100)), -48); ((Range (50, 98)), 2)];
       [((Range (15, 52)), -15); ((Range (52, 54)), -15); ((Range (0, 15)), 39)];
       [((Range (53, 61)), -4); ((Range (11, 53)), -11); ((Range (0, 7)), 42);
         ((Range (7, 11)), 50)];
       [((Range (18, 25)), 70); ((Range (25, 95)), -7)];
       [((Range (77, 100)), -32); ((Range (45, 64)), 36); ((Range (64, 77)), 4)];
       [((Range (69, 70)), -69); ((Range (0, 69)), 1)];
       [((Range (56, 93)), 4); ((Range (93, 97)), -37)]]) |}]

let%expect_test "parse_all seeds" =
  seeds_text
  |>| Parser.parse_almanac
  |>| Contract.show_almanac
  |>| print_endline;
  [%expect
    {|
  ([(Range (41218238, 462709951)); (Range (481818804, 715390783));
     (Range (944138913, 1195243719)); (Range (1255413673, 1605944579));
     (Range (1920342932, 2048122653)); (Range (2109326496, 2648036258));
     (Range (2906248740, 3172696372)); (Range (3454130719, 3504775048));
     (Range (3579244700, 3846478050)); (Range (4173137165, 4233317049))],
   [[((Range (1222450723, 1308640992)), 167026865);
      ((Range (3429737174, 3557245377)), -1060409606);
      ((Range (1366319913, 1548974917)), -1278196439);
      ((Range (405321476, 446641973)), 1070346381);
      ((Range (536917987, 578090738)), 722021839);
      ((Range (3404859218, 3429737174)), -1480592822);
      ((Range (957158780, 990438941)), 805540923);
      ((Range (3222194776, 3404859218)), 230334061);
      ((Range (1924266396, 2097020452)), 272307116);
      ((Range (990438941, 996605330)), -557261994);
      ((Range (2681059373, 2711764761)), 1366032962);
      ((Range (1626695089, 1808537666)), -1187351753);
      ((Range (2940939059, 3066665187)), -991794707);
      ((Range (446641973, 536917987)), 533077578);
      ((Range (2711764761, 2724264425)), -527690913);
      ((Range (2605613670, 2633553947)), 1472184053);
      ((Range (4240432416, 4294967296)), -2165561936);
      ((Range (3066665187, 3170733409)), -18126919);
      ((Range (1124514126, 1222450723)), -503328213);
      ((Range (773812762, 858089779)), 383734894);
      ((Range (20266514, 26921882)), 1279846063);
      ((Range (1808537666, 1814135909)), -656588253);
      ((Range (2724264425, 2741309195)), 711219642);
      ((Range (1308640992, 1361330442)), -928153495);
      ((Range (26921882, 144724435)), 1617975268);
      ((Range (996605330, 1124514126)), 520383024);
      ((Range (858089779, 875204932)), 383734894);
      ((Range (2269053207, 2383690008)), 1663402327);
      ((Range (3854507632, 3970575741)), -535091674);
      ((Range (4237225295, 4240432416)), -2107819935);
      ((Range (2097020452, 2269053207)), 632139549);
      ((Range (3557245377, 3854507632)), 77947902);
      ((Range (578090738, 685689288)), 1217889126);
      ((Range (3970575741, 4117921253)), -1069382985);
      ((Range (2416384374, 2605613670)), 1689353626);
      ((Range (685689288, 773812762)), -685689288);
      ((Range (0, 20266514)), 1903578414);
      ((Range (144724435, 405321476)), 574398075);
      ((Range (3170733409, 3222194776)), -1038120928);
      ((Range (2741309195, 2940939059)), -244473424);
      ((Range (1548974917, 1626695089)), -242206972);
      ((Range (4117921253, 4237225295)), -917809337);
      ((Range (875204932, 957158780)), 194790633);
      ((Range (2633553947, 2681059373)), 519052543);
      ((Range (1814135909, 1923844928)), -1543357431);
      ((Range (1361330442, 1366319913)), 23157675);
      ((Range (2383690008, 2416384374)), 312775627)];
     [((Range (958475699, 1048994066)), 837895615);
       ((Range (4049196179, 4294967296)), -44798846);
       ((Range (3813840430, 3910384589)), -1637962539);
       ((Range (3997904997, 4049196179)), -2031474385);
       ((Range (799623922, 878934768)), 2355527560);
       ((Range (2358444962, 2373725871)), 1891723488);
       ((Range (3910384589, 3939902526)), 355064770);
       ((Range (2534702057, 2602311370)), 552840112);
       ((Range (3631683738, 3745509611)), -2428958357);
       ((Range (2833874802, 2874566090)), -1981517222);
       ((Range (2128818900, 2154545730)), -676086548);
       ((Range (3745509611, 3813840430)), -3454312447);
       ((Range (2602311370, 2662846763)), -1285760116);
       ((Range (2764291908, 2833874802)), -746570114);
       ((Range (445768845, 799623922)), 52733658);
       ((Range (1860885729, 2064355253)), 1506792752);
       ((Range (1124639771, 1219038283)), 2720895403);
       ((Range (2373725871, 2534702057)), -1331976676);
       ((Range (2874566090, 3631683738)), -544141569);
       ((Range (127856713, 445768845)), 1350602469);
       ((Range (1680414394, 1712185402)), 1554047934);
       ((Range (1219038283, 1358012803)), -859510300);
       ((Range (1712185402, 1860885729)), -819136534);
       ((Range (1406027225, 1680414394)), 2165120780);
       ((Range (2064355253, 2128818900)), 1875578433);
       ((Range (2662846763, 2764291908)), 603386573);
       ((Range (878934768, 958475699)), 1007954913);
       ((Range (2154545730, 2358444962)), -2067247798);
       ((Range (1358012803, 1406027225)), -1318729293);
       ((Range (3939902526, 3997904997)), -1667480476);
       ((Range (1048994066, 1124639771)), 328092581);
       ((Range (39283510, 127856713)), 2048021178)];
     [((Range (3038666130, 3344814844)), 950152452);
       ((Range (3008779749, 3038666130)), -81015878);
       ((Range (99049201, 381905707)), 25260490);
       ((Range (381905707, 407166197)), -282856506);
       ((Range (2131018623, 2733086980)), -1723852426);
       ((Range (4213146266, 4294967296)), -770378607);
       ((Range (3344814844, 3829932251)), -387164592);
       ((Range (2927763871, 3008779749)), 980038833);
       ((Range (407166197, 2131018623)), 602068357);
       ((Range (3829932251, 4213146266)), -305343562)];
     [((Range (2651787028, 2709466998)), -1579894378);
       ((Range (3396952543, 3478545693)), -2267379923);
       ((Range (2163493623, 2651787028)), -922881909);
       ((Range (2068015044, 2163493623)), -2068015044);
       ((Range (3074252590, 3103698534)), -1863086820);
       ((Range (0, 138938366)), 2592854025);
       ((Range (1948369545, 2017379788)), 575474237);
       ((Range (883610805, 959964298)), 40480143);
       ((Range (174281796, 675966450)), 1847877332);
       ((Range (2923208140, 2994656349)), -1922763699);
       ((Range (959964298, 1228057930)), -864485719);
       ((Range (1228057930, 1467493346)), -543402398);
       ((Range (2709466998, 2923208140)), 701449030);
       ((Range (1627286224, 1948369545)), -1263714013);
       ((Range (2017379788, 2068015044)), 1307557554);
       ((Range (3068711832, 3074252590)), 114429599);
       ((Range (3478545693, 3722250378)), -539108947);
       ((Range (2994656349, 3068711832)), 630000821);
       ((Range (3103698534, 3396952543)), -1374793415);
       ((Range (3978112708, 4294967296)), -47649554);
       ((Range (675966450, 883610805)), 2055825941);
       ((Range (138938366, 174281796)), 3236634232);
       ((Range (3967989739, 3978112708)), 279328003);
       ((Range (3930463154, 3967989739)), 326977557);
       ((Range (3722250378, 3858505531)), -533568189);
       ((Range (1467493346, 1627286224)), 2231219307)];
     [((Range (1952010509, 2078281341)), 2196498947);
       ((Range (936932802, 1034095605)), -80046430);
       ((Range (282271594, 309517876)), 547368496);
       ((Range (1274282332, 1381866650)), -1029838224);
       ((Range (3192525971, 3404004886)), -1664196913);
       ((Range (2178128911, 2970629018)), 388631740);
       ((Range (1162859130, 1214709027)), -684718351);
       ((Range (1528329058, 1548516066)), 2746451230);
       ((Range (265852816, 282271594)), 86175610);
       ((Range (2970629018, 3192525971)), -1230821045);
       ((Range (840381853, 896489131)), -652045023);
       ((Range (0, 265852816)), 529990676);
       ((Range (1911677980, 1952010509)), 513036430);
       ((Range (896489131, 930285729)), -100645639);
       ((Range (324530949, 737335351)), 644531299);
       ((Range (309517876, 324530949)), 644531299);
       ((Range (3868594872, 4294967296)), -509334114);
       ((Range (3404004886, 3505718598)), -938957947);
       ((Range (737335351, 840381853)), -362241074);
       ((Range (1214709027, 1274282332)), -1214709027);
       ((Range (2078281341, 2178128911)), 246585499);
       ((Range (1548516066, 1796186950)), 413188860);
       ((Range (1796186950, 1911677980)), 413188860);
       ((Range (1034095605, 1162859130)), -974522300);
       ((Range (3505718598, 3868594872)), 279914584);
       ((Range (930285729, 936932802)), -561838525)];
     [((Range (927807414, 1014947576)), -281881826);
       ((Range (398577479, 556108732)), -398577479);
       ((Range (3766846194, 3902115759)), -1830693121);
       ((Range (3492411188, 3494368971)), 472389484);
       ((Range (3460150664, 3492411188)), 199881725);
       ((Range (1182630672, 1547435538)), 191495907);
       ((Range (2586583717, 2718858671)), -251644943);
       ((Range (4148156458, 4287308142)), -418163094);
       ((Range (2398735028, 2586583717)), -327312390);
       ((Range (894601505, 927807414)), -432742006);
       ((Range (3902115759, 4148156458)), -2774029879);
       ((Range (1609937892, 1938146733)), 2356820563);
       ((Range (1045187965, 1090418382)), -312122215);
       ((Range (2718858671, 2756559122)), 973434242);
       ((Range (2854070578, 2905216014)), -1115139133);
       ((Range (2758414954, 2854070578)), 1110730094);
       ((Range (670580619, 685902646)), -224043147);
       ((Range (2905216014, 3096494664)), 563537725);
       ((Range (1547435538, 1609937892)), 242641343);
       ((Range (3096494664, 3460150664)), 6747243);
       ((Range (1014947576, 1045187965)), -236651409);
       ((Range (3653280748, 3766846194)), -1186067020);
       ((Range (1959797890, 2323348846)), 779893061);
       ((Range (878535490, 894601505)), -448064033);
       ((Range (836762826, 878535490)), 97411005);
       ((Range (90734051, 170934254)), 181431067);
       ((Range (1128614382, 1182630672)), 1130656945);
       ((Range (2323348846, 2398735028)), -470769611);
       ((Range (272940204, 398577479)), 535596352);
       ((Range (4287308142, 4294967296)), -2359342725);
       ((Range (1128085880, 1128614382)), 807538691);
       ((Range (194834068, 267584884)), 157531253);
       ((Range (685902646, 836762826)), -190837238);
       ((Range (170934254, 194834068)), 77331050);
       ((Range (556108732, 670580619)), 419837763);
       ((Range (0, 90734051)), 157531253);
       ((Range (2756559122, 2758414954)), 710338785);
       ((Range (3494368971, 3653280748)), -913589797);
       ((Range (267584884, 272940204)), 157531253);
       ((Range (1938146733, 1959797890)), 375140884)];
     [((Range (1304834363, 1504470654)), 992760205);
       ((Range (962777545, 1064789172)), 2206933);
       ((Range (2612009119, 2690551992)), 764217613);
       ((Range (3257561655, 3330886375)), -47369976);
       ((Range (2732971245, 2737221548)), -1772237070);
       ((Range (3643184542, 3771711336)), -90431591);
       ((Range (1268999863, 1304834363)), 385967230);
       ((Range (2087320949, 2447035775)), 718166016);
       ((Range (1608745500, 1779941306)), -1536482489);
       ((Range (3861994731, 3931449447)), 363517849);
       ((Range (431398165, 500165575)), 809554687);
       ((Range (298067962, 374723007)), 3396988329);
       ((Range (500165575, 532289611)), 809554687);
       ((Range (260793423, 298067962)), 2507419003);
       ((Range (532289611, 796070824)), -273393050);
       ((Range (1084282606, 1156260177)), 883694391);
       ((Range (2690551992, 2732971245)), -435376677);
       ((Range (0, 28227011)), 1514000396);
       ((Range (2539746108, 2612009119)), -2539746108);
       ((Range (2447035775, 2539746108)), 836480624);
       ((Range (3330886375, 3338013536)), -2097060684);
       ((Range (2866104927, 3212002432)), -2251268257);
       ((Range (88637325, 260793423)), 1253206973);
       ((Range (1779941306, 1995162053)), 260013262);
       ((Range (3931449447, 4294967296)), -69454716);
       ((Range (414403879, 431398165)), 1536578832);
       ((Range (3219988623, 3257561655)), 234780982);
       ((Range (3433704416, 3598194654)), -1742902823);
       ((Range (3338013536, 3433704416)), -1482721705);
       ((Range (1080226916, 1084282606)), 115652568);
       ((Range (1995162053, 2087320949)), -1472484279);
       ((Range (796070824, 962777545)), 1805434881);
       ((Range (1504470654, 1608745500)), 992760205);
       ((Range (380513362, 414403879)), 819421812);
       ((Range (28227011, 88637325)), 3464115626);
       ((Range (2737221548, 2866104927)), -1670225443);
       ((Range (1064789172, 1080226916)), -821330355);
       ((Range (3598194654, 3643184542)), -432992863);
       ((Range (374723007, 380513362)), 3314542929);
       ((Range (3212002432, 3219988623)), 469277313);
       ((Range (1156260177, 1268999863)), 385967230)]
     ]) |}]
