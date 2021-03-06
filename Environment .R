#Ethan Stuart


#Data accessed on Feb 8, 2018 from :
#http://data.giss.nasa.gov/gistemp/tabledata_v3/GLB.Ts+dSST.txt
#t http://www.esrl.noaa.gov/gmd/obop/mlo/programs/esrl/methane/methane.html
#ftp://aftp.cmdl.noaa.gov/data/trace_gases/co2/flask/surface/co2_mlo_surface-flask_1_ccgg_month.txt


nasatemp <- read.table(header=TRUE, text="
Year   Jan  Feb  Mar  Apr  May  Jun  Jul  Aug  Sep  Oct  Nov  Dec    J-D D-N    DJF  MAM  JJA  SON  Year
1880   -29  -18  -11  -19  -11  -23  -21   -9  -16  -23  -20  -23    -19 ***   ****  -14  -18  -20  1880
1881   -15  -17    4    4    2  -20   -6   -2  -13  -20  -21  -10    -10 -11    -18    3   -9  -18  1881
1882    15   15    4  -18  -16  -26  -20   -5  -10  -24  -16  -24    -10  -9      6  -10  -17  -17  1882
1883   -31  -39  -13  -17  -20  -12   -8  -15  -20  -14  -22  -16    -19 -20    -31  -16  -12  -19  1883
1884   -15   -8  -37  -42  -36  -40  -34  -26  -27  -24  -30  -28    -29 -28    -13  -39  -34  -27  1884
1885   -58  -30  -25  -42  -42  -44  -35  -31  -23  -19  -19   -5    -31 -33    -39  -36  -36  -20  1885
1886   -42  -45  -38  -27  -26  -38  -21  -33  -25  -28  -31  -26    -32 -30    -30  -30  -31  -28  1886
1887   -72  -52  -34  -38  -32  -23  -23  -32  -22  -32  -23  -33    -35 -34    -50  -34  -26  -26  1887
1888   -37  -36  -41  -22  -22  -18  -10  -16  -10    2    1   -6    -18 -20    -35  -28  -15   -2  1888
1889   -11   18    8    7   -2  -14  -10  -20  -22  -22  -33  -30    -11  -9      0    4  -14  -26  1889
1890   -44  -45  -40  -37  -46  -27  -27  -38  -40  -24  -48  -31    -37 -37    -40  -41  -31  -37  1890
1891   -42  -49  -19  -29  -19  -21  -21  -16  -15  -22  -35   -5    -24 -27    -41  -22  -19  -24  1891
1892   -28  -12  -36  -35  -25  -23  -32  -25  -13  -12  -41  -38    -27 -24    -15  -32  -27  -22  1892
1893   -80  -54  -23  -30  -36  -28  -16  -27  -21  -17  -17  -35    -32 -32    -57  -29  -24  -18  1893
1894   -53  -34  -25  -48  -35  -44  -26  -22  -25  -21  -25  -20    -32 -33    -41  -36  -31  -24  1894
1895   -44  -43  -28  -25  -27  -24  -17  -16   -9   -8  -14  -15    -22 -23    -35  -27  -19  -11  1895
1896   -26  -14  -26  -30  -18  -14   -3   -4   -3   12   -7   -4    -11 -12    -18  -25   -7    0  1896
1897   -14  -15  -13   -2   -5  -16   -7  -11   -8  -12  -19  -19    -12 -10    -11   -7  -11  -13  1897
1898    -2  -28  -51  -32  -32  -20  -24  -25  -21  -34  -38  -25    -28 -27    -16  -38  -23  -31  1898
1899   -19  -41  -34  -21  -23  -33  -18   -7   -3   -5   12  -28    -18 -18    -28  -26  -19    1  1899
1900   -37   -3    1  -12  -10  -13  -13  -10   -5    9  -10   -7     -9 -11    -23   -7  -12   -2  1900
1901   -24   -4    8   -4  -17  -13  -15  -20  -22  -31  -17  -28    -15 -14    -11   -4  -16  -23  1901
1902   -20   -4  -29  -31  -36  -35  -29  -33  -30  -31  -39  -46    -30 -29    -17  -32  -33  -33  1902
1903   -26   -5  -23  -43  -45  -47  -38  -49  -49  -49  -45  -53    -39 -39    -26  -37  -44  -47  1903
1904   -67  -59  -51  -55  -56  -51  -54  -51  -53  -40  -22  -35    -49 -51    -60  -54  -52  -38  1904
1905   -40  -60  -23  -36  -33  -31  -29  -22  -19  -26  -10  -18    -29 -30    -45  -31  -28  -18  1905
1906   -28  -31  -17   -4  -25  -21  -25  -20  -27  -19  -39  -17    -23 -23    -26  -15  -22  -28  1906
1907   -42  -52  -26  -38  -47  -44  -37  -35  -33  -25  -49  -48    -40 -37    -37  -37  -39  -36  1907
1908   -43  -31  -54  -46  -41  -42  -40  -47  -36  -45  -50  -50    -44 -44    -41  -47  -43  -44  1908
1909   -69  -47  -52  -60  -56  -53  -45  -32  -36  -37  -31  -55    -48 -47    -55  -56  -43  -35  1909
1910   -45  -45  -52  -41  -36  -38  -35  -37  -37  -39  -54  -67    -44 -43    -48  -43  -37  -43  1910
1911   -63  -58  -60  -53  -52  -48  -41  -42  -37  -23  -18  -21    -43 -47    -63  -55  -43  -26  1911
1912   -26  -12  -35  -19  -22  -26  -44  -54  -51  -56  -38  -44    -36 -34    -20  -25  -41  -48  1912
1913   -42  -43  -43  -39  -46  -48  -36  -34  -35  -33  -19   -3    -35 -38    -43  -43  -39  -29  1913
1914     4  -11  -22  -29  -23  -26  -24  -14  -13   -4  -18   -8    -16 -15     -3  -25  -21  -12  1914
1915   -17    0   -9    6   -6  -20  -10  -19  -16  -24  -12  -16    -12 -11     -8   -3  -16  -18  1915
1916    -8  -12  -28  -29  -32  -46  -35  -25  -31  -26  -39  -79    -33 -27    -12  -30  -35  -32  1916
1917   -58  -61  -60  -50  -54  -43  -24  -21  -17  -40  -28  -66    -43 -45    -66  -55  -29  -28  1917
1918   -41  -31  -24  -44  -45  -35  -31  -29  -14   -5   -9  -27    -28 -31    -46  -38  -31   -9  1918
1919   -19  -22  -20  -13  -27  -37  -28  -32  -22  -18  -41  -41    -27 -25    -23  -20  -32  -27  1919
1920   -21  -22   -7  -25  -26  -35  -31  -24  -19  -26  -26  -43    -25 -25    -28  -19  -30  -23  1920
1921    -1  -18  -22  -29  -28  -27  -14  -23  -16   -2  -12  -16    -17 -20    -21  -26  -21  -10  1921
1922   -33  -43  -12  -23  -33  -32  -24  -30  -31  -31  -13  -17    -27 -27    -30  -22  -29  -25  1922
1923   -28  -36  -33  -39  -33  -25  -29  -29  -27  -11    2   -2    -24 -25    -27  -35  -27  -12  1923
1924   -21  -23   -6  -31  -18  -22  -25  -33  -31  -33  -19  -40    -25 -22    -15  -18  -27  -27  1924
1925   -36  -38  -25  -25  -30  -33  -27  -15  -16  -17    4    9    -21 -25    -38  -26  -25   -9  1925
1926    20    7   12  -12  -22  -22  -24  -12  -12   -9   -4  -28     -9  -6     12   -7  -19   -8  1926
1927   -27  -18  -37  -31  -25  -26  -17  -20  -10    0   -3  -32    -20 -20    -24  -31  -21   -4  1927
1928    -1   -7  -27  -28  -29  -37  -18  -22  -18  -17   -9  -17    -19 -20    -13  -28  -25  -15  1928
1929   -45  -59  -31  -39  -38  -40  -33  -30  -23  -13  -12  -53    -35 -32    -41  -36  -34  -16  1929
1930   -30  -27   -8  -25  -23  -20  -19  -13  -12   -9   16   -6    -15 -19    -37  -19  -17   -2  1930
1931   -10  -24   -8  -22  -20   -8   -3   -3   -7    2  -12   -9    -10 -10    -14  -17   -4   -6  1931
1932    14  -19  -19   -7  -19  -30  -24  -24  -12  -11  -27  -25    -17 -16     -5  -15  -26  -16  1932
1933   -34  -34  -30  -26  -29  -34  -21  -24  -27  -21  -30  -46    -30 -28    -31  -28  -27  -26  1933
1934   -25   -4  -33  -30  -11  -16  -11  -11  -16   -8    3   -4    -14 -17    -25  -25  -13   -7  1934
1935   -34   14  -13  -36  -31  -27  -23  -22  -21   -8  -28  -20    -21 -19     -8  -26  -24  -19  1935
1936   -28  -39  -22  -20  -18  -21   -8  -14   -9   -4   -4   -3    -16 -17    -29  -20  -14   -6  1936
1937   -13    3  -18  -17   -8   -5   -5    2   11    8    9  -12     -4  -3     -4  -14   -3   10  1937
1938     1   -3    8    4  -10  -18   -9   -6    2   11    3  -22     -3  -2     -4    1  -11    5  1938
1939    -7   -8  -20  -10   -6   -8   -8   -7   -7   -3    5   44     -3  -8    -13  -12   -8   -2  1939
1940    -6   12   15   18    9    8   12    4   13    7   17   27     11  13     17   14    8   12  1940
1941    21   32    8   17   14   11   21   14    2   33   24   20     18  19     27   13   15   20  1941
1942    25    1    9   11    8    4    0   -4   -5   -1    8    7      5   6     16    9    0    1  1942
1943    -4   14   -8    7    4   -6    8    0    6   22   20   24      7   6      6    1    1   16  1943
1944    35   26   26   19   19   20   18   17   30   25   10    2     21  23     29   22   18   22  1944
1945    10   -2    6   18    4   -1    2   27   22   18    7   -9      9  10      3   10    9   16  1945
1946    15    7    1    6   -8  -21  -14  -17   -7   -6   -6  -36     -7  -5      4   -1  -17   -6  1946
1947   -13   -8    7    7   -4    2   -8  -10  -13    5    2  -17     -4  -6    -19    3   -5   -2  1947
1948     7  -16  -26  -12    1   -5  -12  -13  -12   -8  -12  -23    -11 -10     -9  -12  -10  -11  1948
1949     9  -17   -3  -10   -9  -23  -14  -12  -14   -7  -10  -23    -11 -11    -10   -7  -17  -10  1949
1950   -34  -28   -7  -21  -10   -4  -10  -17  -12  -22  -38  -20    -19 -19    -28  -13  -10  -24  1950
1951   -35  -43  -21  -13    0   -2    3    6    9    5   -2   14     -7  -9    -33  -11    2    4  1951
1952    13   11   -9    2   -2   -4    4    4    7   -1  -14   -1      1   2     12   -3    2   -2  1952
1953     8   14   11   18    9   10    0    6    4    6   -5    2      7   7      7   13    5    2  1953
1954   -28  -13  -15  -16  -22  -17  -19  -16  -10   -4    6  -22    -15 -13    -13  -18  -17   -3  1954
1955    14  -18  -34  -21  -21  -11  -12    1  -11   -5  -24  -30    -14 -14     -8  -25   -8  -13  1955
1956   -16  -26  -23  -27  -31  -16  -12  -26  -20  -24  -17   -7    -20 -22    -24  -27  -18  -20  1956
1957   -11   -4   -6   -1    9   16    1   15    9    2    7   16      4   2     -7    1   11    6  1957
1958    39   24   11    3    7   -7    5   -3   -2    4    2    0      7   8     26    7   -2    2  1958
1959     8    9   18   13    4    2    6   -2   -6   -9   -9   -2      3   3      6   12    2   -8  1959
1960    -1   16  -34  -14   -7   -4   -3    2    7    7  -12   18     -2  -4      5  -18   -2    1  1960
1961     7   19    9   12   11   11    0    3    7    0    3  -15      6   8     15   11    5    3  1961
1962     7   14   12    4   -4    5    2   -1    1    0    7    0      4   3      2    4    2    3  1962
1963    -2   20  -14   -6   -4    5    9   27   20   15   15   -1      7   7      6   -8   14   17  1963
1964    -8  -13  -23  -31  -24   -2   -3  -21  -29  -31  -21  -30    -20 -17     -7  -26   -9  -27  1964
1965    -8  -17  -11  -19  -14   -9  -12   -3  -16   -5   -6   -6    -10 -12    -18  -14   -8   -9  1965
1966   -17    0    5  -13  -11    1   10   -8   -2  -15   -2   -5     -5  -5     -8   -6    1   -6  1966
1967    -6  -21    4   -5   15   -8    1    1   -6    9   -7   -3     -2  -2    -11    5   -2   -1  1967
1968   -26  -15   21   -5  -13   -8  -12   -9  -18   11   -3  -13     -7  -7    -15    1   -9   -4  1968
1969   -10  -14    1   18   19    5   -2    3    9   13   13   28      7   3    -12   13    2   12  1969
1970     9   22    8    9   -4   -2   -2   -9   11    6    2  -12      3   7     20    4   -4    6  1970
1971    -2  -20  -17   -8   -5  -19  -11   -3   -4   -6   -5   -9     -9  -9    -11  -10  -11   -5  1971
1972   -24  -16    2    0   -4    6    1   17    2    9    4   19      1  -1    -16    0    8    5  1972
1973    29   31   27   26   25   17   10    3   10   14    6   -6     16  18     26   26   10   10  1973
1974   -15  -29   -6  -12   -2   -6   -5   11  -12   -8   -7   -9     -8  -8    -16   -7    0   -9  1974
1975     7    6   13    5   16    0   -1  -19   -3   -9  -16  -17     -2  -1      1   11   -7   -9  1975
1976    -2   -6  -21   -8  -24  -16  -10  -14   -9  -26   -4   10    -11 -13     -8  -18  -13  -13  1976
1977    19   22   24   27   30   24   20   16    0    2   17    2     17  18     17   27   20    6  1977
1978     6   14   21   15    6   -2    3  -18    5    0   15   10      6   6      7   14   -6    7  1978
1979    12  -10   18   13    5   13    2   13   25   24   28   47     16  13      4   12   10   26  1979
1980    30   43   30   33   36   17   28   23   19   18   29   19     27  29     40   33   23   22  1980
1981    55   41   49   31   24   31   34   32   16   14   24   43     33  31     38   35   32   18  1981
1982    10   16   -1    9   16    5   13    7   14   13   15   42     13  13     23    8    8   14  1982
1983    52   40   42   31   36   20   17   33   39   16   32   17     31  33     45   36   23   29  1983
1984    30   17   29    8   33    5   17   16   21   16    6   -5     16  18     21   23   13   14  1984
1985    22   -6   17   11   17   18   -1   15   15   12    9   15     12  10      4   15   11   12  1985
1986    29   38   28   24   24   11   11   12    0   13   11   16     18  18     27   25   11    8  1986
1987    36   45   16   22   25   37   45   28   39   32   25   47     33  30     32   21   36   32  1987
1988    57   43   51   45   43   43   34   47   41   39   12   33     41  42     49   46   41   31  1988
1989    15   35   37   33   16   14   33   37   37   32   20   38     29  29     27   29   28   30  1989
1990    40   41   76   55   46   38   43   30   29   41   45   40     44  44     40   59   37   38  1990
1991    40   49   34   52   37   52   47   39   48   30   31   32     41  42     43   41   46   36  1991
1992    44   41   47   23   32   25   12    7    0    8    2   21     22  23     39   34   15    4  1992
1993    37   38   36   27   27   25   28   13   10   22    6   16     24  24     32   30   22   13  1993
1994    27    2   27   40   28   43   31   21   29   41   46   36     31  29     15   32   31   39  1994
1995    50   78   45   47   26   41   47   46   33   47   45   28     44  45     55   39   45   42  1995
1996    25   48   32   36   27   25   36   48   25   19   41   40     33  32     34   32   36   28  1996
1997    31   38   53   36   36   54   35   41   53   62   64   59     47  45     36   42   43   60  1997
1998    60   90   63   63   69   77   68   66   41   44   49   56     62  62     69   65   70   45  1998
1999    48   65   34   33   31   37   37   31   41   39   38   43     40  41     56   33   35   39  1999
2000    24   56   58   57   36   41   39   42   41   28   32   29     40  41     41   50   41   34  2000
2001    42   44   56   51   57   54   60   48   54   51   70   56     54  51     38   55   54   58  2001
2002    75   75   90   57   63   55   61   53   63   55   58   44     62  63     69   70   56   59  2002
2003    72   55   57   54   61   48   54   65   65   74   53   73     61  58     57   57   56   64  2003
2004    58   73   64   63   40   43   24   44   50   63   71   48     53  56     68   56   37   61  2004
2005    70   55   71   68   63   66   64   61   75   76   72   65     67  66     58   67   64   74  2005
2006    56   68   62   50   47   64   52   70   62   67   69   73     62  61     63   53   62   66  2006
2007    94   70   69   74   66   58   60   57   60   57   55   46     64  66     79   69   58   57  2007
2008    23   34   73   51   47   45   59   43   63   63   65   53     52  51     34   57   49   64  2008
2009    61   50   52   59   65   65   70   66   68   64   76   66     63  62     55   59   67   69  2009
2010    73   80   92   85   73   62   59   63   59   69   78   46     70  71     73   83   62   68  2010
2011    49   51   62   62   50   56   71   71   54   63   55   53     58  57     48   58   66   57  2011
2012    45   47   56   67   74   62   54   60   72   75   74   52     61  61     48   66   59   73  2012
2013    66   55   66   52   57   65   57   65   76   67   78   65     64  63     58   58   63   74  2013
2014    73   52   76   77   85   66   56   80   88   81   66   78     73  72     63   79   67   78  2014
2015    81   87   90   74   75   79   71   79   81  107  102  110     86  84     82   80   76   97  2015
2016   115  134  130  107   90   78   82   99   87   89   90   82     99 101    119  109   86   88  2016
2017    96  111  113   93   88   70   81   86   74   88   87   89     90  89     96   98   79   83  2017
")
# code to reshape to each row is a month (Thank you Dr. Grimshaw)
nasatemp<-nasatemp[,1:13] #just takes the months.
dim(nasatemp)
newtemp<-data.frame(temp=matrix(t(nasatemp[2:13]),ncol=1))
newtemp$month<-rep(1:12,138) 
newtemp$year<-rep(1880:2017,each=12)


maunaloa1<- read.table(header=TRUE,text='
site year month co2 
MLO 1969  8   322.51
MLO 1969  9   321.36
MLO 1969 10   320.74
MLO 1969 11   321.98
MLO 1969 12   323.78
MLO 1970  1   325.13
MLO 1970  2   325.54
MLO 1970  3   325.68
MLO 1970  4   326.58
MLO 1970  5   327.68
MLO 1970  6   327.30
MLO 1970  7   326.13
MLO 1970  8   324.91
MLO 1970  9   322.98
MLO 1970 10   322.64
MLO 1970 11   324.03
MLO 1970 12   325.54
MLO 1976  7   332.30
MLO 1976  8   331.04
MLO 1976  9   329.67
MLO 1976 10   329.33
MLO 1976 11   330.46
MLO 1976 12   331.97
MLO 1977  1   332.77
MLO 1977  2   333.10
MLO 1977  3   334.08
MLO 1977  4   335.78
MLO 1977  5   336.49
MLO 1977  6   335.96
MLO 1977  7   334.64
MLO 1977  8   332.31
MLO 1977  9   330.66
MLO 1977 10   330.55
MLO 1977 11   331.72
MLO 1977 12   333.74
MLO 1978  1   335.17
MLO 1978  2   335.78
MLO 1978  3   336.69
MLO 1978  4   337.76
MLO 1978  5   338.01
MLO 1978  6   337.57
MLO 1978  7   336.17
MLO 1978  8   333.89
MLO 1978  9   332.19
MLO 1978 10   332.20
MLO 1978 11   333.24
MLO 1978 12   334.64
MLO 1979  1   336.12
MLO 1979  2   337.20
MLO 1979  3   338.50
MLO 1979  4   339.00
MLO 1979  5   338.93
MLO 1979  6   338.97
MLO 1979  7   337.78
MLO 1979  8   335.57
MLO 1979  9   333.95
MLO 1979 10   334.41
MLO 1979 11   335.66
MLO 1979 12   336.81
MLO 1980  1   338.87
MLO 1980  2   340.17
MLO 1980  3   341.33
MLO 1980  4   342.82
MLO 1980  5   342.90
MLO 1980  6   341.74
MLO 1980  7   339.16
MLO 1980  8   337.04
MLO 1980  9   336.60
MLO 1980 10   336.60
MLO 1980 11   337.55
MLO 1980 12   339.18
MLO 1981  1   340.41
MLO 1981  2   341.39
MLO 1981  3   342.33
MLO 1981  4   343.35
MLO 1981  5   343.56
MLO 1981  6   342.34
MLO 1981  7   340.40
MLO 1981  8   338.67
MLO 1981  9   337.37
MLO 1981 10   337.53
MLO 1981 11   338.88
MLO 1981 12   340.21
MLO 1982  1   341.10
MLO 1982  2   341.99
MLO 1982  3   343.03
MLO 1982  4   343.82
MLO 1982  5   343.76
MLO 1982  6   342.87
MLO 1982  7   341.47
MLO 1982  8   339.26
MLO 1982  9   337.33
MLO 1982 10   337.64
MLO 1982 11   339.03
MLO 1982 12   340.66
MLO 1983  1   341.96
MLO 1983  2   342.37
MLO 1983  3   343.11
MLO 1983  4   345.12
MLO 1983  5   346.09
MLO 1983  6   345.30
MLO 1983  7   343.53
MLO 1983  8   341.20
MLO 1983  9   339.78
MLO 1983 10   339.98
MLO 1983 11   341.14
MLO 1983 12   342.73
MLO 1984  1   343.94
MLO 1984  2   344.59
MLO 1984  3   345.52
MLO 1984  4   346.81
MLO 1984  5   347.38
MLO 1984  6   346.62
MLO 1984  7   345.10
MLO 1984  8   342.86
MLO 1984  9   341.30
MLO 1984 10   341.70
MLO 1984 11   343.00
MLO 1984 12   343.92
MLO 1985  1   344.60
MLO 1985  2   345.65
MLO 1985  3   346.86
MLO 1985  4   347.80
MLO 1985  5   348.57
MLO 1985  6   348.19
MLO 1985  7   346.22
MLO 1985  8   343.94
MLO 1985  9   342.83
MLO 1985 10   342.87
MLO 1985 11   344.05
MLO 1985 12   345.31
MLO 1986  1   346.10
MLO 1986  2   346.61
MLO 1986  3   347.69
MLO 1986  4   349.29
MLO 1986  5   349.94
MLO 1986  6   349.06
MLO 1986  7   347.06
MLO 1986  8   345.26
MLO 1986  9   344.05
MLO 1986 10   343.81
MLO 1986 11   345.37
MLO 1986 12   346.84
MLO 1987  1   347.51
MLO 1987  2   348.25
MLO 1987  3   349.26
MLO 1987  4   350.75
MLO 1987  5   351.77
MLO 1987  6   351.40
MLO 1987  7   349.62
MLO 1987  8   347.48
MLO 1987  9   346.45
MLO 1987 10   346.81
MLO 1987 11   348.11
MLO 1987 12   349.42
MLO 1988  1   350.28
MLO 1988  2   351.35
MLO 1988  3   352.52
MLO 1988  4   353.72
MLO 1988  5   354.35
MLO 1988  6   353.83
MLO 1988  7   352.34
MLO 1988  8   350.12
MLO 1988  9   348.78
MLO 1988 10   349.12
MLO 1988 11   350.14
MLO 1988 12   351.61
MLO 1989  1   352.74
MLO 1989  2   353.14
MLO 1989  3   353.96
MLO 1989  4   355.54
MLO 1989  5   356.16
MLO 1989  6   355.45
MLO 1989  7   354.00
MLO 1989  8   351.37
MLO 1989  9   349.51
MLO 1989 10   349.90
MLO 1989 11   351.53
MLO 1989 12   352.88
MLO 1990  1   354.02
MLO 1990  2   355.11
MLO 1990  3   355.90
MLO 1990  4   356.71
MLO 1990  5   357.17
MLO 1990  6   356.32
MLO 1990  7   354.53
MLO 1990  8   352.44
MLO 1990  9   351.21
MLO 1990 10   351.52
MLO 1990 11   353.10
MLO 1990 12   354.66
MLO 1991  1   355.24
MLO 1991  2   356.09
MLO 1991  3   357.54
MLO 1991  4   358.68
MLO 1991  5   359.34
MLO 1991  6   358.28
MLO 1991  7   355.86
MLO 1991  8   353.82
MLO 1991  9   352.42
MLO 1991 10   352.53
MLO 1991 11   354.00
MLO 1991 12   355.44
MLO 1992  1   356.64
MLO 1992  2   357.29
MLO 1992  3   358.12
MLO 1992  4   359.17
MLO 1992  5   359.94
MLO 1992  6   359.48
MLO 1992  7   357.17
MLO 1992  8   355.02
MLO 1992  9   353.69
MLO 1992 10   353.56
MLO 1992 11   354.63
MLO 1992 12   355.97
MLO 1993  1   356.81
MLO 1993  2   357.39
MLO 1993  3   358.25
MLO 1993  4   359.56
MLO 1993  5   360.47
MLO 1993  6   359.69
MLO 1993  7   357.74
MLO 1993  8   355.54
MLO 1993  9   353.84
MLO 1993 10   354.20
MLO 1993 11   355.41
MLO 1993 12   356.90
MLO 1994  1   358.28
MLO 1994  2   358.86
MLO 1994  3   359.69
MLO 1994  4   361.04
MLO 1994  5   361.73
MLO 1994  6   360.83
MLO 1994  7   358.93
MLO 1994  8   356.94
MLO 1994  9   355.33
MLO 1994 10   355.55
MLO 1994 11   357.49
MLO 1994 12   359.57
MLO 1995  1   360.24
MLO 1995  2   360.61
MLO 1995  3   361.82
MLO 1995  4   362.93
MLO 1995  5   363.63
MLO 1995  6   363.28
MLO 1995  7   361.36
MLO 1995  8   359.17
MLO 1995  9   358.04
MLO 1995 10   358.09
MLO 1995 11   359.44
MLO 1995 12   361.02
MLO 1996  1   362.10
MLO 1996  2   363.20
MLO 1996  3   364.18
MLO 1996  4   364.81
MLO 1996  5   365.35
MLO 1996  6   365.13
MLO 1996  7   363.51
MLO 1996  8   361.13
MLO 1996  9   359.30
MLO 1996 10   359.38
MLO 1996 11   360.92
MLO 1996 12   362.02
MLO 1997  1   363.07
MLO 1997  2   363.89
MLO 1997  3   364.91
MLO 1997  4   366.56
MLO 1997  5   366.66
MLO 1997  6   365.49
MLO 1997  7   364.09
MLO 1997  8   362.15
MLO 1997  9   360.50
MLO 1997 10   360.71
MLO 1997 11   362.52
MLO 1997 12   364.16
MLO 1998  1   365.26
MLO 1998  2   366.04
MLO 1998  3   367.12
MLO 1998  4   368.65
MLO 1998  5   369.39
MLO 1998  6   368.98
MLO 1998  7   367.74
MLO 1998  8   365.74
MLO 1998  9   364.13
MLO 1998 10   364.14
MLO 1998 11   365.60
MLO 1998 12   367.52
MLO 1999  1   368.36
MLO 1999  2   368.63
MLO 1999  3   369.76
MLO 1999  4   370.89
MLO 1999  5   370.89
MLO 1999  6   370.42
MLO 1999  7   369.14
MLO 1999  8   366.95
MLO 1999  9   365.12
MLO 1999 10   365.43
MLO 1999 11   366.82
MLO 1999 12   368.36
MLO 2000  1   369.62
MLO 2000  2   370.13
MLO 2000  3   370.85
MLO 2000  4   371.81
MLO 2000  5   371.90
MLO 2000  6   371.38
MLO 2000  7   370.07
MLO 2000  8   368.11
MLO 2000  9   366.89
MLO 2000 10   367.14
MLO 2000 11   368.37
MLO 2000 12   369.61
MLO 2001  1   370.73
MLO 2001  2   371.81
MLO 2001  3   372.84
MLO 2001  4   373.69
MLO 2001  5   374.18
MLO 2001  6   373.36
MLO 2001  7   371.35
MLO 2001  8   369.41
MLO 2001  9   368.18
MLO 2001 10   368.45
MLO 2001 11   370.13
MLO 2001 12   371.43
MLO 2002  1   372.18
MLO 2002  2   372.94
MLO 2002  3   373.92
MLO 2002  4   374.80
MLO 2002  5   375.51
MLO 2002  6   375.55
MLO 2002  7   373.86
MLO 2002  8   371.19
MLO 2002  9   369.72
MLO 2002 10   370.45
MLO 2002 11   372.40
MLO 2002 12   373.65
MLO 2003  1   374.63
MLO 2003  2   375.77
MLO 2003  3   376.81
MLO 2003  4   378.08
MLO 2003  5   378.88
MLO 2003  6   378.26
MLO 2003  7   376.52
MLO 2003  8   374.53
MLO 2003  9   372.95
MLO 2003 10   373.13
MLO 2003 11   374.84
MLO 2003 12   376.03
MLO 2004  1   376.85
MLO 2004  2   377.81
MLO 2004  3   379.46
MLO 2004  4   380.74
MLO 2004  5   380.78
MLO 2004  6   379.62
MLO 2004  7   377.37
MLO 2004  8   375.56
MLO 2004  9   374.23
MLO 2004 10   374.54
MLO 2004 11   376.24
MLO 2004 12   377.61
MLO 2005  1   378.58
MLO 2005  2   380.19
MLO 2005  3   381.75
MLO 2005  4   382.15
MLO 2005  5   382.49
MLO 2005  6   382.25
MLO 2005  7   380.80
MLO 2005  8   378.62
MLO 2005  9   376.79
MLO 2005 10   377.05
MLO 2005 11   378.65
MLO 2005 12   380.10
MLO 2006  1   381.22
MLO 2006  2   381.92
MLO 2006  3   382.84
MLO 2006  4   384.73
MLO 2006  5   385.42
MLO 2006  6   384.06
MLO 2006  7   382.30
MLO 2006  8   380.06
MLO 2006  9   378.71
MLO 2006 10   379.11
MLO 2006 11   380.39
MLO 2006 12   381.80
MLO 2007  1   383.26
MLO 2007  2   384.42
MLO 2007  3   385.40
MLO 2007  4   386.49
MLO 2007  5   386.89
MLO 2007  6   386.31
MLO 2007  7   384.54
MLO 2007  8   381.65
MLO 2007  9   380.31
MLO 2007 10   381.16
MLO 2007 11   382.63
MLO 2007 12   384.06
MLO 2008  1   385.53
MLO 2008  2   385.71
MLO 2008  3   385.57
MLO 2008  4   387.05
MLO 2008  5   388.70
MLO 2008  6   388.46
MLO 2008  7   386.66
MLO 2008  8   384.24
MLO 2008  9   382.77
MLO 2008 10   382.91
MLO 2008 11   384.12
MLO 2008 12   385.66
MLO 2009  1   386.97
MLO 2009  2   387.71
MLO 2009  3   388.41
MLO 2009  4   389.34
MLO 2009  5   390.11
MLO 2009  6   389.52
MLO 2009  7   387.71
MLO 2009  8   385.84
MLO 2009  9   384.44
MLO 2009 10   384.60
MLO 2009 11   386.00
MLO 2009 12   387.51
MLO 2010  1   388.59
MLO 2010  2   389.72
MLO 2010  3   391.07
MLO 2010  4   392.60
MLO 2010  5   393.37
MLO 2010  6   392.25
MLO 2010  7   390.09
MLO 2010  8   388.00
MLO 2010  9   386.55
MLO 2010 10   387.21
MLO 2010 11   388.57
MLO 2010 12   390.06
MLO 2011  1   391.56
MLO 2011  2   392.14
MLO 2011  3   392.64
MLO 2011  4   393.69
MLO 2011  5   394.26
MLO 2011  6   393.80
MLO 2011  7   392.35
MLO 2011  8   389.96
MLO 2011  9   388.61
MLO 2011 10   389.16
MLO 2011 11   390.47
MLO 2011 12   392.17
MLO 2012  1   393.00
MLO 2012  2   393.25
MLO 2012  3   394.66
MLO 2012  4   396.56
MLO 2012  5   396.98
MLO 2012  6   395.86
MLO 2012  7   394.26
MLO 2012  8   392.19
MLO 2012  9   390.84
MLO 2012 10   391.11
MLO 2012 11   393.06
MLO 2012 12   394.64
MLO 2013  1   395.66
MLO 2013  2   396.80
MLO 2013  3   397.75
MLO 2013  4   398.70
MLO 2013  5   399.61
MLO 2013  6   399.15
MLO 2013  7   397.19
MLO 2013  8   395.01
MLO 2013  9   393.73
MLO 2013 10   393.87
MLO 2013 11   395.43
MLO 2013 12   397.03
MLO 2014  1   397.91
MLO 2014  2   398.33
MLO 2014  3   399.75
MLO 2014  4   401.64
MLO 2014  5   402.24
MLO 2014  6   401.39
MLO 2014  7   398.96
MLO 2014  8   396.52
MLO 2014  9   395.35
MLO 2014 10   395.67
MLO 2014 11   397.50
MLO 2014 12   399.26
MLO 2015  1   400.05
MLO 2015  2   400.38
MLO 2015  3   401.58
MLO 2015  4   403.68
MLO 2015  5   404.16
MLO 2015  6   402.92
MLO 2015  7   400.93
MLO 2015  8   398.90
MLO 2015  9   397.65
MLO 2015 10   398.23
MLO 2015 11   399.95
MLO 2015 12   401.53
MLO 2016  1   402.72
MLO 2016  2   403.62
MLO 2016  3   405.23
MLO 2016  4   407.35
MLO 2016  5   407.87
MLO 2016  6   406.57
MLO 2016  7   404.37
MLO 2016  8   401.69
MLO 2016  9   400.23
MLO 2016 10   401.47
MLO 2016 11   403.53
MLO 2016 12   404.66
')
maunaloa2 <- read.table(header=TRUE,text='
site year month methane
MLO 1983  5  1639.23
MLO 1983  6  1633.53
MLO 1983  7  1633.19
MLO 1983  8  1631.38
MLO 1983  9  1648.43
MLO 1983 10  1663.75
MLO 1983 11  1658.22
MLO 1983 12  1654.33
MLO 1984  1  1658.97
MLO 1984  2  1656.47
MLO 1984  3  1655.76
MLO 1984  4  1657.71
MLO 1984  5  1649.33
MLO 1984  6  1633.93
MLO 1984  7  1629.71
MLO 1984  8  1643.76
MLO 1984  9  1663.31
MLO 1984 10  1673.72
MLO 1984 11  1676.09
MLO 1984 12  1671.23
MLO 1985  1  1662.44
MLO 1985  2  1665.22
MLO 1985  3  1677.36
MLO 1985  4  1674.26
MLO 1985  5  1665.95
MLO 1985  6  1658.63
MLO 1985  7  1653.49
MLO 1985  8  1653.73
MLO 1985  9  1667.63
MLO 1985 10  1680.79
MLO 1985 11  1679.95
MLO 1985 12  1677.30
MLO 1986  1  1675.09
MLO 1986  2  1666.10
MLO 1986  3  1672.14
MLO 1986  4  1687.78
MLO 1986  5  1685.33
MLO 1986  6  1684.31
MLO 1986  7  1682.00
MLO 1986  8  1669.87
MLO 1986  9  1679.75
MLO 1986 10  1690.42
MLO 1986 11  1691.76
MLO 1986 12  1697.38
MLO 1987  1  1692.00
MLO 1987  2  1689.67
MLO 1987  3  1694.41
MLO 1987  4  1695.58
MLO 1987  5  1694.19
MLO 1987  6  1687.17
MLO 1987  7  1680.59
MLO 1987  8  1678.21
MLO 1987  9  1681.06
MLO 1987 10  1700.84
MLO 1987 11  1715.96
MLO 1987 12  1707.57
MLO 1988  1  1697.68
MLO 1988  2  1700.56
MLO 1988  3  1705.48
MLO 1988  4  1707.58
MLO 1988  5  1705.13
MLO 1988  6  1698.45
MLO 1988  7  1687.25
MLO 1988  8  1691.20
MLO 1988  9  1700.78
MLO 1988 10  1708.26
MLO 1988 11  1719.26
MLO 1988 12  1725.77
MLO 1989  1  1720.86
MLO 1989  2  1712.63
MLO 1989  3  1715.06
MLO 1989  4  1723.47
MLO 1989  5  1724.35
MLO 1989  6  1713.17
MLO 1989  7  1699.86
MLO 1989  8  1706.65
MLO 1989  9  1720.84
MLO 1989 10  1721.99
MLO 1989 11  1726.19
MLO 1989 12  1730.72
MLO 1990  1  1727.94
MLO 1990  2  1728.47
MLO 1990  3  1733.87
MLO 1990  4  1737.55
MLO 1990  5  1734.57
MLO 1990  6  1722.31
MLO 1990  7  1712.70
MLO 1990  8  1718.02
MLO 1990  9  1733.91
MLO 1990 10  1736.92
MLO 1990 11  1741.48
MLO 1990 12  1749.67
MLO 1991  1  1734.88
MLO 1991  2  1731.02
MLO 1991  3  1746.57
MLO 1991  4  1749.61
MLO 1991  5  1747.34
MLO 1991  6  1740.02
MLO 1991  7  1730.94
MLO 1991  8  1722.04
MLO 1991  9  1734.65
MLO 1991 10  1751.77
MLO 1991 11  1750.32
MLO 1991 12  1752.46
MLO 1992  1  1755.62
MLO 1992  2  1750.06
MLO 1992  3  1752.44
MLO 1992  4  1753.30
MLO 1992  5  1744.58
MLO 1992  6  1743.34
MLO 1992  7  1736.29
MLO 1992  8  1725.76
MLO 1992  9  1729.88
MLO 1992 10  1740.78
MLO 1992 11  1755.35
MLO 1992 12  1758.41
MLO 1993  1  1747.88
MLO 1993  2  1745.69
MLO 1993  3  1748.00
MLO 1993  4  1746.77
MLO 1993  5  1749.61
MLO 1993  6  1745.59
MLO 1993  7  1731.93
MLO 1993  8  1732.02
MLO 1993  9  1744.57
MLO 1993 10  1760.24
MLO 1993 11  1768.05
MLO 1993 12  1763.59
MLO 1994  1  1759.89
MLO 1994  2  1752.11
MLO 1994  3  1754.68
MLO 1994  4  1763.82
MLO 1994  5  1755.15
MLO 1994  6  1746.67
MLO 1994  7  1737.43
MLO 1994  8  1734.00
MLO 1994  9  1757.24
MLO 1994 10  1778.62
MLO 1994 11  1781.15
MLO 1994 12  1779.38
MLO 1995  1  1764.55
MLO 1995  2  1757.10
MLO 1995  3  1764.86
MLO 1995  4  1762.96
MLO 1995  5  1762.21
MLO 1995  6  1759.23
MLO 1995  7  1749.37
MLO 1995  8  1751.18
MLO 1995  9  1761.52
MLO 1995 10  1768.91
MLO 1995 11  1768.32
MLO 1995 12  1773.02
MLO 1996  1  1771.38
MLO 1996  2  1770.46
MLO 1996  3  1769.92
MLO 1996  4  1759.12
MLO 1996  5  1755.04
MLO 1996  6  1753.80
MLO 1996  7  1747.19
MLO 1996  8  1745.61
MLO 1996  9  1767.21
MLO 1996 10  1774.69
MLO 1996 11  1769.96
MLO 1996 12  1773.12
MLO 1997  1  1769.96
MLO 1997  2  1768.67
MLO 1997  3  1775.66
MLO 1997  4  1780.66
MLO 1997  5  1776.62
MLO 1997  6  1768.26
MLO 1997  7  1751.54
MLO 1997  8  1750.35
MLO 1997  9  1775.74
MLO 1997 10  1784.89
MLO 1997 11  1784.07
MLO 1997 12  1783.30
MLO 1998  1  1772.27
MLO 1998  2  1766.13
MLO 1998  3  1774.66
MLO 1998  4  1780.43
MLO 1998  5  1781.20
MLO 1998  6  1779.11
MLO 1998  7  1762.53
MLO 1998  8  1757.23
MLO 1998  9  1771.19
MLO 1998 10  1787.42
MLO 1998 11  1798.38
MLO 1998 12  1803.08
MLO 1999  1  1796.37
MLO 1999  2  1784.76
MLO 1999  3  1788.19
MLO 1999  4  1788.42
MLO 1999  5  1773.25
MLO 1999  6  1771.20
MLO 1999  7  1767.00
MLO 1999  8  1768.66
MLO 1999  9  1785.91
MLO 1999 10  1794.49
MLO 1999 11  1796.52
MLO 1999 12  1801.93
MLO 2000  1  1802.08
MLO 2000  2  1793.84
MLO 2000  3  1791.95
MLO 2000  4  1786.89
MLO 2000  5  1773.88
MLO 2000  6  1772.35
MLO 2000  7  1769.46
MLO 2000  8  1762.08
MLO 2000  9  1773.90
MLO 2000 10  1796.96
MLO 2000 11  1798.68
MLO 2000 12  1790.80
MLO 2001  1  1789.20
MLO 2001  2  1793.33
MLO 2001  3  1801.05
MLO 2001  4  1797.36
MLO 2001  5  1783.16
MLO 2001  6  1773.35
MLO 2001  7  1769.25
MLO 2001  8  1764.68
MLO 2001  9  1779.92
MLO 2001 10  1790.89
MLO 2001 11  1793.03
MLO 2001 12  1796.09
MLO 2002  1  1787.42
MLO 2002  2  1782.86
MLO 2002  3  1786.15
MLO 2002  4  1781.95
MLO 2002  5  1778.33
MLO 2002  6  1774.58
MLO 2002  7  1766.41
MLO 2002  8  1766.10
MLO 2002  9  1780.44
MLO 2002 10  1795.45
MLO 2002 11  1801.15
MLO 2002 12  1792.65
MLO 2003  1  1785.94
MLO 2003  2  1791.76
MLO 2003  3  1796.92
MLO 2003  4  1794.97
MLO 2003  5  1786.89
MLO 2003  6  1790.58
MLO 2003  7  1785.33
MLO 2003  8  1773.06
MLO 2003  9  1788.25
MLO 2003 10  1806.05
MLO 2003 11  1807.61
MLO 2003 12  1794.57
MLO 2004  1  1789.81
MLO 2004  2  1797.91
MLO 2004  3  1806.94
MLO 2004  4  1805.52
MLO 2004  5  1787.75
MLO 2004  6  1779.82
MLO 2004  7  1774.59
MLO 2004  8  1769.80
MLO 2004  9  1778.21
MLO 2004 10  1790.72
MLO 2004 11  1800.49
MLO 2004 12  1796.73
MLO 2005  1  1788.48
MLO 2005  2  1796.08
MLO 2005  3  1802.82
MLO 2005  4  1791.30
MLO 2005  5  1780.86
MLO 2005  6  1780.05
MLO 2005  7  1771.32
MLO 2005  8  1766.46
MLO 2005  9  1787.27
MLO 2005 10  1806.12
MLO 2005 11  1803.11
MLO 2005 12  1798.37
MLO 2006  1  1794.46
MLO 2006  2  1785.84
MLO 2006  3  1786.21
MLO 2006  4  1800.64
MLO 2006  5  1798.33
MLO 2006  6  1779.65
MLO 2006  7  1765.36
MLO 2006  8  1762.14
MLO 2006  9  1775.52
MLO 2006 10  1788.46
MLO 2006 11  1791.44
MLO 2006 12  1794.62
MLO 2007  1  1799.19
MLO 2007  2  1802.60
MLO 2007  3  1802.63
MLO 2007  4  1801.65
MLO 2007  5  1795.23
MLO 2007  6  1781.34
MLO 2007  7  1771.38
MLO 2007  8  1778.95
MLO 2007  9  1793.78
MLO 2007 10  1801.88
MLO 2007 11  1803.32
MLO 2007 12  1805.15
MLO 2008  1  1809.46
MLO 2008  2  1802.99
MLO 2008  3  1792.38
MLO 2008  4  1792.12
MLO 2008  5  1796.01
MLO 2008  6  1791.32
MLO 2008  7  1782.40
MLO 2008  8  1779.41
MLO 2008  9  1794.58
MLO 2008 10  1813.66
MLO 2008 11  1811.90
MLO 2008 12  1812.40
MLO 2009  1  1816.16
MLO 2009  2  1814.73
MLO 2009  3  1815.28
MLO 2009  4  1807.06
MLO 2009  5  1802.72
MLO 2009  6  1806.24
MLO 2009  7  1794.25
MLO 2009  8  1788.09
MLO 2009  9  1801.87
MLO 2009 10  1811.50
MLO 2009 11  1814.68
MLO 2009 12  1815.55
MLO 2010  1  1810.92
MLO 2010  2  1817.87
MLO 2010  3  1821.24
MLO 2010  4  1818.01
MLO 2010  5  1816.50
MLO 2010  6  1803.08
MLO 2010  7  1793.50
MLO 2010  8  1800.61
MLO 2010  9  1810.57
MLO 2010 10  1826.33
MLO 2010 11  1830.53
MLO 2010 12  1821.20
MLO 2011  1  1823.70
MLO 2011  2  1819.68
MLO 2011  3  1815.48
MLO 2011  4  1816.01
MLO 2011  5  1811.52
MLO 2011  6  1810.25
MLO 2011  7  1803.09
MLO 2011  8  1799.91
MLO 2011  9  1813.09
MLO 2011 10  1823.80
MLO 2011 11  1827.44
MLO 2011 12  1831.82
MLO 2012  1  1827.56
MLO 2012  2  1819.24
MLO 2012  3  1824.07
MLO 2012  4  1834.90
MLO 2012  5  1829.55
MLO 2012  6  1814.01
MLO 2012  7  1803.91
MLO 2012  8  1803.99
MLO 2012  9  1815.80
MLO 2012 10  1828.91
MLO 2012 11  1836.46
MLO 2012 12  1836.27
MLO 2013  1  1833.35
MLO 2013  2  1836.92
MLO 2013  3  1838.80
MLO 2013  4  1830.36
MLO 2013  5  1825.20
MLO 2013  6  1814.80
MLO 2013  7  1804.80
MLO 2013  8  1813.64
MLO 2013  9  1838.16
MLO 2013 10  1844.51
MLO 2013 11  1840.42
MLO 2013 12  1848.10
MLO 2014  1  1840.72
MLO 2014  2  1831.02
MLO 2014  3  1847.06
MLO 2014  4  1855.57
MLO 2014  5  1844.18
MLO 2014  6  1831.50
MLO 2014  7  1818.96
MLO 2014  8  1824.29
MLO 2014  9  1841.04
MLO 2014 10  1844.03
MLO 2014 11  1854.77
MLO 2014 12  1863.90
MLO 2015  1  1853.42
MLO 2015  2  1848.67
MLO 2015  3  1858.38
MLO 2015  4  1866.30
MLO 2015  5  1862.18
MLO 2015  6  1844.58
MLO 2015  7  1831.99
MLO 2015  8  1827.55
MLO 2015  9  1840.39
MLO 2015 10  1860.36
MLO 2015 11  1866.15
MLO 2015 12  1863.49
MLO 2016  1  1858.25
MLO 2016  2  1858.33
MLO 2016  3  1864.58
MLO 2016  4  1873.36
MLO 2016  5  1870.71
MLO 2016  6  1855.71
MLO 2016  7  1835.76
MLO 2016  8  1840.68
MLO 2016  9  1863.21
MLO 2016 10  1876.06
MLO 2016 11  1874.51
MLO 2016 12  1864.68
')

# merge datasets and clean up (Dr. Grimshaw's code)
climate<-merge(merge(newtemp,maunaloa1,by=c("year","month"),all.x=TRUE),maunaloa2,by=c("year","month"),all.x=TRUE)
climate<-subset(climate,!is.na(site.x) & !is.na(site.y) ) 
climate<-climate[,c("year","month","temp","co2","methane")]


tail(climate)
#EDA

co2m <- lm(temp~co2,data=climate)
summary(co2m)
plot(climate$co2,climate$temp,xlab="[CO2] (ppm)",ylab="Temperature(.01 Degree C above 1950-80 mean)",main="[CO2]XTemperature")

#CO2
#Correlation Coefficient: 1.003. R^2: .6203. Reasonably strong positive correlation betwen co2 and temp.

methm <- lm(temp~methane,data=climate)
summary(methm)
plot(climate$methane,climate$temp,xlab="[Methane] (ppb)",ylab="Temperature",main="[Methane]XTemperature (.01 Degree C above 1950-'80 mean)")

#Methane
# Correlation Coefficient: 0.32005. R^2: 0.5761. Decently strong positive correlation betwen methane and temp. 
# Not as positive or strong as CO2

#It's an observational study, so we can't ever conclude causation. Only correlation.



#Analysis:
# Response V: Temperature
# Explanatory Vs: Co2 Concentration and Methane concentration
#Model: temp = Bo + B1*CO2 + B2*Methane + E, E~N(0,sd^2)

# Estimates:hats- Bo,B1,B2,-S^2, std errors

climate.out <- lm(temp~co2 + methane,data=climate)
summary(climate.out)

#0.74107 Estimated partial slope of CO2. Standard Error: 0.099 When Methane is held constant, this is the slope.
# for a one unit change in CO2 (ppm) there is an estimated expected increase of 0.741 (.01 C) 
# in monthly earth temperature. 

#0.0944. Estimated partial slope of Methane.Std Err: .033 When CO2 levels are held constant, for a one unit
#change of Methane concentration(ppb) there is an estimated expected increase of 0.0944 (.01 C)
# in monthly earth temperature.

#Partial effect plots
library(car)
avPlots(climate.out)

# Inference: Ho: no greenhouse gas effect, B1=0 and B2=0
# ANOVA: testing a single hypothesis, comparing two models. (reduced model) 
#1)Assume Ho is true lm(temp ~ +1) the plus one is a built in default usually for a y intercept
#2)Assume Ha is true   2-- use climate.out lm(temp~co2+methane)

#Test Ho: no green house gas effect
# fit model assuming Ho is true
nogreenhousegas.climate <- lm(temp~ +1,data=climate)
anova(nogreenhousegas.climate,climate.out)
# DF two because of the two hypothesis parts, B1 and B2

# There is statistically significant greenhouse gas effect.
# Fstat: 338.47. P-val: <.00001
# With a P-value of <0.0001 we reject the null hypothesis and conclude that
# there is a significant greenhouse gas effect. 
# There is evidence from the data that suggests that there is a greenhouse effect.

#Followup: CO2 only, methane only, CO2 and methane
# T-test: Ho: B1=0; Ho: B2=0; can get from summary(climate.out). 
summary(climate.out)

# As followup to concluding there is a statistically significant greenhouse gas effect:
#95% CI 
confint(climate.out) #Does the interval cover zero? the intercept doesn't really matter, it's definitely significant.

#Write a paragraph summarizing anaylsis.

#Prediction: How good is our prediction going to be? Model okay?
#Data:  train and test. Test: last year. Train: before last year (2016 for this data)
# Fit model on train. Predict on test. Compare actual and prediction.

# to evaluate prediction performance on year into the future
# create train and test
climate.train <- subset(climate,year<= 2015)
climate.test <- subset(climate,year=2016)
# fit model
out.train.climate <- lm(temp~co2+methane,data=climate.train)
#evaluate prediction performance
predict(out.train.climate,newdata=climate.test)
#or
sides <- cbind(climate.test$temp,
      predict(out.train.climate,newdata=climate.test))
plot(predict(out.train.climate,newdata=climate.test),climate.test$temp)
abline(0,1,col="gray")

#Error
mean(abs(sides[,1]-sides[,2]))
#10.942 degrees C

# There is a large amount of data available, most of which can be lined up so that
# only a little bit of the temperature data was lost due to lack of CO2 
# and Methane data. Linear regression of two variables matches the data perfectly.

#Analysis weakness: Prediction was far too low for most of the values. 
# 10 degrees celsius average error isn't good. 

#Challenge: Using the data in the link, predict systolic blood pressure
#based on age and weight.
# http://college.cengage.com/mathematics/brase/understandable_statistics/7e/students/datasets/mlr/frames/frame.html

