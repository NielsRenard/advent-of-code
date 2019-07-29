{-# LANGUAGE NoImplicitPrelude #-}

module Year2016.Day06
  ()
where

import qualified Utils                         as U
import qualified Data.List.Split               as Split
import           Prelude                        ( last
                                                , head
                                                , read
                                                , (!!)
                                                )
import qualified RIO.Map                       as M
import           RIO
import qualified RIO.List                      as L
import qualified RIO.Text                      as T


answerOne =
  let mostFrequent xs =
          fst $ last $ L.sortBy (comparing snd) $ U.frequencies xs
      nthChar n = mostFrequent $ L.map (T.take 1) $ L.map (T.drop n) input
  in  T.concat $ L.map nthChar [0 .. 7]

answerTwo =
  let leastFrequent xs =
          fst $ head $ L.sortBy (comparing snd) $ U.frequencies xs
      nthChar n = leastFrequent $ L.map (T.take 1) $ L.map (T.drop n) input
  in  T.concat $ L.map nthChar [0 .. 7]


input = L.map
  T.pack
  [ "drhqjkbv"
  , "dtmukohn"
  , "pblnptvr"
  , "rggqrqre"
  , "ihknljci"
  , "ncoeigib"
  , "qbmbrgzv"
  , "zfahdmvg"
  , "mblaahfc"
  , "irafzbak"
  , "wghpbuwg"
  , "pyijzopp"
  , "dxpqsjkd"
  , "nhrosahg"
  , "ekfkwiqi"
  , "khgwbxsz"
  , "fkkinvci"
  , "xlmjkxql"
  , "rvzsdcve"
  , "josyoqlp"
  , "wnyzmbhh"
  , "bvkkwyxp"
  , "zdnlhnlv"
  , "hbgmztkb"
  , "yugdjalz"
  , "ittbvkxm"
  , "pbnxevdt"
  , "fptnvjdb"
  , "dsypeozk"
  , "gcjmtlcc"
  , "pqsnuztm"
  , "meilciol"
  , "uolptcbc"
  , "jueofzif"
  , "mryrumjt"
  , "jxmdacbz"
  , "plraftrm"
  , "ljgrwfiz"
  , "ligncqig"
  , "hrcmfkwa"
  , "kraqruef"
  , "mawxovke"
  , "fgwermvq"
  , "ipfzrpoz"
  , "rhfgebjy"
  , "ltqkdjag"
  , "dvgdhywz"
  , "ojnzjack"
  , "ktnqjzlc"
  , "uvcyfogs"
  , "anqjxrbb"
  , "rqedwxhi"
  , "iliuzgff"
  , "lanioopk"
  , "kbcwlndi"
  , "wuzfguwq"
  , "oclgdxdn"
  , "euqmnjxb"
  , "rwgdgwac"
  , "gepgmuuw"
  , "xmsfhugr"
  , "cmmgvoza"
  , "mrdnpirq"
  , "muorgsxa"
  , "awdmmpth"
  , "abwkhkot"
  , "hlztzuhm"
  , "qwbxfktf"
  , "gkdslbhk"
  , "rqppdpfj"
  , "juuuxmwh"
  , "sprwgdoi"
  , "eptrqcap"
  , "fdtoubid"
  , "lnkxrjrr"
  , "yxxiuuzb"
  , "kcbgcuno"
  , "ssszanxv"
  , "xajngqze"
  , "dqprwcbs"
  , "tmehzhsh"
  , "eeooqhko"
  , "eacbmenw"
  , "qpbwiznc"
  , "hoofmvah"
  , "noicaaqv"
  , "reqwbpnl"
  , "ctfbzydf"
  , "kkjvsvob"
  , "pwkramtw"
  , "txsebrtx"
  , "wpxomtjx"
  , "xwcxfihp"
  , "iabaswyj"
  , "ubvgfrsw"
  , "yhxvrlwg"
  , "kyighkhi"
  , "kfmcjeci"
  , "wtxvnufi"
  , "eggyqgwj"
  , "azywqbdk"
  , "egwprvgp"
  , "gxtgzqek"
  , "hbypquhd"
  , "oqbpykib"
  , "bsaybmgo"
  , "ctzhjzgf"
  , "uqcatiea"
  , "fkquufln"
  , "mzuepzcn"
  , "rwteqddf"
  , "uandcifd"
  , "chqjvavf"
  , "xrugbecb"
  , "bynqmlhg"
  , "keruzmmy"
  , "tpxbrwkw"
  , "czawkslt"
  , "duunzdgl"
  , "mhhrgsfi"
  , "lhiorqvn"
  , "uroxbpki"
  , "uyqvmdvr"
  , "wbvzxinc"
  , "hvjbywqh"
  , "opztxiye"
  , "jnkvbsss"
  , "awfhhjuh"
  , "wldwnsms"
  , "vzdtdfzz"
  , "qcgjdsxf"
  , "ncungtbd"
  , "bsfmblxe"
  , "iztafyde"
  , "omxfacnb"
  , "bfjgzohh"
  , "idowhvnt"
  , "etxcropc"
  , "dlebejbz"
  , "dpgrvvyg"
  , "yyetaiyd"
  , "zmvvslxw"
  , "iolevdzw"
  , "pwrbpwyt"
  , "ncxnbdcs"
  , "bizuessl"
  , "csomkqnr"
  , "gukypdxo"
  , "zzazkgze"
  , "mhygipbc"
  , "nidnvzql"
  , "oblusiue"
  , "rnuvqhpy"
  , "ybeycdic"
  , "sbzmvzxq"
  , "gvvqcrug"
  , "zkdknqod"
  , "zkztprqv"
  , "xlprotcp"
  , "sndzhrmt"
  , "cntdipcw"
  , "siiqfres"
  , "pnfcbzjo"
  , "twbgdnrv"
  , "zzxnngor"
  , "nmcmxqgt"
  , "jayxwvrm"
  , "pahclzsw"
  , "ztddtnyo"
  , "odidwauv"
  , "tzlaantg"
  , "nttefszm"
  , "jahhbgpt"
  , "pvxsgjsl"
  , "gewlnlqm"
  , "bykgsqzo"
  , "zqrrhrkl"
  , "isxyqlbv"
  , "wfzkqgvt"
  , "mcwizlzj"
  , "ugtgyrez"
  , "qfyzoall"
  , "cfyqxoyx"
  , "vdszizjm"
  , "bgrcpovm"
  , "einnyfdv"
  , "uugncaps"
  , "letylmon"
  , "ocltwlem"
  , "owfskawk"
  , "hvripclk"
  , "jmgrulzy"
  , "kjxyhhle"
  , "ionkwbuu"
  , "tmjsudwf"
  , "kxocxtoa"
  , "ejbuoost"
  , "nzqdzsdd"
  , "dkuiiisp"
  , "dxwhxbvj"
  , "ghipctmd"
  , "mwmcfifl"
  , "vfptvuxo"
  , "zfqsjsbx"
  , "peazobjt"
  , "nlwomnpb"
  , "ndxbvgqn"
  , "zonnhffl"
  , "vjnyiejx"
  , "ghrcsxgl"
  , "fsdiwdmy"
  , "jmhtpgis"
  , "sscjmpev"
  , "oivfuctm"
  , "zwtllrfo"
  , "uuzpmnjx"
  , "wgpccyiu"
  , "ujhilevq"
  , "hjjieaag"
  , "dfbfylpe"
  , "ypegnmyg"
  , "vjnjrgcu"
  , "enylcycn"
  , "ptlneeqo"
  , "nfiplyjo"
  , "dxcrmicb"
  , "vdxohgvl"
  , "iycioixk"
  , "rwqrjyyu"
  , "rfatusnv"
  , "mnyvdlhu"
  , "piyhcljq"
  , "edcykiom"
  , "wrtwvqub"
  , "ejtepubt"
  , "vegruhiu"
  , "qmrlbprr"
  , "wntjsebz"
  , "hkdkuasw"
  , "jxkfblck"
  , "knrwttyx"
  , "pmjitnry"
  , "lrtbermt"
  , "qkhtmeoe"
  , "skmqdpek"
  , "fmxlqqhn"
  , "ureaitwq"
  , "qufcyfph"
  , "wcidvwgt"
  , "ybkrmqem"
  , "qhuacrls"
  , "yvnsqqdb"
  , "xpjchodx"
  , "lcwzkemd"
  , "jokzbvsi"
  , "jjbklvqq"
  , "xfswceep"
  , "xsjwsymu"
  , "slmsyksa"
  , "oaquyavd"
  , "zxteczie"
  , "yglonpuu"
  , "rdfvsbno"
  , "wgxhuxga"
  , "kvpetmdr"
  , "fjeoobow"
  , "atdqjabp"
  , "prjazwst"
  , "yqkisdog"
  , "wvswvdtm"
  , "iogvloma"
  , "srxeqnqq"
  , "ipjfezkx"
  , "joonxwtz"
  , "yjovntqa"
  , "svbbkcvw"
  , "mgyseuqr"
  , "dxflpkvp"
  , "gqbiytmk"
  , "siccxtsn"
  , "oepstegr"
  , "uwjjhhqb"
  , "eosfjfhv"
  , "dsaqqhda"
  , "vudzwxak"
  , "njzjiowv"
  , "anfpqwsz"
  , "geuqffcr"
  , "vhhbkgeb"
  , "flkqpzbn"
  , "fgtdspvd"
  , "yjhxwcps"
  , "aimfdnpv"
  , "hhvklxlo"
  , "yjoxsxhj"
  , "bllophbc"
  , "ntclfhgs"
  , "gtnsuqdp"
  , "dazhoeap"
  , "klczitkw"
  , "tlkedeuy"
  , "cvbuidmk"
  , "vjubfgqg"
  , "qimvfpxg"
  , "hnqegigv"
  , "cppyezxe"
  , "czcmiytj"
  , "ypvezoca"
  , "adjxiooc"
  , "mdshbmjd"
  , "urthwyqf"
  , "dhoijcrh"
  , "vxnnyszn"
  , "ttzkydfs"
  , "lhnbywji"
  , "tyiuyhxa"
  , "fyryagxi"
  , "hboupxaq"
  , "urctvbue"
  , "cirtbbfu"
  , "bkoxlmkm"
  , "rdeoosjs"
  , "qemhixen"
  , "zqfioppk"
  , "vopwlhhe"
  , "gmpihxop"
  , "aamsrrzs"
  , "sdyssprk"
  , "hmrqkghm"
  , "oevtvzwl"
  , "bqufyyuu"
  , "ennrxvaf"
  , "slmshjpz"
  , "qgraeety"
  , "mczjxfan"
  , "wzwmupvu"
  , "sqbkhwxg"
  , "abbotwty"
  , "trafaoli"
  , "bkuarvfz"
  , "wuffaong"
  , "fqevpper"
  , "eekwoblz"
  , "spsztgee"
  , "yjfbfeif"
  , "qcjdtsez"
  , "omujkwzt"
  , "vjpfndxp"
  , "hkpxvjix"
  , "eiigrazh"
  , "jmtdqwuu"
  , "jnsfvufm"
  , "xhkdzgjf"
  , "rvqigugc"
  , "bivqnzgu"
  , "ydadmvyq"
  , "ghsohaqa"
  , "eulugttl"
  , "nvhaafrh"
  , "ikdtvxpu"
  , "fbmztykr"
  , "gmhluyfq"
  , "biovnlho"
  , "xinmgiwl"
  , "xsvlnvnr"
  , "gryuussb"
  , "eeqmavbo"
  , "fuftdkgb"
  , "wwmwybtx"
  , "txshabuj"
  , "ewxcrjmj"
  , "lskdajks"
  , "pabjhzen"
  , "xzfmdhaj"
  , "gojxghyk"
  , "iyqaryra"
  , "bnlovokf"
  , "mehlaadw"
  , "tzqhtnhv"
  , "kgacrpdt"
  , "qxawodku"
  , "fcrouumv"
  , "yzqxkmgi"
  , "xzspfhmp"
  , "omhlnexu"
  , "olstosyp"
  , "gxslgwcn"
  , "juamcglq"
  , "irdvybpr"
  , "ncsacfpd"
  , "hczoulhg"
  , "kqjpowtq"
  , "bzefqjnj"
  , "qnrtwygz"
  , "rztxjfyr"
  , "lcpxloro"
  , "gdibhhkc"
  , "qgwuyhea"
  , "uweyjukp"
  , "hsljwmyy"
  , "ayhxycnx"
  , "klzvtttr"
  , "lgdmpcww"
  , "cqvtjkyw"
  , "rfyjuybh"
  , "tskdjzrt"
  , "mhwspcvf"
  , "xplpnemj"
  , "lrjvgjgs"
  , "crllldzy"
  , "ikczhybc"
  , "oolwtfoj"
  , "pqfgligu"
  , "cdgktmmx"
  , "jbpatjkl"
  , "keqepeax"
  , "qfirnsdi"
  , "juyzjarn"
  , "tuymbnri"
  , "jijsmffx"
  , "xfnbxvzo"
  , "oxmscdkf"
  , "hkfcgeuv"
  , "mkkuxffi"
  , "vjmccxrb"
  , "wrpagtbd"
  , "wcnmlred"
  , "pafkwcph"
  , "tcvjkxyl"
  , "stvhdkom"
  , "cfzhzuif"
  , "avdzxwyd"
  , "wdoyqfpy"
  , "gtolniag"
  , "nvznsjsi"
  , "cabyykqf"
  , "bxfoznta"
  , "mobkmnwc"
  , "sauywroh"
  , "xexkiuyy"
  , "oyevexto"
  , "vqegjclw"
  , "xoifoyic"
  , "rshyscwd"
  , "hvreaslo"
  , "negmieaw"
  , "dmfggrux"
  , "yxmknfpx"
  , "ttyuarnj"
  , "tiwewqnv"
  , "fgmplqux"
  , "fsdeavfu"
  , "uvqczvae"
  , "dydssjnh"
  , "ihlykqnj"
  , "xygdlwae"
  , "usyabtdd"
  , "yqikfwvv"
  , "nvqrjqzh"
  , "rexbjqsu"
  , "nzerccmq"
  , "apzjtxxq"
  , "dgnykfrj"
  , "izahycnb"
  , "ruubddeh"
  , "xkrbsmak"
  , "eooaoaxz"
  , "alwnxxey"
  , "rpxrikkc"
  , "syafkuqf"
  , "fqwavgfy"
  , "jlrielnd"
  , "ylmuftje"
  , "jigqlhyu"
  , "fdbusljj"
  , "ltsvqjss"
  , "ryzsnxja"
  , "sdvtoolp"
  , "ttvfgkan"
  , "zzbzmsdh"
  , "oneyivvm"
  , "lmkooris"
  , "qbwaumyc"
  , "upooekoi"
  , "oaplwhwf"
  , "zvvsffan"
  , "hjujkmzt"
  , "ksocowvu"
  , "xmaxctph"
  , "uohgaowz"
  , "wyiqttrj"
  , "apdmkleo"
  , "ngirmkie"
  , "zdmekxlr"
  , "zghwmffq"
  , "kkwtxmht"
  , "aopxxxla"
  , "cwaxcndp"
  , "mchejzuf"
  , "qiisajma"
  , "paqlvgwk"
  , "neepulbv"
  , "locpxcge"
  , "zqihibab"
  , "mpwwkcnc"
  , "acltdkmx"
  , "tsveirfy"
  , "oyqnektx"
  , "roljqjyy"
  , "tppfxdlm"
  , "pdkdgzbj"
  , "hqohdklw"
  , "ffaqesqq"
  , "cluhwwfj"
  , "bfwsfqjl"
  , "yxeforet"
  , "auhwgphh"
  , "wqeumebp"
  , "stagigac"
  , "typntncz"
  , "hdlxeizg"
  , "daaredsw"
  , "tbgzwdkl"
  , "jnncvszy"
  , "vzzojziz"
  , "uhuiphch"
  , "sflgjnmx"
  , "zjcebfsa"
  , "psvusroj"
  , "isyddacs"
  , "qjnifltk"
  , "lxsjtnwd"
  , "gfrahlus"
  , "vvzirpuo"
  , "zxscyfrr"
  , "asdpeiwa"
  , "hzrjpvlj"
  , "lubqxhin"
  , "kauqzmng"
  , "vlchbkuk"
  , "baivdnom"
  , "cuwbwhml"
  , "inwmiyfq"
  , "ywvdnsqq"
  , "sohppxig"
  , "vnvqqmjy"
  , "aaekdkci"
  , "byyylakp"
  , "vwctbbrq"
  , "pguveyxu"
  , "ccbkkuqw"
  , "qmtotaum"
  , "bbghdbpn"
  , "miuprwir"
  , "lkjlnmus"
  , "zhvpqpwz"
  , "xtxzqnix"
  , "lxckscjy"
  , "lkvlzrta"
  , "feoujqdq"
  , "myecxjgd"
  , "dwkmzbor"
  , "gfdlcijs"
  , "abwlgpdc"
  , "vwspenjl"
  , "cigbugvr"
  , "ugjalecy"
  , "klbkpbsg"
  , "qraxrapw"
  , "vyjcmvmy"
  , "ffhvruro"
  , "eluiytkn"
  ]
