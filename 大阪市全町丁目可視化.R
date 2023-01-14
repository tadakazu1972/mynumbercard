#エクセル　全区データ　前処理
#カンマがCSVに残るので、事前に列ごと数値、カンマなしに変換
#%もCSVに残ると文字として認識されるので小数点３位までの数値に変換しておく
#秘匿処理として、総数６人以下の町丁目の取得率等は空白にする＝色をつけない（６人の基準は市HPの住民基本台帳データの秘匿処理基準と合わせた）
#町丁目の字体はshapeファイルの字体にあわせる。アンマッチ処理、修正する町丁目名は後述参照

#ライブラリ読込
library(dplyr)
library(sf)
library(readr)
library(RColorBrewer)
library(classInt)

#作業ディレクトリ指定
#mac setwd("~/Desktop/mynacard")
setwd("C:/Users/tadak/Desktop/mynacard")

#必要なファイル読込
shape <- st_read("h27ka27.shp") #eStatから大阪府を取得
#大阪府全域のデータから大阪市のみ抽出 2レコード目から1914レコードまで。その後にカンマを忘れないこと。そのあとは全てのデータを含む意味で何も描かないのも注意
shape <- shape[2:1914,]

#マイナンバーカード取得率　全町丁目別データ 文字コードShift-JISを変換指定しないとエラーになる
data1 <- read_csv("data1130.csv", locale=locale(encoding="SJIS")) 

#shapeファイルとcsvファイルを町丁目名をキーに結合
data <- left_join(shape, data1, by=c("MOJI"="町丁目名"))

#40%以下,40-49%,50-59%,60-69%,70%以上の５区分で色分けした場合
column <- "取得率_1130" #ここを取得者数、増減とカラム名に変更していく　数字は「全角」に注意!
col_pal <- data[[column]] %>% classIntervals(., 6, style="fixed", fixedBreaks=c(0,0.4,0.5,0.6,0.7,1)) %>% findColours(.,pal=brewer.pal(6,"RdYlGn"))  
plot(st_geometry(shape[7]), col=col_pal, main="カード取得率 2022/11/30")
legend("topleft", fill=attr(col_pal, "palette"), legend=c("<40%","40%-","50%-","60%-","70%<"),bty="n")

#取得者数
column <- "取得者数" #ここを取得者数、増減とカラム名に変更していく　数字は「全角」に注意!
#一度kmeansで計算して、後にfixedでわかりやすい区切りを作成
#col_pal <- data[[column]] %>% classIntervals(., 5, style="kmeans") %>% findColours(.,pal=brewer.pal(5,"YlGnBu")) 
col_pal <- data[[column]] %>% classIntervals(., 6, style="fixed", fixedBreaks=c(0,500,1000,1500,3000,10000)) %>% findColours(.,pal=brewer.pal(6,"YlGnBu"))
plot(st_geometry(shape[7]), col=col_pal, main="カード取得者数 2022/11/30")
#自動計算された区分けの数値を表示
#legend("topleft", fill=attr(col_pal, "palette"), legend=names(attr(col_pal,"table")),bty="n")
legend("topleft", fill=attr(col_pal, "palette"), legend=c("<500人","500人-","1000人-","1500人-","3000-10000人"),bty="n")


##################################################################
# 町丁目の字体合わせ処理
#必要なファイル読込
shape <- st_read("h27ka27.shp") #eStatから大阪府を取得
#大阪府全域のデータから大阪市のみ抽出 2レコード目から1914レコードまで。その後にカンマを忘れないこと。そのあとは全てのデータを含む意味で何も描かないのも注意
shape <- shape[2:1914,]

#マイナンバーカード取得率　全町丁目別データ 文字コードShift-JISを変換指定しないとエラーになる
data1 <- read_csv("data1130.csv", locale=locale(encoding="SJIS")) 

#data1のカラム名表示
#names(data1)
#[1] "基準日"      "区"          "町丁目名"    "世帯数"      "総数"        "取得者数"    "取得率_0831" "取得率_1130"　[9] "増減"   

#まずは総務省データと大阪市住基システムの町丁目名文字コードの違いを見つける
unmatch <- anti_join(shape, data1, by=c("MOJI"="町丁目名"))
unmatch$MOJI
#[1] 夢洲東           夢洲             夢洲中           <NA>             <NA>             <NA>   #[7] 上本町           堂ヶ芝１丁目     堂ヶ芝２丁目     <NA>             桃ケ池町１丁目   桃ケ池町２丁目
#[13] 帝塚山           万代             南港南           南港南           南港南           南港南    [19] 曽根崎１丁目     曽根崎２丁目     曽根崎新地１丁目 曽根崎新地２丁目

#念の為
unmatch2 <- anti_join(data1, shape, by=c("町丁目名"="MOJI"))
unmatch2$町丁目名
# [1] "曾根崎１丁目"     "曾根崎２丁目"     "曾根崎新地１丁目" "曾根崎新地２丁目" "夢洲東１丁目"    
# [6] "上町１丁目"       "上本町５丁目"     "堂ケ芝１丁目"     "堂ケ芝２丁目"     "帝塚山１丁目"    
#[11] "万代１丁目"       "桃ヶ池町１丁目"   "桃ヶ池町２丁目"  

#csvデータの漢字を総務省データに合わせにいって上書き保存（そちらのほうが修正早い）
#曾根崎 > 曽根崎
#堂ケ芝 > 堂ヶ芝
#桃ヶ池 > 桃ケ池
#夢洲東１丁目 > 夢洲東
#上町１丁目 > 数値を上町に統合してこの行を削除
#上本町５丁目 > 上本町　（１丁目から９丁目は存在するのになぜか５丁目だけ存在しない）
#帝塚山１丁目 > 帝塚山
#万代１丁目 > 万代

#もう一度csv読み込みし直し
data1 <- read_csv("data1130.csv", locale=locale(encoding="SJIS")) 

#アンマッチが減ったか確認
unmatch2 <- anti_join(data1, shape, by=c("町丁目名"="MOJI"))
unmatch2$町丁目名
#character(0)


##################################################################
# 以下、参考

#data1のカラム名表示
names(data1)
#[1] "基準日"      "区"          "町丁目名"    "世帯数"      "総数"        "取得者数"    "取得率_0831" "取得率_1130"　[9] "増減"

#結合後のカラム名と番号表示
names(data)
# [1] "KEY_CODE"    "PREF"        "CITY"        "S_AREA"      "PREF_NAME"   "CITY_NAME"   "S_NAME"      "KIGO_E"     
# [9] "HCODE"       "AREA"        "PERIMETER"   "H27KAxx_"    "H27KAxx_ID"  "KEN"         "KEN_NAME"    "SITYO_NAME" 
#[17] "GST_NAME"    "CSS_NAME"    "KIHON1"      "DUMMY1"      "KIHON2"      "KEYCODE1"    "KEYCODE2"    "AREA_MAX_F" 
#[25] "KIGO_D"      "N_KEN"       "N_CITY"      "KIGO_I"      "MOJI"        "KBSUM"       "JINKO"       "SETAI"      
#[33] "X_CODE"      "Y_CODE"      "KCODE1"      "基準日"      "区"          "世帯数"      "総数"        "取得者数"   
#[41] "取得率_0831" "取得率_1130" "増減"        "geometry"   

#取得率_1130の全町丁目のレンジを確認
summary(data$取得率_0831)

#取り急ぎでカラムの番号を指定して可視化
#取得率_1130
plot(data[,42])

# カラーパレット
pal <- c("white","red")
cls1 <- classIntervals(d$CRIME, n=5, style="quantile") # 等量分類
cls2 <- classIntervals(d$CRIME, n=5, style="equal") # 等間隔分類
cls3 <- classIntervals(d$CRIME, n=5, style="sd") # 標準偏差分類
cls4 <- classIntervals(d$CRIME, n=5, style="fisher") # 自然階級分類
cls5 <- classIntervals(d$CRIME, n=5, style="pretty") # 視覚的に分かりやすい分類
cls6 <- classIntervals(d$CRIME, n=5, style="kmeans") # 非階層クラスタリングによる分類
cls7 <- classIntervals(d$CRIME, n=5, style="hclust") # 階層クラスタリングによる分類
col_km <- data[[column]] %>% classIntervals(., 6, style="fixed", fixedBreaks=c(9,10,20,30,40,50,max(.))) %>% findColours(.,pal=brewer.pal(6,"Greens"))

##########################################
column <- c("０歳","１歳","２歳","３歳","４歳","５歳")

for(i in 1:6){
  #書き出しファイル設定 ファイル名の並び替えのため区が変わるたびにインデックスを変更していくべし
  quartz(type="pdf", file=sprintf("%s%s%s.pdf",kuindex, kuname, column[i]))
  
  #quartz設定
  par(family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
  
  #人口　描画
  col_km <- data[[column[i]]] %>% classIntervals(., 6, style="fixed", fixedBreaks=c(9,10,20,30,40,50,max(.))) %>% findColours(.,pal=brewer.pal(6,"Greens"))
  plot(st_geometry(shape[7]), col=col_km, main=paste(kuname, column[i], "  (平成29年9月末現在)", sep=""))
  text(st_coordinates(shape %>% st_centroid)[,1], st_coordinates(shape %>% st_centroid)[,2], labels=shape$MOJI, cex=0.5)
  text(st_coordinates(shape %>% st_centroid)[,1], st_coordinates(shape %>% st_centroid)[,2]-0.0007, labels=data[[column[i]]], cex=0.7)

  #保育所　描画  該当区だけ抽出
  hoikusyo2 <- hoikusyo1 %>% filter(hoikusyo1$区==kuname)
  points(hoikusyo2$経度, hoikusyo2$緯度, pch=16, col="red")
  
  #企業型保育事業　描画  該当区だけ抽出
  kigyo2 <- kigyo1 %>% filter(kigyo1$区==kuname)
  points(kigyo2$経度, kigyo2$緯度, pch=17, col="blue")
  
  #描画終了
  dev.off()
}


