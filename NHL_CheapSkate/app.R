## app.R ##

library(shinydashboard)
library(ggplot2)
library(plyr)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Loading data needed for the app

df <- read.csv('data/NHLData.csv', header=T)
df_sal <- read.csv('data/NHL_wSalary2.csv', header=T)
df_table <- read.csv('data/5_players.csv', header=T)

df_sal2 <- df_sal
df_sal2 <- rename(df_sal, c("Season_x"="Season", "goals_million"="Goals/Million", "assists_million"="Assists/Million","point_.million"="Points/Million"))


# Defining the x and y lines to draw the quadrants 
xl_s <- c(0,0)
yl_s <- c(0,0)
xl_a <- c(-1.5,3)
yl_a <- c(-1.5,3)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# Start of the Shiny app
ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "NHL Player Peformance Tracker",
                                    titleWidth = 450),
                    dashboardSidebar(
                      selectInput("var", 
                                  label = "Pick A Variable",
                                  choices = c("Goals", "Assists", "Points", "Shots", "TimeonIce"),
                                  selected = "Goals"),
                      selectInput("player", 
                                  label = "Pick A Player",
                                  choices = c("Abdelkader Justin",     "Acciari Noel"     ,     "Adams Craig"        , "Agozzino Andrew"      ,
                                              "Allen Bryan",           "Alzner Karl"        ,   "Anderson Josh"         ,"Andersson Joakim"     ,
                                              "Andreoff Andy" ,        "Andrighetto Sven"   ,   "Angelidis Mike"        ,"Anisimov Artem"       ,
                                              "Arcobello Mark" ,       "Armia Joel"         ,   "Arvidsson Viktor"      ,"Athanasiou Andreas"   ,
                                              "Aulie Keith"     ,      "Backes David"       ,   "Backlund Mikael"       ,"Backstrom Nicklas"    ,
                                              "Baertschi Sven"   ,     "Bailey Casey"       ,   "Bailey Josh"           ,"Bailey Justin"        ,
                                              "Ballard Keith"     ,    "Barberio Mark"      ,   "Barkov Aleksander"     ,"Barrie Tyson"         ,
                                              "Bartkowski Matt"    ,   "Bartley Victor"     ,   "Bass Cody"             ,"Baun Kyle"            ,
                                              "Beagle Jay"          ,  "Beauchemin Francois",   "Beaulieu Nathan"       ,"Beck Taylor"          ,
                                              "Beleskey Matt"      ,   "Bellemore Brett"    ,   "Benn Jamie"            ,"Benn Jordie"          ,
                                              "Bennett Beau"      ,    "Bennett Sam"        ,   "Benoit Andre"          ,"Bergenheim Sean"      ,
                                              "Bergeron Patrice"  ,    "Berglund Patrik"    ,   "Bernier Steve"         ,"Bertschy Christoph"   ,
                                              "Bickel Stu"        ,    "Bickell Bryan"      ,   "Biega Alex"            ,"Bieksa Kevin"         ,
                                              "Bigras Chris"      ,    "Bitetto Anthony"    ,   "Bjorkstrand Oliver"    ,"Bjugstad Nick"        ,
                                              "Blandisi Joseph"   ,    "Blunden Mike"       ,   "Bodie Troy"            ,"Bodnarchuk Andrew"    ,
                                              "Boedker Mikkel"    ,    "Bogosian Zach"      ,   "Boll Jared"            ,"Bolland Dave"         ,
                                              "Bollig Brandon"    ,    "Bonino Nick"        ,   "Booth David"          , "Bordeleau Patrick"    ,
                                              "Borowiecki Mark"   ,    "Bortuzzo Robert"    ,   "Boucher Reid"         , "Boulton Eric"         ,
                                              "Bouma Lance"       ,    "Bournival Michael"  ,   "Bourque Gabriel"      , "Bourque Rene"         ,
                                              "Bourque Ryan"      ,    "Bouwmeester Jay"    ,   "Boychuk Johnny"       , "Boychuk Zach"         ,
                                              "Boyes Brad"        ,    "Boyle Brian"        ,   "Boyle Dan"            , "Bozak Tyler"          ,
                                              "Brassard Derick"   ,    "Braun Justin"       ,   "Brennan T.J."         , "Brewer Eric"          ,
                                              "Brickley Connor"   ,    "Briere Daniel"      ,   "Brodie T.J."          , "Brodin Jonas"         ,
                                              "Brodziak Kyle"     ,    "Brouwer Troy"       ,   "Brown Dustin"         , "Brown J.T."           ,
                                              "Brown Mike"        ,    "Brown Patrick"      ,   "Bulmer Brett"         , "Burakovsky Andre"     ,
                                              "Burish Adam"       ,    "Burmistrov Alexander",  "Burns Brent"          , "Burrows Alexandre"    ,
                                              "Butler Chris"      ,    "Byfuglien Dustin"   ,   "Byron Paul"           , "Callahan Ryan"        ,
                                              "Calvert Matt"      ,    "Campbell Andrew"    ,   "Campbell Brian"       , "Campbell Gregory"     ,
                                              "Carcillo Daniel"   ,    "Carey Matt"         ,   "Carey Paul"           , "Carkner Matt"         ,
                                              "Carle Matt"        ,    "Carlson John"       ,   "Caron Jordan"         , "Carr Daniel"          ,
                                              "Carrick Connor"    ,    "Carrick Trevor"     ,   "Carter Jeff"          , "Carter Ryan"          ,
                                              "Catenacci Daniel"  ,    "Ceci Cody"          ,   "Chara Zdeno"          , "Chiarot Ben"          ,
                                              "Chiasson Alex"     ,    "Chimera Jason"      ,   "Chipchura Kyle"       , "Chorney Taylor"       ,
                                              "Cizikas Casey"     ,    "Claesson Fredrik"   ,   "Clarkson David"       , "Cleary Dan"           ,
                                              "Clendening Adam"   ,    "Clifford Kyle"      ,   "Clitsome Grant"       , "Clowe Ryane"          ,
                                              "Clutterbuck Cal"   ,    "Coburn Braydon"     ,   "Cogliano Andrew"      , "Colaiacovo Carlo"     ,
                                              "Colborne Joe"      ,    "Cole Erik"          ,   "Cole Ian"             , "Comeau Blake"         ,
                                              "Condra Erik"       ,    "Connauton Kevin"    ,   "Connolly Brett"       , "Cooke Matt"           ,
                                              "Copp Andrew"       ,    "Cormier Patrice"    ,   "Corrado Frank"        , "Cousins Nick"         ,
                                              "Couture Logan"     ,    "Couturier Sean"     ,   "Cowen Jared"          , "Coyle Charlie"        ,
                                              "Cracknell Adam"    ,    "Crosby Sidney"      ,   "Cross Tommy"          , "Cullen Matt"          ,
                                              "Cumiskey Kyle"     ,    "Cunningham Craig"   ,   "Czuczman Kevin"       , "Dahlbeck Klas"        ,
                                              "Daley Trevor"      ,    "Dalpe Zac"          ,   "Danault Phillip"      , "Dano Marko"           ,
                                              "Datsyuk Pavel"     ,    "Dauphin Laurent"    ,   "Davidson Brandon"     , "de Haan Calvin"       ,
                                              "De La Rose Jacob"  ,    "DeKeyser Danny"     ,   "Del Zotto Michael"    , "Demers Jason"         ,
                                              "Desharnais David"  ,    "Desjardins Andrew"  ,   "Deslauriers Nicolas"  , "Despres Simon"        ,
                                              "Diaz Raphael"      ,    "Dickinson Jason"    ,   "Dietz Darren"         , "Dillon Brenden"       ,
                                              "Doan Shane"        ,    "Domi Max"           ,   "Donovan Matt"         , "Donskoi Joonas"       ,
                                              "Dorsett Derek"     ,    "Doughty Drew"       ,   "Dowd Nic"             , "Downie Steve"         ,
                                              "Draisaitl Leon"    ,    "Drouin Jonathan"    ,   "Dubinsky Brandon"     , "Duchene Matt"         ,
                                              "Duclair Anthony"   ,    "Dumba Mathew"       ,   "Dumoulin Brian"       , "Dupuis Pascal"        ,
                                              "Dvorak Radek"      ,    "Dzingel Ryan"       ,   "Eakin Cody"           , "Eaves Patrick"        ,
                                              "Eberle Jordan"     ,    "Edler Alexander"    ,   "Edmundson Joel"       , "Ehrhoff Christian"    ,
                                              "Eichel Jack"       ,    "Ekblad Aaron"       ,   "Ekholm Mattias"       , "Elias Patrik"         ,
                                              "Eller Lars"        ,    "Ellerby Keaton"     ,   "Elliott Stefan"       , "Ellis Matt"           ,
                                              "Ellis Morgan"      ,    "Ellis Ryan"         ,   "Emelin Alexei"        , "Engelland Deryk"      ,
                                              "Ennis Tyler"       ,    "Erat Martin"        ,   "Ericsson Jonathan"    , "Eriksson Loui"        ,
                                              "Erixon Tim"        ,    "Erskine John"       ,   "Etem Emerson"         , "Everberg Dennis"      ,
                                              "Fabbri Robby"      ,    "Faksa Radek"        ,   "Falk Justin"          , "Farnham Bobby"        ,
                                              "Fasching Hudson"   ,    "Fast Jesper"        ,   "Faulk Justin"         , "Fayne Mark"           ,
                                              "Fehr Eric"         ,    "Ference Andrew"     ,   "Ferland Michael"      , "Ferraro Landon"       ,
                                              "Fiala Kevin"       ,    "Fiddler Vernon"     ,   "Filppula Valtteri"    , "Fisher Mike"          ,
                                              "Fistric Mark"      ,    "Fleischmann Tomas"  ,   "Flynn Brian"          , "Foligno Marcus"       ,
                                              "Foligno Nick"      ,    "Folin Christian"    ,   "Fontaine Justin"      , "Forbort Derek"        ,
                                              "Forsberg Filip"    ,    "Fowler Cam"         ,   "Franson Cody"         , "Franzen Johan"        ,
                                              "Fraser Mark"       ,    "Fraser Matt"        ,   "Friberg Max"          , "Froese Byron"         ,
                                              "Frolik Michael"    ,    "Gaborik Marian"     ,   "Gabriel Kurtis"       , "Gagne Simon"          ,
                                              "Gagner Sam"        ,    "Galchenyuk Alex"    ,   "Galiardi T.J."        , "Gallagher Brendan"    ,
                                              "Garbutt Ryan"      ,    "Gardiner Jake"      ,   "Garrison Jason"       , "Gaudet Tyler"         ,
                                              "Gaudreau Johnny"   ,    "Gaunce Brendan"     ,   "Gaustad Paul"         , "Gauthier Frederik"    ,
                                              "Gazdic Luke"       ,    "Gelinas Eric"       ,   "Gerbe Nathan"         , "Getzlaf Ryan"         ,
                                              "Gibbons Brian"     ,    "Gilbert Tom"        ,   "Gionta Brian"         , "Gionta Stephen"       ,
                                              "Giordano Mark"     ,    "Girardi Dan"        ,   "Girgensons Zemgus"    , "Giroux Claude"        ,
                                              "Glass Tanner"      ,    "Gleason Tim"        ,   "Glencross Curtis"     , "Glendening Luke"      ,
                                              "Goc Marcel"        ,    "Goligoski Alex"     ,   "Goloubef Cody"        , "Gomez Scott"          ,
                                              "Gonchar Sergei"    ,    "Goodrow Barclay"    ,   "Gordon Boyd"          , "Gorges Josh"          ,
                                              "Gormley Brandon"   ,    "Gostisbehere Shayne",   "Grabner Michael"      , "Grabovski Mikhail"    ,
                                              "Granberg Petter"   ,    "Granlund Markus"    ,   "Granlund Mikael"      , "Grant Alex"           ,
                                              "Grant Derek"       ,    "Graovac Tyler"      ,   "Gravel Kevin"         , "Green Mike"           ,
                                              "Greene Andy"       ,    "Greene Matt"        ,   "Greening Colin"       , "Griffith Seth"        ,
                                              "Grigorenko Mikhail",    "Grimaldi Rocco"     ,   "Gryba Eric"           , "Gudas Radko"          ,
                                              "Gudbranson Erik"   ,    "Guenin Nate"        ,   "Gunnarsson Carl"      , "Gustafsson Erik"      ,
                                              "Hagelin Carl"      ,    "Hainsey Ron"        ,   "Haley Micheal"        , "Halischuk Matt"       ,
                                              "Hall Taylor"       ,    "Hamhuis Dan"        ,   "Hamilton Dougie"      , "Hamilton Freddie"     ,
                                              "Hamilton Ryan"     ,    "Hamonic Travis"     ,   "Hanifin Noah"          ,"Hanley Joel"          ,
                                              "Hannan Scott"      ,    "Hannikainen Markus" ,   "Hansen Jannik"         ,"Hanzal Martin"        ,
                                              "Harpur Ben"        ,    "Harrison Jay"       ,   "Harrold Peter"        , "Hartman Ryan"         ,
                                              "Hartnell Scott"    ,    "Hathaway Garnet"    ,   "Haula Erik"            ,"Havlat Martin"        ,
                                              "Hayes Jimmy"       ,    "Hayes Kevin"        ,   "Heatley Dany"         , "Hedman Victor"        ,
                                              "Hejda Jan"         ,    "Helgeson Seth"      ,   "Helm Darren"           ,"Hemsky Ales"          ,
                                              "Hendricks Matt"    ,    "Henrique Adam"      ,   "Hertl Tomas"           ,"Hickey Thomas"        ,
                                              "Hillen Jack"       ,    "Hinostroza Vincent" ,   "Hjalmarsson Niklas"    ,"Hodgson Cody"         ,
                                              "Hoffman Mike"      ,    "Holden Nick"        ,   "Holland Peter"         ,"Holzer Korbinian"     ,
                                              "Horcoff Shawn"     ,    "Hornqvist Patric"   ,   "Horton Nathan"         ,"Horvat Bo"            ,
                                              "Hossa Marian"      ,    "Howden Quinton"     ,   "Hrivik Marek"          ,"Huberdeau Jonathan"   ,
                                              "Hudler Jiri"       ,    "Hudon Charles"      ,   "Hunt Brad"             ,"Hunwick Matt"         ,
                                              "Hutton Ben"        ,    "Iginla Jarome"      ,   "Irwin Matt"           , "Jackman Barret"       ,
                                              "Jackman Tim"       ,    "Jagr Jaromir"       ,   "Janmark Mattias"       ,"Jarnkrok Calle"       ,
                                              "Jaskin Dmitrij"    ,    "Jenner Boone"       ,   "Joensuu Jesse"         ,"Johansen Ryan"        ,
                                              "Johansson Marcus"  ,    "Johns Stephen"      ,   "Johnson Erik"          ,"Johnson Jack"         ,
                                              "Johnson Tyler"     ,    "Johnston Ross"      ,   "Johnston Ryan"         ,"Jokinen Jussi"        ,
                                              "Jokinen Olli"      ,    "Jokipakka Jyrki"    ,   "Jones David"           ,"Jones Seth"           ,
                                              "Jooris Josh"       ,    "Jordan Michal"      ,   "Josefson Jacob"        ,"Josi Roman"           ,
                                              "Jurco Tomas"       ,    "Kadri Nazem"        ,   "Kaleta Patrick"        ,"Kalinin Sergey"       ,
                                              "Kampfer Steven"    ,    "Kane Evander"       ,   "Kane Patrick"          ,"Karlsson Erik"        ,
                                              "Karlsson Melker"   ,    "Karlsson William"   ,   "Kassian Zack"          ,"Keith Duncan"         ,
                                              "Kelly Chris"       ,    "Kemppainen Joonas"  ,   "Kennedy Tyler"         ,"Kero Tanner"          ,
                                              "Kesler Ryan"       ,    "Khaira Jujhar"      ,   "Khokhlachev Alexander" ,"Kindl Jakub"          ,
                                              "King Dwight"       ,    "Klefbom Oscar"      ,   "Klein Kevin"           ,"Klingberg John"       ,
                                              "Knight Corban"     ,    "Koekkoek Slater"    ,   "Koivu Mikko"          , "Koivu Saku"           ,
                                              "Komarov Leo"       ,    "Kopecky Tomas"      ,   "Kopitar Anze"         , "Korpikoski Lauri"     ,
                                              "Kostka Mike"       ,    "Kozun Brandon"      ,   "Kreider Chris"        , "Krejci David"         ,
                                              "Kronwall Niklas"   ,    "Krug Torey"         ,   "Kruger Marcus"        , "Kucherov Nikita"      ,
                                              "Kuhnhackl Tom"     ,    "Kukan Dean"         ,   "Kulak Brett"          , "Kulemin Nikolai"      ,
                                              "Kulikov Dmitry"    ,    "Kunitz Chris"       ,   "Kunyk Cody"           , "Kuznetsov Evgeny"     ,
                                              "Kylington Oliver"  ,    "Ladd Andrew"        ,   "Laich Brooks"          ,"Lander Anton"         ,
                                              "Landeskog Gabriel" ,    "Lapierre Maxim"     ,   "Larkin Dylan"         , "Larsson Adam"         ,
                                              "Larsson Johan"     ,    "Lashoff Brian"      ,   "Latta Michael"        , "Laughton Scott"       ,
                                              "Lauridsen Oliver"  ,    "Lazar Curtis"       ,   "Lecavalier Vincent"   , "Leddy Nick"           ,
                                              "Lee Anders"        ,    "Legwand David"      ,   "Lehtera Jori"         , "Leopold Jordan"       ,
                                              "Lernout Brett"     ,    "Lessio Lucas"       ,   "Letang Kris"          , "Letestu Mark"         ,
                                              "Lewis Trevor"      ,    "Lindberg Oscar"     ,   "Lindblad Matt"        , "Lindbohm Petteri"     ,
                                              "Lindell Esa"       ,    "Lindholm Elias"     ,   "Lindholm Hampus"      , "Lipon J.C."           ,
                                              "Little Bryan"      ,    "Lovejoy Ben"        ,   "Lowry Adam"           , "Lucic Milan"          ,
                                              "Lupul Joffrey"     ,    "Maatta Olli"        ,   "MacArthur Clarke"     , "MacDonald Andrew"     ,
                                              "MacKenzie Derek"   ,    "MacKinnon Nathan"   ,   "Malhotra Manny"       , "Malkin Evgeni"        ,
                                              "Malone Brad"       ,    "Manning Brandon"    ,   "Manson Josh"          , "Mantha Anthony"       ,
                                              "Marchand Brad"     ,    "Marincin Martin"    ,   "Markov Andrei"        , "Marleau Patrick"      ,
                                              "Maroon Patrick"    ,    "Martin Matt"        ,   "Martin Paul"          , "Martinez Alec"        ,
                                              "Martinook Jordan"  ,    "Martinsen Andreas"  ,   "Mashinter Brandon"    , "Matheson Michael"     ,
                                              "Matteau Stefan"    ,    "Matthias Shawn"     ,   "Mayfield Scott"       , "McBain Jamie"         ,
                                              "McCabe Jake"       ,    "McCann Jared"       ,   "McCarron Michael"     , "McClement Jay"        ,
                                              "McCormick Cody"    ,    "McCormick Max"      ,   "McDavid Connor"       , "McDonagh Ryan"        ,
                                              "McDonald Colin"    ,    "McFarland John"     ,   "McGinn Brock"         , "McGinn Jamie"         ,
                                              "McGinn Tye"        ,    "McGrattan Brian"    ,   "McIlrath Dylan"       , "McKegg Greg"          ,
                                              "McKenzie Curtis"   ,    "McLeod Cody"        ,   "McMillan Brandon"     , "McNabb Brayden"       ,
                                              "McQuaid Adam"      ,    "Megna Jayson"       ,   "Melchiori Julian"     , "Merrill Jon"          ,
                                              "Mersch Michael"    ,    "Meszaros Andrej"    ,   "Methot Marc"          , "Michalek Milan"       ,
                                              "Michalek Zbynek"   ,    "Milano Sonny"       ,   "Miller Andrew"        , "Miller Colin"         ,
                                              "Miller Drew"       ,    "Miller J.T."        ,   "Miller Kevan"         , "Mitchell John"        ,
                                              "Mitchell Torrey"   ,    "Mitchell Willie"    ,   "Moen Travis"          , "Monahan Sean"         ,
                                              "Moore Dominic"     ,    "Moore John"         ,   "Morin Jeremy"         , "Morrow Brenden"       ,
                                              "Moss David"        ,    "Mouillierat Kael"   ,   "Moulson Matt"         , "Mozik Vojtech"        ,
                                              "Mueller Mirco"     ,    "Murphy Connor"      ,   "Murphy Ryan"          , "Murray Ryan"          ,
                                              "Myers Tyler"       ,    "Nakladal Jakub"     ,   "Namestnikov Vladislav", "Nash Rick"            ,
                                              "Nash Riley"        ,    "Neal James"         ,   "Neil Chris"           ,"Nelson Brock"         ,
                                              "Nemeth Patrik"     ,    "Nesterov Nikita"    ,   "Nestrasil Andrej"     , "Nichushkin Valeri"    ,
                                              "Niederreiter Nino" ,    "Nielsen Frans"      ,   "Nieto Matt"           , "Nikitin Nikita"       ,
                                              "Niskanen Matt"     ,    "Noesen Stefan"      ,   "Nolan Jordan"         , "Nordstrom Joakim"     ,
                                              "Nosek Tomas"       ,    "Nurse Darnell"      ,   "Nylander William"     , "Nyquist Gustav"       ,
                                              "Nystrom Eric"      ,    "Oesterle Jordan"    ,   "Okposo Kyle"          , "Oleksiak Jamie"       ,
                                              "Olofsson Gustav"   ,    "Olsen Dylan"        ,   "Orlov Dmitry"         , "Orpik Brooks"         ,
                                              "Orr Colton"        ,    "Oshie T.J."         ,   "Ott Steve"            , "Ouellet Xavier"       ,
                                              "Ovechkin Alex"     ,    "Pacioretty Max"     ,   "Paille Daniel"        , "Pakarinen Iiro"       ,
                                              "Palat Ondrej"      ,    "Paliotta Michael"   ,   "Palmieri Kyle"        , "Panarin Artemi"       ,
                                              "Panik Richard"     ,    "Paquette Cedric"    ,   "Parayko Colton"       , "Pardy Adam"           ,
                                              "Parise Zach"       ,    "Pastrnak David"     ,   "Pateryn Greg"         , "Pavelski Joe"         ,
                                              "Pearson Tanner"    ,    "Pelech Adam"        ,   "Peluso Anthony"       , "Perreault Mathieu"    ,
                                              "Perron David"      ,    "Perry Corey"        ,   "Pesce Brett"          , "Petrovic Alex"        ,
                                              "Petry Jeff"        ,    "Peverley Rich"      ,   "Phaneuf Dion"         , "Phillips Chris"       ,
                                              "Pietila Blake"     ,    "Pietrangelo Alex"   ,   "Pirri Brandon"        , "Piskula Joe"          ,
                                              "Pitlick Tyler"     ,    "Plekanec Tomas"      ,  "Plotnikov Sergei"     , "Poirier Emile"        ,
                                              "Polak Roman"       ,    "Pominville Jason"   ,   "Porter Chris"         , "Porter Kevin"         ,
                                              "Postma Paul"       ,    "Potter Corey"       ,   "Pouliot Benoit"       , "Pouliot Derrick"      ,
                                              "Prince Shane"      ,    "Pronger Chris"      ,   "Prosser Nate"         , "Prout Dalton"         ,
                                              "Prust Brandon"     ,    "Puempel Matt"       ,   "Pulkkinen Teemu"      , "Pulock Ryan"          ,
                                              "Purcell Teddy"     ,    "Pysyk Mark"         ,   "Quincey Kyle"         , "Quine Alan"           ,
                                              "Raffl Michael"     ,    "Rakell Rickard"     ,   "Ramage John"          , "Randell Tyler"        ,
                                              "Rantanen Mikko"    ,    "Rask Victor"        ,   "Rasmussen Dennis"     , "Rattie Ty"            ,
                                              "Rau Kyle"          ,    "Raymond Mason"      ,   "Read Matt"            , "Reaves Ryan"          ,
                                              "Redmond Zach"      ,    "Reese Dylan"        ,   "Regehr Robyn"         , "Regin Peter"          ,
                                              "Regner Brent"      ,    "Reilly Mike"        ,   "Reinhart Griffin"     , "Reinhart Max"         ,
                                              "Reinhart Sam"      ,    "Rendulic Borna"     ,   "Ribeiro Mike"         , "Richards Brad"        ,
                                              "Richards Mike"     ,    "Richardson Brad"    ,   "Rieder Tobias"        , "Rielly Morgan"        ,
                                              "Rinaldo Zac"       ,    "Rissanen Rasmus"    ,   "Ristolainen Rasmus"   , "Ritchie Brett"        ,
                                              "Robak Colby"       ,    "Robidas Stephane"   ,   "Robinson Buddy"       , "Rodrigues Evan"       ,
                                              "Roussel Antoine"   ,    "Roy Derek"          ,   "Rozsival Michal"      , "Ruhwedel Chad"        ,
                                              "Rundblad David"    ,    "Russell Kris"       ,   "Rust Bryan"           , "Ruutu Tuomo"          ,
                                              "Ryan Bobby"        ,    "Ryan Derek"         ,   "Rychel Kerby"         , "Ryder Michael"        ,
                                              "Saad Brandon"      ,    "Salomaki Miikka"    ,   "Salvador Bryce"       , "Samuelsson Philip"    ,
                                              "Santorelli Mike"   ,    "Savard David"       ,   "Sbisa Luca"           , "Scandella Marco"      ,
                                              "Sceviour Colton"   ,    "Schaller Tim"       ,   "Scheifele Mark"       ,"Schenn Brayden"       ,
                                              "Schenn Luke"       ,    "Schlemko David"     ,   "Schmidt Nate"         , "Schneider Cole"       ,
                                              "Schroeder Jordan"  ,    "Schultz Jeff"       ,   "Schultz Justin"       , "Schultz Nick"         ,
                                              "Schwartz Jaden"    ,    "Scott John"         ,   "Scuderi Rob"          ,"Seabrook Brent"       ,
                                              "Sedin Daniel"      ,    "Sedin Henrik"       ,   "Seguin Tyler"         , "Seidenberg Dennis"    ,
                                              "Sekac Jiri"        ,    "Sekera Andrej"      ,   "Selleck Eric"         , "Semin Alexander"      ,
                                              "Setoguchi Devin"   ,    "Severson Damon"     ,   "Sgarbossa Michael"    , "Sharp Patrick"        ,
                                              "Shattenkirk Kevin" ,    "Shaw Andrew"        ,   "Shaw Logan"           , "Sheahan Riley"        ,
                                              "Sheary Conor"      ,    "Sheppard James"     ,   "Shinkaruk Hunter"     , "Shore Devin"          ,
                                              "Shore Drew"        ,    "Shore Nick"         ,   "Sieloff Patrick"      , "Silfverberg Jakob"    ,
                                              "Sill Zach"         ,    "Simmonds Wayne"     ,   "Sislo Mike"           , "Sissons Colton"       ,
                                              "Skille Jack"       ,    "Skinner Jeff"       ,   "Skjei Brady"          , "Slater Jim"           ,
                                              "Slavin Jaccob"     ,    "Slepyshev Anton"    ,   "Smid Ladislav"        , "Smith Ben"            ,
                                              "Smith Brendan"     ,    "Smith Craig"        ,   "Smith Reilly"         , "Smith Trevor"         ,
                                              "Smith Zack"        ,    "Soderberg Carl"     ,   "Souray Sheldon"       , "Spaling Nick"         ,
                                              "Spezza Jason"      ,    "Spooner Ryan"       ,   "Sproul Ryan"          , "Spurgeon Jared"       ,
                                              "St. Louis Martin"  ,    "Staal Eric"         ,   "Staal Jordan"         , "Staal Marc"           ,
                                              "Stafford Drew"     ,    "Stajan Matt"        ,   "Stalberg Viktor"      , "Stamkos Steven"       ,
                                              "Stanton Ryan"      ,    "Stastny Paul"       ,   "Steen Alexander"      , "Stempniak Lee"        ,
                                              "Stepan Derek"       ,   "Stewart Chris"      ,   "Stoll Jarret"         , "Stone Mark"           ,
                                              "Stone Michael"      ,   "Stoner Clayton"     ,   "Strachan Tyson"       , "Strait Brian"         ,
                                              "Stralman Anton"    ,    "Street Ben"         ,   "Streit Mark"          , "Strome Ryan"          ,
                                              "Stuart Brad"       ,    "Stuart Mark"        ,   "Subban P.K."          , "Summers Chris"        ,
                                              "Sundqvist Oskar"   ,    "Sustr Andrej"       ,   "Suter Ryan"           , "Sutter Brandon"       ,
                                              "Sutter Brett"      ,    "Sutter Brody"       ,   "Sutton Andy"          , "Svedberg Viktor"      ,
                                              "Szwarz Jordan"     ,    "Talbot Maxime"      ,   "Tanev Brandon"        , "Tanev Chris"          ,
                                              "Tangradi Eric"     ,    "Tanguay Alex"       ,   "Tarasenko Vladimir"   , "Tatar Tomas"          ,
                                              "Tavares John"      ,    "Tennyson Matt"      ,   "Teravainen Teuvo"     , "Terry Chris"          ,
                                              "Theodore Shea"     ,    "Thomas Christian"   ,   "Thompson Nate"        , "Thompson Paul"        ,
                                              "Thorburn Chris"    ,    "Thornton Joe"       ,   "Thornton Shawn"       , "Tierney Chris"        ,
                                              "Tikhonov Viktor"   ,    "Timonen Kimmo"      ,   "Tinordi Jarred"       , "Tlusty Jiri"          ,
                                              "Toews Jonathan"    ,    "Toffoli Tyler"      ,   "Tolchinsky Sergey"    , "Tootoo Jordin"        ,
                                              "Torres Raffi"      ,    "Trocheck Vincent"   ,   "Tropp Corey"          , "Trotman Zach"         ,
                                              "Trouba Jacob"      ,    "Turris Kyle"        ,   "Tyutin Fedor"         , "Umberger R.J."        ,
                                              "Upshall Scottie"   ,    "Van Brabant Bryce"  ,   "Vanek Thomas"         , "Vatanen Sami"         ,
                                              "Vatrano Frank"     ,    "Veilleux Stephane"  ,   "Vermette Antoine"     , "Versteeg Kris"        ,
                                              "Vey Linden"        ,    "Virtanen Jake"      ,   "Visnovsky Lubomir"    , "Vitale Joe"           ,
                                              "Volchenkov Anton"  ,    "Volpatti Aaron"     ,   "Voracek Jakub"        , "Voynov Slava"         ,
                                              "Vrbata Radim"      ,    "Wagner Chris"       ,   "Ward Joel"            , "Warsofsky David"      ,
                                              "Watson Austin"     ,    "Weal Jordan"        ,   "Weaver Mike"          , "Weber Mike"           ,
                                              "Weber Shea"        ,    "Weber Yannick"      ,   "Weise Dale"           , "Weiss Stephen"        ,
                                              "Wennberg Alexander",    "Wheeler Blake"      ,   "White Ryan"           , "Wideman Chris"        ,
                                              "Wideman Dennis"    ,    "Wiercioch Patrick"  ,   "Williams Justin"      , "Wilson Colin"         ,
                                              "Wilson Garrett"    ,    "Wilson Ryan"        ,   "Wilson Scott"         , "Winchester Jesse"     ,
                                              "Wingels Tommy"     ,    "Winnik Daniel"      ,   "Wisniewski James"     , "Witkowski Luke"       ,
                                              "Wolf David"        ,    "Woods Brendan"      ,   "Wotherspoon Tyler"    , "Yakupov Nail"         ,
                                              "Yandle Keith"      ,    "Zacha Pavel"        ,   "Zadorov Nikita"       , "Zajac Travis"         ,
                                              "Zalewski Michael"  ,    "Zetterberg Henrik"  ,   "Zibanejad Mika"       , "Zidlicky Marek"       ,
                                              "Zolnierczyk Harry" ,    "Zubrus Dainius"     ,   "Zucker Jason")  ,
                                  selected = "Coyle Charlie"),
                      actionButton(inputId = "go", 
                                   label = "Update"),
                      br(),
                      
                      img(src = "nhl.jpg", height = 225, width = 225),
                      br(),
                      htmlOutput("html_link")
                    ),
                    
                    
                    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    #  - - Setting up the main body of the app
                    dashboardBody(
                      fluidRow(
                        # Boxes need to be put in a row (or column)
                        box(title = "Player Performance", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE,
                            plotOutput("scatter2")),
                        
                        box(title = "Performance Compensation Comparison", status = "warning", solidHeader = TRUE,
                            collapsible = TRUE,
                            plotOutput("scatter3")
                        ),
                        
                        fluidRow(
                          # Boxes need to be put in a row (or column)
                          box(title = "Performance per million Dollars",
                              status = "danger", solidHeader = TRUE,
                              collapsible = TRUE,
                              "Performance per million dollars for:",
                              br(),
                              textOutput("Player_Name"),
                              br(),
                              tableOutput("table1")),
                          
                          box(title = "Compensation compared to league", status = "success", solidHeader = TRUE,
                              collapsible = TRUE,
                              plotOutput("hist1"))
                        ) # Closes fluidRow
                      )
                    ) 
)


server <- function(input, output) {
  
  
  output$html_link <- renderUI({
    a("Presentation", href=paste("https://docs.google.com/presentation/d/1sT9frk24fxE-SwIrZrDVfE6xbPKRU3nFNkdH6ACcJd8/edit?usp=sharing", sep=""), target="_blank") 
  })
  
  
  # = = Reactive stuff = = = 
  
  dataPlayer <- eventReactive(input$go, {
    (df_sal[df_sal$NameCode_x == input$player,]) 
  })
  
  # - - - - - Renders variable = selector input
  # reactive object for variable selector
  dataInput <- eventReactive(input$go, {
    switch(input$var, 
           "Goals" = dataPlayer()$G,
           "Assists" = dataPlayer()$A,
           "Points" = dataPlayer()$PTS,
           "Shots" = dataPlayer()$Shot,
           "TimeonIce" = dataPlayer()$TOI)
  }) # Closes dataInput
  
  dataLeagueInput <- eventReactive(input$go, {
    switch(input$var, 
           "Goals" = df_sal$G,
           "Assists" = df_sal$A,
           "Points" = df_sal$PTS,
           "Shots" = df_sal$Shot,
           "TimeonIce" = df_sal$TOI)
  }) # Closes dataInput
  
  
  dataSalaryInput <- eventReactive(input$go, {
    switch(input$var, 
           "Goals" = df_sal$G,
           "Assists" = df_sal$A,
           "Points" = df_sal$PTS,
           "Shots" = df_sal$Shot,
           "TimeonIce" = df_sal$TOI)
  }) # Closes dataInput
  
  dataHistVariableLabel <- eventReactive(input$go, {
    switch(input$var, 
           "Goals" = 'Goals per Million Dollars',
           "Assists" = 'Assists per million dollars',
           "Points" = 'Points per million dollars',
           "Shots" = 'Shots per million dollars',
           "TimeonIce" = 'TimeonIce per million dollars')
  }) # Closes dataInput
  
  
  
  # = = = Setting the player data
  dataPlayerAge <- eventReactive(input$go,{
    (dataPlayer()$Age)
  }) # Closes dataInput
  
  # = = = Setting the player pts scaled
  dataPlayerPTSscaled <- eventReactive(input$go,{
    (tail(dataPlayer()$pts_scaled,1))
  }) # Closes dataInput
  
  # = = = Setting the player pts scaled
  dataPlayerSALscaled <- eventReactive(input$go,{
    (tail(dataPlayer()$sal_scaled,1))
  }) # Closes dataInput
  
  # = = = = = = = = = = = = =
  
  dataPlayerLine <- eventReactive(input$go, {
    switch(input$var, 
           "Goals" = dataPlayer()$goals_million,
           "Assists" = dataPlayer()$assists_million,
           "Points" = dataPlayer()$point_.million,
           "Shots" = dataPlayer()$shots_million,
           "TimeonIce" = dataPlayer()$time_on_ice_million)
  }) # Closes dataPlayerLine
  
  
  dataLeaugeValue <- eventReactive(input$go, {
    switch(input$var, 
           "Goals" = df_sal$goals_million,
           "Assists" = df_sal$assists_million,
           "Points" = df_sal$point_.million,
           "Shots" = df_sal$shots_million,
           "TimeonIce" = df_sal$time_on_ice_million)
  }) # Closes dataPlayerLine
  
  
  
  output$hist1 <- renderPlot({
    t = tail(dataPlayerLine(),1)
    #lt = log(t)
    #lt = dataPlayerLine()
    lx = c(t,t)
    ly = c(0,1000)
    ggplot() +
      geom_histogram(aes(dataLeaugeValue())) +
      #geom_histogram(aes(dataInput() / dataSalaryInput())) +
      geom_line(aes(x = lx, y = ly, color='red'),size=2.5)+
      xlab(dataHistVariableLabel()) +
      ylab('Number of observations') +
      theme(axis.text.x  = element_text(size=16),
            axis.title.x = element_text(size=16),
            axis.text.y  = element_text(size=16),
            axis.title.y = element_text(size=16),
            panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = 0.5, linetype = "solid"))
  })  
  
  
  # output$scatterplot1 <- renderPlot({
  #        plot(dataSalaryInput(),df_sal$Salary_mil,
  #        col='green',
  #       xlab = input$var,
  #        ylab = 'Salary in Millions',
  #        main = 'League Cost for Variable')
  #  })
  
  output$scatter2 <- renderPlot({
    ggplot() +
      geom_line(aes(x = dataPlayerAge(),y = dataInput()),size=1.5) +
      geom_point(aes(x = dataPlayerAge(),y = dataInput()),size=3.25) +
      xlab('Age') +
      ylab(isolate(input$var)) +
      xlim(18,35) +
      #ylim(0, 50) +
      ggtitle(isolate(input$player)) +
      theme(plot.title = element_text(lineheight=.8, size = 30, face="bold"), 
            axis.text=element_text(size=18), axis.title=element_text(size=18,face="bold"),
            legend.position="none",
            axis.text.x  = element_text(size=20),
            axis.title.x = element_text(size=20),
            axis.text.y  = element_text(size=20),
            axis.title.y = element_text(size=20),
            panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = 0.5, linetype = "solid"))
    #theme_bw()
    
    #plot(dataPlayerAge(),dataInput(),
    #     type = "b", 
    #     col = "red", 
    #     lwd = 3,
    #     xlab = "Age",
    #     ylab = input$var,
    #     main = input$player)
  })
  
  output$Player_Name <- renderText({
    input$player
  })
  
  
  # = = = = = = = = = = = = = = = = = =  
  # reactive object for input slider #
  dataTable <- eventReactive(input$go,{
    (df_sal2[df_sal2$NameCode_x == input$player,c(7,48,49,50)])
  }) # Closes data
  # = = = = = = = = = = = = = = = = = = 
  
  output$table1 <- renderTable({
    dataTable()
  })
  
  
  output$scatter3 <- renderPlot({
    ggplot() +
      geom_point(aes(x = df_sal$pts_scaled,y = df_sal$sal_scaled), color = 'grey', size=2.5) +
      geom_point(aes(x = df_sal$pts_scaled, y = df_sal$sal_scaled), color = 'black', pch=21,size = 2.5,alpha=.3) +
      xlim(-1.5, 3) + ylim(-1.5, 3) +
      geom_line(aes(x = xl_a, y = yl_s), color = 'black') +
      geom_line(aes(x = xl_s, y = yl_a), color = 'black') +
      geom_point(aes(x = dataPlayerPTSscaled(), y = dataPlayerSALscaled()), color = 'red', size = 6) +
      xlab('Normalized Points Scored') +
      ylab('Normalized Salary in millions') +
      theme(axis.text.x  = element_text(size=16),
            axis.title.x = element_text(size=16),
            axis.text.y  = element_text(size=16),
            axis.title.y = element_text(size=16),
            panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = 0.5, linetype = "solid"))
  })
  
  
}





shinyApp(ui = ui, server = server)
