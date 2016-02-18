unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Unit2, mmsystem, LCLType;

type

  { TClickAdventure }

  TClickAdventure = class(TForm)
    buttonNorth: TButton; // north,east,... are code names only
    buttonEast: TButton;
    buttonWest: TButton;
    buttonSouth: TButton;
    backgroundImage: TImage;
    buttonItem: TImage;
    buttonBag: TImage;
    guiImage: TImage;
    buttonContinue: TImage;
    labelRegion: TLabel;
    labelVersion: TLabel;
    roomDescription: TMemo;
    riddleText: TMemo;
    riddleButtonOne: TButton;
    riddleButtonTwo: TButton;
    riddleButtonThree: TButton;
    riddleButtonFour: TButton;

    procedure FormShow(Sender: TObject);
    procedure onRoomNavigationButtonClicked(Sender: TObject);
    procedure onRiddleButtonClicked(Sender: TObject);
    procedure onSearchItemClicked(Sender: TObject);
    procedure onBagButtonClicked(Sender: TObject);
    procedure onContinueDialogClicked(Sender: TObject);

    procedure startAdventure;
    procedure buildAdventure;

  private
    { private declarations }
  public
    { public declarations }
  end;

type
  { TItem }
  TItem = class
    id: integer;
    message:string;
    name:string;
    constructor create(i:integer;s,msg:string);
    // TODO: add function to retrieve item image by id
  end;

type
  { TBag }
  TBag = class
    items : array [1..20] of TItem; // NOTE: ONE based
    // TODO: adjust max index
    constructor create;
    function contains(id:integer):boolean;
    procedure add(item:TItem);
  end;

type
  { TDialog }
  TDialog = class
    beforeRiddle: boolean;
    lines: array of string;
    currentLine: integer;
    constructor create;
    procedure show;
    procedure showNext;
    procedure hide;
  end;

type
  { TRiddle }
  TRiddle = class
    question:string;
    optionOne,optionTwo,optionThree,optionFour:string;
    answer:string;
    constructor create(q,o1,o2,o3,o4,a:string);
  end;

type
  { TRoom }
  TRoom = class

    backgroundImagePath:string; // path to img: /img/[name].[extension]
    backgroundMusicPath:string; // path to music: /music/[name].wav
    backgroundMusicPathAfterRiddle:string;

    descriptionBeforeRiddle:string;
    dialogBeforeRiddle:TDialog;
    descriptionAfterRiddle:string;
    dialogAfterRiddle:TDialog;

    region:string; // format: [REGION] - [MAP]

    north,east,south,west:TRoom;
    labelNorth,labelEast,labelSouth,labelWest:string;

    riddleQuestion, riddleAnswer,
      riddleOptionOne, riddleOptionTwo,
        riddleOptionThree, riddleOptionFour:string;

    riddleSolved:boolean;

    item:TItem;
    requiredItem:integer; // id of item required to enter the room, -1 for none
    requiredItemArray:array of integer;
    killWithoutItem:boolean; // reset to spawn when entering without item
    deathMessage:string;
    respawn:TRoom;

    constructor create;

    procedure applyRiddle(riddle:TRiddle);
    function canEnter(bag:TBag):boolean;

  end;

var
  ClickAdventure: TClickAdventure;
  currentRoom: TRoom;
  bag: TBag;
  spawnRoom: TRoom;
  currentDialog:TDialog;
  playerName:string;

implementation

{$R *.lfm}

procedure afterDialogAfterRiddle;
begin

  // show search item button and show bag button
  ClickAdventure.buttonItem.Visible:=true;
  ClickAdventure.buttonBag.Visible:=true;

  // make room navigation buttons visible
  ClickAdventure.buttonNorth.Visible := (currentRoom.north <> nil);
  ClickAdventure.buttonEast.Visible := (currentRoom.east <> nil);
  ClickAdventure.buttonSouth.Visible := (currentRoom.south <> nil);
  ClickAdventure.buttonWest.Visible := (currentRoom.west <> nil);

  // update button label if necessary
  if(ClickAdventure.buttonNorth.Visible) then
  begin
    ClickAdventure.buttonNorth.Visible:=currentRoom.north.canEnter(bag);
    ClickAdventure.buttonNorth.Caption:=currentRoom.labelNorth;
  end;
  if(ClickAdventure.buttonEast.Visible) then
  begin
    ClickAdventure.buttonEast.Visible:=currentRoom.east.canEnter(bag);
    ClickAdventure.buttonEast.Caption:=currentRoom.labelEast;
  end;
  if(ClickAdventure.buttonSouth.Visible) then
  begin
    ClickAdventure.buttonSouth.Visible:=currentRoom.south.canEnter(bag);
    ClickAdventure.buttonSouth.Caption:=currentRoom.labelSouth;
  end;
  if(ClickAdventure.buttonWest.Visible) then
  begin
    ClickAdventure.buttonWest.Visible:=currentRoom.west.canEnter(bag);
    ClickAdventure.buttonWest.Caption:=currentRoom.labelWest;
  end;

end;

procedure onRiddleSolved();
begin

  // save riddle solved state
  currentRoom.riddleSolved:=true;

  // hide riddle options

  ClickAdventure.riddleButtonOne.Visible:=false;
  ClickAdventure.riddleButtonTwo.Visible:=false;
  ClickAdventure.riddleButtonThree.Visible:=false;
  ClickAdventure.riddleButtonFour.Visible:=false;

  // start music
  if(currentRoom.backgroundMusicPathAfterRiddle <> '') then
  begin
    sndPlaySound(pchar(UTF8ToSys(Application.Location + currentRoom.backgroundMusicPathAfterRiddle)), snd_Async or snd_NoDefault);
  end;

  // add room description

  if(currentRoom.descriptionAfterRiddle <> '') then
  begin
    ClickAdventure.roomDescription.Lines.Add(currentRoom.descriptionAfterRiddle);
  end;

  if(currentRoom.dialogAfterRiddle <> nil) then
  begin

    // show dialog
    currentDialog:=currentRoom.dialogAfterRiddle;

    currentDialog.beforeRiddle:=false;
    currentDialog.show;

  end
  else
  begin
    afterDialogAfterRiddle; // no dialog --> go ahead
  end;

end;

procedure afterDialogBeforeRiddle;
begin

  if( (currentRoom.riddleQuestion <> '') and (not currentRoom.riddleSolved) ) then
  begin

    // show riddle

    ClickAdventure.riddleText.Visible:=true;
    ClickAdventure.riddleText.Lines.Add(currentRoom.riddleQuestion);

    // show riddle answers if necessary

    if(currentRoom.riddleOptionOne <> '') then
    begin
      ClickAdventure.riddleButtonOne.Visible:=true;
      ClickAdventure.riddleButtonOne.Caption:=currentRoom.riddleOptionOne;
    end;

    if(currentRoom.riddleOptionTwo <> '') then
    begin
      ClickAdventure.riddleButtonTwo.Visible:=true;
      ClickAdventure.riddleButtonTwo.Caption:=currentRoom.riddleOptionTwo;
    end;

    if(currentRoom.riddleOptionThree <> '') then
    begin
      ClickAdventure.riddleButtonThree.Visible:=true;
      ClickAdventure.riddleButtonThree.Caption:=currentRoom.riddleOptionThree;
    end;

    if(currentRoom.riddleOptionFour <> '') then
    begin
      ClickAdventure.riddleButtonFour.Visible:=true;
      ClickAdventure.riddleButtonFour.Caption:=currentRoom.riddleOptionFour;
    end;

  end
  else
  begin
    onRiddleSolved(); // no riddle, so riddle is immediately 'solved'
  end;

end;

procedure changeRoom(room: TRoom);
begin

  currentRoom := room;

  // reset player to spawn when entering without required item
  if(room.killWithoutItem) then
  begin

    if(not bag.contains(room.requiredItem)) then
    begin

      if(room.deathMessage <> '') then
      begin
        ShowMessage(room.deathMessage);
      end;

      if(room.respawn <> nil) then
      begin
        changeRoom(room.respawn);
      end
      else
      begin
        changeRoom(spawnRoom);
      end;

      exit;
    end;

  end;

  // TODO: update GUI to new room ones

  // load background image
  if(room.backgroundImagePath <> '') then
  begin
    ClickAdventure.backgroundImage.Picture.LoadFromFile(Application.Location + room.backgroundImagePath);
  end;

  // load background music
  if(room.backgroundMusicPath <> '') then
  begin
    sndPlaySound(pchar(UTF8ToSys(Application.Location + room.backgroundMusicPath)), snd_Async or snd_NoDefault);
  end;

  // display region info
  if(room.region <> '') then
  begin
    ClickAdventure.labelRegion.Caption:=room.region;
  end;

  // show room description
  ClickAdventure.roomDescription.Lines.Clear;

  if(room.descriptionBeforeRiddle <> '') then
  begin
    ClickAdventure.roomDescription.Lines.Add(room.descriptionBeforeRiddle);
  end;

  // hide room navigation buttons
  ClickAdventure.buttonNorth.Visible:=false;
  ClickAdventure.buttonEast.Visible:=false;
  ClickAdventure.buttonSouth.Visible:=false;
  ClickAdventure.buttonWest.Visible:=false;

  // hide search item button and show bag button
  ClickAdventure.buttonItem.Visible:=false;
  ClickAdventure.buttonBag.Visible:=false;

  ClickAdventure.riddleText.Lines.Clear;

  if(room.dialogBeforeRiddle <> nil) then
  begin

    // show dialog
    currentDialog := room.dialogBeforeRiddle;

    currentDialog.beforeRiddle:=true;
    currentDialog.show;

  end
  else
  begin
    afterDialogBeforeRiddle; // no dialog --> go ahead
  end;

end;

function compile(s:string):string;
begin
  result:=StringReplace(s,'%n',playerName,[rfReplaceAll]);
end;

{ TRoom }
constructor TRoom.create;
begin

  // initialize with standard values

  backgroundImagePath:='';
  backgroundMusicPath:='';
  backgroundMusicPathAfterRiddle:='';

  descriptionBeforeRiddle:='';
  descriptionAfterRiddle:='';

  region:='';

  labelNorth:='Norden';
  labelEast:='Osten';
  labelSouth:='Süden';
  labelWest:='Westen';

  riddleQuestion:='';
  riddleAnswer:='';
  riddleOptionOne:='';
  riddleOptionTwo:='';
  riddleOptionThree:='';
  riddleOptionFour:='';

  riddleSolved := false;

  requiredItem:=-1;
  SetLength(requiredItemArray,0);
  killWithoutItem:=false;
  deathMessage:='';

end;

procedure TRoom.applyRiddle(riddle:TRiddle);
begin
  riddleQuestion:=riddle.question;
  riddleOptionOne:=riddle.optionOne;
  riddleOptionTwo:=riddle.optionTwo;
  riddleOptionThree:=riddle.optionThree;
  riddleOptionFour:=riddle.optionFour;
  riddleAnswer:=riddle.answer;
end;

function TRoom.canEnter(bag:TBag):boolean;
var
  i:integer;
begin

  result := false;

  if(requiredItem <> -1) then
  begin

    if(not killWithoutItem) then
    begin
      result := bag.contains(requiredItem);
    end
    else
    begin
      result := true;
    end;

  end
  else
  begin
    result := true;
  end;

  if(Length(requiredItemArray) > 0) then
  begin

    for i:=0 to (Length(requiredItemArray)-1) do
    begin

      if(not bag.contains(requiredItemArray[i])) then
      begin
        result := false;
      end;

    end;

  end;

end;

{ TItem }

constructor TItem.create(i:integer;s,msg:string);
begin
  id := i;
  message:=msg;
  name:=s;
end;

{ TBag }
constructor TBag.create;
var
  i:integer;
begin

  for i:=1 to Length(items) do
  begin
    items[i]:=TItem.create(-1,'','');
  end;

end;

function TBag.contains(id:integer):boolean;
var
  i:integer;
begin

  result := false;

  for i:=1 to Length(items) do
  begin

    if(items[i].id=id) then
    begin
      result:=true;
    end;

  end;

end;

procedure TBag.add(item:TItem);
var
  i:integer;
begin

  if(not contains(item.id))then
  begin

    i := 1;

    while(items[i].id <> -1) do
    begin
      i := i+1;
    end;

    items[i]:=item;

  end;

end;

{ TDialog }
constructor TDialog.create;
begin
  beforeRiddle:=true;
  currentLine := 0; // optional: change to start index
end;

procedure TDialog.show;
begin

  //ClickAdventure.buttonItem.Visible:=false;
  //ClickAdventure.buttonBag.Visible:=false;

  showNext;

end;

procedure TDialog.showNext;
begin

  if(currentLine < Length(lines)) then
  begin
    ClickAdventure.roomDescription.Lines.Add(lines[currentLine]);
    currentLine:=currentLine+1;
    ClickAdventure.buttonContinue.Visible:=true;
  end
  else
  begin
    hide;
  end;

end;

procedure TDialog.hide;
begin

  ClickAdventure.buttonContinue.Visible:=false;
  //ClickAdventure.buttonItem.Visible:=true;
  //ClickAdventure.buttonBag.Visible:=true;

  if(beforeRiddle) then
  begin
    afterDialogBeforeRiddle;
  end
  else
  begin
    afterDialogAfterRiddle;
  end;

end;

{ TRiddle }
constructor TRiddle.create(q,o1,o2,o3,o4,a:string);
begin
  question:=q;
  optionOne:=o1;
  optionTwo:=o2;
  optionThree:=o3;
  optionFour:=o4;
  answer:=a;
end;

{ TClickAdventure }

procedure TClickAdventure.onRoomNavigationButtonClicked(Sender: TObject);
begin

  if(Sender = buttonNorth) then
  begin
    changeRoom(currentRoom.north);
  end;

  if(Sender = buttonEast) then
  begin
    changeRoom(currentRoom.east);
  end;

  if(Sender = buttonSouth) then
  begin
    changeRoom(currentRoom.south);
  end;

  if(Sender = buttonWest) then
  begin
    changeRoom(currentRoom.west);
  end;

end;

procedure TClickAdventure.onRiddleButtonClicked(Sender: TObject);
begin

  if(TButton(Sender).Caption=currentRoom.riddleAnswer)then
  begin
    onRiddleSolved();
  end
  else
  begin
    changeRoom(spawnRoom);
  end;

end;

procedure TClickAdventure.onSearchItemClicked(Sender: TObject);
begin

  if(currentRoom.item <> nil) then
  begin

    if(not bag.contains(currentRoom.item.id)) then
    begin
      roomDescription.Lines.Add(currentRoom.item.message);
      bag.add(currentRoom.item);
    end
    else
    begin
      roomDescription.Lines.Add('Du hast kein Item gefunden');
    end;

  end
  else
  begin
    roomDescription.Lines.Add('Du hast kein Item gefunden');
  end;

  buttonItem.Visible:=false;

end;

procedure TClickAdventure.onBagButtonClicked(Sender: TObject);
var
  i:integer;
begin

  for i := 1 to Length(bag.items) do
  begin
    BagView.itemNames[i] := bag.items[i].name;
  end;

  BagView.ShowModal; // optional: replace with .Show

end;

procedure TClickAdventure.onContinueDialogClicked(Sender: TObject);
begin

  currentDialog.showNext;

end;

procedure TClickAdventure.FormShow(Sender: TObject);
begin
  buildAdventure();
end;

procedure TClickAdventure.startAdventure;
begin
  changeRoom(spawnRoom);
end;

procedure TClickAdventure.buildAdventure;
var
  i:integer;
  rooms: array [1..52] of TRoom;
  dialogsBefore, dialogsAfter: array [1..52] of TDialog;
  dialogLinesBefore, dialogLinesAfter: array [1..52] of array of string;

  room25requiredItems:array of integer;

  riddlesEasy: array[1..12] of array[1..3] of TRiddle;
  riddlesBoss: array[1..12] of array[1..3] of TRiddle;

  category1,category2,category3:integer;

begin

  for i := 0 to 52 do
  begin
    rooms[i] := TRoom.create; // create all rooms with default values
  end;

  playerName:=InputBox('Vorbereitung','Gib einen Namen für deinen Charakter ein','Name');

  category1 := QuestionDlg('Vorbereitung','Am schnellsten löse ich landesweite Probleme…',mtCustom,[1,'… durch komplexe mathematische Berechnungen und Formeln. (Mathematik)', 2, '… indem ich das politische Geschehen im Land erforsche und partizipiere. (Politik)', 'IsDefault', 3, '… durch Forschungen in der Geschichte des Landes und Rekonstruierung. (Geschichte)', 4, '… durch Bereicherung der Kultur mit Musik und Kunst. (Musik/ Kunst)'],'');
  category2 := QuestionDlg('Vorbereitung','Eine bevorstehende Katastrophe sehe ich voraus, indem ich…',mtCustom,[5,'… Fehlkonstellationen der Sterne und Anomalien in den physikalischen Gesetzen feststelle. (Physik)', 6, '… die Fehlfunktion der Server sowie der Soft- und Hardware erkenne. (Informatik)', 'IsDefault', 7, '… die Ratschläge und Prognosen der Literatur aller Epochen in einen logischen Kontext bringe. (Literatur)', 8, '… besonders die Tektonik und das Wetter untersuche und Unregelmäßigkeiten feststelle. (Erdkunde)'],'');
  category3 := QuestionDlg('Vorbereitung','Durch… kann einem kranken Menschen am besten geholfen werden.',mtCustom,[9,'… Gebete, Segen und Glauben (Religion)', 10, '… moderne Arzneimittel und biologische Errungenschaften (Biologie)', 'IsDefault', 11, '… chemische Behandlungen, Tränke und unentdeckte Verbindungen (Chemie)', 12, '… Bewegung, körperliches Training und Kraftaufwand (Sport)'],'');

  // add riddles

  // category 'Mathe' (1)
  riddlesEasy[1][1]:=TRiddle.create('Probe 815: Kristallgitterbereich - 1.512 Einheiten. Kristallgitterbegrenzung - 174 Einheiten. Abmessung identifizieren.','756 x 87','63 x 24','72 x 18','57 x 29','63 x 24');
  riddlesEasy[1][2]:=TRiddle.create('Probe 097: Produktion erhöht sich: 6, 18, 30, 42. Berechnung der geschätzten Produktion für nächsten Zyklus mit aktuellem Muster.','54','52','60','56','54');
  riddlesEasy[1][3]:=TRiddle.create('Probe 624: Lagerkapazität: 4250 Einhalten. Durchschnittliche Subjektgöße: 170 Einheiten. Maximale Subjektmenge in Lager berechnen.','15','22','25','18','25');
  riddlesBoss[1][1]:=TRiddle.create('Probe 371: Kreis wird mit 4 Geraden zerteilt. Maximale Anzahl der resultierenden Teilstücke berechnen.','8','9','11','13','11');
  riddlesBoss[1][2]:=TRiddle.create('Probe 965: Straße steigt von 0 - 3 Längeneinheiten nach Funktion: y=0,5(x)^2. Berechne Fläche unter gegebenem Längenabschnitt.','4,5','9','3','6,5','4,5');
  riddlesBoss[1][3]:=TRiddle.create('Probe 459:  Zahl n gesucht. 2n + 0,5n + 0,25n + 1 = 100.','28','38','32','36','36');
  // category 'Politik' (2)
  riddlesEasy[2][1]:=TRiddle.create('Probe 077: Auflistung Deutscher Bundeskanzler fehlerhaft. Ersten Kanzler angeben.','Angela Merkel','Konrad Adenauer','Ludwig Erhard','Theodor Heuss','Konrad Adenauer');
  riddlesEasy[2][2]:=TRiddle.create('Probe 106: Datenspeicher defekt. Datum der nächsten Bundestagswahl angeben.','2016','2019','2017','2020','2017');
  riddlesEasy[2][3]:=TRiddle.create('Probe 369: Schaubild vervollständigen. Nicht existierendes Organ des politischen Systems Deutschlands angeben.','Landesverfassungsgericht','Bundesrat','Bundessenat','Bundeskabinett','Bundessenat');
  riddlesBoss[2][1]:=TRiddle.create('Probe 290: Politikforschung abgeschlossen. Anzahl aller bisherigen deutschen Bundespräsidenten angeben.','8','9','10','11','11');
  riddlesBoss[2][2]:=TRiddle.create('Probe 150: Parteienliste aktualisiert. Partei identifizieren, die in keinem Parlament Deutschlands vertreten ist.','NPD','ALFA','DKP','FDP','DKP');
  riddlesBoss[2][3]:=TRiddle.create('Probe 203: Listen des BIP verglichen. Land mit vergleichsweise höchstem BIP gefunden.','Japan','Russland','Frankreich','Deutschland','Japan');
  // category 'Geschichte' (3)
  riddlesEasy[3][1]:=TRiddle.create('Probe 399: Datensicherung Kriege unvollständig. Datum des Beginns des 1. Weltkrieges angeben.','1914','1939','1789','1871','1914');
  riddlesEasy[3][2]:=TRiddle.create('Probe 257: Forschungsergebnisse manipuliert. Nachname des französischen Kaisers ab 1804  gesucht.','Robespierre','Diderot','Montesquieu','Bonaparte','Bonaparte');
  riddlesEasy[3][3]:=TRiddle.create('Probe 356: Suchmodul gestoppt. Datum der Kaiserkrönung Karls des Großen verloren gegangen.','800','912','753','0','800');
  riddlesBoss[3][1]:=TRiddle.create('Probe 886: Erdzeitalter sortieren. Suche ältestes Zeitalter.','Jura','Quartär','Trias','Kreide','Trias');
  riddlesBoss[3][2]:=TRiddle.create('Probe 808: Zeiteingrenzungsmodul beschädigt. Jahr des Röhm-Putsches gesucht.','1933','1930','1936','1934','1934');
  riddlesBoss[3][3]:=TRiddle.create('Probe 133: Geschichte Deutschlands untersucht. Name des ersten deutschen Kaisers identifizieren.','Otto','Wilhelm','Karl','Leopold','Wilhelm');
  // category 'Musik/Kunst' (4)
  riddlesEasy[4][1]:=TRiddle.create('Probe 769: Vergleich der Notenschlüssel. Gesucht: Note im Bassschlüssel, die im Violinenschlüssel auf dem kleinen d liegt.','c','d','e','f','f');
  riddlesEasy[4][2]:=TRiddle.create('Probe 694: Epochen im Vergleich. Älteste der 4 angegebenen Kunstepochen angeben.','Barock','Gotik','Romantik','Impressionismus','Gotik');
  riddlesEasy[4][3]:=TRiddle.create('Probe 464: Kunstgeschichte erforscht. Jüngsten der angegebenen Künstler identifizieren.','Hundertwasser','Dali','Matisse','Picasso','Hundertwasser');
  riddlesBoss[4][1]:=TRiddle.create('Probe 110: Musikimpressionen alter Bands. Suche Band, die am frühsten gegründet wurde.','Beatles','Simon & Garfunkel','Rolling Stones','Genesis','Simon & Garfunkel');
  riddlesBoss[4][2]:=TRiddle.create('Probe 174: Besuch des Vatikans. Benennung des Künstlers der Deckengemälde der Sixtinischen Kapelle.','Dürer','Botticelli','Michelangelo','Rembrandt','Michelangelo');
  riddlesBoss[4][3]:=TRiddle.create('Probe 065: Goldene Schalplatten eingelesen. Erkennung des meist ausgezeichneten Interpreten.','Elvis Presley','Pink Floyd','Michael Jackson','Beatles','Beatles');
  // category 'Physik' (5)
  riddlesEasy[5][1]:=TRiddle.create('Probe 196: Einheit der Rechnung m*g*h gesucht.','Joule','Newton','Watt','Ampere','Joule');
  riddlesEasy[5][2]:=TRiddle.create('Probe 941: Reagenz 1: Wasser, Reagenz 2: Leichtbenzin. Phasengrenze beschreiben.','Phasengrenze nicht vorhanden','Reagenz 1 über Reagenz 2','Reagenz 2 über Reagenz 1','Reagenz 1 und 2 im Wechsel','Reagenz 2 über Reagenz 1');
  riddlesEasy[5][3]:=TRiddle.create('Probe 117: Kraft benennen, die auf Körper in Flüssigkeiten und Gasen wirkt. Vektor besitzt entgegengesetzte Richtung zur Schwerkraft.','Radialkraft','Auftriebskraft','Treibkraft','Fliehkraft','Auftriebskraft');
  riddlesBoss[5][1]:=TRiddle.create('Probe 328: Fehlerhafte Komponente eines Hebels bestimmen.','Druckarm','Lastarm','Angelpunkt','Kraftarm','Druckarm');
  riddlesBoss[5][2]:=TRiddle.create('Probe 038: Gleichförmig rotierendes System: P1,2 auf Rotationsbahnen. P1: r=5. P2: r= 8. Absolute Geschwindigkeiten vergleichen.','P1 schneller als P2','P2 und P1 gleich schnell','Geschwindigkeiten = 0','P2 schneller als P1','P2 schneller als P1');
  riddlesBoss[5][3]:=TRiddle.create('Probe 762: Faktor x zur Berechnung einer Energie auswählen: E=m*a*x','t','g','q','s','s');
  // category 'Informatik' (6)
  riddlesEasy[6][1]:=TRiddle.create('Probe 960: Datenbanken der Programmiersprachen erfolgreich durchsucht. Nicht-existente Sprache ausfindig machen.','Python','LUA','Turtle','C','Turtle');
  riddlesEasy[6][2]:=TRiddle.create('Probe 297: Display optimiert. Mögliche Signalausgänge der Grafikkarte untersucht. Falschen Output angeben.','HDMI','DVI','Display Port','Klinke','Klinke');
  riddlesEasy[6][3]:=TRiddle.create('Probe 996: Speicherplatzüberprüfung positiv. Größe von 2GB in MB angeben.','2000','2048','2024','1984','2048');
  riddlesBoss[6][1]:=TRiddle.create('Probe 767: Hardware optimiert. Abkürzung ROM erläutern.','Refresh Only Memory','Read Only Memory','Read One Memory','Reliable Online Memory','Read Only Memory');
  riddlesBoss[6][2]:=TRiddle.create('Probe 218: Betriebssysteme installiert. GUI geladen. Erstes Betriebssystem mit grafischer Oberfläche nennen.','Mac OS','DOS 5.0','Linux','Windows 98','Mac OS');
  riddlesBoss[6][3]:=TRiddle.create('Probe 569: Englische Hardwarebezeichnungen untersucht. Bedeutung "mainframe" angeben.','Druckserver','Server im lokalen Netz','zentraler Großcomputer','alleinstehendes Notebook','zentraler Großcomputer');
  // category 'Literatur' (7)
  riddlesEasy[7][1]:=TRiddle.create('Probe 632: Analyse von Goethes Werken abgeschlossen. Identifiziere nicht von Goethe stammendes Werk.','Werther','Faust','Prometheus','Wilhelm Tell','Wilhelm Tell');
  riddlesEasy[7][2]:=TRiddle.create('Probe 755: Literarische Epochen im Vergleich. Jüngste der 4 angegebenen Epochen angeben.','Klassik','Naturalismus','Aufklärung','Sturm und Drang','Naturalismus');
  riddlesEasy[7][3]:=TRiddle.create('Probe 488: Betrachtung der literarischen Großgattungen. Nicht-literarische Großgattung benennen.','Romantik','Lyrik','Dramatik','Epik','Romantik');
  riddlesBoss[7][1]:=TRiddle.create('Probe 913: Untersuchung der Todesursache Heinrichs vin Kleist. Todesursache angeben.','Drogenkonsum','Krankheit','Mord','Suizid','Suizid');
  riddlesBoss[7][2]:=TRiddle.create('Probe 082: Moderne Literatur. Ort der größten deutschen Buchmesse (neben Leipzig) angeben.','Dresden','Köln','Frankfurt','Hamburg','Frankfurt');
  riddlesBoss[7][3]:=TRiddle.create('Probe 858: Klassische Dramen. Bennant werden soll die Gestalt, in der Mephiso Faust erscheint.','Pferd','Katze','Rabe','Pudel','Pudel');
  // category 'Erdkunde' (8)
  riddlesEasy[8][1]:=TRiddle.create('Probe 219: Luftprobe entnommen. Gas mit größtem Volumenanteil in der Luft identifizieren.','Stickstoff','Sauerstoff','Kohlenstoffdioxid','Wasserdampf','Stickstoff');
  riddlesEasy[8][2]:=TRiddle.create('Probe 287: Teleskop verwendet. Anzahl der Planeten im Sonnensystem angeben.','10','9','8','7','8');
  riddlesEasy[8][3]:=TRiddle.create('Probe 644: Weltreise wird abgeschlossen. Hauptstadt Japans gesucht.','Kyoto','Shanghai','Yokohama','Tokio','Tokio');
  riddlesBoss[8][1]:=TRiddle.create('Probe 576: Eisenerzbohrung erfolgreich. Filtere Produkt, welches kein Eisenerz ist.','Magneteisenstein','Schiefereisenstein','Roteisenstein','Raseneisenstein','Schiefereisenstein');
  riddlesBoss[8][2]:=TRiddle.create('Probe 506: Geologische Untersuchung zur Erdkruste. Plattentektonischen Vorgang benennen, bei dem sich eine Lithosphärenplatte unter eine andere schiebt.','Subduktion','Adaption','Subvention','Magmatisierung','Subduktion');
  riddlesBoss[8][3]:=TRiddle.create('Probe 866: Tiefseeforschung macht entdeckung. Bestimmung der tiefsten Stelle im Weltmeer.','Tongagraben','Boningraben','Yapgraben','Marianengraben','Marianengraben');
  // category 'Religion' (9)
  riddlesEasy[9][1]:=TRiddle.create('Probe 502: Religionen im Vergleich. Benennung des heiligen Buchs des Judentums.','Bibel','Koran','Tora','Pali-Kanon','Tora');
  riddlesEasy[9][2]:=TRiddle.create('Probe 376: Neues Testament gelesen. Nennung der Anzahl Jesu Jünger.','12','11','10','13','12');
  riddlesEasy[9][3]:=TRiddle.create('Probe 718: Weltreligionen werden eingeteilt. Anzahl der anerkannten Weltreligionen angeben.','3','4','5','2','5');
  riddlesBoss[9][1]:=TRiddle.create('Probe 687: Untersuche Gleichnisse in den Evangelien. Evangelium ohne Gleichnisse angeben.','Matthäus','Markus','Lukas','Johannes','Johannes');
  riddlesBoss[9][2]:=TRiddle.create('Probe 095: Religiöse Festlichkeiten. Nennung der Persönlichkeit, die an Weihnachten und Ostern den Segen "Urbi et orbi" ereilt.','Bischöfe','Papst','Priester','Kardinäle','Papst');
  riddlesBoss[9][3]:=TRiddle.create('Probe 494: Glaubensbekenntnis. Wer sitzt im Apostolischen Glaubensbekenntnis rechts von Gott?','Maria','Jesus Christus','Heiliger Geist','´Josef','Jesus Christus');
  // category 'Biologie' (10)
  riddlesEasy[10][1]:=TRiddle.create('Probe 632: Biologische Lehren benannt. Name der Lehre von den Zellen angeben.','Neurologie','Cytologie','Ökologie','Somatologie','Cytologie');
  riddlesEasy[10][2]:=TRiddle.create('Probe 989: Geschichte der Evolutionstheorie. Benennung des Begründers der modernen Evolutionstheorie.','Darwin','Dalton','Chaplin','Fleming','Darwin');
  riddlesEasy[10][3]:=TRiddle.create('Probe 218: Untersuchung der Volkskrankheiten. Grund für Diabetes Typ 2 angeben.','Insulinresistenz','Insulin-Überproduktion','Insulin-Unterproduktion','Glucagon-Abbaustörung','Insulinresistenz');
  riddlesBoss[10][1]:=TRiddle.create('Probe 783: Fachbegriffe der Körperbestandteile. Benennung des Darmtraktes (Latein).','Apparatus respiratorius','Systema nervosum','Canalis alimentarius','Medulla ossium','Canalis alimentarius');
  riddlesBoss[10][2]:=TRiddle.create('Probe 179: Untersuchung von Erfrischungsgetränken. Bestimmung des Getränks, welches ursprünglich eine medizinische Funktion hatte.','Ginger Ale','Coca Cola','Almdudler','Tonic Water','Tonic Water');
  riddlesBoss[10][3]:=TRiddle.create('Probe 377: Biologische Prozesse in der Untersuchung. Auswahl der zur Translation nötigen Strukturen (Proteinbiosynthese).','t-RNA, m-RNA, Ribosom','t-RNA, m-RNA, Phosphorrest','t-RNA, Cytoplasma, Protein','m-RNA, Ribosom, Protein','t-RNA, m-RNA, Ribosom');
  // category 'Chemie' (11)
  riddlesEasy[11][1]:=TRiddle.create('Probe 678: Substanz: Wasser. Atomanzahl der Elemente im Molekül angeben.','3x O, 2x H','2x O, 1x H','1x C, 2x O','2x H, 1x O','2x H, 1x O');
  riddlesEasy[11][2]:=TRiddle.create('Probe 412: Substanz: Schwefelsäure. Anzahl Dissoziationsstufen angeben.','1','4','3','2','2');
  riddlesEasy[11][3]:=TRiddle.create('Probe 299: Gesucht: Viertes Homologes Glied der Alkane.','Hexan','Butan','Propan','Tetran','Butan');
  riddlesBoss[11][1]:=TRiddle.create('Probe 894: Vergleich chemischer Polymere. Allgemeine Ordnung nach Siedetemperatur. Höchste SDT gesucht.','Duroplast','Thermoplast','Elastomer','Alle 3 gleich','Duroplast');
  riddlesBoss[11][2]:=TRiddle.create('Probe 220: Begriff 1: LEWIS-Säure, Begriff 2: Nucleophil, Begriff 3: Elektrophil, Begriff 4: LEWIS-Base. Verknüpfungen herstellen.','2 = 3, 1 = 4','1 = 2, 3 = 4','1 = 2 = 3 = 4','1 = 3, 2 = 4','1 = 3, 2 = 4');
  riddlesBoss[11][3]:=TRiddle.create('Probe 014: Prüfung eines Stoffes auf Aromatizität. Name der relevanten Regel angeben.','Jahn-Teller-Regel','Kepler-Fassregel','Hückel-Regel','Regel von Markovnikov','Hückel-Regel');
  // category 'Sport' (12)
  riddlesEasy[12][1]:=TRiddle.create('Probe 691: Auflistung von Kraftarten. Fehlerhafte Kraft entfernen.','Maximalkraft','Schlagkraft','Kraftausdauer','Schnellkraft','Schlagkraft');
  riddlesEasy[12][2]:=TRiddle.create('Probe 858: Olympische Sportarten. Nicht anerkannte Sportart angeben.','Triathlon','Kanusport','Baseball','Gewichteheben','Baseball');
  riddlesEasy[12][3]:=TRiddle.create('Probe 652: Fußball-Datenbanken verglichen. Weltmeister 2010 gesucht.','Spanien','Deutschland','Niederlande','Uruguay','Spanien');
  riddlesBoss[12][1]:=TRiddle.create('Probe 407: Profisport-Organisationen in Listen zusammengetragen. Fehlerhaften Eintrag identifizieren.','MLB','MLT','NFL','NBA','MLT');
  riddlesBoss[12][2]:=TRiddle.create('Probe 825: Kartensuche erfolglos. FIFA-Hauptsitz konnte nicht lokalisiert werden. Lokalisiere FIFA-Hauptsitz.','Frankfurt','Paris','Barcelona','Zürich','Zürich');
  riddlesBoss[12][3]:=TRiddle.create('Probe 885: Zeitplaner durcheinander. Super-Bowl-Termin üblich in welchem Quartal?','2. Quartal','4. Quartal','1. Quartal','3. Quartal','1. Quartal');

  bag := TBag.create;

  // room 1
  rooms[1].descriptionBeforeRiddle:=compile('%n: Wo bin ich? Was ist passiert? Ich kann mich an nichts mehr erinnern… Ich sollte mich zunächst aus diesem Schneegebiet entfernen. Lange halte ich das wahrscheinlich nicht durch… bei diesen Temperaturen.');
  rooms[1].labelEast:='Osten: Wintertau-Wald';
  rooms[1].region:='Wintertau-Lichtung';
  rooms[1].backgroundImagePath:='img\S1.jpg';
  rooms[1].backgroundMusicPath:='music\Snow.wav';
  rooms[1].east:=rooms[2];

  // room 2
  rooms[2].descriptionBeforeRiddle:=compile('%n: Hier gibt es nicht wirklich viele Möglichkeiten, sich zu verlaufen. Der Wald ist viel zu dicht, um Umwege zu gehen. Ich sollte besser dem Weg nach Süden folgen. Hoffentlich finde ich bald eine Antwort auf meine Fragen…');
  rooms[2].labelSouth:='Süden: Doldenvan-Passage';
  rooms[2].labelWest:='Westen: Wintertau-Lichtung';
  rooms[2].region:='Wintertau-Wald';
  rooms[2].backgroundImagePath:='img\S1.jpg';
  rooms[2].south:=rooms[3];
  rooms[2].west:=rooms[1];

  // room 3
  rooms[3].descriptionBeforeRiddle:=compile('%n: Ein umgestürzter Baum blockiert die Straße nach Süden… Offenbar lichtet sich der Wald dort etwas. Vielleicht kann mir der Besitzer dieses Gehöfts helfen.');
  rooms[3].labelNorth:='Norden: Wintertau-Wald';
  rooms[3].labelSouth:='Süden: Eissteppen';
  rooms[3].labelWest:='Westen: Alpenzur-Gehöft';
  rooms[3].region:='Doldenvan-Passage';
  rooms[3].backgroundImagePath:='img\S1.jpg';
  rooms[3].north:=rooms[2];
  rooms[3].west:=rooms[4];
  rooms[3].south:=rooms[5];

  // room 4
  dialogsBefore[4]:=TDialog.create;
  SetLength(dialogLinesBefore[4], 7);
  dialogLinesBefore[4][0]:=compile('%n: Hier ist es schön warm, ich wünschte ich könnte ewig hier bleiben. Soweit ich das erkennen kann, ist das hier eine Holzfällerhütte… all die Sägen hier an den Wänden.');
  dialogLinesBefore[4][1]:='Holzfäller: Grüße, Wanderer. Was verschlägt Euch in meine Hütte?';
  dialogLinesBefore[4][2]:=compile('%n: Ich weiß es selber nicht… Ich muss nach Süden und hatte gehofft, ihr könntet mir mit der versperrten Straße nach Süden weiterhelfen.');
  dialogLinesBefore[4][3]:='Holzfäller: Ach ja, die Frostwinde waren wohl wieder am Werk. Seit Neustem wüten sie hier durch die Zittergipfel und keiner kann es sich wirklich erklären. Muss wohl mit dem Erscheinen der Drachen zusammenhängen…';
  dialogLinesBefore[4][4]:=compile('%n: Drachen?!');
  dialogLinesBefore[4][5]:='Holzfäller: Keine Angst, es ist nur ein Mythos. Lasst Euch davon nicht abhalten. Ich helfe Euch gerne, wenn ihr mir bei einem kleinen Problem helft, was mich schon länger beschäftigt.';
  dialogLinesBefore[4][6]:=compile('%n: Natürlich.');
  dialogsBefore[4].lines:=dialogLinesBefore[4];
  rooms[4].dialogBeforeRiddle:=dialogsBefore[4];
  rooms[4].applyRiddle(riddlesEasy[category1][1]);
  dialogsAfter[4]:=TDialog.create;
  SetLength(dialogLinesAfter[4], 2);
  dialogLinesAfter[4][0]:='Holzfäller: So einfach hätte ich mir das gar nicht vorgestellt, danke. Hier, nehmt diese Axt. Ich habe sie gestern in einer Ranke gefunden, die hier im Schnee lag. Witzig, nicht? Eine Ranke hier im Schnee. Braucht sie mir auch nicht zurückbringen. Ich habe sowieso bessere Äxte. Also dann, gute Reise. Passt nur auf Issormir, den Eiswurm, der seit Wochen diese Gegend heimsucht, auf. Möge Dwayna Euch beschützen.';
  dialogLinesAfter[4][1]:=compile('%n: Danke. Euch auch.');
  dialogsAfter[4].lines:=dialogLinesAfter[4];
  rooms[4].dialogAfterRiddle:=dialogsAfter[4];
  rooms[4].item:=TItem.create(1,'Mystische Holzfälleraxt','Ihr erhaltet die mystische Holzfälleraxt aus den Zittergipfeln.');
  rooms[4].labelEast:='Osten: Doldenvan-Passage';
  rooms[4].region:='Alpenzur-Gehöft';
  rooms[4].backgroundImagePath:='img\S1.jpg';
  rooms[4].east:=rooms[3];

  // room 5
  rooms[5].descriptionBeforeRiddle:=compile('%n: Gut, dass dieser Wald hinter mir liegt. Ich habe gar nicht bemerkt, dass ich von so hohen Bergen umgeben bin. Mir bereiten nur die Drachen ein wenig Sorge… und dieser Issormir… wahrscheinlich nur dumme Gerüchte.');
  rooms[5].labelNorth:='Norden: Doldenvan-Passage';
  rooms[5].labelEast:='Osten: Maulent-Gipfel';
  rooms[5].labelSouth:='Süden: Schneeanstieg';
  rooms[5].requiredItem:=1;
  rooms[5].region:='Eissteppen';
  rooms[5].backgroundImagePath:='img\S3.jpg';
  rooms[5].east:=rooms[6];
  rooms[5].south:=rooms[7];
  rooms[5].north:=rooms[3];

  // room 6
  rooms[6].descriptionBeforeRiddle:=compile('%n: Hier ist es definitiv zu steil. Selbst erfahrene Bergsteiger kommen hier nicht weiter. Ich will auch gar nicht an die Lawinen denken, die hier runter kommen könnten. Besser, ich kehre um.');
  rooms[6].labelWest:='Westen: Eissteppen';
  rooms[6].region:='Maulent-Gipfel';
  rooms[6].backgroundImagePath:='img\S3.jpg';
  rooms[6].west:=rooms[5];

  // room 7
  rooms[7].descriptionBeforeRiddle:=compile('%n: Ich nähere mich dem Gipfel. Der Schnee peitscht mir ins Gesicht. Etwa wieder einer dieser Frostwinde, von denen der Holzfäller gesprochen hat? Ich muss aufs Schlimmste gefasst sein.');
  rooms[7].labelNorth:='Norden: Eissteppen';
  rooms[7].labelSouth:='Süden: Hochgipfel';
  rooms[7].region:='Schneeanstieg';
  rooms[7].backgroundImagePath:='img\S3.jpg';
  rooms[7].north:=rooms[5];
  rooms[7].south:=rooms[8];

  // room 8
  rooms[8].descriptionBeforeRiddle:=compile('%n: Tolle Aussicht hier oben. Schnee, Schnee und nochmal Schnee. Im Westen sehe ich einen Gletscher. Sieht bewacht aus. Auf jeden Fall sollte ich dort ohne Rechtfertigung nicht lang.');
  rooms[8].labelNorth:='Norden: Schneeanstieg';
  rooms[8].labelSouth:='Süden: Abgrund der Verzweifelung';
  rooms[8].labelWest:='Westen: Gletschergraben';
  rooms[8].region:='Hochgipfel';
  rooms[8].backgroundImagePath:='img\S3.jpg';
  rooms[8].north:=rooms[7];
  rooms[8].west:=rooms[9];
  rooms[8].south:=rooms[11];

  // room 9
  rooms[9].deathMessage:='Die bewaffneten Wachen nehmen Euch gefangen und töten Euch. Game Over.';
  dialogsBefore[9]:=TDialog.create;
  SetLength(dialogLinesBefore[9],4);
  dialogLinesBefore[9][0]:='Wache: Halt, sofort stehen bleiben. Svaard empfängt keine Besucher. Identifiziert Euch.';
  dialogLinesBefore[9][1]:=compile('%n: Ich habe den Kopf Issormirs dabei. Mir wurde befohlen, den Eiswurm zu töten, der seit Wochen terrorisiert.');
  dialogLinesBefore[9][2]:='Wache: Eintreten.';
  dialogLinesBefore[9][3]:=compile('%n: (Wer ist Svaard? Ich habe da ein ganz mieses Gefühl…)');
  dialogsBefore[9].lines:=dialogLinesBefore[9];
  rooms[9].dialogBeforeRiddle:=dialogsBefore[9];
  rooms[9].labelEast:='Osten: Hochgipfel';
  rooms[9].labelSouth:='Süden: Svaards Eisschollenpalast';
  rooms[9].region:='Gletschergraben';
  rooms[9].backgroundImagePath:='img\S4.jpg';
  rooms[9].east:=rooms[8];
  rooms[9].south:=rooms[10];
  rooms[9].requiredItem:=3;
  rooms[9].killWithoutItem:=true;

  // room 10
  dialogsBefore[10]:=TDialog.create;
  SetLength(dialogLinesBefore[10],3);
  dialogLinesBefore[10][0]:='Svaard: Tretet vor.';
  dialogLinesBefore[10][1]:=compile('%n: Ich habe Issormir geschlachtet. Hier ist sein Kopf.');
  dialogLinesBefore[10][2]:='Svaard: Schweigt. Die Macht der Erhabenen glüht in Euch. Ihr müsst sterben. Für Kralkatorrik.';
  dialogsBefore[10].lines:=dialogLinesBefore[10];
  rooms[10].backgroundMusicPath:='music\Svaard.wav';
  rooms[10].dialogBeforeRiddle:=dialogsBefore[10];
  rooms[10].applyRiddle(riddlesEasy[category3][1]);
  rooms[10].labelSouth:='Nächstes Rätsel';
  rooms[10].south:=rooms[44];
  // room 44 (10)
  rooms[44].applyRiddle(riddlesBoss[category1][1]);
  dialogsAfter[44]:=TDialog.create;
  SetLength(dialogLinesAfter[44],3);
  dialogLinesAfter[44][0]:=compile('%n: Was ist passiert? Ich wurde angegriffen, sein Schwert traf mich, aber ich konnte ihn irgendwie besiegen. Es war dasselbe Gefühl, wie bei Issormir.');
  dialogLinesAfter[44][1]:='Svaard: Niemand…kann…Ihn…aufhalten…';
  dialogLinesAfter[44][2]:=compile('%n: Huch, er hat irgendetwas fallen gelassen. Sieht aus wie eine Art Siegel. Aber es ist eiskalt. Seine Wachen sind auch verschwunden… als hätte der Gletscher sie verschluckt. Nun aber erst mal raus aus der Kälte hier. Nachdenken kann ich auch noch später.');
  dialogsAfter[44].lines:=dialogLinesAfter[44];
  rooms[44].dialogAfterRiddle:=dialogsAfter[44];
  rooms[44].item:=TItem.create(2,'Verdorbenes Frostsiegel','Ihr findet das verdorbene Frostsiegel Svaards. Man sagt, sein Besitzer könne alles zu Eis erstarren lassen.');
  rooms[44].labelNorth:='Norden: Gletschergraben';
  rooms[44].region:='Svaards Eisschollenpalast';
  rooms[44].backgroundImagePath:='img\S4.jpg';
  rooms[44].backgroundMusicPathAfterRiddle:='music\SnowASvaard.wav';
  rooms[44].north:=rooms[9];

  // room 11
  rooms[11].descriptionBeforeRiddle:=compile('%n: Es ist ziemlich dunkel hier unten, kaum zu glauben, dass oben alles weiß ist. Von Osten spüre ich eine gewisse Wärme… kann das sein?');
  rooms[11].labelNorth:='Norden: Hochgipfel';
  rooms[11].labelEast:='Osten: Erdrutsch-Tiefen';
  rooms[11].labelSouth:='Süden: Nebelriss-Klamm';
  rooms[11].region:='Abgrund der Verzweiflung';
  rooms[11].backgroundImagePath:='img\S2.jpg';
  rooms[11].north:=rooms[8];
  rooms[11].east:=rooms[13];
  rooms[11].south:=rooms[12];

  // room 12
  dialogsBefore[12]:=TDialog.create;
  SetLength(dialogLinesBefore[12],3);
  dialogLinesBefore[12][0]:=compile('%n: Ich kann kaum etwas sehen. Das Licht von oben dringt fast gar nicht bis hier unten durch. Ich sollte wohl lieber umkehren.');
  dialogLinesBefore[12][1]:='Issormir: Raaaaawwwwrrrrg!';
  dialogLinesBefore[12][2]:=compile('%n: Was ist das?! Ein Eiswurm? Issormir! Er greift mich an!');
  dialogsBefore[12].lines:=dialogLinesBefore[12];
  rooms[12].dialogBeforeRiddle:=dialogsBefore[12];
  rooms[12].applyRiddle(riddlesEasy[category2][1]);
  dialogsAfter[12]:=TDialog.create;
  SetLength(dialogLinesAfter[12],1);
  dialogLinesAfter[12][0]:=compile('%n: Ich bin nicht tot? Wie habe ich das geschafft? Ich sah Zeichen, logisch anzuordnen. Hängt das mit den ganzen Geschehnissen zusammen? Naja… Issormir ist zumindest enthauptet. Ich nehme besser seinen Kopf mit.');
  dialogsAfter[12].lines:=dialogLinesAfter[12];
  rooms[12].dialogAfterRiddle:=dialogsAfter[12];
  rooms[12].item:=TItem.create(3,'Kopf Issormirs','Ihr erhaltet den Kopf Issormirs. Er sieht sehr wertvoll aus.');
  rooms[12].labelNorth:='Norden: Abgrund der Verzweifelung';
  rooms[12].region:='Nebelriss-Klamm';
  rooms[12].backgroundImagePath:='img\S2.jpg';
  rooms[12].north:=rooms[11];

  // room 13
  rooms[13].descriptionBeforeRiddle:=compile('%n: Hier ist es erstaunlich heiß für ein verschneites Gebirge. Doch der Weg ist durch einen Erdrutsch versperrt. Hätte ich doch nur etwas, um den Erdrutsch zu entfernen…');
  rooms[13].labelEast:='Osten: Dämonenschlund';
  rooms[13].labelWest:='Westen: Abgrund der Verzweifelung';
  rooms[13].region:='Erdrutsch-Tiefen';
  rooms[13].backgroundImagePath:='img\S2.jpg';
  rooms[13].east:=rooms[14];
  rooms[13].west:=rooms[11];

  // room 14
  rooms[14].backgroundMusicPath:='music\Lava.wav';
  rooms[14].descriptionBeforeRiddle:=compile('%n: Der Erdrutsch! Das Siegel hat ihn zu Eis verwandelt, welches dann wegen der Hitze hier geschmolzen ist. Genial! Aber wo bin ich denn jetzt hier gelandet? Ein unterirdischer Vulkan? Ich kann nördlich von hier eine Art Lager erkennen. Vielleicht sollte ich mal nachfragen.');
  rooms[14].labelNorth:='Norden: Geheimes Basislager';
  rooms[14].labelSouth:='Süden: Urteilsfelsen';
  rooms[14].labelWest:='Westen: Erdrutsch-Tiefen';
  rooms[14].region:='Dämonenschlund';
  rooms[14].backgroundImagePath:='img\L3.jpg';
  rooms[14].north:=rooms[16];
  rooms[14].south:=rooms[15];
  rooms[14].west:=rooms[13];

  // room 15
  rooms[15].descriptionBeforeRiddle:=compile('%n: Bei Dwayna! Was ist denn hier passiert? Eine gigantische Rankenwand blockiert den Weg nach Osten. Es ist unmöglich, weiterzugehen. Etwas stimmt hier nicht. Ich sollte später wiederkommen…');
  rooms[15].labelNorth:='Norden: Dämonenschlund';
  rooms[15].labelEast:='Osten: Mattierte Baumkrone';
  rooms[15].region:='Urteilsfelsen';
  rooms[15].backgroundImagePath:='img\L3.jpg';
  rooms[15].north:=rooms[14];
  rooms[15].east:=rooms[35];

  // room 16
  rooms[16].backgroundMusicPath:='music\Lava2.wav';
  dialogsBefore[16]:=TDialog.create;
  SetLength(dialogLinesBefore[16],15);
  dialogLinesBefore[16][0]:=compile('%n: Grüßt Euch, könnt Ihr mir vielleicht in dieser Gegend hier weiterhelfen?');
  dialogLinesBefore[16][1]:='Explorator: Natürlich, wir sind die Exploratoren und erforschen den Mahlstrom seit vier Jahren! Insofern kennen wir uns hier perfekt aus.';
  dialogLinesBefore[16][2]:=compile('%n: Der Mahlstrom?');
  dialogLinesBefore[16][3]:='Explorator: Ich merke, Ihr kommt nicht von hier. Ihr befindet euch direkt neben einem gigantischen unterirdischen Vulkan- und Höhlensystem. Wir nennen es den „Mahlstrom“. Wir haben hier in der Aschen-Senke unser Basislager für die Expeditionen aufgeschlagen, um die Geheimnisse dieses Vulkans zu lüften.';
  dialogLinesBefore[16][4]:=compile('%n: Vielen Dank! Ist es denn gefährlich, hier ohne Begleitung herumzulaufen?');
  dialogLinesBefore[16][5]:='Explorator: Nein, nein. Das Gebiet ist größtenteils gesichert. Solange Ihr Gaheron in seiner Festung in Ruhe lasst, sollte Euch nichts zustoßen.';
  dialogLinesBefore[16][6]:=compile('%n: Gaheron?');
  dialogLinesBefore[16][7]:='Explorator: Euch muss man aber auch alles erklären. Gaheron Baelfeuer ist ein widerliches Geschöpf, vollgestopft mit Machtgier und bösem Willen. Irgend so ein Katzenwesen, können wir uns hier kaum vorstellen.';
  dialogLinesBefore[16][8]:=compile('%n: Böser Willen… das kommt mir bekannt vor. Sagt Euch der Name Svaard irgendetwas?');
  dialogLinesBefore[16][9]:='Explorator: Hmm, habe ich schon mal gehört. Soll ein Jäger in den Zittergipfeln sein. Mit riesigem Palast in einer Eisscholle.';
  dialogLinesBefore[16][10]:=compile('%n: Er erzählte irgendetwas von „Erhabenen“ und „Kralkatorrik“. Wisst Ihr etwas darüber? Mir bereitet das alles ein wenig Sorgen im Moment…');
  dialogLinesBefore[16][11]:='Explorator: Unmöglich. Kralkatorrik ist vor Jahrtausenden ausgestorben. Er war laut Mythos ein Drache, der in den fernen östlichen Wäldern gelebt hat. In welchem Kontext hat Svaard dies zu Euch gesagt?';
  dialogLinesBefore[16][12]:=compile('%n: Naja, er hat mir irgendetwas von den „Erhabenen“ erzählt und mich dann angegriffen, während er „Für Kralkatorrik“ gebrüllt hat. Ich kann mich nur noch daran erinnern, dass er danach tot vor mir lag.');
  dialogLinesBefore[16][13]:='Explorator: Wenn Ihr die Wahrheit sagt, dann müssen wir hier schleunigst verschwinden. Geht zu meinen 3 Kollegen hier in der Nähe und helft ihnen beim Aufbruch. Sie sollten sich in der Aschen-Senke, im Mahlstromkern und in der Mahlstromgalle aufhalten.Wir haben keine Zeit zu verlieren.';
  dialogLinesBefore[16][14]:=compile('%n: Warten Sie, sie haben etwas fallen gelassen… Zu spät. Ich sollte das lieber aufsammeln.');
  dialogsBefore[16].lines:=dialogLinesBefore[16];
  rooms[16].dialogBeforeRiddle:=dialogsBefore[16];
  rooms[16].item:=TItem.create(4,'Flammenbrecher-Fragment 1','Ihr erhaltet ein Flammenbrecher-Fragment.');
  rooms[16].labelNorth:='Norden: Mahlstromkern';
  rooms[16].labelEast:='Osten: Rußberne';
  rooms[16].labelSouth:='Süden: Dämonenschlund';
  rooms[16].labelWest:='Westen: Aschen-Senke';
  rooms[16].region:='Geheimes Basislager';
  rooms[16].backgroundImagePath:='img\L1.jpg';
  rooms[16].north:=rooms[20];
  rooms[16].east:=rooms[17];
  rooms[16].south:=rooms[14];
  rooms[16].west:=rooms[18];

  // room 17
  rooms[17].descriptionBeforeRiddle:=compile('%n: Hier hat man einen guten Überblick über das Lager. Im Norden und Westen sehe ich weitere Exploratoren. Ich sollte beim Aufbruch helfen, obwohl ich noch nicht weiß, wozu das alles…');
  rooms[17].labelWest:='Westen: Geheimes Basislager';
  rooms[17].region:='Rußberne';
  rooms[17].backgroundImagePath:='img\L3.jpg';
  rooms[17].west:=rooms[16];

  // room 18
  dialogsBefore[18]:=TDialog.create;
  SetLength(dialogLinesBefore[18],4);
  dialogLinesBefore[18][0]:='Explorator: Seid ihr der Reisende, der von Kralkatorrik erzählt hat?';
  dialogLinesBefore[18][1]:=compile('%n: ICH habe nicht von ihm erzählt, eigentlich war das Svaard.');
  dialogLinesBefore[18][2]:='Explorator: Egal, helft mir hier. Sonst sind wir alle beide bald nur noch Asche.';
  dialogLinesBefore[18][3]:=compile('%n: Es hat keinen Sinn, den jetzt noch weiter zu befragen. Ich sollte ihm lieber helfen.');
  dialogsBefore[18].lines:=dialogLinesBefore[18];
  rooms[18].dialogBeforeRiddle:=dialogsBefore[18];
  rooms[18].applyRiddle(riddlesEasy[category1][2]);
  dialogsAfter[18]:=TDialog.create;
  SetLength(dialogLinesAfter[18],4);
  dialogLinesAfter[18][0]:='Explorator: Danke vielmals. Nehmt bitte dieses Fragment. Ich habe es heute bei den Ausgrabungen gefunden. Ihr könnt bestimmt etwas damit anfangen.';
  dialogLinesAfter[18][1]:=compile('%n: Wer oder was sind die „Erhabenen“?');
  dialogLinesAfter[18][2]:='Explorator: Ein Gerücht. Nichts weiter. Es gibt keine übernatürlichen Kräfte. Niemand kann mit Hilfe seines Verstandes Macht erzeugen.';
  dialogLinesAfter[18][3]:=compile('%n: Danke…');
  dialogsAfter[18].lines:=dialogLinesAfter[18];
  rooms[18].dialogAfterRiddle:=dialogsAfter[18];
  rooms[18].item:=TItem.create(5,'Flammenbrecher-Fragment 2','Ihr erhaltet ein Flammenbrecher-Fragment.');
  rooms[18].labelNorth:='Norden: Mahlstromgalle';
  rooms[18].labelEast:='Osten: Geheimes Basislager';
  rooms[18].region:='Aschen-Senke';
  rooms[18].backgroundImagePath:='img\L1.jpg';
  rooms[18].north:=rooms[19];
  rooms[18].east:=rooms[16];

  // room 19
  dialogsBefore[19]:=TDialog.create;
  SetLength(dialogLinesBefore[19],2);
  dialogLinesBefore[19][0]:='Explorator: Da seid ihr ja. Bitte helft mir, das Lager abzubauen.';
  dialogLinesBefore[19][1]:=compile('%n: Hatte nichts anderes vor…');
  dialogsBefore[19].lines:=dialogLinesBefore[19];
  rooms[19].dialogBeforeRiddle:=dialogsBefore[19];
  rooms[19].applyRiddle(riddlesEasy[category2][2]);
  dialogsAfter[19]:=TDialog.create;
  SetLength(dialogLinesAfter[19],4);
  dialogLinesAfter[19][0]:=compile('%n: Kann ich Euch noch zu den Ranken hier in der Gegend befragen?');
  dialogLinesAfter[19][1]:='Explorator: Wir sind hier unter einem Dschungel, da ist es doch kein Wunder, mal ab und zu eine Ranke zu finden. Meint Ihr nicht?';
  dialogLinesAfter[19][2]:=compile('%n: Es ergibt keinen Sinn, dass sich die Ranken so willkürlich anordnen, wie ich sie manchmal sehe.');
  dialogLinesAfter[19][3]:='Explorator: Wer versteht schon die Natur… Hier, für Eure gute Arbeit!';
  dialogsAfter[19].lines:=dialogLinesAfter[19];
  rooms[19].dialogAfterRiddle:=dialogsAfter[19];
  rooms[19].item:=TItem.create(6,'Flammenbrecher-Fragment 3','Ihr erhaltet ein Flammenbrecher-Fragment.');
  rooms[19].labelEast:='Osten: Mahlstromkern';
  rooms[19].labelSouth:='Süden: Aschen-Senke';
  rooms[19].region:='Mahlstromgalle';
  rooms[19].backgroundImagePath:='img\L1.jpg';
  rooms[19].east:=rooms[20];
  rooms[19].south:=rooms[18];

  // room 20
  dialogsBefore[20]:=TDialog.create;
  SetLength(dialogLinesBefore[20],3);
  dialogLinesBefore[20][0]:='Explorator: Vorsicht! Wenn ihr hier hinunterfallt, seid ihr gebraten.';
  dialogLinesBefore[20][1]:=compile('%n: Ich passe schon auf. Kann ich Euch ein Paar Fragen stellen?');
  dialogLinesBefore[20][2]:='Explorator: Wenn das mit Kralkatorrik stimmt, dann sollten wir jetzt erstmal aufbrechen. Helft mir bitte.';
  dialogsBefore[20].lines:=dialogLinesBefore[20];
  rooms[20].dialogBeforeRiddle:=dialogsBefore[20];
  rooms[20].applyRiddle(riddlesEasy[category3][2]);
  dialogsAfter[20]:=TDialog.create;
  SetLength(dialogLinesAfter[20],3);
  dialogLinesAfter[20][0]:='Explorator: Vielen Dank.';
  dialogLinesAfter[20][1]:=compile('%n: Also, zu meiner Frage. Was wisst Ihr über Gaheron?');
  dialogLinesAfter[20][2]:='Explorator: Er ist ein hundsgemeiner Kerl. Passt auf jeden Fall auf vor ihm… Er wurde zwar seit längerem nicht mehr in seiner Festung gesehen, doch ganz sicher ist das dennoch nicht. Ich beantworte Euch auf dem Weg nach Norden mehr Fragen. Wir sollten nur erst einmal aufbrechen. Auf dem Weg kommen wir übrigens auch an Gaherons Festung vorbei, falls Ihr Euch ein Bild machen möchtet. Hier, das habe ich für Euch. Es wird Euch vielleicht helfen, im Norden durch die Flammenwand zu kommen.';
  dialogsAfter[20].lines:=dialogLinesAfter[20];
  rooms[20].dialogAfterRiddle:=dialogsAfter[20];
  rooms[20].item:=TItem.create(7,'Flammenbrecher-Fragment 4','Ihr erhaltet ein Flammenbrecher-Fragment.');
  rooms[20].labelNorth:='Norden: Höllenschlund-Fälle';
  rooms[20].labelSouth:='Süden: Geheimes Basislager';
  rooms[20].labelWest:='Westen: Mahlstromgalle';
  rooms[20].region:='Mahlstromkern';
  rooms[20].backgroundImagePath:='img\L1.jpg';
  rooms[20].north:=rooms[21];
  rooms[20].west:=rooms[19];
  rooms[20].south:=rooms[16];

  // room 21
  rooms[21].descriptionBeforeRiddle:=compile('%n: Hier ist es extrem steil, ich sollte vorsichtig sein. Mir bereiten nur die Informationen von gerade eben ein wenig Kopfschmerzen. Wer ist Gaheron? Und was, wenn ich ihm begegne?');
  rooms[21].labelEast:='Osten: Herz des Vulkans.';
  rooms[21].labelSouth:='Süden: Mahlstromkern';
  rooms[21].region:='Höllenschlund-Fälle';
  rooms[21].backgroundImagePath:='img\L2.jpg';
  rooms[21].east:=rooms[22];
  rooms[21].south:=rooms[20];

  // room 22
  rooms[22].descriptionBeforeRiddle:=compile('%n: Hier teilt sich der Weg. Weiter im Osten sehe ich eine monströse Festung… Dürfte wohl Gaherons Festung sein. Der Weg nach Norden ist durch eine Flammenwand versperrt.');
  rooms[22].labelNorth:='Norden: Trügerische Gaslöcher.';
  rooms[22].labelEast:='Osten: Igni Castrum.';
  rooms[22].labelWest:='Westen: Höllenschlund-Fälle.';
  rooms[22].region:='Herz des Vulkans';
  rooms[22].backgroundImagePath:='img\L2.jpg';
  rooms[22].north:=rooms[25];
  rooms[22].east:=rooms[23];
  rooms[22].west:=rooms[21];

  // room 23
  rooms[23].descriptionBeforeRiddle:=compile('%n: Ein teuflischer Ort. Passend zu einem teuflischen Herrscher. Überall Leichenteile und Feuer. Mir persönlich etwas zu ungemütlich… Ich sehe eine Art Schrein mit leerem Sockel. Da muss sicherlich etwas rein.');
  rooms[23].labelEast:='Osten: Gaherons Höllenschrein';
  rooms[23].labelWest:='Westen: Herz des Vulkans';
  rooms[23].region:='Igni Castrum';
  rooms[23].backgroundImagePath:='img\L2.jpg';
  rooms[23].east:=rooms[24];
  rooms[23].west:=rooms[22];

  // room 24
  rooms[24].backgroundMusicPath:='music\Gaheron.wav';
  dialogsBefore[24]:=TDialog.create;
  SetLength(dialogLinesBefore[24],6);
  dialogLinesBefore[24][0]:=compile('%n: Das Totem ist in Flammen aufgegangen. Doch nichts ist passiert. Ich sollte besser hier verschwinden bevor…');
  dialogLinesBefore[24][1]:='Gaheron Baelfeuer: Ich kenne Euch nicht, Eindringling. Doch die anderen, die es versucht haben, habt Ihr gerade verbrannt auf dem Boden liegen gesehen. Gleichgültig. Eine Mahlzeit mehr.';
  dialogLinesBefore[24][2]:=compile('%n: Wartet! Ich will Euch nichts tun, Gaheron! Ich habe nur ein paar Fragen. Was könnt Ihr mir über die Erhabenen und Kralkatorrik sagen? Ist das alles nur ein Gerücht?');
  dialogLinesBefore[24][3]:='Gaheron Baelfeuer: Die Erhabenen müssen vernichtet werden. Kralkatorriks Diener werden sein Werk vollbringen.';
  dialogLinesBefore[24][4]:=compile('%n: (Mein Kopf tut höllisch weh, als würde jemand versuchen, darauf zuzugreifen!)');
  dialogLinesBefore[24][5]:='Gaheron Baelfeuer: Erhabener, hier ist die Sackgasse Eurer Reise!';
  dialogsBefore[24].lines:=dialogLinesBefore[24];
  rooms[24].dialogBeforeRiddle:=dialogsBefore[24];
  rooms[24].applyRiddle(riddlesBoss[category2][1]);
  rooms[24].labelEast:='Nächstes Rätsel';
  rooms[24].backgroundImagePath:='img\L4.jpg';
  rooms[24].east:=rooms[45];
  // room 45 (24)
  rooms[45].applyRiddle(riddlesBoss[category3][1]);
  rooms[45].backgroundMusicPathAfterRiddle:='music\LavaAGaheron.wav';
  dialogsAfter[45]:=TDialog.create;
  SetLength(dialogLinesAfter[45],1);
  dialogLinesAfter[45][0]:=compile('%n: Was passiert hier?! Warum bin ich hier? Wie konnte ich ihn besiegen? Ich muss schnell hier weg und eine Antwort finden. Wenn Gaheron die Wahrheit gesagt hat, macht das alles einen Sinn. Kralkatorrik muss irgendwie am Leben sein. Am besten nehme ich mir eine von Gaherons Waffen mit; kann bestimmt nicht schaden. Sie müssen hier irgendwo liegen.');
  dialogsAfter[45].lines:=dialogLinesAfter[45];
  rooms[45].dialogAfterRiddle:=dialogsAfter[45];
  rooms[45].item:=TItem.create(8,'Magmatisches Zepter','Ihr findet Gaherons magmatisches Zepter. Ihr spürt seine Macht durch euer Blut fließen. Die Flammen dieses vom Bösen durchflossenen Zepters reichen aus, um jedes Lebewesen zu vernichten.');
  rooms[45].labelWest:='Westen: Igni Castrum';
  rooms[45].region:='Gaherons Höllenschrein';
  rooms[45].backgroundImagePath:='img\L4.jpg';
  rooms[45].west:=rooms[23];

  // room 25
  rooms[25].descriptionBeforeRiddle:=compile('%n: Ich wünschte, ich könnte auch ohne den Stein durch Feuer laufen… Vergiss es, %n. Das wird nie passieren. Ich sollte mich mehr auf meine Mission konzentrieren. Die Exploratoren sind schon weit voraus.');
  rooms[25].labelSouth:='Süden: Herz des Vulkans';
  rooms[25].labelWest:='Westen: Dysphorischer Rand';
  rooms[25].region:='Trügerische Gaslöcher';
  rooms[25].backgroundImagePath:='img\L2.jpg';
  SetLength(room25requiredItems,4);
  room25requiredItems[0]:=4;
  room25requiredItems[1]:=5;
  room25requiredItems[2]:=6;
  room25requiredItems[3]:=7;
  rooms[25].requiredItemArray:=room25requiredItems;
  rooms[25].south:=rooms[22];
  rooms[25].west:=rooms[26];

  // room 26
  rooms[26].descriptionBeforeRiddle:=compile('%n: Der Weg spaltet sich schon wieder. Ich glaube, im Norden kann ich den Ausgang aus dieser Hölle hier sehen. Im Westen geht es zwar auch Bergauf, aber wohin genau, weiß ich nicht.');
  rooms[26].labelNorth:='Norden: Vermoderter Pass';
  rooms[26].labelEast:='Osten: Trügerische Gaslöcher';
  rooms[26].labelWest:='Westen: Aufstieg des Helden';
  rooms[26].region:='Dysphorischer Rand';
  rooms[26].backgroundImagePath:='img\L1.jpg';
  rooms[26].north:=rooms[29];
  rooms[26].east:=rooms[25];
  rooms[26].west:=rooms[27];

  // room 27
  rooms[27].descriptionBeforeRiddle:=compile('%n: Ich habe gleich den Gipfel erreicht. Mit jedem Schritt wird es merklich kühler. Ein Segen.');
  rooms[27].labelNorth:='Norden: Signalgipfel';
  rooms[27].labelEast:='Osten: Dysphorischer Rand';
  rooms[27].region:='Aufstieg des Helden';
  rooms[27].backgroundImagePath:='img\L3.jpg';
  rooms[27].north:=rooms[28];
  rooms[27].east:=rooms[26];

  // room 28
  rooms[28].descriptionBeforeRiddle:=compile('%n: Gipfel und Sackgasse. Weiter komme ich nicht. Was ist denn das da hinten? Etwas Kleines scheint leicht zu glühen. Besser, ich gucke mal nach.');
  rooms[28].item:=TItem.create(9,'Geschnitztes Totem','Ihr hebt ein kleines geschnitztes Totem auf. Es sieht aus, als würde es irgendwo hinein passen.');
  rooms[28].labelSouth:='Süden: Aufstieg des Helden.';
  rooms[28].region:='Signalgipfel';
  rooms[28].backgroundImagePath:='img\L3.jpg';
  rooms[28].south:=rooms[27];

  // room 29
  rooms[29].descriptionBeforeRiddle:=compile('%n: Endlich bin ich raus aus dem Vulkan. Östlich von mir sehe ich zum ersten Mal seit langer Zeit wieder etwas Grünes. Wo ist denn nur der Explorator-Trupp hin?');
  rooms[29].labelEast:='Osten: Dornengeflecht';
  rooms[29].labelSouth:='Süden: Dysphorischer Rand';
  rooms[29].region:='Vermoderter Pass';
  rooms[29].backgroundImagePath:='img\L3.jpg';
  rooms[29].east:=rooms[30];
  rooms[29].south:=rooms[26];

  // room 30
  rooms[30].backgroundMusicPath:='music\Jungle.wav';
  rooms[30].descriptionBeforeRiddle:=compile('%n: Autsch, überall Dornen. Alles halb verdorrt. Nach einem gesunden Dschungel sieht mir das aber nicht aus. Ich habe auch wieder leichte Kopfschmerzen. Hoffentlich finde ich bald die Exploratoren wieder…');
  rooms[30].labelEast:='Osten: Rankenwand';
  rooms[30].labelWest:='Westen: Vermoderter Pass';
  rooms[30].region:='Dornengeflecht';
  rooms[30].backgroundImagePath:='img\J1.jpg';
  rooms[30].east:=rooms[31];
  rooms[30].west:=rooms[29];

  // room 31
  rooms[31].descriptionBeforeRiddle:=compile('%n: Schon wieder eine Rankenwand im Süden?? Das kann nichts Gutes verheißen. Die Kopfschmerzen werden stärker. Ich nähere mich wohl dem Bösen. Vielleicht beendet dies meine Reise… Vielleicht muss ich mich einfach meinem Schicksal stellen.');
  rooms[31].labelEast:='Osten: Domäne des Drachen';
  rooms[31].labelSouth:='Süden: Exhumierte Anhöhe';
  rooms[31].labelWest:='Westen: Dornengeflecht';
  rooms[31].region:='Rankenwand';
  rooms[31].backgroundImagePath:='img\J1.jpg';
  rooms[31].east:=rooms[33];
  rooms[31].south:=rooms[32];
  rooms[31].west:=rooms[30];

  // room 32
  rooms[32].descriptionBeforeRiddle:=compile('%n: Das verbirgt sich also hinter den Ranken! Tausende Leichen. Kralkatorrik hat wahllos unschuldige Menschen umgebracht. Da liegen auch die vier Exploratoren! Völlig zerfetzt. Sie haben es nicht geschafft. Kralkatorrik hat seinen Tribut gezahlt. Aber ich bin aus der Sache noch nicht draußen. Am besten schaue ich mal, was ich mir hier Nützliches mitnehmen kann.');
  rooms[32].item:=TItem.create(10,'Machete','Ihr nehmt die Machete eines toten Explorators an Euch. Sie sollte stark genug sein, um die restlichen Ranken zu zerschneiden.');
  rooms[32].labelNorth:='Norden: Rankenwand';
  rooms[32].region:='Exhumierte Anhöhe';
  rooms[32].backgroundImagePath:='img\J1.jpg';
  rooms[32].requiredItem:=11;
  rooms[32].north:=rooms[31];

  // room 33
  rooms[33].descriptionBeforeRiddle:=compile('%n: Ich spüre es. Südlich von mir ist etwas. Etwas Mächtiges. Etwas Lebendiges. Meine Kopfschmerzen sind nicht auszuhalten. Ich sollte nicht ohne entsprechende Waffe fortfahren. Sonst könnte wirklich mein letztes Stündchen geschlagen haben.');
  rooms[33].labelSouth:='Süden: Kralkatorriks Vermächtnis';
  rooms[33].labelWest:='Westen: Rankenwand';
  rooms[33].region:='Domäne des Drachen';
  rooms[33].backgroundImagePath:='img\J1.jpg';
  rooms[33].south:=rooms[34];
  rooms[33].west:=rooms[31];

  // room 34
  rooms[34].deathMessage:='Ihr seht die Silhouette eines Wesens, doch bevor ihr blinzeln könnt, seid ihr tot. Game Over.';
  rooms[34].respawn:=rooms[14];
  rooms[34].requiredItem:=8;
  rooms[34].killWithoutItem:=true;
  rooms[34].backgroundMusicPath:='music\Kralkatorrik.wav';
  dialogsBefore[34]:=TDialog.create;
  SetLength(dialogLinesBefore[34],5);
  dialogLinesBefore[34][0]:=compile('%n: Zeigt Euch! Wer seid ihr?!');
  dialogLinesBefore[34][1]:='KRALKATORRIK: ICH BIN ÜBERALL! IHR KÖNNT MICH NICHT BEZWINGEN! IHR ENDET GENAU SO, WIE ES DIE ANDEREN ERHABENEN GETAN HABEN!';
  dialogLinesBefore[34][2]:=compile('%n: Ich bin kein Erhabener! Lasst mich gehen! Warum bin ich hier?!');
  dialogLinesBefore[34][3]:='KRALKATORRIK: IHR HATTET DIE MACHT, MEINE CHAMPIONS SVAARD UND GAHERON ZU BESIEGEN. NUR EIN ERHABENER BESITZT DIESE MACHT. LASST MICH EUCH AUS DEM WEG RÄUMEN.';
  dialogLinesBefore[34][4]:=compile('%n: (Es hat keinen Sinn. Ich muss wohl kämpfen. Bringt es zu Ende! Gaherons Zepter wird mir helfen.)');
  dialogsBefore[34].lines:=dialogLinesBefore[34];
  rooms[34].dialogBeforeRiddle:=dialogsBefore[34];
  rooms[33].backgroundImagePath:='img\J2.jpg';
  rooms[34].applyRiddle(riddlesBoss[category1][2]);
  rooms[34].labelSouth:='Nächstes Rätsel';
  rooms[34].south:=rooms[46];
  // room 46 (34)
  rooms[46].applyRiddle(riddlesBoss[category2][2]);
  rooms[46].labelSouth:='Nächstes Rätsel';
  rooms[46].south:=rooms[47];
  // room 47 (34)
  // TODO: stop music after riddle
  rooms[47].applyRiddle(riddlesBoss[category3][2]);
  dialogsAfter[47]:=TDialog.create;
  SetLength(dialogLinesAfter[47],3);
  dialogLinesAfter[47][0]:=compile('%n: Er… ist besiegt. Es war wieder dasselbe Gefühl, wie bei Svaard und Gaheron. Es lässt sich schwer beschreiben… Ist es jetzt vorbei? Was geschieht jetzt mit mir...');
  dialogLinesAfter[47][1]:='Stimme: Erhabener, ihr habt den Fluch gebrochen. Ich warte im Süden auf Euch, damit Ihr Euch der Prüfung stellen könnt.';
  dialogLinesAfter[47][2]:=compile('%n: Prüfung? Oh… die Stimme ist wieder weg…');
  dialogsAfter[47].lines:=dialogLinesAfter[47];
  rooms[47].dialogAfterRiddle:=dialogsAfter[47];
  rooms[47].item:=TItem.create(11,'Rankenzorn-Flöte','Die Ranken, aus denen das Abbild Kralkatorriks zusammengesetzt war, formen sich zu einer Flöte. Mit ihr lässt sich die nächstgelegene Rankenwand entfernen.');
  rooms[47].labelNorth:='Norden: Domäne des Drachen';
  rooms[47].region:='Kralkatorriks Vermächtnis';
  rooms[47].backgroundImagePath:='img\J2.jpg';
  rooms[47].backgroundMusicPathAfterRiddle:='music\JungleAKralkatorrik.wav';
  rooms[47].north:=rooms[33];

  // room 35
  rooms[35].backgroundMusicPath:='music\Jungle2.wav';
  rooms[35].descriptionBeforeRiddle:=compile('%n: Das hat sich also hier verborgen! Ein weiterer Wald? Zumindest die Baumkronen. Ein Weg führt nach unten. Mal sehen, wo er hin führt…');
  rooms[35].labelNorth:='Norden: Güldener Abstieg';
  rooms[35].labelWest:='Westen: Urteilsfelsen';
  rooms[35].region:='Mattierte Baumkrone';
  rooms[35].backgroundImagePath:='img\J3.jpg';
  rooms[35].requiredItem:=10;
  rooms[35].north:=rooms[36];
  rooms[35].west:=rooms[15];

  // room 36
  dialogsBefore[36]:=TDialog.create;
  SetLength(dialogLinesBefore[36],2);
  dialogLinesBefore[36][0]:='Stimme: Hier entlang. Es ist nicht mehr weit bis zum heiligen Ort.';
  dialogLinesBefore[36][1]:=compile('%n: Das klingt Spannend. Doch wo führt mich diese Stimme hin? Etwa dieser kleine Tempel vor mir?');
  dialogsBefore[36].lines:=dialogLinesBefore[36];
  rooms[36].dialogBeforeRiddle:=dialogsBefore[36];
  rooms[36].labelEast:='Osten: Tarir Die Vergessene Stadt';
  rooms[36].labelSouth:='Süden: Mattierte Baumkrone';
  rooms[36].region:='Güldener Abstieg';
  rooms[36].backgroundImagePath:='img\J3.jpg';
  rooms[36].east:=rooms[37];
  rooms[36].south:=rooms[35];

  // room 37
  dialogsBefore[37]:=TDialog.create;
  SetLength(dialogLinesBefore[37],7);
  dialogLinesBefore[37][0]:='Stimme: Willkommen in Tarir, der Vergessenen Stadt. Die Erhabenen haben sie vor Jahrtausenden aus purem Gold errichtet, um hier in Frieden zu leben und zu lernen.';
  dialogLinesBefore[37][1]:=compile('%n: Bitte nicht so schnell. Wer genau bist du?');
  dialogLinesBefore[37][2]:='Stimme: Ich bin nur die Stimme deines Unterbewusstseins. Du bist schließlich ein Erhabener.';
  dialogLinesBefore[37][3]:=compile('%n: Was ich schon die ganze Zeit wissen will. Was ist ein Erhabener? Ich wurde deswegen schon dreimal fast getötet.');
  dialogLinesBefore[37][4]:='Stimme: Ein Erhabener ist eine Inkarnation aus Wissen. Er verwendet das Wissen, um Macht zu erzeugen und ist somit eine Gefahr für die übrigen Machthaber. Du bist der letzte Überlebende Erhabene. Deswegen wollte Kralkatorrik dich mit seinen Champions loswerden, damit er die Macht behält.';
  dialogLinesBefore[37][5]:=compile('%n: Ich verstehe. Und warum sollte ich jetzt in diese Stadt kommen?');
  dialogLinesBefore[37][6]:='Stimme: Sieh dich in Ruhe um und erfahre mehr über die Vergangenheit und dein Dasein. Ich erwarte dich bei der Prüfung, südlich der zentralen Kammer.';
  dialogsBefore[37].lines:=dialogLinesBefore[37];
  rooms[37].dialogBeforeRiddle:=dialogsBefore[37];
  rooms[37].labelNorth:='Norden: Vergoldete Senke';
  rooms[37].labelEast:='Osten: Zentrale Kammer';
  rooms[37].labelWest:='Westen: Güldener Abstieg';
  rooms[37].region:='Tarir Die Vergessene Stadt';
  rooms[37].backgroundImagePath:='img\J4.jpg';
  rooms[37].north:=rooms[38];
  rooms[37].east:=rooms[42];
  rooms[37].west:=rooms[36];

  // room 38
  dialogsBefore[38]:=TDialog.create;
  SetLength(dialogLinesBefore[38],2);
  dialogLinesBefore[38][0]:=compile('%n: Ich bin also eine Inkarnation aus Wissen… Deswegen konnte ich mich wohl durch das Lösen von Rätseln verteidigen. Und als letzter Erhabener stand ich also Kralkatorrik im Weg… Aber war er nicht schon seit langer Zeit tot?');
  dialogLinesBefore[38][1]:='Stimme: Kralkatorrik an sich starb vor langer Zeit durch die Kriegerin Dwayna. Er riss sie mit in den Tod. Sie wurde daraufhin als Göttin des Schutzes und des Lebens wiedergeboren; Kralkatorriks Böser Geist ruhte jedoch weiterhin in der Welt und befiel verschiedene Wesen. Die Gestalt, die Ihr im Dschungel besiegt habt, waren Kralkatorriks letzte Überreste. Das Gleichgewicht in der Welt wird sich wieder einpendeln. Fragt sich nur, in welchem Jahrtausend nach unserer Zeit das geschehen wird.';
  dialogsBefore[38].lines:=dialogLinesBefore[38];
  rooms[38].dialogBeforeRiddle:=dialogsBefore[38];
  rooms[38].labelNorth:='Norden: Versiegelter Durchgang';
  rooms[38].labelEast:='Osten: Halle der Gefallenen';
  rooms[38].labelSouth:='Süden: Tarir Die Vergessene Stadt';
  rooms[38].region:='Vergoldete Senke';
  rooms[38].backgroundImagePath:='img\J4.jpg';
  rooms[38].north:=rooms[39];
  rooms[38].east:=rooms[41];
  rooms[38].south:=rooms[37];

  // room 39
  dialogsBefore[39]:=TDialog.create;
  SetLength(dialogLinesBefore[39],3);
  dialogLinesBefore[39][0]:=compile('%n: Was befindet sich hier unten?');
  dialogLinesBefore[39][1]:='Stimme: Hier lebte unsere Königin. Wir nannten sie die Luminatin. Sie war der Inbegriff der puren Weisheit. Ich kann Euch hier leider nicht folgen. Dieser Durchgang ist nur für materielle Wesen zugänglich. Ich warte oben. Geht hinein und findet die Erleuchtung.';
  dialogLinesBefore[39][2]:=compile('%n: Mache ich.');
  dialogsBefore[39].lines:=dialogLinesBefore[39];
  rooms[39].dialogBeforeRiddle:=dialogsBefore[39];
  rooms[39].labelSouth:='Süden: Vergoldete Senke';
  rooms[39].labelWest:='Westen: Thron der Luminatin';
  rooms[39].region:='Versiegelter Durchgang';
  rooms[39].backgroundImagePath:='img\J4.jpg';
  rooms[39].south:=rooms[38];
  rooms[39].west:=rooms[40];

  // room 40
  rooms[40].descriptionBeforeRiddle:=compile('%n: Dieser Raum ist prächtig geschmückt. Die Erhabenen hatten so viel Besitz. Heutzutage überhaupt nicht mehr vorstellbar. Sieht so aus, als wäre der Thron hohl. Vielleicht verbirgt sich darin etwas.');
  rooms[40].item:=TItem.create(12,'merkwürdige Inschrift','Ihr findet eine merkwürdige Inschrift: 01010010 01100101 01100001 01100100 01101101 01100101');
  rooms[40].labelEast:='Osten: Versiegelter Durchgang';
  rooms[40].region:='Thron der Luminatin';
  rooms[40].backgroundImagePath:='img\J4.jpg';
  rooms[40].east:=rooms[39];

  // room 41
  dialogsBefore[41]:=TDialog.create;
  SetLength(dialogLinesBefore[41],3);
  dialogLinesBefore[41][0]:=compile('%n: An den Wänden hier sind überall Schriften.');
  dialogLinesBefore[41][1]:='Stimme: Das hier ist die Halle der Gefallenen. Hier verewigen wir jeden Erhabenen, der gelebt hat. Wie Ihr seht seid Ihr ganz unten im Stammbaum. Schaut mal, wer hier oben steht.';
  dialogLinesBefore[41][2]:=compile('%n: Mein Vater… Lord-%n. Er war also auch hier…');
  dialogsBefore[41].lines:=dialogLinesBefore[41];
  rooms[41].dialogBeforeRiddle:=dialogsBefore[41];
  rooms[41].labelSouth:='Süden: Zentrale Kammer';
  rooms[41].labelWest:='Westen: Vergoldete Senke';
  rooms[41].region:='Halle der Gefallenen';
  rooms[41].backgroundImagePath:='img\J4.jpg';
  rooms[41].south:=rooms[42];
  rooms[41].west:=rooms[38];

  // room 42
  dialogsBefore[42]:=TDialog.create;
  SetLength(dialogLinesBefore[42],4);
  dialogLinesBefore[42][0]:=compile('%n: Stimme, sagt mir, was ist meine Bestimmung.');
  dialogLinesBefore[42][1]:='Stimme: Die habt Ihr bereits erfüllt. Kralkatorrik endgültig zu vernichten und das Gleichgewicht wiederherzustellen. Dafür seid ihr auf die Welt gekommen und dafür geht ihr auch wieder.';
  dialogLinesBefore[42][2]:=compile('%n: Heißt das, ich muss… jetzt… Abschied nehmen?');
  dialogLinesBefore[42][3]:='Stimme: Das Schicksal schreibt es vor. Kralkatorrik hat Narben der Finsternis bei Euch hinterlassen. Sie dürfen sich nicht erneut entflammen. Deswegen müsst Ihr auch die Prüfung ablegen. Folgt mir in den letzten Raum Tarirs. Wir werden das Ritual gemeinsam Ablegen und den Erhabenen ihre verdiente Ruhe gewähren.';
  dialogsBefore[42].lines:=dialogLinesBefore[42];
  rooms[42].dialogBeforeRiddle:=dialogsBefore[42];
  rooms[42].labelNorth:='Norden: Halle der Gefallenen';
  rooms[42].labelSouth:='Süden: Prüfung der Erhabenen';
  rooms[42].labelWest:='Westen: Tarir Die Vergessene Stadt';
  rooms[42].region:='Zentrale Kammer';
  rooms[42].backgroundImagePath:='img\J4.jpg';
  rooms[42].south:=rooms[43];

  // room 43
  dialogsBefore[43]:=TDialog.create;
  SetLength(dialogLinesBefore[43],2);
  dialogLinesBefore[43][0]:=compile('Stimme: Hier ist sie. Die Prüfungsstätte. Ihr werdet eine Legende, %n.');
  dialogLinesBefore[43][1]:=compile('%n: Ich werde es tun. Beginnt.');
  dialogsBefore[43].lines:=dialogLinesBefore[43];
  rooms[43].dialogBeforeRiddle:=dialogsBefore[43];
  rooms[43].backgroundMusicPath:='music\Tarir.wav';
  rooms[43].applyRiddle(riddlesEasy[category1][3]);
  rooms[43].labelSouth:='Nächstes Rätsel';
  rooms[43].south:=rooms[48];
  // room 48 (43)
  rooms[48].applyRiddle(riddlesEasy[category2][3]);
  rooms[48].labelSouth:='Nächstes Rätsel';
  rooms[48].south:=rooms[49];
  // room 49 (43)
  rooms[49].applyRiddle(riddlesEasy[category3][3]);
  rooms[49].labelSouth:='Nächstes Rätsel';
  rooms[49].south:=rooms[50];
  // room 50 (43)
  rooms[50].applyRiddle(riddlesBoss[category1][3]);
  rooms[50].labelSouth:='Nächstes Rätsel';
  rooms[50].south:=rooms[51];
  // room 51 (43)
  rooms[51].applyRiddle(riddlesBoss[category2][3]);
  rooms[51].labelSouth:='Nächstes Rätsel';
  rooms[51].south:=rooms[52];
  // room 52 (43)
  rooms[52].applyRiddle(riddlesBoss[category3][3]);
  dialogsAfter[52]:=TDialog.create;
  SetLength(dialogLinesAfter[52],3);
  dialogLinesAfter[52][0]:=compile('%n: Ich habe mein Werk auf dieser Welt erfüllt. Ich muss gehen.');
  dialogLinesAfter[52][1]:=compile('Stimme: Nehmt Abschied. Eure Nachfolger werden in Eurem Frieden leben können. Habt Dank, %n.');
  dialogLinesAfter[52][2]:='Ende.';
  dialogsAfter[52].lines:=dialogLinesAfter[52];
  rooms[52].dialogAfterRiddle:=dialogsAfter[52];
  rooms[52].region:='Prüfung der Erhabenen';
  rooms[52].backgroundImagePath:='img\J4.jpg';


  spawnRoom := rooms[1];
  startAdventure;

end;

end.

