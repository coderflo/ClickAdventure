unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Unit2, mmsystem;

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
  { TRoom }
  TRoom = class

    backgroundImagePath:string; // path to img: /img/[name].[extension]
    backgroundMusicPath:string; // path to music: /music/[name].wav

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

    constructor create;

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

      changeRoom(spawnRoom);
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
  rooms: array [1..43] of TRoom;
  dialogsBefore, dialogsAfter: array [1..43] of TDialog;
  dialogLinesBefore, dialogLinesAfter: array [1..43] of array of string;

begin

  for i := 0 to 43 do
  begin
    rooms[i] := TRoom.create; // create all rooms with default values
  end;

  playerName:='Player';

  bag := TBag.create;

  // room 1
  rooms[1].descriptionBeforeRiddle:=compile('%n: Wo bin ich? Was ist passiert? Ich kann mich an nichts mehr erinnern… Ich sollte mich zunächst aus diesem Schneegebiet entfernen. Lange halte ich das wahrscheinlich nicht durch… bei diesen Temperaturen.');
  rooms[1].labelEast:='Osten: Geht tiefer in den Wald hinein.';
  rooms[1].region:='Wintertau-Lichtung';
  rooms[1].backgroundImagePath:='img\S1.jpg';
  rooms[1].backgroundMusicPath:='music\Snow.wav';
  rooms[1].east:=rooms[2];

  // room 2
  rooms[2].descriptionBeforeRiddle:=compile('%n: Hier gibt es nicht wirklich viele Möglichkeiten, sich zu verlaufen. Der Wald ist viel zu dicht, um Umwege zu gehen. Ich sollte besser dem Weg nach Süden folgen. Hoffentlich finde ich bald eine Antwort auf meine Fragen…');
  rooms[2].labelSouth:='Süden: Schlagt den Weg zur Doldenvan-Passage ein.';
  rooms[2].labelWest:='Westen: Geht zurück zur Lichtung, an der Ihr aufgewacht seid.';
  rooms[2].region:='Wintertau-Wald';
  rooms[2].backgroundImagePath:='img\S1.jpg';
  rooms[2].south:=rooms[3];
  rooms[2].west:=rooms[1];

  // room 3
  rooms[3].descriptionBeforeRiddle:=compile('%n: Ein umgestürzter Baum blockiert die Straße nach Süden… Offenbar lichtet sich der Wald dort etwas. Vielleicht kann mir der Besitzer dieses Gehöfts helfen.');
  rooms[3].labelNorth:='Norden: Geht zurück in den dichten Teil des Waldes.';
  rooms[3].labelSouth:='Süden: Zerschlage den Baumstamm mit der mystischen Axt und verlasse den Wald.';
  rooms[3].labelWest:='Westen: Betretet das Gehöft.';
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
  // TODO: add riddle
  dialogsAfter[4]:=TDialog.create;
  SetLength(dialogLinesAfter[4], 2);
  dialogLinesAfter[4][0]:='Holzfäller: So einfach hätte ich mir das gar nicht vorgestellt, danke. Hier, nehmt diese Axt. Ich habe sie gestern in einer Ranke gefunden, die hier im Schnee lag. Witzig, nicht? Eine Ranke hier im Schnee. Braucht sie mir auch nicht zurückbringen. Ich habe sowieso bessere Äxte. Also dann, gute Reise. Passt nur auf Issormir, den Eiswurm, der seit Wochen diese Gegend heimsucht, auf. Möge Dwayna Euch beschützen.';
  dialogLinesAfter[4][1]:=compile('%n: Danke. Euch auch.');
  dialogsAfter[4].lines:=dialogLinesAfter[4];
  rooms[4].dialogAfterRiddle:=dialogsAfter[4];
  rooms[4].item:=TItem.create(1,'Mystische Holzfälleraxt','Ihr erhaltet die mystische Holzfälleraxt aus den Zittergipfeln.');
  rooms[4].labelEast:='Osten: Verlasst das Gehöft.';
  rooms[4].region:='Alpenzur-Gehöft';
  rooms[4].backgroundImagePath:='img\S1.jpg';
  rooms[4].east:=rooms[3];

  // room 5
  rooms[5].descriptionBeforeRiddle:=compile('%n: Gut, dass dieser Wald hinter mir liegt. Ich habe gar nicht bemerkt, dass ich von so hohen Bergen umgeben bin. Mir bereiten nur die Drachen ein wenig Sorge… und dieser Issormir… wahrscheinlich nur dumme Gerüchte.');
  rooms[5].labelNorth:='Norden: Betretet den Wald.';
  rooms[5].labelEast:='Osten: Geht in Richtung Maulent-Gipfel.';
  rooms[5].labelSouth:='Süden: Besteigt die verschneite Anhöhe.';
  rooms[5].requiredItem:=1;
  rooms[5].region:='Eissteppen';
  rooms[5].backgroundImagePath:='img\S3.jpg';
  rooms[5].east:=rooms[6];
  rooms[5].south:=rooms[7];
  rooms[5].north:=rooms[3];

  // room 6
  rooms[6].descriptionBeforeRiddle:=compile('%n: Hier ist es definitiv zu steil. Selbst erfahrene Bergsteiger kommen hier nicht weiter. Ich will auch gar nicht an die Lawinen denken, die hier runter kommen könnten. Besser, ich kehre um.');
  rooms[6].labelWest:='Westen: Kehrt zur Gabelung zurück.';
  rooms[6].region:='Maulent-Gipfel';
  rooms[6].backgroundImagePath:='img\S3.jpg';
  rooms[6].west:=rooms[5];

  // room 7
  rooms[7].descriptionBeforeRiddle:=compile('%n: Ich nähere mich dem Gipfel. Der Schnee peitscht mir ins Gesicht. Etwa wieder einer dieser Frostwinde, von denen der Holzfäller gesprochen hat? Ich muss aufs Schlimmste gefasst sein.');
  rooms[7].labelNorth:='Norden: Geht zur Gabelung zurück.';
  rooms[7].labelSouth:='Süden: Erklimmt den Hochgipfel.';
  rooms[7].region:='Schneeanstieg';
  rooms[7].backgroundImagePath:='img\S3.jpg';
  rooms[7].north:=rooms[5];
  rooms[7].south:=rooms[8];

  // room 8
  rooms[8].descriptionBeforeRiddle:=compile('%n: Tolle Aussicht hier oben. Schnee, Schnee und nochmal Schnee. Im Westen sehe ich einen Gletscher. Sieht bewacht aus. Auf jeden Fall sollte ich dort ohne Rechtfertigung nicht lang.');
  rooms[8].labelNorth:='Norden: Steigt den Berg in Richtung des Waldes hinab.';
  rooms[8].labelSouth:='Süden: Seilt Euch in den Abgrund hinab.';
  rooms[8].labelWest:='Westen: Betretet den bewachten Gletscher.';
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
  rooms[9].labelEast:='Osten: Verlasst den Gletescher.';
  rooms[9].labelSouth:='Süden: Betretet den Palast im Gletscher.';
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
  rooms[10].dialogBeforeRiddle:=dialogsBefore[10];
  // TODO: add riddle
  dialogsAfter[10]:=TDialog.create;
  SetLength(dialogLinesAfter[10],3);
  dialogLinesAfter[10][0]:=compile('%n: Was ist passiert? Ich wurde angegriffen, sein Schwert traf mich, aber ich konnte ihn irgendwie besiegen. Es war dasselbe Gefühl, wie bei Issormir.');
  dialogLinesAfter[10][1]:='Svaard: Niemand…kann…Ihn…aufhalten…';
  dialogLinesAfter[10][2]:=compile('%n: Huch, er hat irgendetwas fallen gelassen. Sieht aus wie eine Art Siegel. Aber es ist eiskalt. Seine Wachen sind auch verschwunden… als hätte der Gletscher sie verschluckt. Nun aber erst mal raus aus der Kälte hier. Nachdenken kann ich auch noch später.');
  dialogsAfter[10].lines:=dialogLinesAfter[10];
  rooms[10].dialogAfterRiddle:=dialogsAfter[10];
  rooms[10].item:=TItem.create(2,'Verdorbenes Frostsiegel','Ihr findet das verdorbene Frostsiegel Svaards. Man sagt, sein Besitzer könne alles zu Eis erstarren lassen.');
  rooms[10].labelNorth:='Norden: Verlasst Svaards Palast.';
  rooms[10].region:='Svaards Eisschollenpalast';
  rooms[10].backgroundImagePath:='img\S4.jpg';
  rooms[10].backgroundMusicPath:='music\SnowASvaard.wav';
  // optional: Svaard.wav
  rooms[10].north:=rooms[9];

  // room 11
  rooms[11].descriptionBeforeRiddle:=compile('%n: Es ist ziemlich dunkel hier unten, kaum zu glauben, dass oben alles weiß ist. Von Osten spüre ich eine gewisse Wärme… kann das sein?');
  rooms[11].labelNorth:='Norden: Erklimmt den Hochgipfel.';
  rooms[11].labelEast:='Osten: Folgt der Wärme weiter nach unten.';
  rooms[11].labelSouth:='Süden: Geht in Richtung des Eisklamms.';
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
  // TODO: add riddle
  dialogsAfter[12]:=TDialog.create;
  SetLength(dialogLinesAfter[12],1);
  dialogLinesAfter[12][0]:=compile('%n: Ich bin nicht tot? Wie habe ich das geschafft? Ich sah Zeichen, logisch anzuordnen. Hängt das mit den ganzen Geschehnissen zusammen? Naja… Issormir ist zumindest enthauptet. Ich nehme besser seinen Kopf mit.');
  dialogsAfter[12].lines:=dialogLinesBefore[12];
  rooms[12].dialogAfterRiddle:=dialogsAfter[12];
  rooms[12].item:=TItem.create(3,'Kopf Issormirs','Ihr erhaltet den Kopf Issormirs. Er sieht sehr wertvoll aus.');
  rooms[12].labelNorth:='Norden: Verlasst den Klamm.';
  rooms[12].region:='Nebelriss-Klamm';
  rooms[12].backgroundImagePath:='img\S2.jpg';
  rooms[12].north:=rooms[11];

  // room 13
  rooms[13].descriptionBeforeRiddle:=compile('%n: Hier ist es erstaunlich heiß für ein verschneites Gebirge. Doch der Weg ist durch einen Erdrutsch versperrt. Hätte ich doch nur etwas, um den Erdrutsch zu entfernen…');
  rooms[13].labelEast:='Osten: Verwendet Svaards Frostsiegel.';
  rooms[13].labelWest:='Westen: Kehrt um.';
  rooms[13].region:='Erdrutsch-Tiefen';
  rooms[13].backgroundImagePath:='img\S2.jpg';
  rooms[13].east:=rooms[14];
  rooms[13].west:=rooms[11];

  // room 14
  rooms[14].backgroundMusicPath:='music\Lava.wav';
  rooms[14].descriptionBeforeRiddle:=compile('%n: Der Erdrutsch! Das Siegel hat ihn zu Eis verwandelt, welches dann wegen der Hitze hier geschmolzen ist. Genial! Aber wo bin ich denn jetzt hier gelandet? Ein unterirdischer Vulkan? Ich kann nördlich von hier eine Art Lager erkennen. Vielleicht sollte ich mal nachfragen.');
  rooms[14].labelNorth:='Norden: Geht zum Basislager.';
  rooms[14].labelSouth:='Süden: Geht bergauf zum Urteilsfelsen.';
  rooms[14].labelWest:='Westen: Verlasst das Vulkangebiet.';
  rooms[14].region:='Dämonenschlund';
  rooms[14].backgroundImagePath:='img\L3.jpg';
  rooms[14].north:=rooms[16];
  rooms[14].south:=rooms[15];
  rooms[14].west:=rooms[13];

  // room 15
  rooms[15].descriptionBeforeRiddle:=compile('%n: Bei Dwayna! Was ist denn hier passiert? Eine gigantische Rankenwand blockiert den Weg nach Osten. Es ist unmöglich, weiterzugehen. Etwas stimmt hier nicht. Ich sollte später wiederkommen…');
  rooms[15].labelNorth:='Norden: Geht zurück zum Vulkan.';
  rooms[15].labelEast:='Osten: Zerschneidet die Rankenwand.';
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
  dialogLinesBefore[16][13]:='Explorator: Wenn Ihr die Wahrheit sagt, dann müssen wir hier schleunigst verschwinden. Geht zu meinen 3 Kollegen hier in der Nähe und helft ihnen beim Aufbruch. Wir haben keine Zeit zu verlieren.';
  dialogLinesBefore[16][14]:=compile('%n: Warten Sie, sie haben etwas fallen gelassen… Zu spät. Ich sollte das lieber aufsammeln.');
  dialogsBefore[16].lines:=dialogLinesBefore[16];
  rooms[16].dialogBeforeRiddle:=dialogsBefore[16];
  rooms[16].item:=TItem.create(4,'Flammenbrecher-Fragment 1','Ihr erhaltet ein Flammenbrecher-Fragment.');
  rooms[16].labelNorth:='Norden: Nähert Euch den Tiefen des Mahlstroms.';
  rooms[16].labelEast:='Osten: Begebt Euch auf eine nahe gelegene Anhöhe.';
  rooms[16].labelSouth:='Süden: Verlasst das Basislager.';
  rooms[16].labelWest:='Westen: Dringt tiefer in die Aschen-Senke vor.';
  rooms[16].region:='Geheimes Basislager';
  rooms[16].backgroundImagePath:='img\L1.jpg';
  rooms[16].north:=rooms[20];
  rooms[16].east:=rooms[17];
  rooms[16].south:=rooms[14];
  rooms[16].west:=rooms[18];

  spawnRoom := rooms[1];
  startAdventure;

end;

end.

