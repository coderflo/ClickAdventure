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
    killWithoutItem:boolean; // reset to spawn when entering without item

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
    // ERROR: Incompatible type for arg no. 1: Got 'AnsiString', expected 'PChar'
    //sndPlaySound(Application.Location + room.backgroundMusicPath,SND_NODEFAULT Or SND_ASYNC);
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
  killWithoutItem:=false;

end;

function TRoom.canEnter(bag:TBag):boolean;
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

  spawnRoom := rooms[1];
  startAdventure;

end;

end.

