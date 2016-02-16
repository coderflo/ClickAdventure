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
    sndPlaySound(Application.Location + room.backgroundMusicPath,SND_NODEFAULT Or SND_ASYNC);
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
  start,first,second,third:TRoom;
  item:TItem;

  dialogOne,dialogTwo:TDialog;
  dialogLinesOne, dialogLinesTwo: array of string;

begin

  bag := TBag.create;

  start := TRoom.create;
  first := TRoom.create;
  second := TRoom.create;
  third := TRoom.create;

  start.descriptionBeforeRiddle:='Spawn Raum';
  start.region:='SPAWN - MAP #1';
  start.north:=first;
  spawnRoom:=start;

  first.descriptionBeforeRiddle:='Löse dieses Rätsel!';
  first.region:='TEMPEL - MAP #1';
  first.riddleQuestion:='Was ist 1+1?';
  first.riddleOptionOne:='1';
  first.riddleOptionTwo:='2';
  first.riddleOptionThree:='3';
  first.riddleAnswer:=first.riddleOptionTwo;
  first.descriptionAfterRiddle:='Die Tür nach Osten ist zaw offen, aber du musst erst ein Item finden, ansonsten stirbst du beim Versuch sie zu durchqueren';
  first.west:=second;
  first.east:=third;

  dialogOne := TDialog.create;
  SetLength(dialogLinesOne, 2);
  dialogLinesOne[0]:='Erste Dialogzeile des ersten Dialoges';
  dialogLinesOne[1]:='Zweite Dialogzeile des ersten Dialoges';
  dialogOne.lines:=dialogLinesOne;

  dialogTwo := TDialog.create;
  SetLength(dialogLinesTwo, 2);
  dialogLinesTwo[0]:='Erste Dialogzeile des zweiten Dialoges';
  dialogLinesTwo[1]:='Zweite Dialogzeile des zweiten Dialoges';
  dialogTwo.lines:=dialogLinesTwo;

  first.dialogBeforeRiddle:=dialogOne;
  first.dialogAfterRiddle:=dialogTwo;

  item := TItem.create(1,'Test Item','Tür geöffnet');

  second.item:=item;
  second.descriptionBeforeRiddle:='Hier findest du das Item, um nach Osten zu gehen';
  second.east:=first;

  third.requiredItem:=1;
  third.killWithoutItem:=true;
  third.descriptionBeforeRiddle:='Gewonnen!';

  startAdventure;

end;

end.

