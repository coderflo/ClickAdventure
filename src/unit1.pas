unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Unit2;

type

  { TClickAdventure }

  TClickAdventure = class(TForm)
    bagButton: TButton;
    buttonItem: TButton;
    buttonNorth: TButton; // north,east,... are code names only
    buttonEast: TButton;
    buttonWest: TButton;
    buttonSouth: TButton;
    backgroundImage: TImage;
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
  { TRoom }
  TRoom = class

    backgroundImagePath:string; // path to img: /img/[name].[extension]

    descriptionBeforeRiddle:string;
    descriptionAfterRiddle:string;

    north,east,south,west:TRoom;
    labelNorth,labelEast,labelSouth,labelWest:string;

    riddleQuestion, riddleAnswer,
      riddleOptionOne, riddleOptionTwo,
        riddleOptionThree, riddleOptionFour:string;

    riddleSolved:boolean;

    item:TItem;
    requiredItem:integer; // id of item required to enter the room, -1 for none

    constructor create;

    function canEnter(bag:TBag):boolean;

  end;

var
  ClickAdventure: TClickAdventure;
  currentRoom: TRoom;
  bag: TBag; // TODO: move to character class

implementation

{$R *.lfm}

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

  // show search item button
  ClickAdventure.buttonItem.Visible:=true;

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

procedure changeRoom(room: TRoom);
begin

  currentRoom := room;

  // TODO: update GUI to new room ones

  // load background image
  if(room.backgroundImagePath <> '') then
  begin
    ClickAdventure.backgroundImage.Picture.LoadFromFile(Application.Location + room.backgroundImagePath);
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

  // hide search item button
  ClickAdventure.buttonItem.Visible:=false;

  ClickAdventure.riddleText.Lines.Clear;
  if( (room.riddleQuestion <> '') and (not room.riddleSolved) ) then
  begin

    // show riddle

    ClickAdventure.riddleText.Visible:=true;
    ClickAdventure.riddleText.Lines.Add(room.riddleQuestion);

    // show riddle answers if necessary

    if(room.riddleOptionOne <> '') then
    begin
      ClickAdventure.riddleButtonOne.Visible:=true;
      ClickAdventure.riddleButtonOne.Caption:=room.riddleOptionOne;
    end;

    if(room.riddleOptionTwo <> '') then
    begin
      ClickAdventure.riddleButtonTwo.Visible:=true;
      ClickAdventure.riddleButtonTwo.Caption:=room.riddleOptionTwo;
    end;

    if(room.riddleOptionThree <> '') then
    begin
      ClickAdventure.riddleButtonThree.Visible:=true;
      ClickAdventure.riddleButtonThree.Caption:=room.riddleOptionThree;
    end;

    if(room.riddleOptionFour <> '') then
    begin
      ClickAdventure.riddleButtonFour.Visible:=true;
      ClickAdventure.riddleButtonFour.Caption:=room.riddleOptionFour;
    end;

  end
  else
  begin
    onRiddleSolved(); // no riddle, so riddle is immediately 'solved'
  end;

end;

{ TRoom }
constructor TRoom.create;
begin

  // initialize with standard values

  backgroundImagePath:='';

  descriptionBeforeRiddle:='';
  descriptionAfterRiddle:='';

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

end;

function TRoom.canEnter(bag:TBag):boolean;
begin

  result := false;

  if(requiredItem <> -1) then
  begin
    result := bag.contains(requiredItem);
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
    // TODO: die();
  end;

end;

procedure TClickAdventure.onSearchItemClicked(Sender: TObject);
begin

  if(currentRoom.item <> nil) then
  begin
    roomDescription.Lines.Add(currentRoom.item.message);
    bag.add(currentRoom.item);
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

  for i := 0 to Length(bag.items) do
  begin
    BagView.itemNames[i] := bag.items[i].name;
  end;

  BagView.ShowModal;

end;

procedure TClickAdventure.FormShow(Sender: TObject);
begin
  buildAdventure();
end;

procedure TClickAdventure.buildAdventure;
var
  start,first,second,third:TRoom;
  item:TItem;
begin

  bag := TBag.create;

  start := TRoom.create;
  first := TRoom.create;
  second := TRoom.create;
  third := TRoom.create;

  start.descriptionBeforeRiddle:='Spawn Raum';
  start.north:=first;

  first.descriptionBeforeRiddle:='Löse dieses Rätsel!';
  first.riddleQuestion:='Was ist 1+1?';
  first.riddleOptionOne:='1';
  first.riddleOptionTwo:='2';
  first.riddleOptionThree:='3';
  first.riddleAnswer:=first.riddleOptionTwo;
  first.descriptionAfterRiddle:='Perfekt! Die Tür nach Osten ist verschlossen, gehe nach Westen um sie zu öffnen!';
  first.west:=second;
  first.east:=third;

  item := TItem.create(1,'Test Item','Tür geöffnet');

  second.item:=item;
  second.descriptionBeforeRiddle:='Hier findest du das Item, um nach Osten zu gehen';
  second.east:=first;

  third.requiredItem:=1;
  third.descriptionBeforeRiddle:='Gewonnen!';

  changeRoom(start);

end;

end.

