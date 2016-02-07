unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TClickAdventure }

  TClickAdventure = class(TForm)
    buttonNorth: TButton; // north,east,... are code names only
    buttonEast: TButton;
    buttonWest: TButton;
    buttonSouth: TButton;
    roomDescription: TMemo;
    riddleText: TMemo;
    riddleButtonOne: TButton;
    riddleButtonTwo: TButton;
    riddleButtonThree: TButton;
    riddleButtonFour: TButton;

    procedure onRoomNavigationButtonClicked(Sender: TObject);
    procedure onRiddleButtonClicked(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

type
  TRoom = class
    // TODO: add field for backgroundImage
    descriptionBeforeRiddle:string;
    descriptionAfterRiddle:string;
    north,east,south,west:TRoom;
    labelNorth,labelEast,labelSouth,labelWest:string;

    riddleQuestion, riddleAnswer,
      riddleOptionOne, riddleOptionTwo,
        riddleOptionThree, riddleOptionFour:string;

  end;

type
  TItem = class
    id: integer;
    constructor create(i:integer);
    // TODO: add function to retrieve item image by id
  end;

var
  ClickAdventure: TClickAdventure;
  currentRoom: TRoom;

implementation

{$R *.lfm}

procedure onRiddleSolved();
begin

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

  // make room navigation buttons visible
  ClickAdventure.buttonNorth.Visible := (currentRoom.north <> nil);
  ClickAdventure.buttonEast.Visible := (currentRoom.east <> nil);
  ClickAdventure.buttonSouth.Visible := (currentRoom.south <> nil);
  ClickAdventure.buttonWest.Visible := (currentRoom.west <> nil);

  // update button label if necessary
  if(ClickAdventure.buttonNorth.Visible) then
  begin
    ClickAdventure.buttonNorth.Caption:=currentRoom.labelNorth;
  end;
  if(ClickAdventure.buttonEast.Visible) then
  begin
    ClickAdventure.buttonEast.Caption:=currentRoom.labelEast;
  end;
  if(ClickAdventure.buttonSouth.Visible) then
  begin
    ClickAdventure.buttonSouth.Caption:=currentRoom.labelSouth;
  end;
  if(ClickAdventure.buttonWest.Visible) then
  begin
    ClickAdventure.buttonWest.Caption:=currentRoom.labelWest;
  end;

end;

procedure changeRoom(room: TRoom);
begin

  currentRoom := room;

  // TODO: update GUI to new room ones

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

  if(room.riddleQuestion <> '') then
  begin

    // show riddle

    ClickAdventure.riddleText.Visible:=true;
    ClickAdventure.riddleText.Lines.Clear;
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
    onRiddleSolved();
  end;

end;

{ TItem }

constructor TItem.create(i:integer);
begin
  id := i;
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

end.

