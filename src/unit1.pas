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

    procedure onRoomNavigationButtonClicked(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

type
  TRoom = class
    // TODO: add field for backgroundImage
    north,east,south,west:TRoom;
    labelNorth,labelEast,labelSouth,labelWest:string;

  end;

var
  ClickAdventure: TClickAdventure;
  currentRoom: TRoom;

implementation

{$R *.lfm}

procedure changeRoom(room: TRoom);
begin

  // TODO: update GUI to new room ones

  // TODO: make buttons invisible and back visible only when riddle is solved
  ClickAdventure.buttonNorth.Visible := (room.north <> nil);
  ClickAdventure.buttonEast.Visible := (room.east <> nil);
  ClickAdventure.buttonSouth.Visible := (room.south <> nil);
  ClickAdventure.buttonWest.Visible := (room.west <> nil);

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

end.

