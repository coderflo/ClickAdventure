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

end;

{ TClickAdventure }

procedure TClickAdventure.onRoomNavigationButtonClicked(Sender: TObject);
begin

end;

end.

