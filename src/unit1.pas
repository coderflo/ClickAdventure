unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs;

type
  TClickAdventure = class(TForm)
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

implementation

{$R *.lfm}



end.

