unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs;

type
  TBagView = class(TForm)
  private
    { private declarations }
  public
    { public declarations }
    const itemNames : array[1..20] of string = ('','','','','','','','','','','','','','','','','','','','');
          // TODO: adjust max array size!
  end;

var
  BagView: TBagView;

implementation

{$R *.lfm}

end.

