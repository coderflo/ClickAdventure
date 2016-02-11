unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TBagView }

  TBagView = class(TForm)
    Label1: TLabel;
    itemNameList: TListBox;
    procedure FormShow(Sender: TObject);
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

{ TBagView }

procedure TBagView.FormShow(Sender: TObject);
var
  i:integer;
begin

  itemNameList.Items.Clear;

  for i := 1 to Length(itemNames) do
  begin

    if itemNames[i] <> '' then
    begin

      itemNameList.Items.Add(itemNames[i]);

    end;

  end;

end;

end.

