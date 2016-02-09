unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TBagView }

  TBagView = class(TForm)
    itemStringList: TListBox;

  private
    { private declarations }
  public
    { public declarations }
  end;

var
  BagView: TBagView;

implementation

{$R *.lfm}

procedure clearItems();
begin
  BagView.itemStringList.Items.Clear;
end;

procedure addItem(name:string);
begin
  BagView.itemStringList.Items.Add(name);
end;

end.

