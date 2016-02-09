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

end.

