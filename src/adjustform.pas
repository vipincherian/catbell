{

Copyright (C) 2019 Vipin Cherian

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the
Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
Boston, MA  02110-1301, USA.

}
unit adjustform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  DateTimePicker;

type

  { TfrmTimerAdjust }

  TfrmTimerAdjust = class(TForm)
    bbApply: TBitBtn;
    cmbOptions: TComboBox;
    dtpAdjust: TDateTimePicker;
    procedure cmbOptionsChange(Sender: TObject);
  private

  public

  end;

var
  frmTimerAdjust: TfrmTimerAdjust;
const
  ADJUST_SHORTEN = 0;
  ADJUST_EXTEND = 1;
  ADJUST_STOPBY = 2;

implementation

{$R *.lfm}

{ TfrmTimerAdjust }

procedure TfrmTimerAdjust.cmbOptionsChange(Sender: TObject);
begin
  case cmbOptions.ItemIndex of
    ADJUST_SHORTEN:
      dtpAdjust.Kind := dtkTime;
    ADJUST_EXTEND:
      dtpAdjust.Kind := dtkTime;
    ADJUST_STOPBY:
      dtpAdjust.Kind := dtkDateTime;
  end;
end;

end.

