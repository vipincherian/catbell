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
  DateTimePicker, dateutils;

type

  { TfrmTimerAdjust }

  TfrmTimerAdjust = class(TForm)
    bbApply: TBitBtn;
    bbCancel: TBitBtn;
    cmbOptions: TComboBox;
    dtpDiff: TDateTimePicker;
    dtpTill: TDateTimePicker;
    procedure bbApplyClick(Sender: TObject);
    procedure bbCancelClick(Sender: TObject);
    procedure cmbOptionsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure SetTillDateTime;
  public
    { This usage of notify event is genuine, no need to clean up
    Some of the checks related to adjustment has to be executed at the
    parent timer frame. }
    OnAdjust: TNotifyEvent;
  end;

var
  frmTimerAdjust: TfrmTimerAdjust;

const
  ADJUST_SHORTEN = 0;
  ADJUST_EXTEND = 1;
  ADJUST_STOPBY = 2;
  ADJUST_STOPBY_TEXT = 'Stop timer by';

implementation

{$R *.lfm}

{ TfrmTimerAdjust }

procedure TfrmTimerAdjust.cmbOptionsChange(Sender: TObject);
begin
  case cmbOptions.ItemIndex of
    ADJUST_SHORTEN:
    begin
      dtpDiff.Show;
      dtpTill.Hide;
    end;
    ADJUST_EXTEND:
    begin
      dtpDiff.Show;
      dtpTill.Hide;
    end;
    ADJUST_STOPBY:
    begin
      dtpDiff.Hide;
      dtpTill.Show;
      SetTillDateTime;
    end;
  end;
end;

procedure TfrmTimerAdjust.FormCreate(Sender: TObject);
begin
  dtpDiff.Show;
  dtpTill.Hide;
  dtpTill.Left := dtpDiff.Left;
  dtpDiff.Time:=EncodeTime(0, 5, 0, 0);
  SetTillDateTime;
end;

procedure TfrmTimerAdjust.FormShow(Sender: TObject);
begin
  SetTillDateTime;
end;

procedure TfrmTimerAdjust.SetTillDateTime;
begin
  dtpTill.DateTime := IncMinute(Now, 5);
end;

procedure TfrmTimerAdjust.bbApplyClick(Sender: TObject);
begin
  if OnAdjust <> nil then
    OnAdjust(Self);
end;

procedure TfrmTimerAdjust.bbCancelClick(Sender: TObject);
begin
  Close;
end;

end.
