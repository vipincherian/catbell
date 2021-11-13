{

Copyright (C) 2021 Vipin Cherian

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
  DateTimePicker, dateutils, settings, constants;

type

  { TfrmAdjust }

  TfrmAdjust = class(TForm)
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
    Id: longword;
    { This usage of notify event is genuine, no need to clean up
    Some of the checks related to adjustment has to be executed at the
    parent timer frame. }
    OnAdjust: TNotifyEvent;
  end;

var
  frmAdjust: TfrmAdjust;


implementation

{$R *.lfm}

{ TfrmAdjust }

procedure TfrmAdjust.cmbOptionsChange(Sender: TObject);
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

procedure TfrmAdjust.FormCreate(Sender: TObject);
begin
  dtpDiff.Show;
  dtpTill.Hide;
  dtpTill.TimeFormat := TTimeFormat(UserConfig.DefaultTimeFormat);
  dtpTill.Left := dtpDiff.Left;

  dtpDiff.Time := UserConfig.AdjustDiff;
  SetTillDateTime;

  Id := longword(-1);
end;

procedure TfrmAdjust.FormShow(Sender: TObject);
begin
  dtpTill.TimeFormat := TTimeFormat(UserConfig.DefaultTimeFormat);
  dtpDiff.Time:=UserConfig.AdjustDiff;
  //dtpTill.Date:=Now;
  SetTillDateTime;
end;

procedure TfrmAdjust.SetTillDateTime;
begin
  dtpTill.DateTime := IncHour(Now, HourOf(UserConfig.AdjustCompleteby));
  dtpTill.DateTime := IncMinute(Now, MinuteOf(UserConfig.AdjustCompleteby));
  dtpTill.DateTime := IncSecond(Now, SecondOf(UserConfig.AdjustCompleteby))
end;

procedure TfrmAdjust.bbApplyClick(Sender: TObject);
begin
  if OnAdjust <> nil then
    OnAdjust(Self);
end;

procedure TfrmAdjust.bbCancelClick(Sender: TObject);
begin
  Close;
end;

end.
