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
unit timeralertform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LCLType, ExtCtrls, Buttons, ComCtrls, timerframe, fgl, dateutils;

type

  { TfrmAlert }

  TfrmAlert = class(TForm)
    bbClose: TBitBtn;
    Label1: TLabel;
    //FTimer: TTimer;
    lsvMessages: TListView;
    procedure bbCloseClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FTimers: TTimerFrameList;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    procedure AddTimer(Timer: TfraTimer);
    { public declarations }
  end;

var
  frmAlert: TfrmAlert;

implementation

{$R *.lfm}

{ TfrmAlert }

procedure TfrmAlert.FormCreate(Sender: TObject);
begin
  //AlphaBlend:=True;
  AlphaBlendValue := 50;
  FTimers := TTimerFrameList.Create;
end;

procedure TfrmAlert.FormDestroy(Sender: TObject);
begin
  FTimers.Clear;
  FTimers.Free;
end;

procedure TfrmAlert.FormShow(Sender: TObject);
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
  //FTimer.Interval:=2000;
end;

procedure TfrmAlert.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or WS_THICKFRAME;
end;

procedure TfrmAlert.AddTimer(Timer: TfraTimer);
var
  Item: TListItem;
  Hours: word;
  Minutes: word;
  Seconds: word;

  Duration: TDateTime;
  DurationText: string;
begin
  FTimers.Add(Timer);

  Duration := Timer.Duration;

  Hours := HourOf(Duration);
  Minutes := MinuteOf(Duration);
  Seconds := SecondOf(Duration);
  DurationText := Format('%.2d', [Hours]) + ':' + Format('%.2d', [Minutes]) +
    ':' + Format('%.2d', [Seconds]);

  lsvMessages.BeginUpdate;
  Item := lsvMessages.Items.Add;
  Item.Caption := Timer.Caption;
  Item.SubItems.Add(DurationText);

  lsvMessages.EndUpdate;
end;

procedure TfrmAlert.bbCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmAlert.FormActivate(Sender: TObject);
begin

end;

procedure TfrmAlert.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  Timer: TfraTimer = nil;
begin
  lsvMessages.BeginUpdate;
  lsvMessages.Items.Clear;
  lsvMessages.EndUpdate;

  for Timer in FTimers do
  begin
    Timer.Stop(True);
  end;
  FTimers.Clear;

end;

end.


