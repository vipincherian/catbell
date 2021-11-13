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
unit timeralertform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LCLType, ExtCtrls, Buttons, ComCtrls, timerframe, {fgl,} dateutils, settings;

type

  { TfrmAlert }

  TfrmAlert = class(TForm)
    bbClose: TBitBtn;
    bbRestart: TBitBtn;
    cbFromFinish: TCheckBox;
    Label1: TLabel;
    //FTimer: TTimer;
    lsvMessages: TListView;
    procedure bbCloseClick(Sender: TObject);
    procedure bbRestartClick(Sender: TObject);
    procedure cbFromFinishChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lsvMessagesItemChecked(Sender: TObject; {%H-}Item: TListItem);
  private
    { private declarations }
    //FTimers: TTimerFrameList;
    procedure StopTimers;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure ReenableControls;
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
  //FTimers := TTimerFrameList.Create;
  bbRestart.Enabled := False;
  cbFromFinish.Checked := UserConfig.RestartFromFinish;

end;

procedure TfrmAlert.FormDestroy(Sender: TObject);
begin
  //FTimers.Clear;
  //FTimers.Free;
end;

procedure TfrmAlert.FormShow(Sender: TObject);
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
  //FTimer.Interval:=2000;
end;

procedure TfrmAlert.lsvMessagesItemChecked(Sender: TObject; Item: TListItem);
begin
  ReenableControls;
end;


procedure TfrmAlert.StopTimers;
var
  Timer: TfraTimer = nil;
  Item: TListItem;
begin
  //for Timer in FTimers do
  //begin
  //  Timer.Stop(True);
  //end;

  for Item in lsvMessages.Items do
  begin
    Timer := TfraTimer(Item.Data);
    Timer.Stop(True);
  end;
end;

procedure TfrmAlert.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or WS_THICKFRAME;
end;

procedure TfrmAlert.ReenableControls;
var
  Item: TListItem;
  AtLeastOneSelected: boolean = False;
begin
  for Item in lsvMessages.Items do
  begin
    if Item.Checked then
    begin
      AtLeastOneSelected := True;
      Break;
    end;
  end;
  bbRestart.Enabled := AtLeastOneSelected;
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
  //FTimers.Add(Timer);

  Duration := Timer.Duration;

  Hours := HourOf(Duration);
  Minutes := MinuteOf(Duration);
  Seconds := SecondOf(Duration);
  DurationText := Format('%.2d', [Hours]) + ':' + Format('%.2d', [Minutes]) +
    ':' + Format('%.2d', [Seconds]);

  lsvMessages.BeginUpdate;
  Item := lsvMessages.Items.Add;
  Item.Data := Timer;
  Item.Caption := Timer.Caption;
  Item.SubItems.Add(DurationText);

  lsvMessages.EndUpdate;
end;

procedure TfrmAlert.bbCloseClick(Sender: TObject);
begin
  StopTimers;
  Close;
end;

procedure TfrmAlert.bbRestartClick(Sender: TObject);
var
  Entry: TfraTimer;
  Item: TListItem;
  ErrorMessage: string;
  Failures: integer = 0;
begin
  Cursor := crHourGlass;
  StopTimers;
  //for Entry in FTimers do Entry.RestartFromLastFinish;
  ErrorMessage := '';
  for Item in lsvMessages.Items do
  begin
    if not Item.Checked then
      Continue;
    Entry := TfraTimer(Item.Data);
    if not Entry.RestartFromLastFinish then
    begin
      Inc(Failures);
      ErrorMessage += IntToStr(Failures) + '. ' + Entry.Caption + LineEnding;
    end;
  end;
  //ShowMessage('The following timers failed to restart:' + LineEnding + ErrorMessage);
  if Failures > 0 then
    MessageDlg('The following timers failed to restart: ' + LineEnding +
      '(time duration elapsed).' + LineEnding + ErrorMessage, mtError, [mbOK],
      0);
  Cursor := crDefault;
  Close;
end;

procedure TfrmAlert.cbFromFinishChange(Sender: TObject);
begin
  UserConfig.RestartFromFinish := cbFromFinish.Checked;
end;

procedure TfrmAlert.FormActivate(Sender: TObject);
begin

end;

procedure TfrmAlert.FormClose(Sender: TObject; var CloseAction: TCloseAction);
//var
//  Timer: TfraTimer = nil;
begin
  lsvMessages.BeginUpdate;
  lsvMessages.Items.Clear;
  lsvMessages.EndUpdate;

  ReenableControls;
  //FTimers.Clear;

end;

end.
