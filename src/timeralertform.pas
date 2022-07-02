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
  LCLType, ExtCtrls, Buttons, ComCtrls, timerframe, fgl, dateutils,
  settings, alertentryframe, sequence, log, DateTimePicker;

type
  TAlertEntryList = specialize TFPGList<TfraAlertEntry>;

  { TfrmAlert }

  TfrmAlert = class(TForm)
    bbClose: TBitBtn;
    hdrEntries: THeaderControl;
    Label1: TLabel;
    pnlMaster: TPanel;
    pnlEntries: TPanel;
    sbxEntries: TScrollBox;
    tmrAlert: TTimer;
    procedure bbCloseClick(Sender: TObject);
    //procedure bbRestartClick(Sender: TObject);
    //procedure cbFromFinishChange(Sender: TObject);
    //procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbxEntriesResize(Sender: TObject);
    procedure tmrAlertTimer(Sender: TObject);
    //procedure lsvMessagesItemChecked(Sender: TObject; {%H-}Item: TListItem);
  private
    { private declarations }
    FEntryIDGenerator: TSequence;
    FEntries: TAlertEntryList;
    //procedure StopTimers;
    procedure RestartTimer(Sender: TObject);
    procedure StartTimer(Sender: TObject);
    procedure RemoveAlert(Entry: TfraAlertEntry);
    procedure EmptyAlertsAndClose;
    procedure ResizeHeaderSections;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    //procedure ReenableControls;
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
  //AlphaBlendValue := 50;
  //bbRestart.Enabled := False;
  //cbFromFinish.Checked := UserConfig.RestartFromFinish;

  FEntryIDGenerator := TSequence.Create;
  FEntries := TAlertEntryList.Create;
end;

procedure TfrmAlert.FormDestroy(Sender: TObject);
begin
  FEntries.Free;
  FEntryIDGenerator.Free;
end;

procedure TfrmAlert.FormShow(Sender: TObject);
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TfrmAlert.sbxEntriesResize(Sender: TObject);
begin
  ResizeHeaderSections;
end;

procedure TfrmAlert.tmrAlertTimer(Sender: TObject);
var
  Open: integer = 0;
  Entry: TfraAlertEntry;
begin
  for Entry in FEntries do
  begin
    if Entry.bbRestart.Enabled then
      if not Entry.Timer.QueryRestartFromLastFinish then
        Entry.bbRestart.Enabled := False
      else
        Inc(Open);
  end;

  { Timer alert needs to be active only if there are entries that still have
  restart option enabled. }
  tmrAlert.Enabled := (Open > 0);
end;

//procedure TfrmAlert.lsvMessagesItemChecked(Sender: TObject; Item: TListItem);
//begin
//  ReenableControls;
//end;


//procedure TfrmAlert.StopTimers;
//var
//  Timer: TfraTimer = nil;
//  Item: TListItem;
//begin
//  for Item in lsvMessages.Items do
//  begin
//    Timer := TfraTimer(Item.Data);
//    Timer.Stop(True);
//  end;
//end;

procedure TfrmAlert.RestartTimer(Sender: TObject);
var
  Entry: TfraAlertEntry;
begin
  Entry := TfraAlertEntry(Sender);
  //ShowMessage('Got message');
  Entry.Timer.Stop(True);
  Entry.Timer.RestartFromLastFinish;
  RemoveAlert(Entry);
end;

procedure TfrmAlert.StartTimer(Sender: TObject);
var
  Entry: TfraAlertEntry;
begin
  Entry := TfraAlertEntry(Sender);
  //ShowMessage('Got message');
  Entry.Timer.Stop(True);
  Entry.Timer.Start;
  RemoveAlert(Entry);
end;

procedure TfrmAlert.RemoveAlert(Entry: TfraAlertEntry);
var
  Index: integer;
  PrevEntry, NextEntry: TfraAlertEntry;
begin
  Index := FEntries.IndexOf(Entry);

  Assert(Index >= 0);

  if Index < (FEntries.Count - 1) then
  begin
    NextEntry := FEntries.Items[Index + 1];
    if Index > 0 then
    begin
      PrevEntry := FEntries.Items[Index - 1];
      NextEntry.AnchorSide[akTop].Control := PrevEntry;
    end
    else
      NextEntry.AnchorSide[akTop].Control := nil;
  end;

  FEntries.Remove(Entry);
  //Entry.Free;
  { Cannot free Entry here, as we are in Entry's button's callback.
  Application.ReleaseComponent will do it asynchronously }
  Application.ReleaseComponent(Entry);
  pnlEntries.Refresh;

  if FEntries.Count = 0 then
  begin
    FEntryIDGenerator.Reset;
    Close;
  end;

end;

procedure TfrmAlert.EmptyAlertsAndClose;
var
  Item: TfraAlertEntry;
begin
  while FEntries.Count > 0 do
  begin
    Item := FEntries.First;
    Item.Timer.Stop(True);
    FEntries.Remove(Item);
    Item.Free;
  end;
  tmrAlert.Enabled := False;
  Close;
end;

procedure TfrmAlert.ResizeHeaderSections;
var
  Entry: TfraAlertEntry;
  TempRight: integer = 0;
  TempLeft: integer = 0;
begin
  if FEntries.Count <= 0 then
    Exit;

  Entry := FEntries.First;

  TempRight := Entry.stDescription.Left + Entry.stDescription.Width;
  Inc(TempRight, (Entry.stDuration.Left - TempRight) div 2);
  hdrEntries.Sections.Items[0].Width := TempRight;
  TempLeft := TempRight;

  TempRight := Entry.stDuration.Left + Entry.stDuration.Width;
  Inc(TempRight, (Entry.stCompletedAt.Left - TempRight) div 2);
  hdrEntries.Sections.Items[1].Width := TempRight - TempLeft;
  TempLeft := TempRight;

  TempRight := Entry.stCompletedAt.Left + Entry.stCompletedAt.Width;
  Inc(TempRight, (Entry.bbRestart.Left - TempRight) div 2);
  hdrEntries.Sections.Items[2].Width := TempRight - TempLeft;
  TempLeft := TempRight;

  hdrEntries.Sections.Items[3].Width := hdrEntries.ClientWidth - TempRight;

end;

procedure TfrmAlert.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or WS_THICKFRAME;
end;

//procedure TfrmAlert.ReenableControls;
//var
//  Item: TListItem;
//  AtLeastOneSelected: boolean = False;
//begin
//  for Item in lsvMessages.Items do
//  begin
//    if Item.Checked then
//    begin
//      AtLeastOneSelected := True;
//      Break;
//    end;
//  end;
//  bbRestart.Enabled := AtLeastOneSelected;
//end;

procedure TfrmAlert.AddTimer(Timer: TfraTimer);
var
  Item: TListItem;
  Hours: word;
  Minutes: word;
  Seconds: word;

  Duration: TDateTime;
  DurationText: string;

  Entry: TfraAlertEntry;
  Index: integer;

  Formatter: string = '';
begin
  Duration := Timer.Duration;

  Hours := HourOf(Duration);
  Minutes := MinuteOf(Duration);
  Seconds := SecondOf(Duration);
  DurationText := Format('%.2d', [Hours]) + ':' + Format('%.2d', [Minutes]) +
    ':' + Format('%.2d', [Seconds]);

  //lsvMessages.BeginUpdate;
  //Item := lsvMessages.Items.Add;
  //Item.Data := Timer;
  //Item.Caption := Timer.Caption;
  //Item.SubItems.Add(DurationText);

  //lsvMessages.EndUpdate;

  Entry := TfraAlertEntry.Create(sbxEntries, Timer);
  Entry.Name := Entry.Name + IntToStr(FEntryIDGenerator.NextVal());
  Entry.Parent := pnlEntries;

  Entry.Width := pnlEntries.ClientWidth;
  Entry.Left := 0;
  Entry.Anchors := [akTop, akLeft, akRight];

  Entry.stDescription.Caption := Timer.Caption;
  Entry.stDuration.Caption := DurationText;

  if UserConfig.DefaultTimeFormat = integer(tf24) then
    Formatter := 'hh:nn'
  else
    Formatter := 'hh:nn am/pm';

  Entry.stCompletedAt.Caption :=
    FormatDateTime(Formatter, Entry.Timer.LastCompletedAt);


  Entry.AnchorSide[akRight].Side := asrRight;
  Entry.AnchorSide[akRight].Control := pnlEntries;

  Entry.AnchorSide[akLeft].Side := asrLeft;
  Entry.AnchorSide[akLeft].Control := pnlEntries;

  Entry.OnTimerRestart := @RestartTimer;
  Entry.OnTimerStart := @StartTimer;

  {TODO: Thread safety}
  Index := FEntries.Add(Entry);

  if Index = 0 then
  begin
    Entry.AnchorSide[akTop].Side := asrTop;
    Entry.AnchorSide[akTop].Control := pnlEntries;
    ResizeHeaderSections;
  end
  else
  begin
    Entry.AnchorSide[akTop].Side := asrBottom;
    Entry.AnchorSide[akTop].Control := FEntries.Items[Index - 1];
  end;

  tmrAlert.Enabled := True;
end;

procedure TfrmAlert.bbCloseClick(Sender: TObject);
begin
  EmptyAlertsAndClose;
end;

//procedure TfrmAlert.bbRestartClick(Sender: TObject);
//var
//  Entry: TfraTimer;
//  Item: TListItem;
//  ErrorMessage: string;
//  Failures: integer = 0;
//begin
//  Cursor := crHourGlass;

//  try
//    StopTimers;
//    ErrorMessage := '';
//    for Item in lsvMessages.Items do
//    begin
//      if not Item.Checked then
//        Continue;
//      Entry := TfraTimer(Item.Data);
//      if cbFromFinish.Checked then
//      begin
//        if not Entry.RestartFromLastFinish then
//        begin
//          Inc(Failures);
//          ErrorMessage += IntToStr(Failures) + '. ' + Entry.Caption + LineEnding;
//        end;
//      end
//      else
//        Entry.Start;
//    end;

//    if Failures > 0 then
//      MessageDlg('The following timers failed to restart: ' +
//        LineEnding + '(time duration elapsed).' + LineEnding +
//        ErrorMessage, mtError, [mbOK],
//        0);
//  finally
//    Cursor := crDefault;
//  end;
//  Close;
//end;

//procedure TfrmAlert.cbFromFinishChange(Sender: TObject);
//begin
//  UserConfig.RestartFromFinish := cbFromFinish.Checked;
//end;

//procedure TfrmAlert.FormActivate(Sender: TObject);
//begin
//
//end;

procedure TfrmAlert.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  //lsvMessages.BeginUpdate;
  //lsvMessages.Items.Clear;
  //lsvMessages.EndUpdate;

  EmptyAlertsAndClose;
  //ReenableControls;
end;

end.
