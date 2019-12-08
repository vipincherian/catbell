{

Copyright (C) 2017 Vipin Cherian

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
unit timerframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DateTimePicker, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, EditBtn, Dialogs, ActnList, dateutils, settings, observers;

const
  TIMER_IMG_GREY_TIMER: integer = 0;
  TIMER_IMG_COLOUR_TIMER: integer = 1;
  TIMER_IMG_NOTIFY_YES: integer = 2;
  TIMER_IMG_NOTIFY_NO: integer = 3;

  CLOCK_HEIGHT = 38;
  DEF_COUNTDOWN_CAPTION: string = '00:00:00';
  TIMER_PROGRESS_FINISHED: single = 2.0;

  TIMER_CONF_CLOCKS = 'clocks';
  TIMER_CONF_TIMERS = 'timers';
  TIMER_CONF_TITLE = 'timer_title';
  TIMER_CONF_TIME = 'time';
  TIMER_CONF_HOURS = 'hours';
  TIMER_CONF_MINUTES = 'minutes';
  TIMER_CONF_SECONDS = 'seconds';
  TIMER_CONF_NOTIFIER = 'notifier';
  //TIMER_CONF_ID = 'id';
  TIMER_CONF_COUNT = 'count';
  TIMER_CONF_ORDER = 'order';

type

  { TfraTimer }

  TfraTimer = class(TFrame, ITimerSubject)
    aiStop: TAction;
    aiPause: TAction;
    aiPlay: TAction;
    alTimer: TActionList;
    bbPlay: TBitBtn;
    bbPause: TBitBtn;
    bbEdit: TBitBtn;
    bbStop: TBitBtn;
    cbSelect: TCheckBox;
    dtpSet: TDateTimePicker;
    edtTitle: TEdit;
    ilTimer: TImageList;
    imgTimer: TImage;
    lblCountdown: TLabel;
    sbNotify: TSpeedButton;
    procedure aiPauseExecute(Sender: TObject);
    procedure aiPlayExecute(Sender: TObject);
    procedure aiStopExecute(Sender: TObject);
    procedure dtpSetChange(Sender: TObject);
    procedure imgTimerClick(Sender: TObject);
    procedure sbNotifyClick(Sender: TObject);
    procedure UpdateNotifyButton;
  private
    { private declarations }
    FId: longword;
    //FObservers: TTimerObserverList;
    FShortTimer: TTimer;
    FNotifier: boolean;

    FOrigTickDuration: longword;
    FStartTickCount: longword;
    FEndTickCount: longword;
    FPendingTickCount: longword;

    FPaused: boolean;
    FRunning: boolean;
    FObservers: TListTimerObservers;
    procedure SetId(AValue: longword);
    function GetCaption: string;
    function GetCounter: string;
    function GetDuration: TDateTime;
    function GetDurationEnabled: boolean;
    function GetImageGreyed: boolean;
    function GetNotifyEnabled: boolean;
    function GetPauseButtonEnabled: boolean;
    function GetPlayButtonEnabled: boolean;
    function GetSelected: boolean;
    function GetStopButtonEnabled: boolean;
    procedure SetCaption(AValue: string);
    //procedure SetCounter(AValue: string);
    procedure SetDuration(AValue: TDateTime);
    procedure SetDurationEnabled(AValue: boolean);
    //procedure SetId(AValue: longword);
    procedure SetImageGreyed(AValue: boolean);
    procedure SetNotifyEnabled(AValue: boolean);
    procedure SetPauseButtonEnabled(AValue: boolean);
    procedure SetPlayButtonEnabled(AValue: boolean);
    procedure SetStopButtonEnabled(AValue: boolean);
    function GetId: longword;
    procedure SetNotifier(AValue: boolean);
    {function GetTop: integer;
    procedure SetTop(AValue: integer);
    function GetHeight: integer;
    procedure SetHeight(AValue: integer);
    function GetWidth: integer;
    procedure SetWidth(AValue: integer);   }
  public
    { public declarations }
    OnNotifyClick: TNotifyEvent;
    OnNotifyChange: TNotifyEvent;
    OnPlay: TNotifyEvent;
    OnStop: TNotifyEvent;
    OnPause: TNotifyEvent;
    OnNotify: TNotifyEvent;
    OnSelect: TNotifyEvent;
    procedure PlayClicked(Sender: TObject);
    procedure Stopclicked(Sender: TObject);
    procedure PauseClicked(Sender: TObject);
    procedure NotifyClicked(Sender: TObject);
    procedure ClockSelected(Sender: TObject);
    procedure Hide;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Id: longword read FId write SetId;
    procedure SetCounter(AValue: string);
    procedure CheckForZeroTime;

    procedure OnShortTimer(Sender: TObject);
    procedure Start(Sender: TObject);
    procedure Pause(Sender: TObject);
    procedure Stop(Sender: TObject);
    procedure NotifyChange(Sender: TObject);
    procedure Finish;
    procedure PublishProgress(Percent: single);
    procedure AddSubscription(aObserver: ITimerObserver);
    procedure RemoveSubscription(aObserver: ITimerObserver);


    property PlayButtonEnabled: boolean read GetPlayButtonEnabled
      write SetPlayButtonEnabled;
    property PauseButtonEnabled: boolean read GetPauseButtonEnabled
      write SetPauseButtonEnabled;
    property StopButtonEnabled: boolean read GetStopButtonEnabled
      write SetStopButtonEnabled;
    property Counter: string read GetCounter write SetCounter;
    property Duration: TDateTime read GetDuration write SetDuration;
    property DurationEnabled: boolean read GetDurationEnabled write SetDurationEnabled;
    property Caption: string read GetCaption write SetCaption;
    property ImageGreyed: boolean read GetImageGreyed write SetImageGreyed;
    property NotifyEnabled: boolean read GetNotifyEnabled write SetNotifyEnabled;
    property Selected: boolean read GetSelected;
    //property Id: longword read GetId write SetId;
    //property Top: integer read GetTop write SetTop;
    //property Height: integer read GetHeight write SetHeight;
    //property Width: integer read GetWidth write SetWidth;

    property Notifier: boolean read FNotifier write SetNotifier;
    property Running: boolean read FRunning;
  end;


implementation

{$R *.lfm}

{ TfraTimer }


procedure TfraTimer.dtpSetChange(Sender: TObject);
begin
  CheckForZeroTime;
end;

procedure TfraTimer.aiPlayExecute(Sender: TObject);
begin
  PlayClicked(Sender);
end;

procedure TfraTimer.aiStopExecute(Sender: TObject);
begin
  Stopclicked(Sender);
end;

procedure TfraTimer.aiPauseExecute(Sender: TObject);
begin
  PauseClicked(Sender);
end;


procedure TfraTimer.imgTimerClick(Sender: TObject);
begin

end;

procedure TfraTimer.sbNotifyClick(Sender: TObject);
begin
  UpdateNotifyButton;
  if OnNotifyClick <> nil then
    OnNotifyClick(Self)
  else
    ShowMessage('Unexpected error at ' +
{$I %FILE%}
      +' ' +
{$I %LINE%}
      +': OnNotifyClick was found to be Nil');
end;

procedure TfraTimer.UpdateNotifyButton;
begin
  if sbNotify.Down then
    ilTimer.GetBitmap(TIMER_IMG_NOTIFY_YES, sbNotify.Glyph)
  else
    ilTimer.GetBitmap(TIMER_IMG_NOTIFY_NO, sbNotify.Glyph);
end;


procedure TfraTimer.SetId(AValue: longword);
begin
  Assert(AValue > 0);
  if FId > 0 then
    Exit;
  if FId = AValue then
    Exit;
  FId := AValue;
  Name := Name + IntToStr(AValue);
  //Id := IdNew;
end;

function TfraTimer.GetCaption: string;
begin
  Result := edtTitle.Text;
end;

function TfraTimer.GetCounter: string;
begin
  Result := lblCountdown.Caption;
end;

function TfraTimer.GetDuration: TDateTime;
begin
  Result := dtpSet.Time;
end;

function TfraTimer.GetDurationEnabled: boolean;
begin
  Result := dtpSet.Enabled;
end;

function TfraTimer.GetImageGreyed: boolean;
begin
  Result := imgTimer.Visible;
end;

function TfraTimer.GetNotifyEnabled: boolean;
begin
  Result := sbNotify.Down;
end;

function TfraTimer.GetPauseButtonEnabled: boolean;
begin
  Result := bbPause.Enabled;
end;

function TfraTimer.GetPlayButtonEnabled: boolean;
begin
  Result := bbPlay.Enabled;
end;

function TfraTimer.GetSelected: boolean;
begin
  Result := cbSelect.Checked;
end;

function TfraTimer.GetStopButtonEnabled: boolean;
begin
  Result := bbStop.Enabled;
end;

procedure TfraTimer.SetCaption(AValue: string);
begin
  edtTitle.Text := AValue;
end;

procedure TfraTimer.SetDuration(AValue: TDateTime);
begin
  dtpSet.Time := AValue;
  CheckForZeroTime;
end;

procedure TfraTimer.SetDurationEnabled(AValue: boolean);
begin
  dtpSet.Enabled := AValue;
end;

procedure TfraTimer.SetImageGreyed(AValue: boolean);
begin
  if AValue then
    ilTimer.GetBitmap(TIMER_IMG_GREY_TIMER, imgTimer.Picture.Bitmap)
  else
    ilTimer.GetBitmap(TIMER_IMG_COLOUR_TIMER, imgTimer.Picture.Bitmap);
  //FFrame.imgTimer.Picture.Bitmap;
end;

procedure TfraTimer.SetNotifyEnabled(AValue: boolean);
begin
  sbNotify.Down := AValue;
  UpdateNotifyButton;
end;

procedure TfraTimer.SetPauseButtonEnabled(AValue: boolean);
begin
  bbPause.Enabled := AValue;
end;

procedure TfraTimer.SetPlayButtonEnabled(AValue: boolean);
begin
  bbPlay.Enabled := AValue;
end;

procedure TfraTimer.SetStopButtonEnabled(AValue: boolean);
begin
  bbStop.Enabled := AValue;
end;

function TfraTimer.GetId: longword;
begin
  Result := FId;
end;

procedure TfraTimer.SetNotifier(AValue: boolean);
begin
  if FNotifier = AValue then
    Exit;
  FNotifier := AValue;
  NotifyEnabled := AValue;
end;

procedure TfraTimer.PlayClicked(Sender: TObject);
begin
  //ShowMessage('In Widget');
  if OnPlay <> nil then
  begin
    OnPlay(Self);
  end
  else
    ShowMessage('Unexpected error at ' +
{$I %FILE%}
      +' ' +
{$I %LINE%}
      +': OnPlay was found to be Nil');
end;

procedure TfraTimer.Stopclicked(Sender: TObject);
begin
  if OnStop <> nil then
  begin
    OnStop(Self);
  end
  else
    ShowMessage('Unexpected error at ' +
{$I %FILE%}
      +' ' +
{$I %LINE%}
      +': OnStop was found to be Nil');
end;

procedure TfraTimer.PauseClicked(Sender: TObject);
begin
  if OnPause <> nil then
  begin
    OnPause(Self);
  end
  else
    ShowMessage('Unexpected error at ' +
{$I %FILE%}
      +' ' +
{$I %LINE%}
      +': OnPause was found to be Nil');
end;

procedure TfraTimer.NotifyClicked(Sender: TObject);
begin
  if OnNotify <> nil then
  begin
    OnNotify(Self);
  end
  else
    ShowMessage('Unexpected error at ' +
{$I %FILE%}
      +' ' +
{$I %LINE%}
      +': OnNotify was found to be Nil');
end;

procedure TfraTimer.ClockSelected(Sender: TObject);
begin
  //ShowMessage('TTimerClockWidget.Selected');
  if OnSelect <> nil then
  begin
    OnSelect(Self);
  end
  else
    ShowMessage('Unexpected error at ' +
{$I %FILE%}
      +' ' +
{$I %LINE%}
      +': OnSelect was found to be Nil');
end;

procedure TfraTimer.Hide;
begin
  Top := Top + Height;
end;

{function TfraTimer.GetTop: integer;
begin
  Result := FFrame.Top;
end;

procedure TfraTimer.SetTop(AValue: integer);
begin
  FFrame.Top := AValue;
end;

function TfraTimer.GetHeight: integer;
begin
  Result := FFrame.Height;
end;

procedure TfraTimer.SetHeight(AValue: integer);
begin
  FFrame.Height := AValue;
end;

function TfraTimer.GetWidth: integer;
begin
  Result := FFrame.Width;
end;

procedure TfraTimer.SetWidth(AValue: integer);
begin
  FFrame.Width := AValue;
end; }

constructor TfraTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FId := 0;
  //teSet.Time := EncodeTime(0, GlobalDefault.TimerInitMins, 0, 0);
  with GlobalUserConfig do
  begin
    dtpSet.Time := EncodeTime(DefaultTimerHours, DefaultTimerMins,
      DefaultTimerSecs, 0);
    edtTitle.Text := DefaultTimerTitle;
  end;

  //  sbNotify.Glyph;
  ilTimer.GetBitmap(TIMER_IMG_NOTIFY_NO, sbNotify.Glyph);
  //teSet.

  OnPlay := nil;

  Parent := TWinControl(AOwner);
  //Name := Name + IntToStr(IdNew);
  //Id := IdNew;
  //sbPlay.OnClick := @PlayClicked;
  //sbStop.OnClick := @StopClicked;
  //sbPause.OnClick := @PauseClicked;
  OnNotifyClick := @NotifyClicked;
  cbSelect.OnChange := @ClockSelected;

  ilTimer.GetBitmap(TIMER_IMG_GREY_TIMER, imgTimer.Picture.Bitmap);

  FRunning := False;
  FPaused := False;
  FNotifier := False;

  FObservers := TListTimerObservers.Create;

  FShortTimer := TTimer.Create(nil);
  FShortTimer.Interval := 200;
  FShortTimer.Enabled := False;
  FShortTimer.OnTimer := @OnShortTimer;

  bbPlay.Caption:='';
  bbPause.Caption:='';
  bbStop.Caption:='';
end;

destructor TfraTimer.Destroy;
begin
  Parent := Nil;
  FShortTimer.Free;
  FObservers.Free;
  inherited Destroy;
end;

procedure TfraTimer.SetCounter(AValue: string);
begin
  lblCountdown.Caption := AValue;
end;

procedure TfraTimer.CheckForZeroTime;
var
  TempDuration: TDateTime;
  Hours: word;
  Minutes: word;
  Seconds: word;
begin
  TempDuration := dtpSet.Time;
  Hours := HourOf(TempDuration);
  Minutes := MinuteOf(TempDuration);
  Seconds := SecondOf(TempDuration);
  // If Hours, Minutes, Seconds, all are zero then disable play button
  bbPlay.Enabled := not ((Hours = 0) and (Minutes = 0) and (Seconds = 0));
end;

procedure TfraTimer.OnShortTimer(Sender: TObject);
var
  Elapsed: longword;
  ElapsedMilliseconds: longword;
  Hours: integer;
  Minutes: integer;
  Seconds: integer;
  //Observer: ITimerObserver;
  CounterText: string;
  Progress: single;
begin
  { If the countdown timer is not running, then default to 00:00:00}
  //if FRunning = False then
    //Counter := DEF_COUNTDOWN_CAPTION
  //FEndTickCount
  //else if FPaused = False then
  if FRunning and (FPaused = False) then
  begin
    ElapsedMilliseconds := FEndTickCount - GetTickCount64;
    Elapsed := ElapsedMilliseconds div 1000;
    if Elapsed <= 0 then
    begin
      {Stop(Self);
      //TODO: 0 is not okay
      //TODO: Title is hardcoded
      for Observer in FObservers do
        Observer.Finished(0, 'Countdown timer!', FWidget.Duration);}
      Finish;
      Exit;
    end;
    Seconds := Elapsed mod 60;
    Minutes := Elapsed div 60;
    Hours := Minutes div 60;
    Minutes := Minutes mod 60;
    CounterText := Format('%.2d', [Hours]) + ':' + Format('%.2d', [Minutes]) +
      ':' + Format('%.2d', [Seconds]);

    if Counter <> CounterText then
      Counter := CounterText;

    // Calculate percentage progress
    if FNotifier then
    begin
      Progress := 1 - (ElapsedMilliseconds / FOrigTickDuration);
      // Elapsed time can exceed total pending tick duration, in certain cases.
      // The system could go on sleep mode while a timer is running
      // (and while the timer event is being processed) and on
      // waking up, the pre-caculated tick duration could have already been
      // overshot. If that is the case, mark progress as zero, and the next
      // timer event will mark it as completed.
      if Progress < 0 then
        Progress := 0;
      Assert(Progress <= 1);
      PUblishProgress(1 - (ElapsedMilliseconds / FOrigTickDuration));
    end;

  end;
end;

procedure TfraTimer.Start(Sender: TObject);
var
  TempDuration: TDateTime;
  Hours: word;
  Minutes: word;
  Seconds: word;
  //Milliseconds: word;
begin
  TempDuration := Duration;

  Hours := HourOf(TempDuration);
  Minutes := MinuteOf(TempDuration);
  Seconds := SecondOf(TempDuration);

  if (Hours = 0) and (Minutes = 0) and (Seconds = 0) then
    Exit;

  FShortTimer.Enabled := True;
  if FPaused = False then
  begin
    FStartTickCount := GetTickCount64;

    FEndTickCount :=
      FStartTickCount + (((3600 * longword(Hours)) + (60 * longword(Minutes)) +
      longword(Seconds)) * 1000);

    FOrigTickDuration := FEndTickCount - FStartTickCount;
    if GlobalUserConfig.AutoProgress = True then
    begin
      Notifier:=True;
      NotifyChange(Self);
    end;
  end
  else
  begin
    FStartTickCount := GetTickCount64;
    FEndTickCount := FStartTickCount + FPendingTickCount;
  end;

  FRunning := True;
  FPaused := False;

 // with FWidget do
  //begin
    PlayButtonEnabled := False;
    PauseButtonEnabled := True;
    StopButtonEnabled := True;
    DurationEnabled := False;
    ImageGreyed := False;

  //end;

end;

procedure TfraTimer.Pause(Sender: TObject);
begin
  //ShowMessage('Pause');
  FShortTimer.Enabled := False;

  FPendingTickCount := FEndTickCount - GetTickCount64;
  if FPendingTickCount <= 0 then
    Finish;

  FPaused := True;
  //with FWidget do
  //begin
    PlayButtonEnabled := True;
    PauseButtonEnabled := False;
    StopButtonEnabled := True;
    DurationEnabled := False;
  //end;

end;

procedure TfraTimer.Stop(Sender: TObject);
begin

  FRunning := False;
  FPaused := False;
  FShortTimer.Enabled := False;



  //with FWidget do
  //begin
    PlayButtonEnabled := True;
    PauseButtonEnabled := False;
    StopButtonEnabled := False;
    DurationEnabled := True;
    ImageGreyed := True;
    Counter := DEF_COUNTDOWN_CAPTION;
  //end;

  if FNotifier then
    PublishProgress(TIMER_PROGRESS_FINISHED);

  FEndTickCount := 0;
  FOrigTickDuration := 0;
  FStartTickCount := 0;
end;

procedure TfraTimer.NotifyChange(Sender: TObject);
begin
  if OnNotifyChange <> nil then
  begin
    OnNotifyChange(Self);
    //ShowMessage('Change');
  end
  else
    ShowMessage('Unexpected error at ' +
{$I %FILE%}
      +' ' +
{$I %LINE%}
      +': OnNotifyChange was found to be Nil');
end;

procedure TfraTimer.Finish;
var
  Observer: ITimerObserver;
begin

  for Observer in FObservers do
  begin
    Observer.TimerFinished(FId);
  end;

  if Notifier then
    PublishProgress(TIMER_PROGRESS_FINISHED);
  Stop(Self);
end;

procedure TfraTimer.PublishProgress(Percent: single);
var
  Observer: ITimerObserver;
begin
  for Observer in FObservers do
    Observer.ProgressUpdate(Percent);

end;

procedure TfraTimer.AddSubscription(aObserver: ITimerObserver);
begin
  FObservers.Add(aObserver);
end;

procedure TfraTimer.RemoveSubscription(aObserver: ITimerObserver);
begin
  FObservers.Remove(aObserver);
end;

end.
