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
unit timerframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DateTimePicker, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, EditBtn, Dialogs, ActnList, dateutils, settings,
  editform, Graphics, Math, LazLogger, adjustform, sndfile, portaudio,
  ctypes, LMessages, LCLIntf;

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

  //UM_PLAY_AUDIO = LM_USER + 1;
  UM_FINISHED_AUDIO = LM_USER + 2;

//BUFFER_SIZE = 1024;

type

  TUserInfo = record
    //name : string[30];
    //age  : byte;
    SoundFile: PSndFile;
    Info: SF_INFO;
    Handle: THandle;
  end;
  PAudioInfo = ^TUserInfo;
  { TfraTimer }

  TfraTimer = class(TFrame)
    aiAdjust: TAction;
    aiEdit: TAction;
    aiStop: TAction;
    aiPause: TAction;
    aiPlay: TAction;
    alTimer: TActionList;
    bbPlay: TBitBtn;
    bbPause: TBitBtn;
    bbEdit: TBitBtn;
    bbStop: TBitBtn;
    bbAdjust: TBitBtn;
    cbSelect: TCheckBox;
    ckbIconProgress: TCheckBox;
    dtpSet: TDateTimePicker;
    edtTitle: TEdit;
    ilTimer: TImageList;
    imgTimer: TImage;
    lblCountdown: TLabel;
    procedure aiAdjustExecute(Sender: TObject);
    procedure aiEditExecute(Sender: TObject);
    procedure aiPauseExecute(Sender: TObject);
    procedure aiPlayExecute(Sender: TObject);
    procedure aiStopExecute(Sender: TObject);
    procedure bbAdjustClick(Sender: TObject);
    procedure ckbIconProgressChange(Sender: TObject);
    procedure dtpSetChange(Sender: TObject);
    procedure imgTimerClick(Sender: TObject);
    //procedure sbNotifyClick(Sender: TObject);
    //procedure UpdateNotifyButton;
  private
    { private declarations }
    FId: longword;

    FModalAlert: boolean;
    FTrayNotification: boolean;
    FTitleEditable: boolean;
    //FObservers: TTimerObserverList;
    //FShortTimer: TTimer;
    //FNotifier: boolean;

    FOrigTickDuration: longword;
    FStartTickCount: longword;
    FEndTickCount: longword;
    FPendingTickCount: longword;

    FPaused: boolean;
    FRunning: boolean;
    FAudioPlaying: boolean;

    FProgress: single;

    FAudioFile: string;
    FAudioLength: double;
    FSoundFile: PSndFile;

    FUserInfo: TUserInfo;
    FStream: PPaStream;

    FInfo: SF_INFO;

    //FAudioCallback: PPaStreamCallback;
    //FObservers: TListTimerObservers;
    //function GetShowProgressOnIcon: boolean;
    procedure SetId(AValue: longword);
    function GetCaption: string;
    function GetCounter: string;
    function GetDuration: TDateTime;
    function GetDurationEnabled: boolean;
    //function GetImageGreyed: boolean;
    function GetIsProgressOnIcon: boolean;
    function GetPauseButtonEnabled: boolean;
    function GetPlayButtonEnabled: boolean;
    function GetSelected: boolean;
    function GetStopButtonEnabled: boolean;
    procedure SetCaption(AValue: string);
    //procedure SetCounter(AValue: string);
    procedure SetDuration(AValue: TDateTime);
    procedure SetDurationEnabled(AValue: boolean);
    //procedure SetId(AValue: longword);
    //procedure SetImageGreyed(AValue: boolean);
    procedure SetIsProgressOnIcon(AValue: boolean);
    procedure SetModalAlert(AValue: boolean);
    procedure SetPauseButtonEnabled(AValue: boolean);
    procedure SetPlayButtonEnabled(AValue: boolean);
    //procedure SetShowProgressOnIcon(AValue: boolean);
    procedure SetStopButtonEnabled(AValue: boolean);
    function GetId: longword;
    procedure SetTitleEditable(AValue: boolean);
    procedure SetTrayNotification(AValue: boolean);
    procedure UpdateProgress(const PendingMilliseconds: longword);

    //procedure SetNotifier(AValue: boolean);
    {function GetTop: integer;
    procedure SetTop(AValue: integer);
    function GetHeight: integer;
    procedure SetHeight(AValue: integer);
    function GetWidth: integer;
    procedure SetWidth(AValue: integer);   }
  public
    { public declarations }
    {TODO: Review these two events and remove dynamic bindings}
    //OnNotifyClick: TNotifyEvent;
    //OnNotifyChange: TNotifyEvent;
    //OnProgressOnIconChanged: TNotifyEvent;
    //OnPlay: TNotifyEvent;
    //OnStop: TNotifyEvent;
    //OnPause: TNotifyEvent;
    //OnNotify: TNotifyEvent;
    //OnSelect: TNotifyEvent;
    //OnTimerStart: TNotifyEvent;
    //OnTimerPause: TNotifyEvent;
    //OnTimerStop: TNotifyEvent;

    LastProgressIconIndex: integer;
    //OnTimerProgressUpdate: TNotifyEvent;

    // Callback on progress-on-icon checkbox change only if
    // this variable is true. Used to avoid unending triggering of events.
    CallbackOnProgressOnIconChange: boolean;
    //procedure PlayClicked(Sender: TObject);
    //procedure Stopclicked(Sender: TObject);
    //procedure PauseClicked(Sender: TObject);
    //procedure NotifyClicked(Sender: TObject);
    procedure ClockSelected(Sender: TObject);
    procedure Hide;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Id: longword read FId write SetId;
    procedure SetCounter(AValue: string);
    procedure CheckForZeroTime;

    procedure HandleTimerTrigger();
    procedure Start;
    procedure Pause;
    procedure Stop(UserInitiated: boolean);
    //procedure NotifyChange(Sender: TObject);
    procedure Finish;
    procedure PublishProgress(Percent: single);
    //procedure AddSubscription(aObserver: ITimerObserver);
    //procedure RemoveSubscription(aObserver: ITimerObserver);
    procedure AdjustTimer(Sender: TObject);
    function SetAudioFile(AValue: string; out Error: string): boolean;
    procedure PlayAudio;
    procedure FinishedAudio(var Msg: TLMessage); message UM_FINISHED_AUDIO;

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
    //property ImageGreyed: boolean read GetImageGreyed write SetImageGreyed;
    property IsProgressOnIcon: boolean read GetIsProgressOnIcon
      write SetIsProgressOnIcon;
    //property ShowProgressOnIcon: boolean read GetShowProgressOnIcon write SetShowProgressOnIcon;
    property Selected: boolean read GetSelected;
    //property Id: longword read GetId write SetId;
    //property Top: integer read GetTop write SetTop;
    //property Height: integer read GetHeight write SetHeight;
    //property Width: integer read GetWidth write SetWidth;

    //property Notifier: boolean read FNotifier write SetNotifier;
    property Running: boolean read FRunning;
    property ModalAlert: boolean read FModalAlert write SetModalAlert;
    property TrayNotification: boolean read FTrayNotification write SetTrayNotification;
    property TitleEditable: boolean read FTitleEditable write SetTitleEditable;
    property Progress: single read FProgress;
    property AudioFile: string read FAudioFile;
    //function AudioCallback(const input: pointer; output: pointer; frameCount: culong; const timeInfo: PPaStreamCallbackTimeInfo; statusFlags: PaStreamCallbackFlags; userData: pointer): cint; cdecl;
  end;

//function AudioCallback(input: pointer; output: pointer; frameCount: culong; timeInfo: PPaStreamCallbackTimeInfo; statusFlags: PaStreamCallbackFlags; userData: pointer): cint; cdecl;
implementation

uses
  main;

{This function is called by PortAudio to request for audio data, and is passed
as a parameter while opening the stream.
It tries to read from the sound file and supply the data.
If read does not return any data, or if it is less than the number of frames
requested, it assumes tha the entire data has been exhausted and
it closes the sound file and returns paComplete.
This will stop the stream and the associated callback - which
triggers on stoppage of the steam - gets called.}

function FeedStream(input: pointer; output: pointer; frameCount: culong;
  timeInfo: PPaStreamCallbackTimeInfo; statusFlags: PaStreamCallbackFlags;
  userData: pointer): cint; cdecl;
var
  AudioInfo: PAudioInfo;
  //AudBuffer: pointer;
  readCount: cint;
begin
  //DebugLn('Inside audio callback');
  AudioInfo := PAudioinfo(userData);

  readCount := 0;
  readCount := sf_read_float(AudioInfo^.SoundFile, output, frameCount *
    (AudioInfo^.Info.channels));

  if readCount = (frameCount * AudioInfo^.Info.channels) then
  begin
    Result := cint(paContinue);
  end
  else
  begin
    //sf_close(AudioInfo^.SoundFile);
    Result := cint(paComplete);
  end;

end;

{This function is called by PortAudio to signal that the stream has stopped.
As this is a non-class function, it sends a message to frame using the frame's
handle.}
procedure StreamFinished(UserData: pointer); cdecl;
var
  AudioInfo: PAudioInfo;
begin
  //DebugLn('Inside streamFinished');
  AudioInfo := PAudioinfo(userData);

  PostMessage(AudioInfo^.Handle, UM_FINISHED_AUDIO, 0, 0);
end;

{$R *.lfm}

{ TfraTimer }


procedure TfraTimer.dtpSetChange(Sender: TObject);
begin
  CheckForZeroTime;
end;

procedure TfraTimer.aiPlayExecute(Sender: TObject);
begin
  //PlayClicked(Sender);
  Start;
end;

procedure TfraTimer.aiStopExecute(Sender: TObject);
begin
  //Stopclicked(Sender);
  Stop(True);
end;

procedure TfraTimer.bbAdjustClick(Sender: TObject);
begin

end;

procedure TfraTimer.ckbIconProgressChange(Sender: TObject);
begin
  //if not ckbIconProgress.Checked then
  //  Exit;
  if CallbackOnProgressOnIconChange then
  begin
    MainForm.HandleTimerFrameIconProgressChange(Self);
    {if OnProgressOnIconChanged <> nil then
    begin
      OnProgressOnIconChanged(Self);
    end
    else
      ShowMessage('Unexpected error at ' +
{$I %FILE%}
        +' ' +
{$I %LINE%}
        +': OnProgressOnIconChanged was found to be Nil');}
  end;
end;

procedure TfraTimer.aiPauseExecute(Sender: TObject);
begin
  //PauseClicked(Sender);
  Pause;
end;

procedure TfraTimer.aiEditExecute(Sender: TObject);
var
  //Hour, Min, Sec, Milli: word;
  ErrorText: string;
begin
  frmEditTimer.Description := edtTitle.Text;
  frmEditTimer.Duration := dtpSet.Time;
  frmEditTimer.TrayNotification := FTrayNotification;
  frmEditTimer.ModalAlert := FModalAlert;
  frmEditTimer.SetAudioFile(FAudioFile, ErrorText);
  if frmEditTimer.ShowForEdit(Self) then
  begin
    Caption := frmEditTimer.Description;
    dtpSet.Time := frmEditTimer.Duration;
    FTrayNotification := frmEditTimer.TrayNotification;
    FModalAlert := frmEditTimer.ModalAlert;
    //FAudioFile := frmEditTimer.AudioFile;
    SetAudioFile(frmEditTimer.AudioFile, ErrorText);
  end;
end;

procedure TfraTimer.aiAdjustExecute(Sender: TObject);
begin
  frmTimerAdjust.OnAdjust := @AdjustTimer;
  if not FPaused then
  begin
    if frmTimerAdjust.cmbOptions.Items.Count <= 2 then
      frmTimerAdjust.cmbOptions.Items.Add(ADJUST_STOPBY_TEXT);
  end
  else
  begin
    if frmTimerAdjust.cmbOptions.Items.Count = 3 then
      frmTimerAdjust.cmbOptions.Items.Delete(ADJUST_STOPBY);
  end;
  frmTimerAdjust.cmbOptions.ItemIndex := 0;
  frmTimerAdjust.bbApply.Enabled := True;
  frmTimerAdjust.dtpDiff.Show;
  frmTimerAdjust.dtpTill.Hide;
  frmTimerAdjust.Id := FId;
  frmTimerAdjust.ShowModal;
end;


procedure TfraTimer.imgTimerClick(Sender: TObject);
begin

end;

{procedure TfraTimer.sbNotifyClick(Sender: TObject);
begin
  //UpdateNotifyButton;
  if OnNotifyClick <> nil then
    OnNotifyClick(Self)
  else
    ShowMessage('Unexpected error at ' +
{$I %FILE%}
      +' ' +
{$I %LINE%}
      +': OnNotifyClick was found to be Nil');
end;}

{procedure TfraTimer.UpdateNotifyButton;
begin
  if sbNotify.Down then
    ilTimer.GetBitmap(TIMER_IMG_NOTIFY_YES, sbNotify.Glyph)
  else
    ilTimer.GetBitmap(TIMER_IMG_NOTIFY_NO, sbNotify.Glyph);
end;}


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

{function TfraTimer.GetShowProgressOnIcon: boolean;
begin
  Result:=ckbIconProgress.Checked;
end;}

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

{function TfraTimer.GetImageGreyed: boolean;
begin
  Result := imgTimer.Visible;
end;}

function TfraTimer.GetIsProgressOnIcon: boolean;
begin
  //Result := sbNotify.Down;
  Result := ckbIconProgress.Checked;
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

{procedure TfraTimer.SetImageGreyed(AValue: boolean);
begin
  ;{if AValue then
    ilTimer.GetBitmap(TIMER_IMG_GREY_TIMER, imgTimer.Picture.Bitmap)
  else
    ilTimer.GetBitmap(TIMER_IMG_COLOUR_TIMER, imgTimer.Picture.Bitmap);}
  //FFrame.imgTimer.Picture.Bitmap;
end;}

procedure TfraTimer.SetIsProgressOnIcon(AValue: boolean);
begin
  //sbNotify.Down := AValue;
  ckbIconProgress.Checked := AValue;
  //UpdateNotifyButton;
  //FNotifier:=AValue;
end;

procedure TfraTimer.SetModalAlert(AValue: boolean);
begin
  if FModalAlert = AValue then
    Exit;
  FModalAlert := AValue;
end;

procedure TfraTimer.SetPauseButtonEnabled(AValue: boolean);
begin
  bbPause.Enabled := AValue;
end;

procedure TfraTimer.SetPlayButtonEnabled(AValue: boolean);
begin
  bbPlay.Enabled := AValue;
end;

{procedure TfraTimer.SetShowProgressOnIcon(AValue: boolean);
begin
  ckbIconProgress.Checked:=AValue;
end;}

procedure TfraTimer.SetStopButtonEnabled(AValue: boolean);
begin
  bbStop.Enabled := AValue;
end;

function TfraTimer.GetId: longword;
begin
  Result := FId;
end;

procedure TfraTimer.SetTitleEditable(AValue: boolean);
begin
  FTitleEditable := AValue;
  edtTitle.ReadOnly := (not AValue);
  if Avalue then
    edtTitle.Color := clDefault
  else
    edtTitle.Color := clForm;
end;

procedure TfraTimer.SetTrayNotification(AValue: boolean);
begin
  if FTrayNotification = AValue then
    Exit;
  FTrayNotification := AValue;
end;

procedure TfraTimer.UpdateProgress(const PendingMilliseconds: longword);
var
  ProgressPercentage: single;
  CounterText: string;
  Seconds: integer;
  Minutes: integer;
  Hours: integer;
  Elapsed: longword;
begin
  Elapsed := Ceil(PendingMilliseconds / 1000);
  //DebugLn('Elapsed in ms is ' + IntToStr(PendingMilliseconds) + ' of ' + IntToStr(FOrigTickDuration));

  Seconds := Elapsed mod 60;
  Minutes := Elapsed div 60;
  Hours := Minutes div 60;
  Minutes := Minutes mod 60;
  CounterText := Format('%.2d', [Hours]) + ':' + Format('%.2d', [Minutes]) +
    ':' + Format('%.2d', [Seconds]);

  if Counter <> CounterText then
    Counter := CounterText;

  // Calculate percentage ProgressPercentage
  //if IsProgressOnIcon then
  //begin
  ProgressPercentage := 1 - (PendingMilliseconds / FOrigTickDuration);
  // Elapsed time can exceed total pending tick duration, in certain cases.
  // The system could go on sleep mode while a timer is running
  // (and while the timer event is being processed) and on
  // waking up, the pre-caculated tick duration could have already been
  // overshot. If that is the case, mark ProgressPercentage as zero, and the next
  // timer event will mark it as completed.
  if ProgressPercentage < 0 then
    ProgressPercentage := 0;
  Assert(ProgressPercentage <= 1);
  PUblishProgress(1 - (PendingMilliseconds / FOrigTickDuration));
  //end;
end;

{procedure TfraTimer.SetNotifier(AValue: boolean);
begin
  if FNotifier = AValue then
    Exit;
  FNotifier := AValue;
  IsProgressOnIcon := AValue;
end;}

{procedure TfraTimer.PlayClicked(Sender: TObject);
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
end;}

{procedure TfraTimer.NotifyClicked(Sender: TObject);
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
end;}

procedure TfraTimer.ClockSelected(Sender: TObject);
begin
  //ShowMessage('TTimerClockWidget.Selected');
  {if OnSelect <> nil then
  begin
    OnSelect(Self);
  end
  else
    ShowMessage('Unexpected error at ' +
{$I %FILE%}
      +' ' +
{$I %LINE%}
      +': OnSelect was found to be Nil');}
  MainForm.ClockSelected(Self);
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
  FProgress := 0.0;
  //teSet.Time := EncodeTime(0, GlobalDefault.TimerInitMins, 0, 0);
  with GlobalUserConfig do
  begin
    dtpSet.Time := EncodeTime(DefaultTimerHours, DefaultTimerMins,
      DefaultTimerSecs, 0);
    edtTitle.Text := DefaultTimerTitle;
  end;

  //  sbNotify.Glyph;
  //ilTimer.GetBitmap(TIMER_IMG_NOTIFY_NO, sbNotify.Glyph);
  //teSet.

  //OnPlay := nil;

  Parent := TWinControl(AOwner);
  //Name := Name + IntToStr(IdNew);
  //Id := IdNew;
  //sbPlay.OnClick := @PlayClicked;
  //sbStop.OnClick := @StopClicked;
  //sbPause.OnClick := @PauseClicked;
  //OnNotifyClick := @NotifyClicked;
  cbSelect.OnChange := @ClockSelected;
  //OnProgressOnIconChanged := nil;

  //ilTimer.GetBitmap(TIMER_IMG_GREY_TIMER, imgTimer.Picture.Bitmap);

  FRunning := False;
  FPaused := False;
  FAudioPlaying := False;
  //FNotifier := False;

  //FObservers := TListTimerObservers.Create;

  {FShortTimer := TTimer.Create(nil);
  FShortTimer.Interval := 200;
  FShortTimer.Enabled := False;
  FShortTimer.OnTimer := @HandleTimerTrigger;}

  bbPlay.Caption := '';
  bbPause.Caption := '';
  bbStop.Caption := '';
  bbAdjust.Caption := '';
  bbAdjust.Enabled := False;
  bbEdit.Caption := '';

  bbPlay.DoubleBuffered := True;
  bbPause.DoubleBuffered := True;
  bbStop.DoubleBuffered := True;
  bbAdjust.DoubleBuffered := True;
  bbEdit.DoubleBuffered := True;
  DoubleBuffered := True;

  CallbackOnProgressOnIconChange := True;

  FStream := nil;
  FSoundFile := nil;
end;

destructor TfraTimer.Destroy;
begin
  Parent := nil;
  //FShortTimer.Free;
  //FObservers.Free;
  if FSoundFile <> nil then
  begin
    sf_close(FSoundFile);
  end;
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

procedure TfraTimer.HandleTimerTrigger();
var
  PendingMilliseconds: longword;
  CurrTickCount: longword;
begin
  { If the countdown timer is not running, then default to 00:00:00}
  //if FRunning = False then
  //Counter := DEF_COUNTDOWN_CAPTION
  //FEndTickCount
  //else if FPaused = False then
  if FRunning and (FPaused = False) then
  begin
    CurrTickCount := GetTickCount64;

    if FEndTickCount <= CurrTickCount then
    begin
      {Stop(Self);
      //TODO: 0 is not okay
      //TODO: Title is hardcoded
      for Observer in FObservers do
        Observer.Finished(0, 'Countdown timer!', FWidget.Duration);}
      Counter := DEF_COUNTDOWN_CAPTION;
      Finish;
      Exit;
    end;
    PendingMilliseconds := FEndTickCount - CurrTickCount;
    UpdateProgress(PendingMilliseconds);

  end;
end;

procedure TfraTimer.Start;
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

  //FShortTimer.Enabled := True;
  if FPaused = False then
  begin
    FStartTickCount := GetTickCount64;

    FEndTickCount :=
      FStartTickCount + (((3600 * longword(Hours)) + (60 * longword(Minutes)) +
      longword(Seconds)) * 1000);

    FOrigTickDuration := FEndTickCount - FStartTickCount;
    if GlobalUserConfig.AutoProgress = True then
    begin
      IsProgressOnIcon := True;
      //NotifyChange(Self);
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
  //ImageGreyed := False;
  bbAdjust.Enabled := True;
  //end;

  {if OnTimerStart <> nil then
    OnTimerStart(Self);}
  MainForm.TimerStarted(Self);

  if frmEditTimer.Showing and (frmEditTimer.Id = FId) then
    frmEditTimer.dtpDuration.Enabled := True;

end;

procedure TfraTimer.Pause;
begin
  //ShowMessage('Pause');
  //FShortTimer.Enabled := False;

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
  //if OnTimerPause <> nil then
  //  OnTimerPause(Self);
  MainForm.TimerPaused(Self);

end;

procedure TfraTimer.Stop(UserInitiated: boolean);
var
  PaErrCode: PaError;
begin
  { The audio is playing and the user request is to terminate the audio.}
  if FAudioPlaying then
  begin
    FRunning := False;
    FPaused := False;
    PlayButtonEnabled := True;
    PauseButtonEnabled := False;
    StopButtonEnabled := False;
    DurationEnabled := True;
    //Counter := DEF_COUNTDOWN_CAPTION;
    bbAdjust.Enabled := False;

    PaErrCode := Pa_AbortStream(FStream);
    if (paErrCode <> Int32(paNoError)) then
    begin
      WriteLn('Pa_AbortStream failed ' + Pa_GetErrorText(paErrCode));
      WriteLn('Error after Pa_AbortStream ' + IntToHex(PaErrCode, 8));
    end;

    {There is no need to close the stream. Stopping/aborting the stream
    will trigger the callback for stream stoppage. The stream will be closed
    in that callback function}

    FAudioPlaying := False;
    Exit;
  end
  else
    {The timer run has completed. Stop the timer and play audio if required}
  begin
    FRunning := False;
    FPaused := False;
    Counter := DEF_COUNTDOWN_CAPTION;
    bbAdjust.Enabled := False;

    PublishProgress(TIMER_PROGRESS_FINISHED);

    FEndTickCount := 0;
    FOrigTickDuration := 0;
    FStartTickCount := 0;

    if frmEditTimer.Showing and (frmEditTimer.Id = FId) then
      frmEditTimer.dtpDuration.Enabled := True;

    if frmTimerAdjust.Showing and (frmTimerAdjust.Id = Fid) then
      frmTimerAdjust.bbApply.Enabled := False;

    PauseButtonEnabled := False;
    DurationEnabled := True;

    if (FAudioFile <> '') and (not UserInitiated) then
    begin
      Assert(FSoundFile <> nil);
      PlayButtonEnabled := False;
      StopButtonEnabled := True;
      PlayAudio;
    end
    else
    begin
      PlayButtonEnabled := True;
      StopButtonEnabled := False;
    end;
  end;

end;

{procedure TfraTimer.NotifyChange(Sender: TObject);
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
end;}

procedure TfraTimer.Finish;
{var
  Observer: ITimerObserver;}
begin

  //WriteLn('At TfraTimer.Finish');
  //DebugLn('Entering TfraTimer.Finish');
  //DebugLn('Entering TfraTimer.Finish. Timer ID - ' + InttoStr(FId));
  Stop(False);

  //if OnTimerStop <> nil then
  //  OnTimerStop(Self);

  MainForm.TimerFinished(Self);

  {for Observer in FObservers do
  begin
    Observer.TimerFinished(FId);
  end;}

  if IsProgressOnIcon then
    PublishProgress(TIMER_PROGRESS_FINISHED);

  //DebugLn('Exiting TfraTimer.Finish. Timer ID - ' + InttoStr(FId));
end;

procedure TfraTimer.PublishProgress(Percent: single);
{var
  Observer: ITimerObserver;}
begin
  //for Observer in FObservers do
  //  Observer.ProgressUpdate(Percent);
  FProgress := Percent;
  //if OnTimerProgressUpdate <> nil then
  //  OnTimerProgressUpdate(Self);
  MainForm.ProgressUpdate(Self, Percent);

end;

procedure TfraTimer.AdjustTimer(Sender: TObject);
var
  Hours, Mins, Secs: word;
  NewEndTickCount, NewPendingTickCount, CurrTickCount, Adjustment: longword;
  Diff: int64;
  EndTime: TDateTime;
begin

  // If the timer is not running, there is nothing to do
  // We should not even be in such a situation, but that is a different
  if not FRunning then
  begin
    frmTimerAdjust.Close;
    Exit;
  end;

  case frmTimerAdjust.cmbOptions.ItemIndex of
    ADJUST_SHORTEN:
      //frmTimerAdjust.dtpAdjust.Time:=;
    begin
      Hours := HourOf(frmTimerAdjust.dtpDiff.Time);
      Mins := MinuteOf(frmTimerAdjust.dtpDiff.Time);
      Secs := SecondOf(frmTimerAdjust.dtpDiff.Time);

      if (Hours = 0) and (Mins = 0) and (Secs = 0) then
      begin
        frmTimerAdjust.Close;
        Exit;
      end;

      Adjustment := (((3600 * longword(Hours)) + (60 * longword(Mins)) +
        longword(Secs)) * 1000);
      if FPaused = False then
      begin
        if Adjustment > FEndTickCount then
        begin
          if MessageDlg('Confirm', 'This will stop the timer',
            mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
            Exit;
          // Adjustment is too large and cannot be used for computations
          // As a circumvention, set NewEndTickCount to 0;
          NewEndTickCount := 0;
        end
        else
        begin
          NewEndTickCount := FEndTickCount - Adjustment;
          CurrTickCount := GetTickCount64;
          if CurrTickCount >= NewEndTickCount then
          begin
            if MessageDlg('Confirm', 'This will stop the timer',
              mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
              Exit;
          end;
        end;
        FEndTickCount := NewEndTickCount;
        HandleTimerTrigger();
      end
      else  // If paused
      begin
        if Adjustment > FPendingTickCount then
        begin
          if MessageDlg('Confirm', 'This will stop the timer',
            mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
            Exit;
          // Adjustment is too large and cannot be used for computations
          // As a circumvention, set NewEndTickCount to 0;
          NewPendingTickCount := 0;
        end
        else
        begin
          NewPendingTickCount := FPendingTickCount - Adjustment;
        end;
        FPendingTickCount := NewPendingTickCount;
        UpdateProgress(FPendingTickCount);
      end;
      frmTimerAdjust.Close;
    end;
    ADJUST_EXTEND:
    begin
      Hours := HourOf(frmTimerAdjust.dtpDiff.Time);
      Mins := MinuteOf(frmTimerAdjust.dtpDiff.Time);
      Secs := SecondOf(frmTimerAdjust.dtpDiff.Time);

      if (Hours = 0) and (Mins = 0) and (Secs = 0) then
      begin
        frmTimerAdjust.Close;
        Exit;
      end;

      Adjustment := (((3600 * longword(Hours)) + (60 * longword(Mins)) +
        longword(Secs)) * 1000);
      Inc(FOrigTickDuration, Adjustment);
      if FPaused = False then
      begin
        //FEndTickCount:= FEndTickCount + Adjustment;
        Inc(FEndTickCount, Adjustment);
        //FOrigTickDuration := FOrigTickDuration + Adjustment;
        HandleTimerTrigger();
      end
      else
      begin
        //FPendingTickCount:=FPendingTickCount + Adjustment;
        Inc(FPendingTickCount, Adjustment);
        //FOrigTickDuration := FOrigTickDuration + Adjustment;
        UpdateProgress(FPendingTickCount);
      end;
      frmTimerAdjust.Close;
    end;
    ADJUST_STOPBY:
    begin
      ;//Diff := MilliSecondsBetween(Now, frmTimerAdjust.dtpTill.DateTime);
      CurrTickCount := GetTickCount64;
      if CurrTickCount >= FEndTickCount then
        Exit;
      EndTime := IncMilliSecond(Now, (FEndTickCount - CurrTickCount));

      // If extension
      if EndTime < frmTimerAdjust.dtpTill.DateTime then
      begin
        Diff := MilliSecondsBetween(EndTime, frmTimerAdjust.dtpTill.DateTime);
        Inc(FEndTickCount, Diff);
        Inc(FOrigTickDuration, Diff);
      end
      else // If shortening
      begin
        Diff := MilliSecondsBetween(frmTimerAdjust.dtpTill.DateTime, EndTime);
        NewEndTickCount := FEndTickCount - Diff;
        if NewEndTickCount <= CurrTickCount then
        begin
          if MessageDlg('Confirm', 'This will stop the timer',
            mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
          begin
            Exit;
          end;
        end;
        FEndTickCount := NewEndTickCount;
      end;
      HandleTimerTrigger();
      frmTimerAdjust.Close;
    end;
  end;
end;

function TfraTimer.SetAudioFile(AValue: string; out Error: string): boolean;
  //var
  //Info: SF_INFO;
  //SoundFile: PSndFile;
begin
  Result := False;
  FInfo.format := 0;

  if AValue = '' then
  begin
    FAudioFile := '';
    FAudioLength := -1.0;
    ;
    //lblLengthText.Visible:=False;
    //edtAudioFile.Text:='';
    //lblLenthVal.Caption:=FloatToStr(RoundTo(FAudioLength, -2));
    //lblLenthVal.Visible:=False;
    Result := True;
    Exit;
  end;
  if FSoundFile <> nil then
    sf_close(FSoundFile);
  FSoundFile := sf_open(PChar(AValue), SFM_READ, @FInfo);
  if (FSoundFile = nil) then
  begin
    DebugLn('Error in sf_open');
    //sf_perror(nil);
    //ReadKey;
    //exit;
    Error := 'SoundFile is nil';
    Exit;
  end;
  DebugLn(IntToHex(FInfo.format, 8));
  DebugLn(IntToStr(FInfo.channels));
  DebugLn(IntToStr(FInfo.frames));
  DebugLn(IntToStr(FInfo.samplerate));
  DebugLn(IntToStr(FInfo.sections));
  FAudioLength := (FInfo.frames) / (FInfo.samplerate);
  //ShowMessage('length is ' + FloatToStr(AudioLength));

  FAudioFile := AValue;

  //sf_close(SoundFile);


  //lblLengthText.Visible:=True;
  //edtAudioFile.Text:=FAudioFile;
  //lblLenthVal.Caption:=FloatToStr(RoundTo(FAudioLength, -2));
  //lblLenthVal.Visible:=True;
  Result := True;

end;

procedure TfraTimer.PlayAudio;
var
  //SoundFile: PSndFile;
  //Info: SF_INFO;
  PaErrCode: PaError;
  StreamParams: PaStreamParameters;

begin
  DebugLn('Playing audio');
  FInfo.format := 0;
  {SoundFile := sf_open(PChar(FAudioFile), SFM_READ, @Info);
  if (SoundFile = nil) then
  begin
    DebugLn('Error in sf_open ');
    sf_perror(nil);
    //ReadKey;
    exit;
  end;}
  {DebugLn(IntToHex(Info.format, 8));
  DebugLn(IntToStr(Info.channels));
  DebugLn(IntToStr(Info.frames));
  DebugLn(IntToStr(Info.samplerate));
  DebugLn(IntToStr(Info.sections));}

  Assert(FSoundFile <> nil);

  if sf_seek(FSoundFile, 0, SEEK_SET) = -1 then
  begin
    DebugLn('Sf_seek returned error');
  end;

  StreamParams.device := Pa_GetDefaultOutputDevice();

  StreamParams.channelCount := FInfo.channels;
  StreamParams.sampleFormat := paFloat32;

  Streamparams.suggestedLatency :=
    Pa_GetDeviceInfo(StreamParams.device)^.defaultLowOutputLatency;
  StreamParams.hostApiSpecificStreamInfo := nil;
  DebugLn('Default device is ' + IntToStr(StreamParams.device));

  //Callback := @FeedStream;
  FUserInfo.SoundFile := FSoundFile;
  FUserInfo.Handle := Handle;
  Move(FInfo, FUserInfo.Info, SizeOf(SF_INFO));

  PaErrCode := Pa_OpenStream(@FStream, nil, @StreamParams, FInfo.samplerate,
    paFramesPerBufferUnspecified, paClipOff, PPaStreamCallback(@FeedStream),
    @FUserInfo);
  if (paErrCode <> Int32(paNoError)) then
  begin
    DebugLn('Pa_OpenStream failed ' + Pa_GetErrorText(paErrCode));
    DebugLn('Error after Pa_OpenStream ' + IntToHex(PaErrCode, 8));
  end;

  PaErrCode := Pa_SetStreamFinishedCallback(FStream,
    PPaStreamFinishedCallback(@StreamFinished));
  if (paErrCode <> Int32(paNoError)) then
  begin
    DebugLn('Pa_SetStreamFinishedCallback failed ' + Pa_GetErrorText(paErrCode));
    DebugLn('Error after Pa_SetStreamFinishedCallback ' + IntToHex(PaErrCode, 8));
  end;

  PaErrCode := Pa_StartStream(FStream);
  if (paErrCode <> Int32(paNoError)) then
  begin
    DebugLn('Pa_StartStream failed ' + Pa_GetErrorText(paErrCode));
    DebugLn('Error after Pa_StartStream ' + IntToHex(PaErrCode, 8));
  end;

  FAudioPlaying := True;

  //DebugLn('All went well');
  //DebugLn('Played audio');
end;

procedure TfraTimer.FinishedAudio(var Msg: TLMessage);
var
  PaErrCode: PaError;
begin
  //DebugLn('Inside finished audio ');

  PlayButtonEnabled := True;
  PauseButtonEnabled := False;
  StopButtonEnabled := False;
  DurationEnabled := True;
  //ImageGreyed := True;
  //Counter := DEF_COUNTDOWN_CAPTION;
  bbAdjust.Enabled := False;

  FAudioPlaying := False;


  {This check might be redundant. Just to be safe}
  paErrCode := Pa_IsStreamStopped(FStream);
  if paErrCode = 0 then
  begin
    paErrCode := Pa_StopStream(FStream);
    if (paErrCode <> Int32(paNoError)) then
    begin
      DebugLn('Pa_StopStream failed ' + Pa_GetErrorText(paErrCode));
      DebugLn('Error after Pa_StopStream ' + IntToHex(PaErrCode, 8));
    end;
  end
  else if PaErrCode <> 1 then
  begin
    DebugLn('Pa_IsStreamStopped failed ' + Pa_GetErrorText(paErrCode));
    DebugLn('Error after Pa_IsStreamStopped ' + IntToHex(PaErrCode, 8));
  end;
  paErrCode := Pa_CloseStream(FStream);
  if (paErrCode <> Int32(paNoError)) then
  begin
    DebugLn('Pa_CloseStream failed ' + Pa_GetErrorText(paErrCode));
    DebugLn('Error after Pa_CloseStream ' + IntToHex(PaErrCode, 8));
  end;
  FStream := nil;
end;

{function TfraTimer.AudioCallback(const input: pointer; output: pointer;
  frameCount: culong; const timeInfo: PPaStreamCallbackTimeInfo;
  statusFlags: PaStreamCallbackFlags; userData: pointer): cint; cdecl;
begin
  DebugLn('Inside callback');
  Result := cint(paAbort);
end;}

{procedure TfraTimer.AddSubscription(aObserver: ITimerObserver);
begin
  FObservers.Add(aObserver);
end;

procedure TfraTimer.RemoveSubscription(aObserver: ITimerObserver);
begin
  FObservers.Remove(aObserver);
end;}

end.
